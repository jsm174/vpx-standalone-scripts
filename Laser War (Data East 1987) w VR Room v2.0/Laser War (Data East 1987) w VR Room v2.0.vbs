'
' LL           AAAAA     SSSSSSS   EEEEEEEE   RRRRRRRR       WW     WW     AAAAA    RRRRRRRR 
' LL          AA   AA   SS     SS  EE         RR     RR      WW     WW    AA   AA   RR     RR
' LL         AA     AA  SS         EE         RR     RR      WW     WW   AA     AA  RR     RR
' LL         AA     AA  SS         EE         RR     RR      WW     WW   AA     AA  RR     RR
' LL         AAAAAAAAA   SSSSSSS   EEEEEEE    RRRRRRRR       WW     WW   AAAAAAAAA  RRRRRRRR
' LL         AA     AA         SS  EE         RR  RR         WW     WW   AA     AA  RR  RR
' LL         AA     AA         SS  EE         RR   RR        WW  W  WW   AA     AA  RR   RR
' LL         AA     AA  SS     SS  EE         RR    RR       WW  W  WW   AA     AA  RR    RR
' LLLLLLLLL  AA     AA   SSSSSSS   EEEEEEEEE  RR     RR       WWWWWWW    AA     AA  RR     RR   by Data East, 1987
'
'
' Development by Herweh 2019 for Visual Pinball 10
' Version 1.0
'
' Very special thanks to
' - Schreibi34: Thank you so much for the awesome laser tower and for cleaning up the plastic ramp
' - OldSkoolGamer: For providing me all the wonderful artwork he designed for the VP9 build
' - Francisco666: For the reworked playfield image of the VP9 build
'
' Special thanks to
' - OldSkoolGamer, ICPjuggla, Herweh (:-): For creating a very cool VP9 build from where I borrowed a few table elements
' - Flupper: For his plastic and wire ramp tutorial and for the flasher domes
' - Dark: For the gates primitives template and the flipper primitives from 'Jurassic Park'
' - Dark and nFozzy: For locknuts and screws
' - DJRobX, RothbauerW and maybe some other ones: For some code in my script like the SSF routines
' - Mark70, Thalamus and digable: For beta testing and a lot great helpful feedback. Without you guys the table wouldn't be what it is
' - VPX development team: For the continous improvement of VP
' - All I have forgotten. Please send me a PM if you think I should add you to the 'Thx' section here. My fault!
'
' Release notes:
' Version 1.0: Initial release
'

'******** Revisions done by UnclePaulie on Hybrid version 0.1 - 2.0 *********
' v0.01 Changed the desktop background image to something more simple
'		Changed tilt sensitivity to 6, and nudge to 1
'		Added FrameTimer and GameTimer; added display timers to them
'		Added hybrid mode for VR room, cabinet, and desktop
'		Added minimal VR room.  
'		Added VR elements (power cord, several options: clock, posters, topper)
' 		Added DTRails collection for left and right rails view in desktop mode
'		Added 4 ball options, dark, bright, and brightest, and original
'		Changed the desktop display digits intensity from 30 to 12.
' v0.02	Changed GIOverhead, GIOverheadBlue, GIOverheadRed and GIOverheadYellow to a flasher from a light.  Looked wierd in VR, and wasn't angled correctly.  
'		Removed old ball shadow method
'		Got rid of LetTheBallJump code.  VPW physics will replace.  Also removed old sound calls.
'		Adjusted flipper size slightly to match table images, and other data east tables of that era.  Also put VPW recommended physics settings.
'		Insesrted Fleep Sounds
' v0.03	Removed rollingtimer, removed debug mode code and ballcontroltimer, ballsearch, and manual ball control.
'		Removed old rollingtimer sub 
'		Removed startcontrol manual ball control trigger.  No longer needed.
'		Removed spinnertimer and added spinner sub to FrameTimer
'		Added all script code for Fleep sounds, VPW physics, flippers, etc.  
'		Removed GetBallID and lockedBallID, as not a part of updated code.
'		Completely changed the trough logic to handle balls on table
'		Changed the apron wall structure to create a trough
' 		No longer destroy balls.  Use a global gBOT throughout all of script.
'		Removed all the getballs calls and am only useing gBOT.  Will help eliminate stutter on slower CPU machines
' 		Added option to turn magnasave option on and off 
'		Updated to nFozzy / Roth flipper physics
'		Flipper shadows and options moved to FlipperVisualUpdate timer.  Removed graphicstimer.
'		VPW ball rolling sounds implemented
'		Added dynamic shadows solution to the solution created by iaakki, apophis, Wylte
'		Added knocker solenoid subroutine and KnockerPosition Primitive for sound
'		Enababled bumper sounds
'		Put gilvl in GI Sub
'		Added a ballcntover call in the SaucerAction sub.  Didn't want that kicker eject sound at start of game.
'		Added groove to plunger lane, and small ball release ramp to get up.  
'		Ensured all lights stayed within the playfield... could see lights outside of table in VR.
' v0.04	Moved the apron prim down from 75 to 20.  Was way too high.
'		Adjusted the VR cab Position
'		Aligned the triggers per the real table, and adjusted the height.  Also moved the plunger location slightly to the right to align correctly.
' 		Added sideblades to DTRails.  Didn't look right in VR 
'		Added a startbutton and animated it. Also animated plunger and flippers in VR.  
' v0.05	Modified the flippers slightly for size, angle, and physics.  Similar to other VPW data east tables of that era, and matching photos.
'		Tweaked physics of bumpers and slings slightly.
'		Removed the "aphysics" materials and replaced with zCol_ physics
'		Finished adding physics primitives for posts, sleeves, gates, rubber bands, etc for Fleep sounds and VPW physics
'		Changed initial material on rApronPlunger to "apron".  
' v0.06 Changed the physics on the ramps to VPW zCol physics.  (they were set at zero friction and other properties)
'		Adjusted the playfield physics and general table settings.
'		Increased sling shot force a little stronger
'		Changed the sensor switches on the top left rubberbands to the new physics collidable wall ZCol_RubberBand007 and 9.
'		Adjusted the level of table difficulty from 5 to 6, and then set at 90% difficulty (= slope of 5.9).  (was set at only 6.2)
'		Added an adjustable kick variance to each saucer.  SW25 - changed variance to 0, and set power a little lower (10 to 8), and angle (180 to 200).
' v0.07 Added animated VR backglass and lighting. Also the VR Digits, and stereo image.
' v0.08	Adjusted the physical upper left wall by red saucer to ensure the ball rolls off correctly and onto the left flipper
'		There was an Iron Maiden Virtual Time mod done to this table.  Ensuring the ROM sound comes on with this table.
'		Slight adjustment of bumper force from 10 to 11, and hit threshold of 1.6 from 1.8.
'		Added updated ramprolling sounds solution created by nFozzy and VPW. 
'		Added GI relay sounds
' v0.09	Added updated standup targets with VPW physics, original code created by Rothbauerw and VPW team
'		Adjusted the height of standup targets and hiddin bouncer walls. 
'		Adjusted the ball brightness GI level, and ensured new stand up targets worked in GI 
' v0.10 Added Flupper Domes.  Required removing significant portions out of initflasher sub, and other code mods.
'		Redid the VR Backglass flashers with new method.
'		Added dome flasher relay sounds, and adjusted volume of all relay sounds.
'		Adjusted the disablelighting of the flasher dome lights to fade with the same GI / GIStep like the rest of the table.
'		Made the flupper dome flare less bright.(from 0.3 to 0.1)
'		Adjusted the rubbers height up to align with posts.
'		In VR, you could see the 3 saucers light up through the walls. Changed material to active.
'		Put sensor walls in the holes between the posts in upper right side
'		Modified the backwall image slightly for around flasher domes
' v0.11 Added GI Bulb primitives (pBase,pBulb,pFiliment) to all the GI Bulbs.  The GI lights were a big "halo" in VR.
'		Added bulb bases for GI, added to a collection and adjust the material like rest of GI materials
'		Added option for desktop and cab sidewalls image (plywood or black)
'		Adjusted the cab pov.
' v0.12	Added plywood cutouts for the target, sling, and trigger holes.  Cut holes out of playfield.
'		Moved the flippers end angle to 68.  (was 65).  Was a little too high up.
' v0.13	Added 3D insert prims.  Updated the playfield for insert cutouts.  Added a text only ramp overlay.
' 		Added Lampz in code
'		Removed the pflightscount routines. No longer needed.
'		Put the text image for inserts at alpha = 25
'		Adjusting light insert values: started with default values: falloff 50, falloff power 2, intensity 5, scale mesh 10, transmit 0.5.
'		Adjusted all the light colors
'		Adjusted the Lampz.FadeSpeedDown(x) = 1/20 (VPW default is 1/40).
'		Blue inserts were too bright, darkened the material for those.
'		Lots of insert lighting adjustments
'		Made the inserts fade faster, as hard to see blink.  Lampz.fadespeedup to 1/3 and speed down to 1/8.  
'		Adjusted the inserts under the star triggers to look real.  Was just an image on playfield... now inserts.  Also the center explosion is an insert.
'		Added GI control to the new inserts under star triggers and explosion insert.
'		Changed the Playfield Material colors, and alpha level.  The overall colors weren't right.  Also adjusted day/night slider down some.
' v0.14	Some VR adjustments.
'		The WAR inserts needed to be increased, as well as yellow triangles.
'		Adjusted the insert blooms to different halo height, color, falloff, alloff power, intensity, speed, depth bias.
'		Added script to make the insert lights get brighter as GI gets darker  
' 		Adjusted insert light intensities: White = 8, Blue=8, Red=5, Green=5, Yellow=5, Star Trigger Inserts=3.5
' v0.15 Tomate fixed the primitives for the ramp and side curved metal walls. They didn't have correct faces, you can see through one side of them.
'		Adjusted two white insert prims intensity slightly down.
'		Adjusted the physics of the flippers slightly.  Also set the strenght at 2700.
' v0.16	Slight adjustment to bumpers, matching videos.
' 		Updated the DMD speakers / stereo light image and added GI to it.
'		A couple minor lighting adjustments
' v0.17	Sixtoe corrected the upper right peg/rubber location based on actual laser war rebuild screen shots.  Corrected ball rolling down drain too easily.
' v0.18	Minor cleanups, and ensured VR checked to not visible.
'		Updated the material on Playfield Outlines to match playfield. (for the insert text)
'		Adjusted the slingshot force down down to 4 (was 5), based on VPW feedback.
'		Reduced the power of the kickback as well as it's randomness addition; based on VPW feedback.
' v0.19	Added updated slingshot corrections based off of Apophis example.  (Thanks AstroNasty for pointing out).
'		Moved physical wall, rMetalWall5, to match wall primitive and Sixtoe's corrected rubbers.
' v0.20 Based on AstroNasty's testing and feedback, I was able to see a couple ball bounce areas... one on left and right orbit.
'		Had to move rMetalWall5 a bit more and reshape it slightly.  
'		Also had to adjust the left gate and associated rubbers, peg, and prim.  (rotated gate 2 degrees)
' v0.21	Backglass Flashers - Tied backglass flashers to solenoid 27 and 28, fade timers added and repositioned them to match real table. (leojreimroc)
' v2.0	Released version.  Slight adjustment to overall table lighting.

' Thank you to the VPW team, especially Tomate, AstroNasty, Sixtoe, PinStratsDan, Smaug, leojreimroc, and Rajo Joey for testing, feedback, and some updates!  


' -------------------------------------------------
' IF YOU HAVE PERFORMANCE ISSUES or LOW FRAME RATE:
' -------------------------------------------------
' Try disabling "Reflect Elements On Playfield". This will increase performance for lesser powerful systems, and increase frame rate.
' Try turning off dynamic shadows.
' Turn off In-gmae AO, Post-proc AA, and 4xAA on the table objects
' Try to set some of the objects to "Static rendering". Open layer 7, select all screws and/or locknuts and select "Static Rendering" in the options windows


Option Explicit
Randomize

Dim GIColorMod, EnableGI, EnableFlasher, EnableReflectionsAtBall, ShadowOpacityGIOff, ShadowOpacityGIOn, RampColorMod


'******************************************************************************************
'* TABLE OPTIONS **************************************************************************
'******************************************************************************************

const VR_Room = 0 ' 1 = VR Room; 0 = desktop or cab mode

	Dim cab_mode, DesktopMode: DesktopMode = LaserWar.ShowDT
	If Not DesktopMode and VR_Room=0 Then cab_mode=1 Else cab_mode=0

const BallBrightness = 2 '0 = dark, 1 = not as dark (Herweh's original ball), 2 = bright, 3 = brightest 
const sidewalls = 0 '0 = black, 1 = plywood (for cabinet and desktop only.  VR ONLY has black)
const cabsideblades = 1 '0 = off, 1 = on;  some users want sideblades in the cabinet, some don't.

' *** If using VR Room: ***

const CustomWalls = 0 	 'set to 0 for Modern Minimal Walls, floor, and roof, 1 for Sixtoe's original walls and floor
const WallClock = 1	  	 '1 Shows the clock in the VR room only   
const topper = 1		 '0 = Off 1= On - Topper visible in VR Room only
const poster = 1		 '1 Shows the flyer posters in the VR room only 
const poster2 = 1		 '1 Shows the flyer posters in the VR room only 


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
'   0 = White GI (default)
'   1 = Colored GI
GIColorMod = 0

' MAGNA SAVE MOD ADJUSTMENTS 
' Ability to adjust Color or Flipper Mod with Left / Right Magna Save buttons during gameplay
const MagnaOn = 0  '1 = Ability On, 0 = Ability Off

' SILVER OR BLACK RAMP MOD
'	0 = black
'	1 = silver
'   2 = random
RampColorMod = 1

' ENABLE/DISABLE GI (general illumination)
'	0 = GI is off
'	1 = GI is on (value is a multiplicator for GI intensity - decimal values like 0.7 or 1.33 are valid too)
EnableGI = 1

' ENABLE/DISABLE flasher
'	0 = Flashers are off
'	1 = Flashers are on
EnableFlasher = 1

' ENABLE/DISABLE insert reflections at the ball
'	0 = reflections are off
'	1 = reflections are on
EnableReflectionsAtBall = 1


' PLAYFIELD SHADOW INTENSITY DURING GI OFF OR ON (adds additional visual depth) 
' usable range is 0 (lighter) - 100 (darker)  
ShadowOpacityGIOff = 90
ShadowOpacityGIOn  = 65


'***************************************************************************************************************************************************************
'***************************************************************************************************************************************************************


' ****************************************************
' standard definitions
' ****************************************************

Const UseSolenoids 	= 2
Const UseLamps 		= 0
Const UseSync 		= 0
Const HandleMech 	= 0
Const UseGI			= 1


'***************************************************************************************************************************************************************
'***************************************************************************************************************************************************************

Const cDMDRotation 	= -1 			'-1 for No change, 0 - DMD rotation of 0?, 1 - DMD rotation of 90?
Const cGameName 	= "lwar_a83"	'ROM name
Const ballsize 		= 50
Const ballmass 		= 1
Const UsingROM = True	

'***********************

Const tnob = 3						'Total number of balls
Const lob = 0						'Locked balls

Dim tablewidth: tablewidth = LaserWar.width
Dim tableheight: tableheight = LaserWar.height
Dim i, MBall1, MBall2, MBall3, gBOT
Dim BIPL : BIPL = False				'Ball in plunger lane
dim bbgi  	'Ball Brightness GI

If Version < 10600 Then
	MsgBox "This table requires Visual Pinball 10.6 or newer!" & vbNewLine & "Your version: " & Replace(Version/1000,",","."), , "Laser War VPX"
End If

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package."
On Error Goto 0

LoadVPM "01550000", "DE.VBS", 3.26

If RampColorMod = 2 Then RampColorMod = Int(Rnd()*2)


' ****************************************************
' table init
' ****************************************************
Sub LaserWar_Init()
	vpmInit Me 
	With Controller
        .GameName 			= cGameName
        .SplashInfoLine 	= "Laser War (Data East 1987)"
		.Games(cGameName).Settings.Value("sound") = 1 ' Set sound (0=OFF, 1=ON)	
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
	vpmNudge.TiltObj		= Array(Bumper43, Bumper44, Bumper45, LeftSlingshot, RightSlingshot)
	


	'************  Trough	**************
	Set MBall1 = BallRelease.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set MBall2 = sw11.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set MBall3 = sw10.CreateSizedballWithMass(Ballsize/2,Ballmass)
	gBOT = Array(MBall1,MBall2, MBall3)

	Controller.Switch(10) = 1
	Controller.Switch(11) = 1
	Controller.Switch(12) = 1

	' init lights, flippers, bumpers, ...
	InitFlasher_orig
	InitGI True
	InitRamp
	bbgi = 0
	InitDomeLights
	
	' init timers
	PinMAMETimer.Interval 				= PinMAMEInterval
    PinMAMETimer.Enabled  				= True
	GITimer.Interval					= 15
	FlasherTimer_Orig.Interval				= 15
	InsertFlasherTimer.Interval			= 15
	InsertLightsFlasherTimer.Interval	= 15

' VR Backglass lighting
	If VR_Room = 1 Then
		SetBackglass
	End If

End Sub


Sub LaserWar_Paused() 	: Controller.Pause = True : End Sub
Sub LaserWar_UnPaused() : Controller.Pause = False : End Sub
Sub LaserWar_Exit()		: Controller.Stop : End Sub

'*******************************************
'  Timers
'*******************************************

' The game timer interval is 10 ms
Sub GameTimer_Timer()
	Cor.Update 						'update ball tracking
	RollingUpdate					'update rolling sounds
	saucers_on()
	DoSTAnim						'handle stand up target animations
End Sub

' The frame timer interval is -1, so executes at the display frame rate
Sub FrameTimer_Timer()
	FlipperVisualUpdate				'update flipper shadows and primitives
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
	SpinnerTimer
	UpdateBallBrightness
	If VR_Room = 1 Then
		DisplayTimerVR
	End If

	If VR_Room = 0 AND cab_mode = 0 Then
		DisplayTimer
	End If

End Sub


'***************************************************************************
' VR Plunger Code
'***************************************************************************

Sub TimerVRPlunger_Timer
  If PinCab_Shooter.Y < -25 then
       PinCab_Shooter.Y = PinCab_Shooter.Y + 5
  End If
End Sub

Sub TimerVRPlunger1_Timer
  PinCab_Shooter.Y = -115 + (5* Plunger.Position) -20
End Sub

' ****************************************************
' keys
' ****************************************************

Sub LaserWar_KeyDown(ByVal keycode)
    If keycode = PlungerKey Then 
		Plunger.Pullback
		SoundPlungerPull
		TimerVRPlunger.Enabled = True
		TimerVRPlunger1.Enabled = False
		PinCab_Shooter.Y = -115
	End If

	If keycode = LeftFlipperKey  Then 
		FlipperActivate LeftFlipper, LFPress
		Controller.Switch(47) = True
		Primary_flipperbuttonleft.X = Primary_flipperbuttonleft.X +10
	End If

	If keycode = RightFlipperKey Then
		FlipperActivate RightFlipper, RFPress
		Controller.Switch(46) = True
		Primary_flipperbuttonright.X = Primary_flipperbuttonright.X - 10
	End If

	if keycode = StartGameKey then 
		Primary_start_button.y= 1100 - 5
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
		If keycode = LeftMagnaSave Then RampColorMod = (RampColorMod + 1) MOD 2 : ResetRamp
		If keycode = RightMagnaSave Then GIColorMod = (GIColorMod + 1) MOD 2 : ResetGI
	End If

	If vpmKeyDown(keycode) Then Exit Sub
End Sub


Sub LaserWar_KeyUp(ByVal keycode)

    If keycode = PlungerKey Then 
		Plunger.Fire
		If BIPL = 1 Then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
		TimerVRPlunger.Enabled = False
		TimerVRPlunger1.Enabled = True
		PinCab_Shooter.Y = -160
	end if

	If keycode = LeftFlipperKey  Then
		FlipperDeActivate LeftFlipper, LFPress
		Controller.Switch(47) = False
		Primary_flipperbuttonleft.X = Primary_flipperbuttonleft.X -10
	End If
	If keycode = RightFlipperKey Then
		FlipperDeActivate RightFlipper, RFPress
		Controller.Switch(46) = False
		Primary_flipperbuttonright.X = Primary_flipperbuttonright.X +10
	End If

	if keycode = StartGameKey then 
		Primary_start_button.y= 1100
	End If

    If vpmKeyUp(keycode) Then Exit Sub
End Sub


' ****************************************************
' *** solenoids
' ****************************************************
' flasher
SolCallback(1) 			= "SolFlasherInsert 1," 		' explosion
SolCallback(2) 			= "SolFlasherInsert 2," 		' red hotdog
SolCallback(3) 			= "SolFlasherInsert 3,"			' yellow hotdog"
SolCallback(4) 			= "SolFlasherInsert 4," 		' blue hotdog"
SolCallback(5) 			= "SolFlasherCannon"			' white flasher in cannon tower
SolCallback(6) 			= "Flash6"						' yellow flasher
SolCallback(7) 			= "Flash7"						' red flasher
SolCallback(8) 			= "Flash8"						' blue flasher and right backwall flasher
SolCallback(25) 		= "SolFlasherRampMultiplier"	' ramp multiplier flasher and middle backwall flasher
SolCallback(26) 		= "SolFlasherGreenShield " 		' green shield and left backwall flasher
SolCallback(27)			= "Sol27"	
SolCallback(28)			= "Sol28"

' tools
SolCallback(9) 			= "SolBallRelease"
SolCallback(11) 		= "SolGI"
SolCallback(12) 		= "SolRedEject"
SolCallback(13) 		= "SolYellowEject"
SolCallback(14) 		= "SolBlueEject"
SolCallback(15) 		= "SolKickback"
SolCallback(16)         = "SolOuthole"
SolCallback(29) 		= "SolKnocker"

' flipper
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sLRFlipper) = "SolRFlipper"


'******************************************************
'			TROUGH BASED ON FOZZY'S
'******************************************************

Sub BallRelease_Hit():Controller.Switch(12) = 1: UpdateTrough: End Sub
Sub BallRelease_UnHit():Controller.Switch(12) = 0: UpdateTrough:End Sub
Sub sw11_Hit():Controller.Switch(11) = 1: UpdateTrough:End Sub
Sub sw11_UnHit():Controller.Switch(11) = 0: UpdateTrough:End Sub
Sub sw10_Hit():Controller.Switch(10) = 1:UpdateTrough:End Sub
Sub sw10_UnHit():Controller.Switch(10) = 0:UpdateTrough:End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If BallRelease.BallCntOver = 0 Then 
		sw11.kick 60, 10
	End If
	If sw11.BallCntOver = 0 Then sw10.kick 60, 10	
	Me.Enabled = 0
End Sub


' ******************************************************
' outhole, drain and ball release
' ******************************************************

Sub SolOuthole(Enabled)
	If Enabled Then 
		Drain.kick 60, 16
		UpdateTrough
	End If
End Sub

Sub SolBallRelease(Enabled)
	If Enabled Then 
		RandomSoundBallRelease BallRelease
		BallRelease.kick 60, 12
		UpdateTrough
	End If
End Sub

Sub Drain_Hit()
	Controller.Switch(13) = 1
	RandomSoundDrain Drain
End Sub

Sub Drain_UnHit()
	Controller.Switch(13) = 0
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
	pLeftFlipper.ObjRotZ  = LeftFlipper.CurrentAngle
	pRightFlipper.ObjRotZ = RightFlipper.CurrentAngle
	pLeftFlipperShadow.ObjRotZ  = LeftFlipper.CurrentAngle + 1
	pRightFlipperShadow.ObjRotZ = RightFlipper.CurrentAngle + 1
End Sub


' ****************************************************
' sling shots and animations
' ****************************************************
Dim LeftStep, RightStep

Sub LeftSlingShot_Slingshot()
	LS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 15
	RandomSoundSlingshotLeft pLeftSlingHammer
    LeftStep = 0
	LeftSlingShot.TimerInterval = 15
	LeftSlingShot_Timer
End Sub
Sub LeftSlingShot_Timer()
    Select Case LeftStep
		Case 0: LeftSling1.Visible = False : LeftSling3.Visible = True : pLeftSlingHammer.TransZ = -28 : LeftSlingShot.TimerEnabled = True
        Case 1: LeftSling3.Visible = False : LeftSling2.Visible = True : pLeftSlingHammer.TransZ = -10
        Case 2: LeftSling2.Visible = False : LeftSling1.Visible = True : pLeftSlingHammer.TransZ = 0 : LeftSlingShot.TimerEnabled = False
    End Select
    LeftStep = LeftStep + 1
End Sub

Sub RightSlingShot_Slingshot()
	RS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 14
	RandomSoundSlingshotRight pRightSlingHammer
    RightStep = 0
	RightSlingShot.TimerInterval = 15
	RightSlingShot_Timer
End Sub
Sub RightSlingShot_Timer()
    Select Case RightStep
		Case 0: RightSling1.Visible = False : RightSling3.Visible = True : pRightSlingHammer.TransZ = -28 : RightSlingShot.TimerEnabled = True
        Case 1: RightSling3.Visible = False : RightSling2.Visible = True : pRightSlingHammer.TransZ = -10
        Case 2: RightSling2.Visible = False : RightSling1.Visible = True : pRightSlingHammer.TransZ = 0 : RightSlingShot.TimerEnabled = False
    End Select
    RightStep = RightStep + 1
End Sub


' ****************************************************
' bumpers
' ****************************************************
Sub Bumper43_Hit() : vpmTimer.PulseSw 43 : RandomSoundBumperTop Bumper43 : End Sub
Sub Bumper44_Hit() : vpmTimer.PulseSw 44 : RandomSoundBumperBottom Bumper44 : End Sub
Sub Bumper45_Hit() : vpmTimer.PulseSw 45 : RandomSoundBumperTop Bumper45 : End Sub


' ****************************************************
' switches
' ****************************************************
' stand-up targets

Sub sw22_Hit
	STHit 22
End Sub
Sub sw22o_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw23_Hit
	STHit 23
End Sub
Sub sw23o_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw24_Hit
	STHit 24
End Sub
Sub sw24o_Hit
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



' top lanes
Sub sw40_Hit()   : Controller.Switch(40) = True  : End Sub
Sub sw40_Unhit() : Controller.Switch(40) = False : End Sub
Sub sw41_Hit()   : Controller.Switch(41) = True  : End Sub
Sub sw41_Unhit() : Controller.Switch(41) = False : End Sub
Sub sw42_Hit()   : Controller.Switch(42) = True  : End Sub
Sub sw42_Unhit() : Controller.Switch(42) = False : End Sub

' rollover stars
Sub sw26_Hit()   : Controller.Switch(26) = True  : End Sub
Sub sw26_Unhit() : Controller.Switch(26) = False : End Sub
Sub sw34_Hit()   : Controller.Switch(34) = True  : End Sub
Sub sw34_Unhit() : Controller.Switch(34) = False : End Sub
Sub sw39_Hit()   : Controller.Switch(39) = True  : End Sub
Sub sw39_Unhit() : Controller.Switch(39) = False : End Sub
Sub sw48_Hit()   : Controller.Switch(48) = True  : End Sub
Sub sw48_Unhit() : Controller.Switch(48) = False : End Sub
Sub sw49_Hit()   : Controller.Switch(49) = True  : End Sub
Sub sw49_Unhit() : Controller.Switch(49) = False : End Sub
Sub sw50_Hit()   : Controller.Switch(50) = True  : End Sub
Sub sw50_Unhit() : Controller.Switch(50) = False : End Sub

' kickback at left outlane
Sub sw17_Hit()   : Controller.Switch(17) = True  : End Sub
Sub sw17_Unhit() : Controller.Switch(17) = False : End Sub

' inlanes and outlanes
Sub sw18_Hit()   : Controller.Switch(18) = True  : End Sub
Sub sw18_Unhit() : Controller.Switch(18) = False : End Sub
Sub sw19_Hit()   : Controller.Switch(19) = True  : End Sub
Sub sw19_Unhit() : Controller.Switch(19) = False : End Sub
Sub sw20_Hit()   : Controller.Switch(20) = True  : End Sub
Sub sw20_Unhit() : Controller.Switch(20) = False : End Sub

' shooter lane
Sub sw52_Hit()   : Controller.Switch(52) = True  : End Sub   
Sub sw52_UnHit() : Controller.Switch(52) = False : isBallMovedStraightUp = False : End Sub

' ramp trigger
Sub sw29_Hit()   : Controller.Switch(29) = True  : End Sub
Sub sw29_Unhit() : Controller.Switch(29) = False : End Sub

' spinners
Sub sw27_Spin()  : vpmTimer.PulseSw 27 : SoundSpinner sw27 : End Sub
Sub sw51_Spin()  : vpmTimer.PulseSw 51 : SoundSpinner sw51 : End Sub


' wire loop triggers
Sub trOrbitLaneRampStart_Hit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub trOrbitWireRampStart_Hit()
	WireRampOff ' Turn off the Ramp Sound
End Sub

Sub trOrbitWireRampStart_UnHit()
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound
End Sub

Sub trOrbitRampEnd_Hit()
	WireRampOff ' Turn off the Ramp Sound
End Sub

Sub trShooterLaneRampStart_Hit()
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound
End Sub

Sub trShooterLaneRampEnd_Hit()
	WireRampOff ' Turn off the Ramp Sound
End Sub



Sub ZCol_RubberBand007_Hit() : vpmTimer.PulseSw 21 : ZCol_RubberBand007_Timer : End Sub
Sub ZCol_RubberBand007_Timer()
	If Not ZCol_RubberBand007.TimerEnabled Then ZCol_RubberBand007.TimerEnabled = 20 : ZCol_RubberBand007.TimerEnabled = True
	If rRubberBand5.Visible Then
		rRubberBand5.Visible  		= False
		rRubberBand5a.Visible 		= True
		SwitchSound sw50
	Else
		rRubberBand5.Visible  		= True
		rRubberBand5a.Visible 		= False
		ZCol_RubberBand007.TimerEnabled 	= False
	End If
End Sub
Sub ZCol_RubberBand009_Hit() : vpmTimer.PulseSw 21 : ZCol_RubberBand009_Timer : End Sub
Sub ZCol_RubberBand009_Timer()
	If Not ZCol_RubberBand009.TimerEnabled Then ZCol_RubberBand009.TimerEnabled = 20 : ZCol_RubberBand009.TimerEnabled = True
	If rRubberBand6.Visible Then
		rRubberBand6.Visible  		= False
		rRubberBand6a.Visible 		= True
		SwitchSound sw49
	Else
		rRubberBand6.Visible  		= True
		rRubberBand6a.Visible 		= False
		ZCol_RubberBand009.TimerEnabled 	= False
	End If
End Sub


Sub SwitchSound(switch)
	PlaySoundAtLevelStatic ("fx_sensor"), 1, switch
End Sub

' add some variation to the ball direction under the top lanes
Dim isBallMovedStraightUp : isBallMovedStraightUp = False
Sub sw40Dir_Hit(): MoveBallUnderTopLane ActiveBall : End Sub
Sub sw41Dir_Hit(): MoveBallUnderTopLane ActiveBall : End Sub
Sub sw42Dir_Hit(): MoveBallUnderTopLane ActiveBall : End Sub
Sub MoveBallUnderTopLane(movingBall)
	With movingBall
		Do While True
			.VelX = .VelX + Rnd() * 4 - 2
			If Abs(.VelX) > 0.1 Then Exit Do
			If Not isBallMovedStraightUp Then isBallMovedStraightUp = True : Exit Do
		Loop
	End With
End Sub


' ****************************************************
' rotating spinner
' ****************************************************
Sub SpinnerTimer()
	pSpinner27.RotX 		= 360 - sw27.CurrentAngle
    pSpinnerRod27.TransZ 	= -Sin(sw27.CurrentAngle * 2 * 3.14 / 360) * 5
    pSpinnerRod27.TransX 	= Sin((sw27.CurrentAngle - 90) * 2 * 3.14 / 360) * -5
	pSpinner51.RotX 		= 360 - sw51.CurrentAngle
    pSpinnerRod51.TransZ 	= -Sin(sw51.CurrentAngle * 2 * 3.14 / 360) * 5
    pSpinnerRod51.TransX 	= Sin((sw51.CurrentAngle - 90) * 2 * 3.14 / 360) * -5
End Sub


' ****************************************************
' saucer
' ****************************************************
Dim sw25Step : sw25Step = 0
Dim sw33Step : sw33Step = 0
Dim sw38Step : sw38Step = 0
Dim sw25Ball : Set sw25Ball = Nothing
Dim sw33Ball : Set sw33Ball = Nothing
Dim sw38Ball : Set sw38Ball = Nothing

Sub sw25_Hit()   : SoundSaucerLock: SlowDownBall ActiveBall : End Sub
Sub sw25_Unhit() : Controller.Switch(25) = False : End Sub
Sub sw33_Hit()   : SoundSaucerLock: SlowDownBall ActiveBall : End Sub
Sub sw33_Unhit() : Controller.Switch(33) = False : End Sub
Sub sw38_Hit()   : SoundSaucerLock: SlowDownBall ActiveBall : End Sub
Sub sw38_Unhit() : Controller.Switch(38) = False : End Sub

Sub SolRedEject(Enabled)
	If Enabled Then
		MoveHammer pSaucer25Hammer, 0
		sw25Step 			= 0
		sw25.TimerInterval 	= 11
		sw25.TimerEnabled 	= True
	End If
End Sub
Sub SolYellowEject(Enabled)
	If Enabled Then
		MoveHammer pSaucer33Hammer, 0
		sw33Step 			= 0
		sw33.TimerInterval 	= 11
		sw33.TimerEnabled 	= True
	End If
End Sub
Sub SolBlueEject(Enabled)
	If Enabled Then
		MoveHammer pSaucer38Hammer, 0
		sw38Step 			= 0
		sw38.TimerInterval 	= 11
		sw38.TimerEnabled 	= True
	End If
End Sub

Sub sw25_Timer()
	SaucerAction sw25Step, pSaucer25Hammer, sw25Ball, 25, sw25, 190, 6, 0 'red
	sw25Step = sw25Step + 1
End Sub
Sub sw33_Timer()
	SaucerAction sw33Step, pSaucer33Hammer, sw33Ball, 33, sw33, 80, 18, 10 'yellow
	sw33Step = sw33Step + 1
End Sub
Sub sw38_Timer()
	SaucerAction sw38Step, pSaucer38Hammer, sw38Ball, 38, sw38, 180, 11, 10 'blue
	sw38Step = sw38Step + 1
End Sub


Sub SaucerAction(step, pHammer, kBall, id, switch, kickangle, kickpower, kickvar)
	Select Case step
		Case 0		: MoveHammer pHammer, -1 : If switch.ballcntover = 1 Then SoundSaucerKick 1, switch
		Case 1		: MoveHammer pHammer, 16 : If Controller.Switch(id) Then KickBall kBall, kickangle, kickvar, kickpower, 5, 30
		Case 13		: Controller.Switch(id) = False
		Case 25,26,27,28,29 : MoveHammer pHammer, -2
		Case 30 	: MoveHammer pHammer,  0 : switch.TimerEnabled = False
		Case Else 	: ' nothing to do
	End Select
End Sub

Sub KickBall(kBall, kAngle, kAngleVar, kVel, kVelZ, kLiftZ)
	Dim rAngle
	rAngle 	= 3.14159265 * (kAngle + (kAngleVar / 2 - kAngleVar * Rnd()) - 90) / 180
	kVel	= kVel + (kVel/20 - kVel/10 * Rnd())
	kVelZ	= kVelZ + (kVelZ/20 - kVelZ/10 * Rnd())
	With kBall
		.Z = .Z + kLiftZ
		.VelZ = kVelZ
		.VelX = cos(rAngle) * kVel
		.VelY = sin(rAngle) * kVel
	End With
End Sub

Sub MoveHammer(hammer, rotZ)
	If rotZ = 0 Then
		hammer.RotZ = 0
	Else
		hammer.RotZ = hammer.RotZ + rotZ
	End If
End Sub

Sub SlowDownBall(actBall)
	With actBall
		If .VelY < 0 Then .VelY = .VelY / 2 : .VelX = .VelX / 2
	End With
End Sub



Sub saucers_on()
    Dim b
    For b = 0 to UBound(gBOT)

		' saucers sw25, sw33 and sw38
		If InCircle(gBOT(b).X, gBOT(b).Y, sw25.X, sw25.Y, 20) Then
			If Abs(gBOT(b).VelX) < 3 And Abs(gBOT(b).VelY) < 3 Then
				gBOT(b).VelX = gBOT(b).VelX / 3
				gBOT(b).VelY = gBOT(b).VelY / 3
				If Not Controller.Switch(25) Then
					Controller.Switch(25) 	= True
					Set sw25Ball 			= gBOT(b)
				End If
			End If
		End If
		If InCircle(gBOT(b).X, gBOT(b).Y, sw33.X, sw33.Y, 20) Then
			If Abs(gBOT(b).VelX) < 3 And Abs(gBOT(b).VelY) < 3 Then
				gBOT(b).VelX = gBOT(b).VelX / 3
				gBOT(b).VelY = gBOT(b).VelY / 3
				If Not Controller.Switch(33) Then
					Controller.Switch(33) 	= True
					Set sw33Ball 			= gBOT(b)
				End If
			End If
		End If
		If InCircle(gBOT(b).X, gBOT(b).Y, sw38.X, sw38.Y, 20) Then
			If Abs(gBOT(b).VelX) < 3 And Abs(gBOT(b).VelY) < 3 Then
				gBOT(b).VelX = gBOT(b).VelX / 3
				gBOT(b).VelY = gBOT(b).VelY / 3
				If Not Controller.Switch(38) Then
					Controller.Switch(38) 	= True
					Set sw38Ball 			= gBOT(b)
				End If
			End If
		End If
	Next
End Sub



' ****************************************************
' kick back
' ****************************************************
Sub SolKickback(Enabled)
   	Kickback.Enabled = Enabled
End Sub 

Dim kickbackBallVel : kickbackBallVel = 1
Dim kickbackBall    : Set kickbackBall = Nothing
Sub Kickback_Hit()

' Corner condition code if during multiball, more than one ball goes to the kickback 
  if Kickback.TimerEnabled  = True Then
	pKickback.TransY = 0
		Kickback.Kick 0, Int(30 + kickbackBallVel*1/4 + Rnd()*15)
	Kickback.TimerEnabled = False
  end If


	Kickback.TimerEnabled  = False
	kickbackBallVel  = BallVel(ActiveBall)
	Set kickbackBall = ActiveBall
	Kickback.TimerInterval = 40
   	Kickback.TimerEnabled  = True
End Sub

Sub Kickback_Timer()
	If Kickback.TimerInterval = 40 Then
		Kickback.Kick 0, Int(30 + kickbackBallVel*1/4 + Rnd()*15)
		kickbackBall.VelX = 0
		PlaySoundAtLevelStatic ("fx_solenoid"), 1, Kickback
		pKickback.TransY = 0
		Kickback.TimerInterval = 20
	ElseIf Kickback.TimerInterval = 20 Then
		pKickback.TransY = pKickback.TransY + 10
		If pKickback.TransY = 50 Then Kickback.TimerInterval = 500
	ElseIf Kickback.TimerInterval = 500 Then
		Kickback.TimerInterval = 50
	ElseIf Kickback.TimerInterval = 50 Then
		pKickback.TransY = pKickback.TransY - 5
		If pKickback.TransY = 0 Then Kickback.TimerInterval = 1000
	ElseIf Kickback.TimerInterval = 1000 Then
		Kickback.TimerEnabled = False
	End If
End Sub


'*******************************************
'  Knocker Solenoid
'*******************************************

Sub SolKnocker(Enabled)
	If enabled Then
		KnockerSolenoid
	End If
End Sub



' *********************************************************************
' lamps and illumination
' *********************************************************************
' inserts

Dim isGIOn

Sub InsertLightsFlasherTimer_Timer()
	Dim obj, setTimerOff
	setTimerOff = True
	For Each obj In InsertLightsFlasher
		If obj.IntensityScale = 0.93 Or obj.IntensityScale = 0.62 Or obj.IntensityScale = 0.31 Then
			obj.IntensityScale = obj.IntensityScale - 0.31
			If obj.IntensityScale > 0 Then setTimerOff = False
		End If
		If obj.IntensityScale = 0.1 Or obj.IntensityScale = 0.4 Or obj.IntensityScale = 0.7 Then
			obj.IntensityScale = obj.IntensityScale + 0.3
			If obj.IntensityScale < 1 Then setTimerOff = False
		End If
	Next
	If setTimerOff Then InsertLightsFlasherTimer.Enabled = False
End Sub

Dim isWhiteFlasherOn : isWhiteFlasherOn = False
Sub SetTowerLight(mode, isLightOn)
	Dim obj, coll
	Select Case mode
	Case 1
		If Not isWhiteFlasherOn Then
			For Each obj In GIRedTowerLights : obj.State = isLightOn : Next
		End If
	Case 2
		If Not isWhiteFlasherOn Then
			For Each obj In GIYellowTowerLights : obj.State = isLightOn : Next
		End If
	Case 3
		If Not isWhiteFlasherOn Then
			For Each obj In GIBlueTowerLights : obj.State = isLightOn : Next
		End If
	Case 4
		isWhiteFlasherOn = isLightOn
		If isLightOn Then
			For Each coll In Array(GIRedTowerLights,GIYellowTowerLights,GIBlueTowerLights)
				For Each obj In coll : obj.State = LightStateOff : Next
			Next
		End If
		For Each obj In GIWhiteTowerLights : obj.State = isLightOn : Next
	End Select
End Sub

' flasher
Const minFlasherNo = 1
Const maxFlasherNo = 13
ReDim fValue(maxFlasherNo,8) : For i = minFlasherNo To maxFlasherNo : fValue(i,0) = 0 : Next

Sub InitFlasher_orig()
	' playfield
	fValue(1,5) 		= Array(fLight1a,fLight1b)
	fValue(1,6) 		= InsertFlasher
	fValue(2,5) 		= Array(fLight2a,fLight2b,fLight2c,fLight2d)
	fValue(2,6) 		= InsertFlasher
	fValue(3,5) 		= Array(fLight3a,fLight3b,fLight3c,fLight3d)
	fValue(3,6) 		= InsertFlasher
	fValue(4,5) 		= Array(fLight4a,fLight4b,fLight4c,fLight4d)
	fValue(4,6) 		= InsertFlasher
	fValue(10,5) 		= Array(fLight9Aa,fLight9Ab,fLight9Ac)
	fValue(10,6) 		= InsertFlasher
	fValue(12,5) 		= Array(fLight10Aa,fLight10Ab,fLight10Ac)
	fValue(12,6) 		= InsertFlasher
	' start flasher timer
	FlasherTimer_Orig.Interval 		= 15
	InsertFlasherTimer.Interval = 15
	If Not FlasherTimer_Orig.Enabled Then FlasherTimer_Orig_Timer
End Sub


Sub SolFlasher(flasherNo, flasherValue)
	If EnableFlasher = 0 Then Exit Sub
	' set value
	fValue(flasherNo,0) = 100
	' start flasher timer
	If Not FlasherTimer_Orig.Enabled Then FlasherTimer_Orig_Timer
End Sub


Sub SolFlasherInsert(flasherNo, flasherValue)
	If EnableFlasher = 0 Then Exit Sub
	' set value
	fValue(flasherNo,0) = 100
	' start flasher timer
	If Not InsertFlasherTimer.Enabled Then InsertFlasherTimer_Timer
End Sub

Sub SolFlasherRampMultiplier(flasherValue)
	SolFlasherInsert 10, flasherValue
	Objlevel(2) = 1 : FlasherFlash2_Timer
	Sound_Flash_Relay 1, Flasherbase2
End Sub
Sub SolFlasherGreenShield(flasherValue)
	SolFlasherInsert 12, flasherValue
	Objlevel(1) = 1 : FlasherFlash1_Timer
	Sound_Flash_Relay 1, Flasherbase1
End Sub
Sub SolFlasherCannon(flasherValue)
	SetTowerLight 4, flasherValue
End Sub

Sub FlasherTimer_Orig_Timer()
	Dim ii, allZero, flashx3, matdim, obj
	allZero = True
	If Not FlasherTimer_Orig.Enabled Then FlasherTimer_Orig.Enabled = True
	For ii = minFlasherNo To maxFlasherNo
		If (IsObject(fValue(ii,1)) Or IsArray(fValue(ii,1)) Or IsObject(fValue(ii,4)) Or IsArray(fValue(ii,4))) And fValue(ii,0) >= 0 Then
			allZero = False
			' decrease flasher value
			If fValue(ii,0) = 99 Then 
				fValue(ii,0) = 1
				allZero= True
			Else
				If fValue(ii,0) = 100 Then fValue(ii,0) = 1 Else fValue(ii,0) = fValue(ii,0) * fDecrease(fValue(ii,6)) - 0.01
			End If
			' calc values
			flashx3 = fValue(ii,0) ^ 3
			matdim 	= Round(10 * fValue(ii,0))
		End If
	Next
	If allZero Then FlasherTimer_Orig.Enabled = False
End Sub
Sub InsertFlasherTimer_Timer()
	Dim ii, allZero, fScale, obj
	allZero = True
	If Not InsertFlasherTimer.Enabled Then InsertFlasherTimer.Enabled = True
	For ii = minFlasherNo To maxFlasherNo
		If (IsObject(fValue(ii,5)) Or IsArray(fValue(ii,5))) And fValue(ii,0) >= 0 Then
			allZero = False
			' decrease flasher value
			If fValue(ii,0) = 100 Then fValue(ii,0) = 1 Else fValue(ii,0) = fValue(ii,0) - fDecrease(fValue(ii,6))
			' calc values
			fScale = fValue(ii,0)
			' set flasher object values
			If IsObject(fValue(ii,5)) Then fValue(ii,5).State = LightStateOn : fValue(ii,5).IntensityScale = IIF(fScale<=0,0,fScale)
			If IsArray(fValue(ii,5)) Then
				For Each obj In fValue(ii,5) : obj.State = LightStateOn : obj.IntensityScale = IIF(fScale<=0,0,fScale) : Next
			End If
		End If
	Next
	If allZero Then InsertFlasherTimer.Enabled = False
End Sub

Function fDecrease(fColor)
	If fColor = RedFlasher Then
		fDecrease = RedFlasherDecrease
	ElseIf fColor = BlueFlasher Then
		fDecrease = BlueFlasherDecrease
	ElseIf fColor = YellowFlasher Then
		fDecrease = YellowFlasherDecrease
	ElseIf fColor = InsertFlasher Then
		fDecrease = InsertFlasherDec
	Else
		fDecrease = WhiteFlasherDecrease
	End If
End Function

Dim WhiteFlasher, WhiteFlasherOpacity, WhiteFlasherDecrease
WhiteFlasher			= 1
WhiteFlasherOpacity		= 300
WhiteFlasherDecrease	= 0.9

Dim RedFlasher, RedFlasherOpacity, RedFlasherDecrease
RedFlasher				= 2
RedFlasherOpacity		= 1500
RedFlasherDecrease		= 0.85

Dim BlueFlasher, BlueFlasherOpacity, BlueFlasherDecrease
BlueFlasher				= 3
BlueFlasherOpacity		= 8000
BlueFlasherDecrease		= 0.85

Dim YellowFlasher, YellowFlasherOpacity, YellowFlasherDecrease
YellowFlasher			= 4
YellowFlasherOpacity	= 1000
YellowFlasherDecrease	= 0.9

Dim InsertFlasher, InsertFlasherDec
InsertFlasher			= 5
InsertFlasherDec		= 0.2


' general illumination
Sub ResetGI()
	InitGI False
End Sub
Sub InitGI(startUp)
	If startUp Then
		isGIOn = False
		SolGI False
	End If
	Dim coll, obj
	' init GI overhead
	For Each obj In Array(GIOverhead,GIOverheadRed,GIOverheadYellow,GIOverheadBlue)
		obj.IntensityScale 	= IIF(isGIOn,1,0)
		obj.visible  = 1
			If GIColorMod = 1 And Right(obj.Name,6) = "Yellow" Then
				obj.Color			= YellowOverhead
			ElseIf GIColorMod = 1 And Right(obj.Name,3) = "Red"  Then
				obj.Color			= RedOverhead
			ElseIf GIColorMod = 1 And Right(obj.Name,4) = "Blue" Then
				obj.Color			= BlueOverhead
			Else
				obj.Color			= WhiteOverhead
			End If
	Next
	' init GI bulbs
	For Each obj In GIBulbs
		obj.IntensityScale 	= IIF(isGIOn,1,0)
		obj.State			= LightStateOn
		If GIColorMod = 1 And Right(obj.Name,1) = "y" Then
			obj.Color		= YellowBulbs
			obj.ColorFull	= YellowBulbsFull
			obj.Intensity 	= YellowBulbsI * EnableGI
		ElseIf GIColorMod = 1 And Right(obj.Name,1) = "r" Then
			obj.Color		= RedBulbs
			obj.ColorFull	= RedBulbsFull
			obj.Intensity 	= RedBulbsI * EnableGI
		ElseIf GIColorMod = 1 And Right(obj.Name,1) = "b" Then
			obj.Color		= BlueBulbs
			obj.ColorFull	= BlueBulbsFull
			obj.Intensity 	= BlueBulbsI * EnableGI
		Else
			obj.Color		= WhiteBulbs
			obj.ColorFull 	= WhiteBulbsFull
			obj.Intensity 	= WhiteBulbsI * EnableGI
		End If
	Next
	GIBulb011y.Intensity = GIBulb011y.Intensity / 10
	GIBulb018y.Intensity = GIBulb018y.Intensity / 1000
	GIBulb019y.Intensity = GIBulb019y.Intensity / 1000
	' init GI lights
	For Each obj in GI
		obj.IntensityScale 	= IIF(isGIOn,1,0)
		obj.State			= LightStateOn
		If GIColorMod = 1 And Right(obj.Name,1) = "y" Then
			obj.Color		= Yellow
			obj.ColorFull	= YellowFull
			obj.Intensity 	= YellowI * EnableGI
		ElseIf GIColorMod = 1 And Right(obj.Name,1) = "r" Then
			obj.Color		= Red
			obj.ColorFull	= RedFull
			obj.Intensity 	= RedI * EnableGI
		ElseIf GIColorMod = 1 And Right(obj.Name,1) = "b" Then
			obj.Color		= Blue
			obj.ColorFull	= BlueFull
			obj.Intensity 	= BlueI * EnableGI
		Else
			obj.Color		= White
			obj.ColorFull	= WhiteFull
			obj.Intensity 	= WhiteI * EnableGI
		End If
	Next
	For Each obj In GIPlastics
		obj.IntensityScale 	= IIF(isGIOn,1,0)
		obj.State			= LightStateOn
		If GIColorMod = 1 And Right(obj.Name,1) = "y" Then
			obj.Color		= Yellow
			obj.ColorFull	= YellowFull
			obj.Intensity 	= YellowPlasticI * EnableGI * IIF(obj.TimerInterval=-2,0.5,1)
		ElseIf GIColorMod = 1 And Right(obj.Name,1) = "r" Then
			obj.Color		= Red
			obj.ColorFull	= RedFull
			obj.Intensity 	= RedPlasticI * EnableGI * IIF(obj.TimerInterval=-2,0.5,1)
		ElseIf GIColorMod = 1 And Right(obj.Name,1) = "b" Then
			obj.Color		= Blue
			obj.ColorFull	= BlueFull
			obj.Intensity 	= BluePlasticI * EnableGI * IIF(obj.TimerInterval=-2,0.5,1)
		Else
			obj.Color		= White
			obj.ColorFull	= WhiteFull
			obj.Intensity 	= WhitePlasticI * EnableGI * IIF(obj.TimerInterval=-2,0.5,1)
		End If
		' reduce the intensity a bit for second level plastics
		If obj.Surface = "105h" Then obj.Intensity = obj.Intensity / 5
	Next
	GIPlasticsLight013y.Intensity = GIPlasticsLight013y.Intensity / 2
	GIPlasticsLight014y.Intensity = GIPlasticsLight014y.Intensity / 2
	GIPlasticsLight015y.Intensity = GIPlasticsLight015y.Intensity / 2
	GIPlasticsLight016y.Intensity = GIPlasticsLight016y.Intensity / 2
	' init tower lights
	If startUp Then
		For Each obj In GIRedTowerLights
			obj.Intensity = obj.Intensity / 2
		Next
		For Each obj In GIYellowTowerLights
			obj.Intensity = obj.Intensity / 2
		Next
		For Each obj In GIBlueTowerLights
			obj.Intensity = obj.Intensity / 1.25
		Next
		For Each obj In GIWhiteTowerLights
			obj.Intensity = obj.Intensity * 2
		Next
	End If
	' init additional flasher for lights
	For Each obj In InsertLightsFlasher
		obj.IntensityScale 	= 0
	Next
End Sub

Dim GIDir : GIDir = 0
Dim GIStep : GIStep = 0
dim gilvl:gilvl = 1

Sub SolGI(IsOff)
	If EnableGI = 0 And Not isGIOn Then gilvl = 0 : Exit Sub

	If isGIOn <> (Not IsOff) Then
		isGIOn = Not IsOff
		If isGIOn Then
			' GI goes on
			gilvl = 1
			PinCab_Backglass.image="backglassimagelit"
			PinCab_Backglass.blenddisablelighting = .8
			PinCab_DMD.blenddisablelighting = .8
			Sound_GI_Relay 1, Relay_GI
			GIDir = 1 : GITimer_Timer
			DOF 101, DOFOn
		Else
			' GI goes off
			gilvl = 0
			PinCab_Backglass.image="backglassimage"
			PinCab_Backglass.blenddisablelighting = .3
			PinCab_DMD.blenddisablelighting = .1
			Sound_GI_Relay 0, Relay_GI
			GIDir = -1 : GITimer_Timer
			DOF 101, DOFOff
		End If
	End If
End Sub

Sub GITimer_Timer()
	If Not GITimer.Enabled Then GITimer.Enabled = True
	GIStep = GIStep + GIDir
	' set opacity of the shadow overlays and overhead GI illumination
	SetShadowOpacityAndGIOverhead
	' set GI illumination
	SetGIIllumination
	' set bumper lights
	SetBumperLights
	' set material of targets, posts, pegs, ramps etc
	SetMaterials
	' set ramps
	SetRamps
	' set GI Brightness for Ball
	SetBallGI
	' set GI Brightness for Flasher Domes
	SetDomeLights
	' set GI Brightness for Base Inserts that don't have lights embedded in them.
	SetInsertBaseGI
	' GI on/off goes in 4 steps so maybe stop timer
	If (GIDir = 1 And GIStep = 4) Or (GIDir = -1 And GIStep = 0) Then
		GITimer.Enabled = False
	End If
End Sub
Sub SetShadowOpacityAndGIOverhead()
	fGIOff.Opacity 	= (ShadowOpacityGIOff / 4) * (4 - GIStep)
	fGIOn.Opacity 	= (ShadowOpacityGIOn / 4) * GIStep
	' set GI overhead illumination
	GIOverhead.IntensityScale 		= GIStep/4
	GIOverheadRed.IntensityScale 	= IIF(GIColorMod = 1,GIStep/4,0)
	GIOverheadYellow.IntensityScale	= 0'IIF(GIColorMod = 1,GIStep/4,0)
	GIOverheadBlue.IntensityScale 	= IIF(GIColorMod = 1,GIStep/4,0)
End Sub
Sub SetGIIllumination()
	' set GI illumination
	Dim coll, obj
	For Each coll In Array(GI,GIPlastics,GIBulbs)
		For Each obj In coll
			If obj.TimerInterval <> -1 Then obj.IntensityScale = GIStep/4
		Next
	Next
End Sub
Sub SetBumperLights()
	Dim obj
	For Each obj In GIBumperLights
		obj.IntensityScale 	= GIStep/4
		obj.State 			= IIF(GIStep <= 0, LightstateOff, LightStateOn)
	Next
End Sub

Sub SetBallGI()
	bbgi= IIF(GIStep <= 0, 0, 1)
End Sub

Sub SetInsertBaseGI()
	Dim obj
	For Each obj In GIInsertBaseLight
		obj.intensity= GIStep/2+3
	Next
	ptriginsert1.blenddisablelighting =2*GIStep+Insert_Dl_On_Red-10
	ptriginsert2.blenddisablelighting =2*GIStep+Insert_Dl_On_Red-10
	ptriginsert3.blenddisablelighting =2*GIStep+Insert_Dl_On_Yellow-10
	ptriginsert4.blenddisablelighting =2*GIStep+Insert_Dl_On_Yellow-10
	ptriginsert5.blenddisablelighting =2*GIStep+Insert_Dl_On_Blue-10
	ptriginsert6.blenddisablelighting =2*GIStep+Insert_Dl_On_Blue-10
	pexp.blenddisablelighting = 2*GIStep+insert_dl_on_red+20
	pexpoff.blenddisablelighting = 2*GIStep+1
End Sub

dim fdbc 'flasherdomebrightnessconstant
fdbc = 6

Sub InitDomeLights
	Flasherbase1.blenddisablelighting=FlasherOffBrightness/fdbc
	Flasherbase2.blenddisablelighting=FlasherOffBrightness/fdbc
	Flasherbase3.blenddisablelighting=FlasherOffBrightness/fdbc
	Flasherbase4.blenddisablelighting=FlasherOffBrightness/fdbc
	Flasherbase5.blenddisablelighting=FlasherOffBrightness/fdbc
	Flasherbase6.blenddisablelighting=FlasherOffBrightness/fdbc
	Flasherbase7.blenddisablelighting=FlasherOffBrightness/fdbc
	Flasherbase8.blenddisablelighting=FlasherOffBrightness/fdbc
End Sub

Sub SetDomeLights()
	Flasherbase1.blenddisablelighting=giStep/fdbc
	Flasherbase2.blenddisablelighting=giStep/fdbc
	Flasherbase3.blenddisablelighting=giStep/fdbc
	Flasherbase4.blenddisablelighting=giStep/fdbc
	Flasherbase5.blenddisablelighting=giStep/fdbc
	Flasherbase6.blenddisablelighting=giStep/fdbc
	Flasherbase7.blenddisablelighting=giStep/fdbc
	Flasherbase8.blenddisablelighting=giStep/fdbc
End Sub

' save the insert intensities so they can be updated when GI is off
InitInsertIntensities

Sub InitInsertIntensities
	dim bulb
	for each bulb in InsertLightsInPrim
		bulb.uservalue = bulb.intensity
	next
End sub


Sub SetMaterials()
	UseGIMaterial GIRubbers, "Rubber White", GIStep
	UseGIMaterial GIWireTrigger, "Metal0.8", GIStep
	UseGIMaterial GILocknuts, "Metal Chrome S34", GIStep
	UseGIMaterial GIScrews, "Metal Chrome S34", GIStep
	UseGIMaterial GIPlasticScrews, "Plastic White", GIStep
	UseGIMaterial Array(pMetalWalls), "Metal Light", GIStep
	UseGIMaterial GIMetalChromes, "Metal Chrome S34", GIStep
	UseGIMaterial GIMetalPrims, "Metal S34", GIStep
	UseGIMaterial GIYellowPosts, "Plastic White", GIStep
	UseGIMaterial GIBlackPosts, "Plastic White", GIStep
	UseGIMaterial GIRedTargets, "TargetRed", GIStep
	UseGIMaterial GIYellowTargets, "TargetYellow", GIStep
	UseGIMaterial GIBlueTargets, "TargetBlue", GIStep
	UseGIMaterial GITargetsParts, "Metal S34", GIStep
	UseGIMaterial GIRedPlasticPegs, "TransparentPlasticRed", GIStep
	UseGIMaterial GIYellowPlasticPegs, "TransparentPlasticYellow", GIStep
	UseGIMaterial GIBluePlasticPegs, "TransparentPlasticBlue", GIStep
	UseGIMaterial GIGates, "Metal Wires", GIStep
	UseGIMaterial4Bumper GIBumpers, "Plastic White", GIStep
	UseGIMaterial GIBulbGlasses, "Lamps Glass", GIStep
	UseGIMaterial GIBulbBase, "BulbBase", GIStep
	UseGIMaterial Array(pSpinner27,pSpinner51), "Plastic with an image V", GIStep
	UseGIMaterial Array(pToplaneW,pToplaneWA,pToplaneAR,pToplaneR), "TransparentPlasticYellow", GIStep
	UseGIMaterial Array(pLevelPlate,pLevelPlate2), "Metal0.8", GIStep
	UseGIMaterial Array(pLeftFlipper,pRightFlipper), "Plastic with an image", GIStep
	UseGIMaterial Array(pApron,rApronPlunger), "Apron", GIStep
	UseGIMaterial Array(Plunger), "Plunger", GIStep
	UseGIMaterial Array(rSaucer38StopOnTop), "Metal Wires", GIStep
	UseGIMaterial Array(pBackboard), "Plastics Light", GIStep
End Sub
Sub SetRamps()
	pPlasticRamp.Material 	= CreateMaterialName("Prims platt" & IIF(RampColorMod=0, " Black", ""), GIStep)
	pPlasticTower.Material 	= CreateMaterialName("Prims platt OP", GIStep)
	pWireRamp.Material 		= CreateMaterialName("wireramp", GIStep)
End Sub

Sub UseGIMaterial(coll, matName, currentGIStep)
	Dim obj, mat
	mat = CreateMaterialName(matName, currentGIStep)
	For Each obj In coll : obj.Material = mat : Next
End Sub
Sub UseGIMaterial4Bumper(coll, matName, currentGIStep)
	Dim obj, mat
	mat = CreateMaterialName(matName, currentGIStep)
	For Each obj In coll : obj.SkirtMaterial = mat : Next
End Sub
Function CreateMaterialName(matName, currentGIStep)
	CreateMaterialName = matName & IIF(currentGIStep=0, " Dark", IIF(currentGIStep<4, " Dark" & currentGIStep,""))
End Function


' *********************************************************************
' colors
' *********************************************************************
Dim White, WhiteFull, WhiteI, WhiteP, WhitePlastic, WhitePlasticFull, WhitePlasticI, WhiteBumper, WhiteBumperFull, WhiteBumperI, WhiteBulbs, WhiteBulbsFull, WhiteBulbsI, WhiteOverheadFull, WhiteOverhead, WhiteOverheadI
WhiteFull = rgb(255,255,255) 
White = rgb(255,196,64) 'rgb(255,255,180)
WhiteI = 20
WhitePlasticFull = rgb(255,255,180) 
WhitePlastic = rgb(255,255,180)
WhitePlasticI = 25
WhiteBumperFull = rgb(255,255,180) 
WhiteBumper = rgb(255,255,180)
WhiteBumperI = 25
WhiteBulbsFull = rgb(255,255,180)
WhiteBulbs = rgb(255,255,180)
WhiteBulbsI = 10 * ShadowOpacityGIOff
WhiteOverheadFull = rgb(255,255,180)
WhiteOverhead = rgb(255,255,180)
WhiteOverheadI = .15

Dim Yellow, YellowFull, YellowI, YellowPlastic, YellowPlasticFull, YellowPlasticI, YellowBumper, YellowBumperFull, YellowBumperI, YellowBulbs, YellowBulbsFull, YellowBulbsI, YellowOverheadFull, YellowOverhead, YellowOverheadI
YellowFull = rgb(255,255,0)
Yellow = rgb(255,255,0)
YellowI = 5
YellowPlasticFull = rgb(255,255,0)
YellowPlastic = rgb(255,255,0)
YellowPlasticI = 40
YellowBumperFull = rgb(255,255,0)
YellowBumper = rgb(255,255,0)
YellowBumperI = 1
YellowBulbsFull = rgb(255,255,0)
YellowBulbs = rgb(255,255,0)
YellowBulbsI = 250 * ShadowOpacityGIOff
YellowOverheadFull = rgb(255,255,10)
YellowOverhead = rgb(255,255,10)
YellowOverheadI = 0.25

Dim Red, RedFull, RedI, RedPlastic, RedPlasticFull, RedPlasticI, RedBumper, RedBumperFull, RedBumperI, RedBulbs, RedBulbsFull, RedBulbsI, RedOverheadFull, RedOverhead, RedOverheadI
RedFull = rgb(255,75,75)
Red = rgb(255,75,75)
RedI = 50
RedPlasticFull = rgb(255,75,75)
RedPlastic = rgb(255,75,75)
RedPlasticI = 35
RedBumperFull = rgb(255,75,75)
RedBumper = rgb(255,128,32)
RedBumperI = 75
RedBulbsFull = rgb(255,75,75)
RedBulbs = rgb(255,75,75)
RedBulbsI = 250 * ShadowOpacityGIOff
RedOverheadFull = rgb(255,10,10)
RedOverhead = rgb(255,10,10)
RedOverheadI = 0.25

Dim Blue, BlueFull, BlueI, BluePlastic, BluePlasticFull, BluePlasticI, BlueBumper, BlueBumperFull, BlueBumperI, BlueBulbs, BlueBulbsFull, BlueBulbsI,  BlueOverheadFull, BlueOverhead, BlueOverheadI
BlueFull = rgb(75,75,255)
Blue = rgb(75,75,255)
BlueI = 100
BluePlasticFull = rgb(75,75,255)
BluePlastic = rgb(75,75,255)
BluePlasticI = 35
BlueBumperFull = rgb(0,0,255)
BlueBumper = rgb(0,0,255)
BlueBumperI = 50
BlueBulbsFull = rgb(75,75,255)
BlueBulbs = rgb(75,75,255)
BlueBulbsI = 125 * ShadowOpacityGIOff
BlueOverheadFull = rgb(10,10,255)
BlueOverhead = rgb(10,10,255)
BlueOverheadI = .8





Sub trShooterLaneSlowDown_Hit()
	If BallVel(ActiveBall) > 5 Then
		Dim var : var = 1 + RND() * 0.3
		With ActiveBall
			.VelX = .VelX / 2 * var : .VelY = .VelY / 2 * var
		End With
	End If
End Sub




' *************************************************************
' ramp look
' *************************************************************



Sub ResetRamp()
	InitRamp
End Sub
Sub InitRamp()
	If RampColorMod = 1 Then
		pPlasticRamp.Image	= "RampTextureSilver"
		pPlasticTower.Image = "RampTextureSilver"
	Else
		pPlasticRamp.Image	= "RampTextureBlack"
		pPlasticTower.Image = "RampTextureBlack"
	End If
	SetMaterials
End Sub


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


'****** Part C:  The Magic ******
Dim sourcenames, currentShadowCount, DSSources(30), numberofsources, numberofsources_hold
sourcenames = Array ("","","","","","","","")
currentShadowCount = Array (0,0,0,0)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!
dim objrtx1(4), objrtx2(4)
dim objBallShadow(4)
Dim BallShadowA


BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3)

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
Sub TargetBounce_Hit
	TargetBouncer activeball, 1
End Sub



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
'  FLIPPER POLARITY AND RUBBER DAMPENER AND SLINGSHOT CORRECTION SUPPORTING FUNCTIONS 
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
'		debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
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
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
	End If
End Sub


'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************




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
'****  PHYSICS DAMPENERS
'******************************************************

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

Function Maximum(obj1, obj2)
	If obj1 > obj2 Then
		Maximum = obj1
	Else
		Maximum = obj2
	End If
End Function


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
	For b = UBound(gBOT) + 1 to tnob
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

dim RampMinLoops : RampMinLoops = 4

' RampBalls
'      Setup:        Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RammBalls(6,2)
dim RampBalls(4,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(4)	

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


Dim ST22, ST23, ST24, ST30, ST31, ST32, ST35, ST36, ST37

'Set array with stand-up target objects


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


Set ST22 = (new StandupTarget)(sw22, psw22,22, 0)
Set ST23 = (new StandupTarget)(sw23, psw23,23, 0)
Set ST24 = (new StandupTarget)(sw24, psw24,24, 0)
Set ST30 = (new StandupTarget)(sw30, psw30,30, 0)
Set ST31 = (new StandupTarget)(sw31, psw31,31, 0)
Set ST32 = (new StandupTarget)(sw32, psw32,32, 0)
Set ST35 = (new StandupTarget)(sw35, psw35,35, 0)
Set ST36 = (new StandupTarget)(sw36, psw36,36, 0)
Set ST37 = (new StandupTarget)(sw37, psw37,37, 0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST22, ST23, ST24, ST30, ST31, ST32, ST35, ST36, ST37)




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
			STAction switch
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

Sub STAction(Switch)
	Select Case Switch
		Case 11:
		Case 12:
		Case 13:
	End Select
End Sub

'******************************************************
'		END STAND-UP TARGETS
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

Const RelayFlashSoundLevel = 0.315									'volume level; range [0, 1];
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






' *********************************************************************
' digital display
' *********************************************************************
Dim Digits(32)
Digits(0)  = Array(a_00, a_01, a_02, a_03, a_04, a_05, a_06, a_07, a_08, a_09, a_0a, a_0b, a_0c, a_0d, a_0e, a_0f)
Digits(1)  = Array(a_10, a_11, a_12, a_13, a_14, a_15, a_16, a_17, a_18, a_19, a_1a, a_1b, a_1c, a_1d, a_1e, a_1f)
Digits(2)  = Array(a_20, a_21, a_22, a_23, a_24, a_25, a_26, a_27, a_28, a_29, a_2a, a_2b, a_2c, a_2d, a_2e, a_2f)
Digits(3)  = Array(a_30, a_31, a_32, a_33, a_34, a_35, a_36, a_37, a_38, a_39, a_3a, a_3b, a_3c, a_3d, a_3e, a_3f)
Digits(4)  = Array(a_40, a_41, a_42, a_43, a_44, a_45, a_46, a_47, a_48, a_49, a_4a, a_4b, a_4c, a_4d, a_4e, a_4f)
Digits(5)  = Array(a_50, a_51, a_52, a_53, a_54, a_55, a_56, a_57, a_58, a_59, a_5a, a_5b, a_5c, a_5d, a_5e, a_5f)
Digits(6)  = Array(a_60, a_61, a_62, a_63, a_64, a_65, a_66, a_67, a_68, a_69, a_6a, a_6b, a_6c, a_6d, a_6e, a_6f)

Digits(7)  = Array(b_00, b_01, b_02, b_03, b_04, b_05, b_06, b_07, b_08, b_09, b_0a, b_0b, b_0c, b_0d, b_0e, b_0f)
Digits(8)  = Array(b_10, b_11, b_12, b_13, b_14, b_15, b_16, b_17, b_18, b_19, b_1a, b_1b, b_1c, b_1d, b_1e, b_1f)
Digits(9)  = Array(b_20, b_21, b_22, b_23, b_24, b_25, b_26, b_27, b_28, b_29, b_2a, b_2b, b_2c, b_2d, b_2e, b_2f)
Digits(10) = Array(b_30, b_31, b_32, b_33, b_34, b_35, b_36, b_37, b_38, b_39, b_3a, b_3b, b_3c, b_3d, b_3e, b_3f)
Digits(11) = Array(b_40, b_41, b_42, b_43, b_44, b_45, b_46, b_47, b_48, b_49, b_4a, b_4b, b_4c, b_4d, b_4e, b_4f)
Digits(12) = Array(b_50, b_51, b_52, b_53, b_54, b_55, b_56, b_57, b_58, b_59, b_5a, b_5b, b_5c, b_5d, b_5e, b_5f)
Digits(13) = Array(b_60, b_61, b_62, b_63, b_64, b_65, b_66, b_67, b_68, b_69, b_6a, b_6b, b_6c, b_6d, b_6e, b_6f)

Digits(14) = Array(c_00, c_01, c_02, c_03, c_04, c_05, c_06, c_07)
Digits(15) = Array(c_10, c_11, c_12, c_13, c_14, c_15, c_16)
Digits(16) = Array(c_20, c_21, c_22, c_23, c_24, c_25, c_26)
Digits(17) = Array(c_30, c_31, c_32, c_33, c_34, c_35, c_36, c_37)
Digits(18) = Array(c_40, c_41, c_42, c_43, c_44, c_45, c_46)
Digits(19) = Array(c_50, c_51, c_52, c_53, c_54, c_55, c_56)
Digits(20) = Array(c_60, c_61, c_62, c_63, c_64, c_65, c_66)

Digits(21) = Array(d_00, d_01, d_02, d_03, d_04, d_05, d_06, d_07)
Digits(22) = Array(d_10, d_11, d_12, d_13, d_14, d_15, d_16)
Digits(23) = Array(d_20, d_21, d_22, d_23, d_24, d_25, d_26)
Digits(24) = Array(d_30, d_31, d_32, d_33, d_34, d_35, d_36, d_37)
Digits(25) = Array(d_40, d_41, d_42, d_43, d_44, d_45, d_46)
Digits(26) = Array(d_50, d_51, d_52, d_53, d_54, d_55, d_56)
Digits(27) = Array(d_60, d_61, d_62, d_63, d_64, d_65, d_66)

Digits(28) = Array(e_00, e_01, e_02, e_03, e_04, e_05, e_06)
Digits(29) = Array(e_10, e_11, e_12, e_13, e_14, e_15, e_16)

Digits(30) = Array(f_00, f_01, f_02, f_03, f_04, f_05, f_06)
Digits(31) = Array(f_10, f_11, f_12, f_13, f_14, f_15, f_16)

Sub DisplayTimer()
    Dim chgLED, ii, num, chg, stat, obj
	chgLED = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If Not IsEmpty(chgLED) Then
			For ii = 0 To UBound(chgLED)
				num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
				if (num < 32) then
					For Each obj In Digits(num)
						If chg And 1 Then obj.State = stat And 1
						chg = chg\2 : stat = stat\2
					Next
				End If
			Next
    End If
End Sub



'********************************************
'              VR Display Output
'********************************************

dim DisplayColor, DisplayColorG
DisplayColor =  RGB(255,40,1)

Sub DisplayTimerVR
	Dim ii, jj, obj, b, x
	Dim ChgLED,num, chg, stat
	ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
		If Not IsEmpty(ChgLED) Then
			For ii=0 To UBound(chgLED)
				num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
				For Each obj In VRDigits(num)
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
		Object.Opacity = 80
	Else
		Object.Color = RGB(1,1,1)
		Object.Opacity = 120
	End If
End Sub

Dim VRDigits(32)

 VRDigits(0)=Array(ax00, ax05, ax0c, ax0d, ax08, ax01, ax06, ax0f, ax02, ax03, ax04, ax07, ax0b, ax0a, ax09, ax0e)
 VRDigits(1)=Array(ax10, ax15, ax1c, ax1d, ax18, ax11, ax16, ax1f, ax12, ax13, ax14, ax17, ax1b, ax1a, ax19, ax1e)
 VRDigits(2)=Array(ax20, ax25, ax2c, ax2d, ax28, ax21, ax26, ax2f, ax22, ax23, ax24, ax27, ax2b, ax2a, ax29, ax2e)
 VRDigits(3)=Array(ax30, ax35, ax3c, ax3d, ax38, ax31, ax36, ax3f, ax32, ax33, ax34, ax37, ax3b, ax3a, ax39, ax3e)
 VRDigits(4)=Array(ax40, ax45, ax4c, ax4d, ax48, ax41, ax46, ax4f, ax42, ax43, ax44, ax47, ax4b, ax4a, ax49, ax4e)
 VRDigits(5)=Array(ax50, ax55, ax5c, ax5d, ax58, ax51, ax56, ax5f, ax52, ax53, ax54, ax57, ax5b, ax5a, ax59, ax5e)
 VRDigits(6)=Array(ax60, ax65, ax6c, ax6d, ax68, ax61, ax66, ax6f, ax62, ax63, ax64, ax67, ax6b, ax6a, ax69, ax6e)

 VRDigits(7)=Array(ax70, ax75, ax7c, ax7d, ax78, ax71, ax76, ax7f, ax72, ax73, ax74, ax77, ax7b, ax7a, ax79, ax7e)
 VRDigits(8)=Array(ax80, ax85, ax8c, ax8d, ax88, ax81, ax86, ax8f, ax82, ax83, ax84, ax87, ax8b, ax8a, ax89, ax8e)
 VRDigits(9)=Array(ax90, ax95, ax9c, ax9d, ax98, ax91, ax96, ax9f, ax92, ax93, ax94, ax97, ax9b, ax9a, ax99, ax9e)
 VRDigits(10)=Array(axa0, axa5, axac, axad, axa8, axa1, axa6, axaf, axa2, axa3, axa4, axa7, axab, axaa, axa9, axae)
 VRDigits(11)=Array(axb0, axb5, axbc, axbd, axb8, axb1, axb6, axbf, axb2, axb3, axb4, axb7, axbb, axba, axb9, axbe)
 VRDigits(12)=Array(axc0, axc5, axcc, axcd, axc8, axc1, axc6, axcf, axc2, axc3, axc4, axc7, axcb, axca, axc9, axce)
 VRDigits(13)=Array(axd0, axd5, axdc, axdd, axd8, axd1, axd6, axdf, axd2, axd3, axd4, axd7, axdb, axda, axd9, axde)
 
 VRDigits(14) = Array(LED1x0,LED1x1,LED1x2,LED1x3,LED1x4,LED1x5,LED1x6,LED1x7,LED1x8,LED1x9)
 VRDigits(15) = Array(LED2x0,LED2x1,LED2x2,LED2x3,LED2x4,LED2x5,LED2x6,LED2x7,LED2x8,LED2x9)
 VRDigits(16) = Array(LED3x0,LED3x1,LED3x2,LED3x3,LED3x4,LED3x5,LED3x6,LED3x7,LED3x8,LED3x9)
 VRDigits(17) = Array(LED4x0,LED4x1,LED4x2,LED4x3,LED4x4,LED4x5,LED4x6,LED4x7,LED4x8,LED4x9)
 VRDigits(18) = Array(LED5x0,LED5x1,LED5x2,LED5x3,LED5x4,LED5x5,LED5x6,LED5x7,LED5x8,LED5x9)
 VRDigits(19) = Array(LED6x0,LED6x1,LED6x2,LED6x3,LED6x4,LED6x5,LED6x6,LED6x7,LED6x8,LED6x9)
 VRDigits(20) = Array(LED7x0,LED7x1,LED7x2,LED7x3,LED7x4,LED7x5,LED7x6,LED7x7,LED7x8,LED7x9)

 VRDigits(21) = Array(LED8x0,LED8x1,LED8x2,LED8x3,LED8x4,LED8x5,LED8x6,LED8x7,LED8x8,LED8x9)
 VRDigits(22) = Array(LED9x0,LED9x1,LED9x2,LED9x3,LED9x4,LED9x5,LED9x6,LED9x7,LED9x8,LED9x9)
 VRDigits(23) = Array(LED10x0,LED10x1,LED10x2,LED10x3,LED10x4,LED10x5,LED10x6,LED10x7,LED10x8,LED10x9)
 VRDigits(24) = Array(LED11x0,LED11x1,LED11x2,LED11x3,LED11x4,LED11x5,LED11x6,LED11x7,LED11x8,LED11x9)
 VRDigits(25) = Array(LED12x0,LED12x1,LED12x2,LED12x3,LED12x4,LED12x5,LED12x6,LED12x7,LED12x8,LED12x9)
 VRDigits(26) = Array(LED13x0,LED13x1,LED13x2,LED13x3,LED13x4,LED13x5,LED13x6,LED13x7,LED13x8,LED13x9)
 VRDigits(27) = Array(LED14x0,LED14x1,LED14x2,LED14x3,LED14x4,LED14x5,LED14x6,LED14x7,LED14x8,LED14x9)

 VRDigits(28) = Array(LED1x000,LED1x001,LED1x002,LED1x003,LED1x004,LED1x005,LED1x006,LED1x007,LED1x008,LED1x009)
 VRDigits(29) = Array(LED1x100,LED1x101,LED1x102,LED1x103,LED1x104,LED1x105,LED1x106,LED1x107,LED1x108,LED1x109)
 VRDigits(30) = Array(LED1x200,LED1x201,LED1x202,LED1x203,LED1x204,LED1x205,LED1x206,LED1x207,LED1x208,LED1x209)
 VRDigits(31) = Array(LED1x300,LED1x301,LED1x302,LED1x303,LED1x304,LED1x305,LED1x306,LED1x307,LED1x308,LED1x309)
 

Sub InitDigits()
	dim tmp, x, obj
	for x = 0 to uBound(VRDigits)
		if IsArray(VRDigits(x) ) then
			For each obj in VRDigits(x)
				obj.height = obj.height + 0
				FadeDisplay obj, 0
			next
		end If
	Next
End Sub

If VR_Room=1 Then
	InitDigits
End If




'********************************************
' Hybrid code for VR, Cab, and Desktop
'********************************************

Dim VRThings

if VR_Room = 0 and cab_mode = 0 Then
	for each VRThings in VRStuff:VRThings.visible = 0:Next
	for each VRThings in VRClock:VRThings.visible = 0:Next
	for each VRThings in DTRails:VRThings.visible = 1:Next
	for each VRThings in VRDigitsTop:VRThings.visible = 0: Next
	for each VRThings in VRDigitsBottom:VRThings.visible = 0: Next
	pBlackSideWall.visible = 1
Elseif VR_Room = 0 and cab_mode = 1 Then
	for each VRThings in VRStuff:VRThings.visible = 0:Next
	for each VRThings in VRClock:VRThings.visible = 0:Next
	for each VRThings in DTRails:VRThings.visible = 0:Next
	for each VRThings in VRDigitsTop:VRThings.visible = 0: Next
	for each VRThings in VRDigitsBottom:VRThings.visible = 0: Next
	pBlackSideWall.visible = cabsideblades
Else
	for each VRThings in VRStuff:VRThings.visible = 1:Next
	for each VRThings in VRClock:VRThings.visible = WallClock:Next
	for each VRThings in DTRails:VRThings.visible = 0:Next
	pBlackSideWall.visible = 0

'Custom Walls, Floor, and Roof
	if CustomWalls = 1 Then
		VR_Wall_Left.image = "VR_Wall_Left"
		VR_Wall_Right.image = "VR_Wall_Right"
		VR_Floor.image = "VR_Floor"
		VR_Roof.image = "VR_Roof"
	end if

	If topper = 1 Then
		VRtopper.visible = 1
	Else
		VRtopper.visible = 0
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
' Sidewalls
'********************************************

If sidewalls = 0 Then
	pBlackSideWall.image = "black"
Else
	pBlackSideWall.image = "_sidewalls"
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

if BallBrightness = 0 Then
	LaserWar.BallImage="ball-dark"
	LaserWar.BallFrontDecal="JPBall-Scratches"
elseif BallBrightness = 1 Then
	LaserWar.BallImage="ball-HDR"
	LaserWar.BallFrontDecal="Scratches"
elseif BallBrightness = 2 Then
	LaserWar.BallImage="ball-light-hf"
	LaserWar.BallFrontDecal="g5kscratchedmorelight"
else
	LaserWar.BallImage="ball-lighter-hf"
	LaserWar.BallFrontDecal="g5kscratchedmorelight"
End if



'**********************************************************
'*******	Set Up Backglass and Backglass Flashers	*******
'**********************************************************
' this is for lining up the backglass flashers on top of a backglass image

Sub SetBackglass()
	Dim obj

	For Each obj In BackglassLow
		obj.x = obj.x
		obj.height = - obj.y + 210
		obj.y = -5 'adjusts the distance from the backglass towards the user
	Next

	For Each obj In BackglassMid
		obj.x = obj.x
		obj.height = - obj.y + 210
		obj.y = -6 'adjusts the distance from the backglass towards the user
	Next

	For Each obj In BackglassHigh
		obj.x = obj.x
		obj.height = - obj.y + 210
		obj.y = -10 'adjusts the distance from the backglass towards the user
	Next

	For Each obj In VRDigitsTop
		obj.x = obj.x
		obj.height = - obj.y + 210
		obj.y = 11 'adjusts the distance from the backglass towards the user
	Next

	For Each obj In VRDigitsBottom
		obj.x = obj.x
		obj.height = - obj.y + 210
		obj.y = 17 'adjusts the distance from the backglass towards the user
	Next

End Sub




'Backglass Flashers

Dim sol27lvl, sol28lvl

Sub Sol27(level)
	If VR_Room > 0 Then
		sol27lvl = 1
		sol27timer_timer
	End If
end sub

sub sol27timer_timer			
	dim obj
	if Not sol27timer.enabled then
		for each obj in VRBGFL27 : obj.visible = 1 : Next
		for each obj in VRBGFL27Bulb : obj.visible = 1 : Next
		sol27timer.enabled = true
	end if
	sol27lvl = 0.75 * sol27lvl - 0.01
	if sol27lvl < 0 then sol27lvl = 0
		for each obj in VRBGFL27 : obj.opacity = 100 * sol27lvl^2.5 : Next
		for each obj in VRBGFL27Bulb : obj.opacity = 100 * sol27lvl^1.5 : Next
	if sol27lvl =< 0 Then
		for each obj in VRBGFL27 : obj.visible = 0 : Next
		for each obj in VRBGFL27Bulb : obj.visible = 0 : Next
		sol27timer.enabled = false
	end if
End Sub

Sub Sol28(level)
	If VR_Room > 0 Then
		sol28lvl = 1
		sol28timer_timer
	End If
end sub

sub sol28timer_timer			
	dim obj
	if Not sol28timer.enabled then
		for each obj in VRBGFL28 : obj.visible = 1 : Next	
		sol28timer.enabled = true
	end if
	sol28lvl = 0.75 * sol28lvl - 0.01
	if sol28lvl < 0 then sol28lvl = 0
		for each obj in VRBGFL28 : obj.opacity = 100 * sol28lvl^2 : Next
	if sol28lvl =< 0 Then
		for each obj in VRBGFL28 : obj.visible = 0 : Next
		sol28timer.enabled = false
	end if
End Sub



'**********************************************


' ***************************************************************************
'                                  LAMP CALLBACK
' ****************************************************************************

if VR_Room = 1 Then
	Set LampCallback = GetRef("UpdateMultipleLamps")
End If

Sub UpdateMultipleLamps()

' Backglass Flasher Lamps
	If Controller.Lamp(1) = 0 Then: BGFl1.visible  =0: else: BGFl1.visible  =1
	If Controller.Lamp(2) = 0 Then: BGFl2.visible  =0: else: BGFl2.visible  =1

End Sub



'******************************************************
'****  Ball GI Brightness Level Code
'******************************************************
const BallBrightMax = 255			'Brightness setting when GI is on (max of 255). Only applies for Normal ball.
const BallBrightMin = 100			'Brightness setting when GI is off (don't set above the max). Only applies for Normal ball.
Sub UpdateBallBrightness
	Dim b, brightness
	For b = 0 to UBound(gBOT)
		if bbgi = 0 Then
			gBOT(b).color = BallBrightMin + (BallBrightMin * 256) + (BallBrightMin * 256 * 256)
		Else
			gBOT(b).color = BallBrightMax + (BallBrightMax * 256) + (BallBrightMax * 256 * 256)
		end If
	Next
End Sub




'******************************************************
'*****   FLUPPER DOMES 
'******************************************************
' Based on FlupperDomes2.2

 Sub Flash6(Enabled)
	If Enabled Then		
		Objlevel(5) = 1 : FlasherFlash5_Timer
		Objlevel(6) = 1 : FlasherFlash6_Timer
	End If
	Sound_Flash_Relay enabled, Flasherbase5
	Sound_Flash_Relay enabled, Flasherbase6
 End Sub

 Sub Flash7(Enabled)
	If Enabled Then
		Objlevel(4) = 1 : FlasherFlash4_Timer
		Objlevel(7) = 1 : FlasherFlash7_Timer
	End If
	Sound_Flash_Relay enabled, Flasherbase4
	Sound_Flash_Relay enabled, Flasherbase7
 End Sub

 Sub Flash8(Enabled)
	If Enabled Then		
		Objlevel(8) = 1 : FlasherFlash8_Timer
		Objlevel(3) = 1 : FlasherFlash3_Timer
	End If
	Sound_Flash_Relay enabled, Flasherbase3
	Sound_Flash_Relay enabled, Flasherbase8
 End Sub


Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = LaserWar  		' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.1		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = .1 '.3  ' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherBloomIntensity = 0.2		' *** lower this, if the blooms are too bright (i.e. 0.1)			***	
FlasherOffBrightness = 0.5		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20)
'Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"

InitFlasher 1, "white"
InitFlasher 2, "white"
InitFlasher 3, "white"
InitFlasher 4, "red"
InitFlasher 5, "yellow"
InitFlasher 6, "yellow"
InitFlasher 7, "red"
InitFlasher 8, "blue"

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

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

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

Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
InitLampsNF              	' Setup lamp assignments
LampzTimer.Interval = 16	' Using fixed value so the fading speed is same for every fps
LampzTimer.Enabled = 1

Sub LampzTimer_Timer()
	dim x, chglamp, nr, obj
	chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
			Select Case chglamp(x,0)
			Case 9
				SetTowerLight 1, (chgLamp(x,1) <> 0)
			Case 10
				SetTowerLight 2, (chgLamp(x,1) <> 0)
			Case 11
				SetTowerLight 3, (chgLamp(x,1) <> 0)
			End Select
				For Each obj In InsertLightsFlasher
					If obj.TimerInterval = chgLamp(x,0) Then
						If chgLamp(x,1) = 0 Then
							obj.IntensityScale = 0.93 : InsertLightsFlasherTimer.Enabled = True
						ElseIf chgLamp(x,1) = 1 Then
							obj.IntensityScale = 0.1 : InsertLightsFlasherTimer.Enabled = True
						End If
					End If
				Next
		Next
	End If
	Lampz.Update1	'update (fading logic only)
End Sub

dim FrameTime, InitFrameTime : InitFrameTime = 0
LampTimer2.Interval = -1
LampTimer2.Enabled = True
Sub LampTimer2_Timer()
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
	Lampz.Update 'updates on frametime (Object updates only)
End Sub

Function FlashLevelToIndex(Input, MaxSize)
	FlashLevelToIndex = cInt(MaxSize * Input)
End Function

'***Material Swap***
'Fade material for green, red, yellow colored Bulb prims
Sub FadeMaterialColoredBulb(pri, group, ByVal aLvl)	'cp's script
	'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	Select case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
		Case 3:pri.Material = group(3) 'Full
	End Select
	'if tb.text <> pri.image then tb.text = pri.image : 'debug.print pri.image end If	'debug
	pri.blenddisablelighting = aLvl * 1 'Intensity Adjustment
End Sub


'Fade material for red, yellow colored bulb Filiment prims
Sub FadeMaterialColoredFiliment(pri, group, ByVal aLvl)	'cp's script
	'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	Select case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
		Case 3:pri.Material = group(3) 'Full
	End Select
	'if tb.text <> pri.image then tb.text = pri.image : 'debug.print pri.image end If	'debug
	pri.blenddisablelighting = aLvl * 50  'Intensity Adjustment
End Sub


Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity
End Sub


const insert_dl_on_white = 120
const insert_dl_on_red = 60
const insert_dl_on_blue = 200
const insert_dl_on_yellow = 40
const insert_dl_on_yellow_tri = 180
const insert_dl_on_green = 50
const war=60


Sub InitLampsNF()

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (1 / full MS fading time)
	dim x : for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/3 : Lampz.FadeSpeedDown(x) = 1/8 : next
	Lampz.FadeSpeedUp(102) = 1/4 : Lampz.FadeSpeedDown(102) = 1/16

	'Lampz Assignments
	'  In a ROM based table, the lamp ID is used to set the state of the Lampz objects

	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays

	Lampz.MassAssign(3)= l3
	Lampz.MassAssign(3)= l3a
	Lampz.Callback(3) = "DisableLighting p3, insert_dl_on_red,"
	Lampz.MassAssign(4)= l4
	Lampz.MassAssign(4)= l4a
	Lampz.Callback(4) = "DisableLighting p4, insert_dl_on_red+war,"
	Lampz.MassAssign(5)= l5
	Lampz.MassAssign(5)= l5a
	Lampz.Callback(5) = "DisableLighting p5, insert_dl_on_yellow+war,"
	Lampz.MassAssign(6)= l6
	Lampz.MassAssign(6)= l6a
	Lampz.Callback(6) = "DisableLighting p6, insert_dl_on_blue+war,"
	Lampz.MassAssign(7)= l7
	Lampz.MassAssign(7)= l7a
	Lampz.Callback(7) = "DisableLighting p7, insert_dl_on_white,"
	Lampz.MassAssign(12)= l12
	Lampz.MassAssign(12)= l12a
	Lampz.Callback(12) = "DisableLighting p12, insert_dl_on_green,"
	Lampz.MassAssign(13)= l13
	Lampz.MassAssign(13)= l13a
	Lampz.Callback(13) = "DisableLighting p13, insert_dl_on_red,"
	Lampz.MassAssign(14)= l14
	Lampz.MassAssign(14)= l14a
	Lampz.Callback(14) = "DisableLighting p14, insert_dl_on_yellow_tri,"
	Lampz.MassAssign(15)= l15
	Lampz.MassAssign(15)= l15a
	Lampz.Callback(15) = "DisableLighting p15, insert_dl_on_white,"
	Lampz.MassAssign(16)= l16
	Lampz.MassAssign(16)= l16a
	Lampz.Callback(16) = "DisableLighting p16, insert_dl_on_red,"
	Lampz.MassAssign(17)= l17
	Lampz.MassAssign(17)= l17a
	Lampz.Callback(17) = "DisableLighting p17, insert_dl_on_red,"
	Lampz.MassAssign(18)= l18
	Lampz.MassAssign(18)= l18a
	Lampz.Callback(18) = "DisableLighting p18, insert_dl_on_red,"
	Lampz.MassAssign(19)= l19
	Lampz.MassAssign(19)= l19a
	Lampz.Callback(19) = "DisableLighting p19, insert_dl_on_red,"
	Lampz.MassAssign(20)= l20
	Lampz.MassAssign(20)= l20a
	Lampz.Callback(20) = "DisableLighting p20, insert_dl_on_red,"
	Lampz.MassAssign(21)= l21
	Lampz.MassAssign(21)= l21a
	Lampz.Callback(21) = "DisableLighting p21, insert_dl_on_white,"
	Lampz.MassAssign(22)= l22
	Lampz.MassAssign(22)= l22a
	Lampz.Callback(22) = "DisableLighting p22, insert_dl_on_yellow_tri,"
	Lampz.MassAssign(23)= l23
	Lampz.MassAssign(23)= l23a
	Lampz.Callback(23) = "DisableLighting p23, insert_dl_on_red,"
	Lampz.MassAssign(24)= l24
	Lampz.MassAssign(24)= l24a
	Lampz.Callback(24) = "DisableLighting p24, insert_dl_on_yellow,"
	Lampz.MassAssign(25)= l25
	Lampz.MassAssign(25)= l25a
	Lampz.Callback(25) = "DisableLighting p25, insert_dl_on_yellow,"
	Lampz.MassAssign(26)= l26
	Lampz.MassAssign(26)= l26a
	Lampz.Callback(26) = "DisableLighting p26, insert_dl_on_yellow,"
	Lampz.MassAssign(27)= l27
	Lampz.MassAssign(27)= l27a
	Lampz.Callback(27) = "DisableLighting p27, insert_dl_on_yellow,"
	Lampz.MassAssign(28)= l28
	Lampz.MassAssign(28)= l28a
	Lampz.Callback(28) = "DisableLighting p28, insert_dl_on_yellow,"
	Lampz.MassAssign(29)= l29
	Lampz.MassAssign(29)= l29a
	Lampz.Callback(29) = "DisableLighting p29, insert_dl_on_white,"
	Lampz.MassAssign(30)= l30
	Lampz.MassAssign(30)= l30a
	Lampz.Callback(30) = "DisableLighting p30, insert_dl_on_yellow_tri,"
	Lampz.MassAssign(31)= l31
	Lampz.MassAssign(31)= l31a
	Lampz.Callback(31) = "DisableLighting p31, insert_dl_on_red,"
	Lampz.MassAssign(32)= l32
	Lampz.MassAssign(32)= l32a
	Lampz.Callback(32) = "DisableLighting p32, insert_dl_on_blue,"
	Lampz.MassAssign(33)= l33
	Lampz.MassAssign(33)= l33a
	Lampz.Callback(33) = "DisableLighting p33, insert_dl_on_blue,"
	Lampz.MassAssign(34)= l34
	Lampz.MassAssign(34)= l34a
	Lampz.Callback(34) = "DisableLighting p34, insert_dl_on_blue,"
	Lampz.MassAssign(35)= l35
	Lampz.MassAssign(35)= l35a
	Lampz.Callback(35) = "DisableLighting p35, insert_dl_on_blue,"
	Lampz.MassAssign(36)= l36
	Lampz.MassAssign(36)= l36a
	Lampz.Callback(36) = "DisableLighting p36, insert_dl_on_blue,"
	Lampz.MassAssign(37)= l37
	Lampz.MassAssign(37)= l37a
	Lampz.Callback(37) = "DisableLighting p37, insert_dl_on_white,"
	Lampz.MassAssign(38)= l38
	Lampz.MassAssign(38)= l38a
	Lampz.Callback(38) = "DisableLighting p38, insert_dl_on_yellow_tri,"
	Lampz.MassAssign(39)= l39
	Lampz.MassAssign(39)= l39a
	Lampz.Callback(39) = "DisableLighting p39, insert_dl_on_red,"
	Lampz.MassAssign(40)= l40
	Lampz.MassAssign(40)= l40a
	Lampz.Callback(40) = "DisableLighting p40, insert_dl_on_red,"
	Lampz.MassAssign(41)= l41B
	Lampz.MassAssign(41)= l41Ba
	Lampz.Callback(41) = "DisableLighting p41B, insert_dl_on_white,"
	Lampz.MassAssign(41)= l41A
	Lampz.MassAssign(41)= l41Aa
	Lampz.Callback(41) = "DisableLighting p41A, insert_dl_on_white,"
	Lampz.MassAssign(42)= l42
	Lampz.MassAssign(42)= l42a
	Lampz.Callback(42) = "DisableLighting p42, insert_dl_on_green,"
	Lampz.MassAssign(43)= l43
	Lampz.MassAssign(43)= l43a
	Lampz.Callback(43) = "DisableLighting p43, insert_dl_on_green,"
	Lampz.MassAssign(44)= l44
	Lampz.MassAssign(44)= l44a
	Lampz.Callback(44) = "DisableLighting p44, insert_dl_on_green,"
	Lampz.MassAssign(45)= l45
	Lampz.MassAssign(45)= l45a
	Lampz.Callback(45) = "DisableLighting p45, insert_dl_on_green,"
	Lampz.MassAssign(46)= l46
	Lampz.MassAssign(46)= l46a
	Lampz.Callback(46) = "DisableLighting p46, insert_dl_on_green,"
	Lampz.MassAssign(47)= l47
	Lampz.MassAssign(47)= l47a
	Lampz.Callback(47) = "DisableLighting p47, insert_dl_on_yellow_tri,"
	Lampz.MassAssign(48)= l48
	Lampz.MassAssign(48)= l48a
	Lampz.Callback(48) = "DisableLighting p48, insert_dl_on_red,"
	Lampz.MassAssign(50)= l50
	Lampz.MassAssign(50)= l50a
	Lampz.Callback(50) = "DisableLighting p50, insert_dl_on_red,"
	Lampz.MassAssign(51)= l51
	Lampz.MassAssign(51)= l51a
	Lampz.Callback(51) = "DisableLighting p51, insert_dl_on_red,"
	Lampz.MassAssign(52)= l52
	Lampz.MassAssign(52)= l52a
	Lampz.Callback(52) = "DisableLighting p52, insert_dl_on_red,"
	Lampz.MassAssign(53)= l53
	Lampz.MassAssign(53)= l53a
	Lampz.Callback(53) = "DisableLighting p53, insert_dl_on_red,"
	Lampz.MassAssign(54)= l54
	Lampz.MassAssign(54)= l54a
	Lampz.Callback(54) = "DisableLighting p54, insert_dl_on_red,"
	Lampz.MassAssign(55)= l55
	Lampz.MassAssign(55)= l55a
	Lampz.Callback(55) = "DisableLighting p55, insert_dl_on_yellow,"
	Lampz.MassAssign(56)= l56
	Lampz.MassAssign(56)= l56a
	Lampz.Callback(56) = "DisableLighting p56, insert_dl_on_yellow,"
	Lampz.MassAssign(57)= l57
	Lampz.MassAssign(57)= l57a
	Lampz.Callback(57) = "DisableLighting p57, insert_dl_on_yellow,"
	Lampz.MassAssign(58)= l58
	Lampz.MassAssign(58)= l58a
	Lampz.Callback(58) = "DisableLighting p58, insert_dl_on_yellow,"
	Lampz.MassAssign(59)= l59
	Lampz.MassAssign(59)= l59a
	Lampz.Callback(59) = "DisableLighting p59, insert_dl_on_yellow,"
	Lampz.MassAssign(60)= l60
	Lampz.MassAssign(60)= l60a
	Lampz.Callback(60) = "DisableLighting p60, insert_dl_on_blue,"
	Lampz.MassAssign(61)= l61
	Lampz.MassAssign(61)= l61a
	Lampz.Callback(61) = "DisableLighting p61, insert_dl_on_blue,"
	Lampz.MassAssign(62)= l62
	Lampz.MassAssign(62)= l62a
	Lampz.Callback(62) = "DisableLighting p62, insert_dl_on_blue,"
	Lampz.MassAssign(63)= l63
	Lampz.MassAssign(63)= l63a
	Lampz.Callback(63) = "DisableLighting p63, insert_dl_on_blue,"
	Lampz.MassAssign(64)= l64
	Lampz.MassAssign(64)= l64a
	Lampz.Callback(64) = "DisableLighting p64, insert_dl_on_blue,"

'	Lampz.Callback(102) = "InsertIntensityUpdate"

	'Turn off all lamps on startup
	Lampz.Init	'This just turns state of any lamps to 1

	'Immediate update to turn on GI, turn off lamps
	Lampz.Update

End Sub


Sub InsertIntensityUpdate(ByVal aLvl)
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	dim bulb
	for each bulb in InsertLightsInPrim
		bulb.intensity = bulb.uservalue*(1 + aLvl*2)
	next
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

Class LampFader
	Public FadeSpeedDown(140), FadeSpeedUp(140)
	Private Lock(140), Loaded(140), OnOff(140)
	Public UseFunction
	Private cFilter
	Public UseCallback(140), cCallback(140)
	Public Lvl(140), Obj(140)
	Private Mult(140)
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
			OnOff(x) = False
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
		'if idx = 132 then msgbox out	'debug
		ExecuteGlobal Out

	End Property

	Public Property Let state(ByVal idx, input) 'Major update path
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
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
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
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx : for x = 0 to uBound(OnOff)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(Lvl(x)*Mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*Mult(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(Lvl(x)*Mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)
					End If
				end if
				if TypeName(lvl(x)) <> "Double" and typename(lvl(x)) <> "Integer" then msgbox "uhh " & 2 & " = " & lvl(x)
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x))	'Callback
				If UseCallBack(x) then Proc name & x,Lvl(x)*mult(x)	'Proc
				If Lock(x) Then
					if Lvl(x) = 1 or Lvl(x) = 0 then Loaded(x) = True	'finished fading
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




