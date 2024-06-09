
' PinBlob Version 1.0 by CLV
'
'******** Dev-Log from Donor table Bally Atlantis by Herweh and UnclePaulie *********
'
' Donor table Development by Herweh 2019 for Visual Pinball 10
' Version 1.1
'
' Very special thanks to
' - Schreibi34: For the playfield mesh, playfield insert templates and two awesome sub models
' - 32assassin: For original red bumper caps, sub model, flipper bats, desktop score display and for sure some more. 
' 				And for being a very cool guy who is always helping and supporting with a WIP build or other resources
' - OldSkoolGamer: For providing me all the wonderful artwork he designed for the VP9 build
' - Cosmic80: For reworking nearly all the inserts of the playfield
'
' Special thanks to
' - OldSkoolGamer, ICPjuggla, Herweh (:-): For creating a very cool VP9 build from where I borrowed a few table elements
' - Bord: For the ramp lift stuff and for being such a great guy who is always helping when help is needed
' - Rosve: For the drain plug from his 'Skyrocket' table
' - Batch: For the backglass image of the desktop view
' - Vogliadicane: For the blue and transparent bumper caps from his 'Fathom' mods. They do match perfectly into the 'Atlantis' theme.
' - Flupper: Once again for his plastic and wire ramp tutorial and for the flasher domes
' - Ninuzzu: For his flipper shadows
' - Dark: For the gates primitives template
' - Dark and nFozzy: Locknuts and screws
' - DJRobX, RothbauerW and maybe some other ones: For some code in my script like the SSF routines
' - Thalamus: For beta testing, some good hints and a missing sound ;-)
' - VPX development team: For always lifting VP to the next level
' - All I have forgotten. Please send me a PM if you think I should add you to the 'Thx' section here. My fault!
'
' Release notes:
' Version 1.1  : New backwall image for desktop mode and replaced third ballroling sound
' Version 1.0.2: Some graphical finetuning
'				 - reworked the right wire ramp to get it more authentic
'				 - smoother transition for a lot objects from GI off to on and vice versa
' Version 1.0.1: Some minor fixes
'				 - added a missing rubber to the GI
'				 - added some missing sounds when hitting objects
'				 - improved the shadows of the drop targets
'				 - emended the left bonus held flasher
'				 - reworked the wire ramp a tiny bit
'				 - added a GI option to de-/activate bulbs under the inlanes ('EnableGIInlane')
' Version 1.0: Initial release
'

'******** Revisions done by UnclePaulie on Hybrid version 0.1 - 2.0 *********
' v0.1 	Added hybrid mode for VR room, cabinet, and desktop
'		Added minimal VR room.  
'		Added VR elements (power cord, several options: clock, posters, topper (HiRez image)
'		Added 4 ball options, dark, bright, and brightest
' 		Added DTRails collection for left and right rails in desktop mode
'		Animated plunger, start button, and flippers in VR.  Had to assign individual materials to each.
'		Adjusted the digital tilt sensitivity to 6, and the nudge strength to 1.
'		Removed rollingtimer, removed debug mode code and ballcontroltimer, ballsearch, and manual ball control.
'		Added groove to plunger lane, and small ball release ramp to get up.  
'		Completely changed the trough logic to handle balls on table
'		Changed the apron wall structure to create a trough
' 		No longer destroy balls.  Use a global gBOT throughout all of script.
'		Removed all the getballs calls and am only useing gBOT.  Will help eliminate stutter on slower CPU machines
'		Created a sub to control the VUK and and sw39 saucer.
'		Removed startcontrol manual ball control trigger.  No longer needed.
' 		Added option to turn magnasave option on and off 
' 		Readjusted flipper size down to be 147 VP units per VPW recommendations.  (was 151).  Tweaked physics
'		Updated to nFozzy / Roth flipper physics
'		Got rid of LetTheBallJump code.  VPW physics will replace
'		Modified the GetBallID function 
'		Flipper shadows and options moved to FlipperVisualUpdate timer.  Removed graphicstimer.
'		VPW ball rolling sounds implemented
' v0.2	Redid the desktop backglass displays to align middle vertical LEDs correctly.
' v0.3	Adjusted VR room and LED displays
'		Added optional plywood grain sideblades 
'		Added animated backglass in VR 
' v0.4	Changed GIOverhead and GIOverheadUpperPF to a flasher from a light.  Looked wierd in VR, and wasn't angled correctly.  
'		Ensured all lights stayed within the playfield... could see lights outside of table in VR.
'		Modified the desktop background.  Too busy before.
'		Adjusted the plunger groove
' v0.5	Added dynamic shadows solution to the solution created by iaakki, apophis, Wylte
'		Modded the playfield image to include two more trigger holes in the top lanes
' v0.6	Added plywood cutouts for the target and trigger holes
'		Aligned the triggers per the real table, and adjusted the height.  Also moved the plunger location slightly to the right to align correctly.
'		Positioned the sling and wall primatives correctly.  Adjusted sling rubbers, and moved those and all rubbers up by 5 VP units.
'		Modified the apron primitive image to include a cutout for the ball release
'		Made the apron walls visible, and slightly higher, as you could see under apron easily in VR 
'		Enababled bumper sounds
'		Added knocker solenoid subroutine and KnockerPosition Primitive for sound
' v0.7	Slight flipper trigger position adjustment
'		Adjusted pegs and posts for actual table alignment
'		Added all rubbers, dPosts, dSleeves, metal walls, rollovers, spinner, gates, walls. saucers, and sub for Fleep sounds and VPW physics. 
'		Replaced rest of a_physics materials to new VPW physics materials
' v0.8	Added updated ramprolling sounds solution created by nFozzy and VPW.  Removed LoopTimer
'		Added GI relay sounds
'		Removed spinnertimer and added spinner sub to FrameTimer
' v0.9	Added updated standup targets with VPW physics, original code created by Rothbauerw and VPW team
' v0.10 Put Rothbaierw drop targets in 
'		Removed wDoubleDrop1And2 walls, SW_Timer, and old drop target subs.  Added GI droptarget lights to DTHit sub.  
'		Corrected getting shadows off on a light drop target hit, and it actually didn't drop.
' v0.11	Adjusted the height of standup targets and hiddin bouncer walls.  Also adjusted the amount of drop the drop targets go down.
'		Updated playfield physics, bumpers, slings, ramps per VPW recommendations.
'		The rebounders weren't registering, as they were coded to be just a hit event.  They were made as slings, and needed the sw39_slingshot sub call.
'		Changed the physics on the rebounders as well.  
'		Added a simple trigger to slow the ball down heading for the left saucer slightly, as it would rarely stay in that saucer.
' 		There was a z fighting issue on the metal wall near the right standup targets.
' v0.12	Further adjustments to lighting on desktop backglass
'		Rewrote some of the code for sw39_timer and kickersol. It was forcing a z height when releasing, and sometimes caused balls on the sub trough
'		to drop to a z height of 25, and fall below sub.  I forced the kicker to work as it should, and have a random kickout value.
'		Have SW44 on the wSubRamp wall now, and the sub ramps have walls now.  Just in case.
'		Turned sound off at startup for SW44. It should only sound when a ball is in there.
'		Implemented a similar approach for the VUK.  Using an actual kicker now instead of xyz values.
'		Added a Z height limitation to sw39 saucer circle condition
'		Added code in the saucers_on sub to handle a corner condition where a ball could slow to a stop on top of the right metal guide.
' v0.13	Improved drain kick action, GI Timer control in and not in multiball, and sounds 
'		Removed GetBallID and lockedBallID, as not a part of updated code.
' 		Adjusted when top saucer sound triggered.
'		Edited sounds for drain post.
'		Adjusted the pov for desktop
' v0.14	Moved the post to the right of the left VUK, and the metal guide, and widened the ramp slightly to help getting ball up the left VUK. Too hard before.
'		Also moved the lift ramp slightly, and increased the base by 7 vp units.  Aligns with the primative closer.
' v0.15	Changed all the flasher lights to transmit = 0.1 from zero on layer 11.
'		Modified the image for the model 2 sub texture so you can see ball go in, and the ball that's in there... especially in VR.
'		Added a GIRightTop collection to control the brightness at the upper right area of playfield.  The white was too bright there.
'		Reduced intensity of l66a and l66b from 25 to 20.  
'		Adjusted full cab pov.
'		Put gilvl correctly in GITimer
'		Adjusted the intensity of l12, 12a, and 12b from 200 to 125.  They flash bright when a drop target is hit.  They were two bright.
'		Added dome flasher relay sounds, and adjusted volume of all relay sounds.
' v0.16	Rewrote the RampLiftTimer code to not rely on timer intervals. I think it was impacting an NVRAM corruption corner condition, similar to mousin' around.  
'		Modified the sounds of the ramp lift to only a low solenoid snap.  It's too fast for a motor sound.
'		Code CleanUp
' v0.17	Changed the rebounders to just be bouncy and not a sling. It was causing too much extra energy on the ball.
' v0.18	Added Flupper Flasher Domes
' v0.19	Tomate added new pMetalWallsLift primitive.  The left wall was broken, and didn't have a face.  He corrected the prim, as you could see through one side.  
'		Tomate also added/baked new flippers for Bally.  
'		Adjusted the bally flippers material to colormaxnoreflectionhalf.  Also changed the blenddisablelighting of them to 0.5 (was 0.. a bit too dark)
'		Removed the yellow/green/bally and the gray/dark gray/bally flipper options.
'		Modified the backwall image slightly for around flasher domes
'		Slightly adjusted the left edge in of PlasticL3, as it was sticking through the lift ramp wall.
' v0.20	Added 3D insert prims.  Updated the playfield for insert cutouts.  Added a text only ramp overlay.
'		Changed bulb intensity scale to 0.35
' v0.21	Added Lampz in code
' v0.22 Adjusting light insert values: started with default values: falloff 50, falloff power 2, intensity 10, scale mesh 10, transmit 0.5.
'		Then adjusted using the light editor mode.
'		Put the text image for inserts at alpha = 50
'		Adjusted the Lampz.FadeSpeedDown(x) = 1/20 (was 1/40).
' v0.23	Feedback from apophis: 
'		  The L1 insert should be tied to L46... old way of controlling inserts, via timer, it was called, "automatic light mapping feature". the lamp number needed to be entered in the Timer Interval parameter for the light
'		  Removed the off insert primitives from the Lampz controller code. 
'		  Disabled screeen space reflections.
'		  Removed the pflightscount routines. No longer needed.
'		  Made the flupper dome flare less bright.(from 0.3 to 0.1)
'		  Added bloom lights for all inserts (about half had them previously).  Adjusted to a darker bloom, lower intensity and matching color 
'		Lowered the white rectangle inserts z height slightly.
'		Blue inserts were too bright, darkened the material for those.
'		Lots of insert lighting adjustments
'		Adjusted the targets for GI lighting
' v0.24	Adjusted the disablelighting of the flasher dome lights to fade with the same GI / GIStep like the rest of the table.
' v0.25	Adjusted the ball brightness GI level 
' v0.26	Added script to make the insert lights get brighter as GI gets darker  
' v0.27	Additional Feedback from apophis:
'		  Made the inserts fade faster, as hard to see blink.  Lampz.fadespeedup to 1/3 and speed down to 1/8.  
'		  Reduced amount of dynamic sources for shadows from 20 to 8. This will reduce processing, and if too many some shadows disappear
'		  pDrainPost needed more DL applied to it and lowered the bloom level of the associated L78 lights 
'		  Lowered the brightness of the apron when GI is off.
' v0.28	Changed the apron cards to a ramp rather than decal.  Adjusted the brigthness of the apron cards in the GI script.  Just added to GITarget collection.
' v0.29	Wylte recommended changing the playfield friction from .2 to .25.  Also knocker postion was visible.
' v0.30 Tomate added Wood walls mapped correction
' v0.31	Changed the GI on the ramplift flaps to be controlled via disable lighting rather than a material step.  It was flashing, and turned black during GI before.
' v0.32	Changed the GI on the ApronPlungerCover like apron cards... created a ramp and tied to GITarget collection.
'		Updated cab pov per BountyBob's suggestions.
'		Removing the pflights portion of code removed the backglass jackpot lights.  Added to lampz MassAssign
' v0.33	Aubrel suggested changing the ball's playfield reflection to a smaller value like 0.35 (instead of 1).  Had a slight ball ring.  
'		To better match the PAPA gameplay, increased the bumper force from 10.5 to 11.5, flippers from 2800 to 3200, and slings from 4.5 to 5.
'		Changed the slowdown to saucer sub velocity from "/2" to "/2.5"
'		Updated the spinner to move correctly by putting a negative value in it's .RotX callout.
'		Added a small curved wall to top of metal guides between in and outlanes to stop ball from occassionally stopping there.
'		Added a blocking wall behind the left loop ramp. Pinstratsdan got the ball to jump out of the loop and onto the adjacent plastic. Should have been there from start.
'		Moved the plunger wall back slightly to get ball closer to plunger.
'		The jackpot numbers on the desktop were blurry.  Modified image slightly to look better.
' v0.34 Added a flashing light on the desktop backglass
' v0.35	Added updated center drain post dome from Bord's space shuttle (done by Rothbauerw).
' v2.0	Released version 

'		Thank you to all the testers on the VPW team for testing, feedback, and suggestions for improvement:  
'			nFozzy, apophis, Aubrel, Wylte, Rik, Rajo Joey, iaaki, BountyBob, rothbauerw, tomate, PinStratsDan, Lumi


' -------------------------------------------------
' IF YOU HAVE PERFORMANCE ISSUES or LOW FRAME RATE:
' -------------------------------------------------
' Try to set some of the objects to "Static rendering". Open layer 7, select all screws and/or locknuts and select "Static Rendering" in the options windows
' Try disabling "Reflect Elements On Playfield". This will increase performance for lesser powerful systems, and increase frame rate.


' ALSO NOTE:  If you have issues with the ROM, or it's not accepting coins... it is likely you will need the: 
'	Pre-initialized NVRAM files for Bally MPU A084-91786-AH06 (6803) and Gottlieb System 3 VPM Tables to accept coins:
'	https://www.vpforums.org/index.php?app=downloads&showfile=1362
'

Option Explicit
Randomize

Dim GIColorMod, FlipperColorMod, BumperColorMod, WireColorMod, SubmarineModel, EnableGI, EnableGIInlane, EnableFlasher, ShadowOpacityGIOff, ShadowOpacityGIOn, rampisdown


'******************************************************************************************
'* TABLE OPTIONS **************************************************************************
'******************************************************************************************

const VR_Room = 0 ' 1 = VR Room; 0 = desktop or cab mode

	Dim cab_mode, DesktopMode: DesktopMode = Atlantis.ShowDT
	If Not DesktopMode and VR_Room=0 Then cab_mode=1 Else cab_mode=0

const BallBrightness = 1 '0 = dark, 1 = bright, 2 = brighter, 3 = brightest

' *** If using VR Room: ***

const CustomWalls = 0 	 'set to 0 for Modern Minimal Walls, floor, and roof, 1 for Sixtoe's original walls and floor
const WallClock = 1	  	 '1 Shows the clock in the VR room only   
const topper = 1		 '0 = Off 1= On - Topper visible in VR Room only
const poster = 1		 '1 Shows the flyer posters in the VR room only 
const poster2 = 1		 '1 Shows the flyer posters in the VR room only 
const sideblades = 0	 '1 uses custom plywood sideblades and 0 uses generic black sideblades in VR only


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
'   0 = White and red GI (default)
'   1 = Ice blue/white and red GI
GIColorMod = 0

' MAGNA SAVE MOD ADJUSTMENTS 
' Ability to adjust Color or Flipper Mod with Left / Right Magna Save buttons during gameplay
const MagnaOn = 1  '1 = Ability On, 0 = Ability Off

' FLIPPER COLOR MOD
'	0 = Yellow/red
'	1 = Yellow/red/Bally
'   2 = Yellow/blue/Bally (default)

FlipperColorMod = 2

' BUMPER COLOR MOD
'	0 = Red/blue (Flupper style)
'	1 = Red/white (default)
'	2 = Blue/white
'   3 = Transparent/blue
BumperColorMod = 1

' WIRE COLOR MOD
'	0 = Metal wires (default)
' 	1 = Blue metal wires
WireColorMod = 0

' SUBMARINE MODEL
'	0 = Schreibi34 model 1 (100% render result but a bit blurry from the side or the back)
'	1 = Schreibi34 model 2 (80% render result, crisp from all sides) (default)
'	2 = DCrosby model (thx a lot 32assassin for providing me this model)
SubmarineModel = 1

' ENABLE/DISABLE GI (general illumination)
'	0 = GI is off
'	1 = GI is on (value is a multiplicator for GI intensity - decimal values like 0.7 or 1.33 are valid too)
EnableGI = 1

' ENABLE/DISABLE GI inlane bulbs
'	0 = GI inlane bulbs are off (original pin)
'	1 = GI inlane bulbs are on (default)
EnableGIInlane = 1

' ENABLE/DISABLE flasher
'	0 = Flashers are off
'	1 = Flashers are on
EnableFlasher = 1

' PLAYFIELD SHADOW INTENSITY DURING GI OFF OR ON (adds additional visual depth) 
' usable range is 0 (lighter) - 100 (darker)  
ShadowOpacityGIOff = 92
ShadowOpacityGIOn  = 75

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
Const cGameName 	= "atlantis"	'ROM name
Const ballsize 		= 50
Const ballmass 		= 1
Const UsingROM = True		

'***********************

Const tnob = 3						'Total number of balls
Const lob = 0						'Locked balls

Dim tablewidth: tablewidth = Atlantis.width
Dim tableheight: tableheight = Atlantis.height
Dim i, MBall1, MBall2, MBall3, gBOT, ballinslot1, ballinslot2, inmultiball
Dim BIPL : BIPL = False				'Ball in plunger lane
dim bbgi  	'Ball Brightness GI

If Version < 10500 Then
	MsgBox "This table requires Visual Pinball 10.6 Beta or newer!" & vbNewLine & "Your version: " & Replace(Version/1000,",","."), , "Atlantis VPX"
End If

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package."
On Error Goto 0

LoadVPM "01560000", "6803.VBS", 3.26


' ****************************************************
' table init
' ****************************************************
Sub Atlantis_Init()
	vpmInit Me 
	With Controller
        .GameName 			= cGameName
        .SplashInfoLine 	= "Atlantis (Bally/Midway 1989)"
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
	vpmNudge.TiltSwitch		= 15
	vpmNudge.Sensitivity	= 6
	vpmNudge.TiltObj		= Array(Bumper17, Bumper18, Bumper19, LeftSlingshot, RightSlingshot)
	

	'************  Trough	**************
	Set MBall1 = BallRelease.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set MBall2 = sw47.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set MBall3 = sw46.CreateSizedballWithMass(Ballsize/2,Ballmass)
	gBOT = Array(MBall1,MBall2, MBall3)

	Controller.Switch(46) = 1
	Controller.Switch(47) = 1
	Controller.Switch(48) = 1


	' added these variables to control the GI lighting on and off when a ball gets drained, and some balls are locked in sub.
	ballinslot1 = 1
	ballinslot2 = 1
	inmultiball = 0


	' init lights, flippers, bumpers, ...
	InitFlasher_Orig
	InitGI True
	InitFlippers
	InitBumpers
	InitRampLift
	InitSubModel
	InitDomeLights
	InitDrainPost
	bbgi = 0
	centerpostcollide.Collidable=0
	
	' init timers
	PinMAMETimer.Interval 				= PinMAMEInterval
    PinMAMETimer.Enabled  				= True


' VR Backglass lighting
	If VR_Room = 1 Then
		SetBackglass
		PinCab_Backglass.blenddisablelighting = .2

	' backdrop objects
		l57.Visible = 0
		l73.Visible = 0
		l89.Visible = 0
		l58.Visible = 0
		l74.Visible = 0
		l90.Visible = 0
		l59.Visible = 0
		l75.Visible = 0
		DTBackglassLight.visible = 0

	Else
	' backdrop objects
		l57.Visible = DesktopMode
		l73.Visible = DesktopMode
		l89.Visible = DesktopMode
		l58.Visible = DesktopMode
		l74.Visible = DesktopMode
		l90.Visible = DesktopMode
		l59.Visible = DesktopMode
		l75.Visible = DesktopMode
		DTBackglassLight.visible = DesktopMode
	End If

End Sub

Sub Atlantis_Paused() 	: Controller.Pause = True : End Sub
Sub Atlantis_UnPaused() : Controller.Pause = False : End Sub
Sub Atlantis_Exit()		: Controller.Stop : End Sub


'*******************************************
'  Timers
'*******************************************

' The game timer interval is 10 ms
Sub GameTimer_Timer()
	Cor.Update 						'update ball tracking
	RollingUpdate					'update rolling sounds
	saucers_on
	DoDTAnim 						'handle drop target animations
	DoSTAnim						'handle stand up target animations
End Sub

' The frame timer interval is -1, so executes at the display frame rate
Sub FrameTimer_Timer()
	FlipperVisualUpdate				'update flipper shadows and primitives
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
	SpinnerTimer
UpdateBallBrightness
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
  If VR_Primary_plunger.Y < -65 then
       VR_Primary_plunger.Y = VR_Primary_plunger.Y + 5
  End If
End Sub

Sub TimerVRPlunger1_Timer
  VR_Primary_plunger.Y = -160 + (5* Plunger.Position) -20
End Sub


' ****************************************************
' keys
' ****************************************************
Sub Atlantis_KeyDown(ByVal keycode)

    If keycode = PlungerKey Then 
		Plunger.Pullback
		SoundPlungerPull
		TimerVRPlunger.Enabled = True
		TimerVRPlunger1.Enabled = False
		VR_Primary_plunger.Y = -160
	End If

	If keycode = LeftFlipperKey  Then 
		FlipperActivate LeftFlipper, LFPress
		Controller.Switch(5) = True
		VR_FB_Left.X = VR_FB_Left.X +10
	End If

	If keycode = RightFlipperKey Then
		FlipperActivate RightFlipper, RFPress
		Controller.Switch(7) = True
		VR_FB_Right.X = VR_FB_Right.X - 10
	End If

	if keycode = StartGameKey then 
		Primary_start_button.y= 962.7488 - 5
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
		If keycode = LeftMagnaSave Then SubmarineModel = (SubmarineModel + 1) MOD 3 : InitSubModel
		If keycode = RightMagnaSave Then FlipperColorMod = (FlipperColorMod + 1) MOD 3 : ResetFlippers
	End If

	If vpmKeyDown(keycode) Then Exit Sub

End Sub

Sub Atlantis_KeyUp(ByVal keycode)

    If keycode = PlungerKey Then 
		Plunger.Fire
		If BIPL = 1 Then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
		TimerVRPlunger.Enabled = False
		TimerVRPlunger1.Enabled = True
		VR_Primary_plunger.Y = -160
	end if

	If keycode = LeftFlipperKey  Then
		FlipperDeActivate LeftFlipper, LFPress
		Controller.Switch(5) = False
		VR_FB_Left.X = VR_FB_Left.X -10
	End If
	If keycode = RightFlipperKey Then
		FlipperDeActivate RightFlipper, RFPress
		Controller.Switch(5) = False
		VR_FB_Right.X = VR_FB_Right.X +10
	End If

	if keycode = StartGameKey then 
		Primary_start_button.y= 962.7488
	End If

    If KeyUpHandler(keycode) Then Exit Sub

End Sub


' ****************************************************
' *** solenoids - looks like no ID matches the manual
' ****************************************************
SolCallBack(1)  		= "SolVUKKickout"
SolCallback(2)	       	= "SolDropTargetReset"
SolCallBack(3)  		= "SolRampUp"
SolCallback(9)  		= "SolDrainPost"
SolCallBack(10) 		= "SolRampDown"
SolCallBack(11) 		= "SolSaucerKickout"
SolCallback(12)	       	= "SolBallRelease"
SolCallback(14)         = "SolOuthole"
SolCallback(15)			= "SolKnocker"
SolCallback(18) 		= "SolReleaseLockedBall"

SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sLRFlipper) = "SolRFlipper"


' ******************************************************
' outhole, drain and ball release
' ******************************************************

'******************************************************
'			TROUGH BASED ON FOZZY'S
'******************************************************

Sub BallRelease_Hit():Controller.Switch(48) = 1: ballinslot1 = 1: UpdateTrough: End Sub
Sub BallRelease_UnHit():Controller.Switch(48) = 0: ballinslot1 = 0: UpdateTrough:End Sub
Sub sw47_Hit():Controller.Switch(47) = 1: ballinslot2 = 1: UpdateTrough:End Sub
Sub sw47_UnHit():Controller.Switch(47) = 0: ballinslot2 = 0: UpdateTrough:End Sub
Sub sw46_Hit():Controller.Switch(46) = 1:UpdateTrough:End Sub
Sub sw46_UnHit():Controller.Switch(46) = 0:UpdateTrough:End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If BallRelease.BallCntOver = 0 Then 
		sw47.kick 60, 10
	End If
	If sw47.BallCntOver = 0 Then sw46.kick 60, 10	
	Me.Enabled = 0
End Sub


'******************************************************
'				DRAIN & RELEASE
'******************************************************

Sub Drain_Hit()
	RandomSoundDrain Drain
	Controller.Switch(8) = 1
	if lockedBalls = 0 Then
		if inmultiball = 1 Then 		'if in multiball, don't do GITimer
			if ballinslot1 = 1 AND ballinslot2 = 1 Then
				inmultiball = 0
				GITimer.Interval = 1000 : GITimer_Timer
			end If
		Else							' if not in multiball, do GITimer
			GITimer.Interval = 1000 : GITimer_Timer
		End If
	Elseif lockedBalls = 1 AND ballinslot1 = 1 Then
		GITimer.Interval = 1000 : GITimer_Timer
	Elseif lockedBalls = 2 Then
		GITimer.Interval = 1000 : GITimer_Timer
	End If

End Sub

Sub Drain_UnHit()
	Controller.Switch(8) = 0
	SoundSaucerKick 1, Drain
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
	If FlipperColorMod >= 1 Then
		pLeftFlipperBally.ObjRotZ  = LeftFlipper.CurrentAngle - 90
		pRightFlipperBally.ObjRotZ = RightFlipper.CurrentAngle + 90
	End If
	pLeftFlipperShadow.ObjRotZ  = LeftFlipper.CurrentAngle + 1
	pRightFlipperShadow.ObjRotZ = RightFlipper.CurrentAngle + 1
End Sub


Sub ResetFlippers()
	InitFlippers
End Sub
Sub InitFlippers()
	' initialize flipper colors
	pLeftFlipperBally.Visible 	= (FlipperColorMod >= 1)
	pRightFlipperBally.Visible 	= pLeftFlipperBally.Visible
	LeftFlipper.Visible			= Not pLeftFlipperBally.Visible
	RightFlipper.Visible		= LeftFlipper.Visible
	Select Case FlipperColorMod
	Case 1
		pLeftFlipperBally.Image  	= "Yellow_Red"
		pRightFlipperBally.Image 	= "Yellow_Red"
	Case 2
		pLeftFlipperBally.Image  	= "Yellow_Blue"
		pRightFlipperBally.Image 	= "Yellow_Blue"
	End Select
End Sub


' ****************************************************
' sling shots and animations
' ****************************************************
Dim LeftStep, RightStep

Sub LeftSlingShot_Slingshot()
	vpmTimer.PulseSw 20
	RandomSoundSlingshotLeft pLeftSlingHammer
    LeftStep = 0
	LeftSlingShot.TimerInterval = 20
    LeftSlingShot.TimerEnabled  = True
	LeftSlingShot_Timer
End Sub
Sub LeftSlingShot_Timer()
    Select Case LeftStep
		Case 0: LeftSling1.Visible = False : LeftSling3.Visible = True : pLeftSlingHammer.TransZ = -28
        Case 1: LeftSling3.Visible = False : LeftSling2.Visible = True : pLeftSlingHammer.TransZ = -10
        Case 2: LeftSling2.Visible = False : LeftSling1.Visible = True : pLeftSlingHammer.TransZ = 0 : LeftSlingShot.TimerEnabled = 0
    End Select
    LeftStep = LeftStep + 1
End Sub

Sub RightSlingShot_Slingshot()
	vpmTimer.PulseSw 21
	RandomSoundSlingshotRight pRightSlingHammer
    RightStep = 0
	RightSlingShot.TimerInterval = 20
    RightSlingShot.TimerEnabled  = True
	RightSlingShot_Timer
End Sub
Sub RightSlingShot_Timer()
    Select Case RightStep
		Case 0: RightSling1.Visible = False : RightSling3.Visible = True : pRightSlingHammer.TransZ = -28
        Case 1: RightSling3.Visible = False : RightSling2.Visible = True : pRightSlingHammer.TransZ = -10
        Case 2: RightSling2.Visible = False : RightSling1.Visible = True : pRightSlingHammer.TransZ = 0 : RightSlingShot.TimerEnabled = 0
    End Select
    RightStep = RightStep + 1
End Sub


' ****************************************************
' bumpers
' ****************************************************
Sub InitBumpers()
	pBumperTop17.Visible		= (BumperColorMod = 0)
	pBumperTop18.Visible		= pBumperTop17.Visible
	pBumperTop19.Visible		= pBumperTop17.Visible
	pBumperTop17a.Visible		= Not pBumperTop17.Visible
	pBumperTop18a.Visible		= Not pBumperTop17.Visible
	pBumperTop19a.Visible		= Not pBumperTop17.Visible
	If BumperColorMod = 1 Then
		pBumperTop17a.Image		= "BumperTopRed"
		pBumperTop18a.Image		= "BumperTopRed"
		pBumperTop19a.Image		= "BumperTopRed"
	ElseIf BumperColorMod = 3 Then
		pBumperTop17a.Image		= "BumperTopWhite"
		pBumperTop18a.Image		= "BumperTopWhite"
		pBumperTop19a.Image		= "BumperTopWhite"
	End If
	If BumperColorMod = 1 Or BumperColorMod = 2 Then
		Bumper17.SkirtMaterial	= "Plastic White Dark"
		Bumper18.SkirtMaterial	= "Plastic White Dark"
		Bumper19.SkirtMaterial	= "Plastic White Dark"
	End If
End Sub

' bumpers
Sub Bumper17_Hit() : vpmTimer.PulseSw 17 : RandomSoundBumperTop Bumper17 : End Sub
Sub Bumper18_Hit() : vpmTimer.PulseSw 18 : RandomSoundBumperBottom Bumper18 : End Sub
Sub Bumper19_Hit() : vpmTimer.PulseSw 19 : RandomSoundBumperTop Bumper19 : End Sub


' ****************************************************
' switches
' ****************************************************
' stand-up targets

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


' top lanes
Sub sw12_Hit()   : Controller.Switch(12) = True  : End Sub
Sub sw12_Unhit() : Controller.Switch(12) = False : End Sub
Sub sw13_Hit()   : Controller.Switch(13) = True  : End Sub
Sub sw13_Unhit() : Controller.Switch(13) = False : End Sub

' orbit lanes
Sub sw45a_Hit()  : Controller.Switch(45) = True  : End Sub
Sub sw45a_Unhit(): Controller.Switch(45) = False : End Sub
Sub sw45b_Hit()  : Controller.Switch(45) = True  : End Sub
Sub sw45b_Unhit(): Controller.Switch(45) = False : End Sub

' inlanes and outlanes
Sub sw23_Hit()   : Controller.Switch(23) = True  : End Sub
Sub sw23_Unhit() : Controller.Switch(23) = False : End Sub
Sub sw24_Hit()   : Controller.Switch(24) = True  : End Sub
Sub sw24_Unhit() : Controller.Switch(24) = False : End Sub
Sub sw38a_Hit()  : Controller.Switch(38) = True  : End Sub
Sub sw38a_Unhit(): Controller.Switch(38) = False : End Sub
Sub sw38b_Hit()  : Controller.Switch(38) = True  : End Sub
Sub sw38b_Unhit(): Controller.Switch(38) = False : End Sub

' plunger switch
Sub sw16_Hit()   : Controller.Switch(16) = True  : End Sub   
Sub sw16_UnHit() : Controller.Switch(16) = False : GITimer.Interval = 100 : GITimer_Timer : End Sub

' rebounders

Sub sw31a_hit : vpmTimer.PulseSw 31 : End Sub
Sub sw31b_hit : vpmTimer.PulseSw 31 : End Sub
Sub sw31c_hit : vpmTimer.PulseSw 31 : End Sub

' spinner
Sub sw36_Spin()  : vpmTimer.PulseSw 36 : SoundSpinner sw36 : End Sub

' wire loop triggers
Dim sw32Dir
Sub sw32_Hit()   : Controller.Switch(32) = True  : sw32_Timer : End Sub
Sub sw32_Unhit() : Controller.Switch(32) = False : End Sub
Sub sw32_Timer()
	If Not sw32.TimerEnabled Then sw32.TimerInterval = 15 : sw32.TimerEnabled = True : End If
	If sw32P.ObjRotZ = 20 Then sw32Dir = -4 Else If sw32P.ObjRotZ = 0 Then sw32Dir = 5
	sw32P.ObjRotZ = sw32P.ObjRotZ + sw32Dir
	If sw32P.ObjRotZ = 0 Then sw32.TimerEnabled = False
End Sub

Sub trLoopRampStart_Hit()
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound
End Sub
Sub trLoopRampEnd_Hit()
	WireRampOff ' Turn off the Ramp Sound
End Sub
Sub trVUKRampStart_Hit()
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound
End Sub
Sub trVUKRampEnd_Hit()
	WireRampOff ' Turn off the Ramp Sound
End Sub

Sub trRampLift_Hit()
	if rampisdown = 1 Then
		WireRampOn True 'Play Plastic Ramp Sound
	end If
End Sub


'********************************************
'  Drop Target Controls
'********************************************

ReDim isTargetDown(4)

' Drop targets
Sub sw1_Hit
	DTHit 1
End Sub

Sub sw2_Hit
	DTHit 2
End Sub

Sub sw3_Hit
	DTHit 3
End Sub

Sub sw4_Hit
	DTHit 4
End Sub

Sub SolDropTargetReset(enabled)
	dim xx
	if enabled then
		RandomSoundDropTargetReset sw1p
		DTRaise 1
		DTRaise 2
		DTRaise 3
		DTRaise 4
		For i = 1 To 4 : isTargetDown(i) = False : Next
		UpdateGILightsBehindDropTargets
	end if
End Sub


' ****************************************************
' rotating spinner
' ****************************************************
Sub SpinnerTimer()
	pSpinner.RotX 		= -sw36.CurrentAngle
    pSpinnerRod.TransZ 	= -Sin(sw36.CurrentAngle * 2 * 3.14 / 360) * 5
    pSpinnerRod.TransX 	= Sin((sw36.CurrentAngle - 90) * 2 * 3.14 / 360) * -5
End Sub


' ****************************************************
' saucer
' ****************************************************
Dim sw39Step : sw39Step = 0
Dim sw39Ball : Set sw39Ball = Nothing

Sub sw39_Hit()   : SoundSaucerLock: SlowDownBall ActiveBall : End Sub
Sub sw39_Unhit() : Controller.Switch(39) = False : End Sub

Sub SolSaucerKickout(Enabled)
	If Enabled Then
		MoveHammer pSaucerHammer, 0
		sw39Step 			= 0
		sw39.TimerInterval 	= 11
		sw39.TimerEnabled 	= True
		kSaucerKicker.enabled = 1
	End If
End Sub
Sub sw39_Timer()
	Select Case sw39Step
		Case 0
			MoveHammer pSaucerHammer, -1
			SoundSaucerKick 1,  sw39
		Case 1
			MoveHammer pSaucerHammer, 16
			If Controller.Switch(39) Then
				Dim RNDKickValue
				RNDKickValue = RndInt(25,40)    ' Generate random value between 25 and 40.
				kSaucerKicker.kick 45, RNDKickValue
				wSaucerStop.Collidable	= False
			End If
		Case 7
			Controller.Switch(39) = False
		Case 25,26,27,28,29
			MoveHammer pSaucerHammer, -2
		Case 30
			MoveHammer pSaucerHammer,  0
			wSaucerStop.Collidable	= True
			sw39.TimerEnabled 		= False
			kSaucerKicker.enabled = 0
		Case Else
			' nothing to do
	End Select
	sw39Step = sw39Step + 1
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

Sub SlowDownToSaucer_Hit
	activeball.vely = activeball.vely / 2.5
	activeball.velx = activeball.velx / 2.5
End Sub


' ****************************************************
' VUK
' ****************************************************
Dim sw40Step : sw40Step = 0
Dim sw40Ball : Set sw40Ball = Nothing

Sub sw40_Unhit() : Controller.Switch(40) = False : End Sub

Sub SolVUKKickout(Enabled)
	If Enabled Then
		MoveVUKKicker pVUKKicker, 0
		sw40Step 			= 0
		sw40.TimerInterval 	= 11
		sw40.TimerEnabled 	= True
		kVUKKicker.enabled = 1
	End If
End Sub
Sub sw40_Timer()
	Select Case sw40Step
		Case 0
			MoveVUKKicker pVUKKicker, -5
		Case 1
			MoveVUKKicker pVUKKicker, 35
			If Controller.Switch(40) Then 
				Dim RNDKickValue
				RNDKickValue = RndInt(30,40)    ' Generate random value between 30 and 40.
				kVUKKicker.kick 0, RNDKickValue, 1.5
				SoundSaucerKick 1, sw40
			End If
		Case 7
			Controller.Switch(40) = False
		Case 8,9,10,11,12,13,14,15,16,17
			MoveVUKKicker pVUKKicker, -3
		Case 18
			MoveVUKKicker pVUKKicker,  0
			sw40.TimerEnabled = False
			kVUKKicker.enabled = 0
		Case Else
			' nothing to do
	End Select
	sw40Step = sw40Step + 1
End Sub

Sub MoveVUKKicker(hammer, moveZ)
	If moveZ = 0 Then
		hammer.Z = -20
	Else
		hammer.Z = hammer.Z + moveZ
	End If
End Sub

Sub trVUKSlowDown_Hit()
	With ActiveBall
		.VelX = 0 : .VelY = 0 : .VelZ = Int(Rnd()*6)
	End With
End Sub


Sub saucers_on()
    Dim b
    For b = 0 to UBound(gBOT)

		' saucer sw39
		If InCircle(gBOT(b).X, gBOT(b).Y, sw39.X, sw39.Y, 20) AND gBOT(b).Z < 75 Then
			If Abs(gBOT(b).VelX) < 3 And Abs(gBOT(b).VelY) < 3 Then
				gBOT(b).VelX = gBOT(b).VelX / 3						
				gBOT(b).VelY = gBOT(b).VelY / 3
				If Not Controller.Switch(39) Then
					Controller.Switch(39) 	= True
					Set sw39Ball 			= gBOT(b)
				End If
			End If
		End If

		' VUK sw40
		If InCircle(gBOT(b).X, gBOT(b).Y, sw40.X, sw40.Y, 20) Then
			If Abs(gBOT(b).VelX) < 2 And Abs(gBOT(b).VelY) < 2 Then
				gBOT(b).VelX = gBOT(b).VelX / 3
				gBOT(b).VelY = gBOT(b).VelY / 3
				If Not Controller.Switch(40) Then
					Controller.Switch(40) 	= True
					Set sw40Ball 			= gBOT(b)
					PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd*2)+1), SaucerLockSoundLevel, kVUKKicker
				End If
			End If
		End If

		' Corner condition of ball stuck on right metal guide
		If InCircle(gBOT(b).X, gBOT(b).Y, 792, 1301, 10) AND gBOT(b).Z < 60 Then
			If Abs(gBOT(b).VelX) < .005 And Abs(gBOT(b).VelY) < .005 Then
				gBOT(b).VelX = -0.5
			End If
		End If

		' Corner condition of ball stuck on left metal guide
		If InCircle(gBOT(b).X, gBOT(b).Y, 90, 1301, 10) AND gBOT(b).Z < 60 Then
			If Abs(gBOT(b).VelX) < .005 And Abs(gBOT(b).VelY) < .005 Then
				gBOT(b).VelX = 0.5
			End If
		End If
	Next
End Sub

'*******************************************
'  Knocker Solenoid
'*******************************************

Sub SolKnocker(Enabled)
	If enabled Then
		KnockerSolenoid
	End If
End Sub

' ****************************************************
' ramp lift
' ****************************************************

Sub InitRampLift()
	SolRampUp True
End Sub 

Sub SolRampUp(Enabled)
	If Enabled Then 
		rampisdown = 0
		RampLiftTimer_Timer
	End If
End Sub

Sub SolRampDown(Enabled)
	If Enabled Then 
		rampisdown = 1
		RampLiftTimer_Timer
	End If
End Sub

Dim rampStep : rampStep = 1
Sub RampLiftTimer_Timer()
	' sounds and enable timer
	If Not RampLiftTimer.Enabled Then 
		PlaySoundAtVol "fx_solenoid", pLiftRampLifter, 0.5
		Controller.Switch(37) 	= rampisdown
		RampLiftTimer.Enabled 	= True
		pLiftRampMetal.ObjRotX = 0 : pLiftRampLowerFlap.ObjRotX = 0
	End If
	' move the ramp
	rampStep = rampStep + IIF(rampisdown=0, -0.05, 0.05)
	If rampStep <= 0 Then
		rampStep				= 0
		RampLiftTimer.Enabled 	= False
	ElseIf rampStep >= 1 Then
		rampStep				= 1
		RampLiftTimer.Enabled 	= False
		pLiftRampMetal.ObjRotX = 1 : pLiftRampLowerFlap.ObjRotX = 1.1
	End If
	rLiftRampDown.Collidable	= (rampStep > 0.8)
	rLiftRampUp.Collidable		= (rampStep < 0.33)
	rLiftRampMid.Collidable		= Not (rLiftRampDown.Collidable Or rLiftRampUp.Collidable)
	pLiftRampTopFlapUp.Visible	= rLiftRampUp.Collidable
	pLiftRampTopFlapMid.Visible	= rLiftRampMid.Collidable
	pLiftRampTopFlapDown.Visible= rLiftRampDown.Collidable
	' move the prims
	pLiftRamp.RotX 			= -12 * rampStep
	pLiftRampMetal.RotX		= -12 * rampStep
	pLiftRampLowerFlap.RotX	= -12 * rampStep
	pLiftRampLifter.TransY	= -60 * (1-rampStep)
	pLiftRampLifter.TransZ	= -24 * rampStep
End Sub


' *********************************************************************
' ball locking in the sub
' *********************************************************************
Sub InitSubModel()
	pSubPerspective.Visible = (SubmarineModel = 0)
	pSubBake.Visible 		= (SubmarineModel = 1)
	pSubmarine.Visible 		= (SubmarineModel = 2)
End Sub

Dim lockedBalls  : lockedBalls 	= 0
Sub sw44_Hit()
	SoundSaucerLock
	Controller.Switch(44) 	= True
End Sub

Dim sw42Dir
Sub sw42_Hit()   : Controller.Switch(42) = True  : sw42_Timer : End Sub
Sub sw42_Unhit() : Controller.Switch(42) = False : End Sub
Sub sw42_Timer()
	If Not sw42.TimerEnabled Then sw42.TimerInterval = 15 : sw42.TimerEnabled = True : End If
	If sw42P.ObjRotZ = 30 Then sw42Dir = -5 Else If sw42P.ObjRotZ = 0 Then sw42Dir = 5
	sw42P.ObjRotZ = sw42P.ObjRotZ + sw42Dir
	If sw42P.ObjRotZ = 0 Then sw42.TimerEnabled = False
End Sub

Dim sw43Dir
Sub sw43_Hit()   : Controller.Switch(43) = True  : sw43_Timer : End Sub
Sub sw43_Unhit() : Controller.Switch(43) = False : End Sub
Sub sw43_Timer()
	If Not sw43.TimerEnabled Then sw43.TimerInterval = 15 : sw43.TimerEnabled = True : End If
	If sw43P.ObjRotZ = -30 Then sw43Dir = 5 Else If sw43P.ObjRotZ = 0 Then sw43Dir = -5
	sw43P.ObjRotZ = sw43P.ObjRotZ + sw43Dir
	If sw43P.ObjRotZ = 0 Then sw43.TimerEnabled = False
End Sub

Sub SolReleaseLockedBall(Enabled)
	If Enabled Then
		if controller.Switch(44) = True Then
			PlaySoundAt "fx_solenoid", sw44
		End If
		sw44.Kick 170, 10
		Controller.Switch(44) = False
	End If
End Sub

Sub trLockSpeedUp_Hit()
	With ActiveBall
		.VelX = .VelX - 0.5 + Rnd()
		.VelY = 22 + Int(Rnd()*8)
	End With
End Sub

Sub trLockedBallsStart_Hit()
	lockedBalls	 = lockedBalls + 1
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub
Sub trLockedBallsEnd_Hit()
	lockedBalls	 = lockedBalls - 1
	inmultiball = 1
End Sub


'******************************************************
' 						CENTER POST
'******************************************************
Dim isDrainPostDown : isDrainPostDown = False

Sub InitDrainPost()
	SolDrainPost True
End Sub

Dim CenterPostDir, MovePostSpeed
CenterPostCollide.timerinterval = 8
MovePostSpeed = 7

Sub CenterPostCollide_timer()
	CenterPost.transz = CenterPost.transz + CenterPostDir*MovePostSpeed/5

	if CenterPost.transz > 13 Then
		centerpostcollide.collidable = True
	Else
		centerpostcollide.collidable = False	
	End If

	If CenterPost.transz < 25 Then lp1.state = 0

	If CenterPost.transz < 0.1 Then
		CenterPost.transz = 0
		me.timerenabled = False
	End If
	If CenterPost.transz > 24.9 Then
		CenterPost.transz = 25
		me.timerenabled = False
	End If
	cprod.transz = centerpost.transz
End Sub

Sub SolDrainPost(Enabled)
    If Enabled Then
	  If isDrainPostDown = True Then
		CenterPostDir = 1
		CenterPostcollide.timerenabled = true
		PlaySoundAtVol "DropTarget_Up", centerpost, .25
	  Elseif isDrainPostDown = False Then
		PlaySoundAtVol "DropTarget_Down", centerpost, 1.25
		CenterPostDir = -1
		CenterPostcollide.timerenabled = true
	  End If
		isDrainPostDown 		= Not isDrainPostDown
		Controller.Switch(22) 	= isDrainPostDown	
    End If
End Sub


' *********************************************************************
' lamps and illumination - even the lamp IDs doesn't match the manual
' *********************************************************************
' inserts
Dim currentGILevel, isGIWhiteOn, isGIRedOn


Sub InsertFlasherTimer_Timer()
	Dim obj, setTimerOff
	setTimerOff = True
	For Each obj In InsertFlasher
		If obj.IntensityScale = 0.93 Or obj.IntensityScale = 0.62 Or obj.IntensityScale = 0.31 Then
			obj.IntensityScale = obj.IntensityScale - 0.31
			If obj.IntensityScale > 0 Then setTimerOff = False
		End If
		If obj.IntensityScale = 0.1 Or obj.IntensityScale = 0.4 Or obj.IntensityScale = 0.7 Then
			obj.IntensityScale = obj.IntensityScale + 0.3
			If obj.IntensityScale < 1 Then setTimerOff = False
		End If
	Next
	If setTimerOff Then InsertFlasherTimer.Enabled = False
End Sub

Sub BackboardLampTimer_Timer()
	If l85.Opacity > 50 Then
		l85.Opacity = l85.Opacity / 2
	Else
		l85.Visible = False
		BackboardLampTimer.Enabled = False
	End If
End Sub

' flasher
Const minFlasherNo = 1
Const maxFlasherNo = 8
ReDim fValue(maxFlasherNo,14) : For i = minFlasherNo To maxFlasherNo : fValue(i,0) = 0 : Next

Sub InitFlasher_Orig()
	' playfield
	fValue(1,4) 		= Array(fLight13,fLight13a,FlasherBulb13,fPlasticsLight13)
	fValue(1,5) 		= WhiteFlasher
	fValue(4,4) 		= Array(fLight91,FlasherBulb91,fPlasticsLight91)
	fValue(4,5) 		= WhiteFlasher
	fValue(5,4) 		= Array(fLight92,FlasherBulb92,fPlasticsLight92)
	fValue(5,5) 		= WhiteFlasher
	' start flasher timer
	FlasherTimer.Interval = 15
	If Not FlasherTimer.Enabled Then FlasherTimer_Timer
End Sub

Sub Flash(flasherNo, flasherValue)
	If EnableFlasher = 0 Then Exit Sub
	' set value
	fValue(flasherNo,0) = IIF((flasherValue=1),1,fDecrease(fValue(flasherNo,7)))
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
			If IsObject(fValue(ii,1)) Then
				If Not fValue(ii,1).Visible Then 
					fValue(ii,1).Visible = True : If IsObject(fValue(ii,2)) Then fValue(ii,2).Visible = True
				End If
			End If
			' calc values
			flashx3 = fValue(ii,0) ^ 3
			matdim 	= Round(10 * fValue(ii,0))
			' set flasher object values
			If IsObject(fValue(ii,1)) Then fValue(ii,1).Opacity = fOpacity(fValue(ii,5)) * flashx3
			If IsObject(fValue(ii,2)) Then fValue(ii,2).BlendDisableLighting = 10 * flashx3 : fValue(ii,2).Material = "domelit" & matdim : fValue(ii,2).Visible = Not (fValue(ii,0) < 0.15)
			If IsObject(fValue(ii,3)) Then fValue(ii,3).BlendDisableLighting =  flashx3
			If IsObject(fValue(ii,4)) Then fValue(ii,4).IntensityScale = flashx3 : fValue(ii,4).State = IIF(flashx3<=0,LightstateOff,LightStateOn)
			If IsArray(fValue(ii,4)) Then
				For Each obj In fValue(ii,4) : obj.IntensityScale = flashx3 : obj.State = IIF(flashx3<=0,LightstateOff,LightStateOn) : Next
			End If
			' decrease flasher value
			If fValue(ii,0) < 1 Then fValue(ii,0) = fValue(ii,0) * fDecrease(fValue(ii,5)) - 0.01
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

Dim WhiteFlasher, WhiteFlasherOpacity, WhiteFlasherDecrease
WhiteFlasher			= 1
WhiteFlasherOpacity		= 1000
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

' general illumination
Sub ResetGI()
	InitGI False
End Sub
Sub InitGI(startUp)
	If startUp Then
		isGIWhiteOn 	= False
		isGIRedOn 		= False
		SetGI
		SetShadowOpacityAndGIOverhead
		SetMaterials
	End If
	Dim coll, obj
	' init GI overhead
	For Each obj In Array(GIOverhead,GIOverheadUpperPF)
		obj.IntensityScale 	= IIF(isGIWhiteOn Or isGIRedOn,1,0)
		obj.visible  = 1
		If isGIRedOn Then
			obj.Color		= RedOverhead
		Else
			If GIColorMod = 1 Then
				obj.Color		= IceBlueOverhead
			Else
				obj.Color		= WhiteOverhead
			End If
		End If
	Next
	' init GI bulbs
	For Each obj In GIBulbs
		obj.State = LightStateOn
		If Right(obj.Name,1) = "r" Then
			obj.IntensityScale 	= IIF(isGIRedOn,1,0)
			obj.Color			= RedBulbs
			obj.ColorFull		= RedBulbsFull
			obj.Intensity 		= RedBulbsI * EnableGI
		Else
			obj.IntensityScale 	= IIF(isGIWhiteOn,1,0)
			If GIColorMod = 1 And Right(obj.Name,1) = "b" Then
				obj.Color		= IceBlueBulbs
				obj.ColorFull	= IceBlueBulbsFull
				obj.Intensity 	= IceBlueBulbsI * EnableGI
			Else
				obj.Color		= WhiteBulbs
				obj.ColorFull 	= WhiteBulbsFull
				obj.Intensity 	= WhiteBulbsI * EnableGI
			End If
		End If
	Next
	' init GI lights
	For Each coll In Array(GILeft,GIRight,GIDropTargets)
		For Each obj in coll
			obj.State = LightStateOn
			If Right(obj.Name,1) = "r" Then
				obj.IntensityScale 	= IIF(isGIRedOn,1,0)
				obj.Color			= Red
				obj.ColorFull		= RedFull
				obj.Intensity 		= RedI * EnableGI
			Else
				obj.IntensityScale 	= IIF(isGIWhiteOn,1,0)
				If GIColorMod = 1 And Right(obj.Name,1) = "b" Then
					obj.Color		= IceBlue
					obj.ColorFull	= IceBlueFull
					obj.Intensity 	= IceBlueI * EnableGI
				Else
					obj.Color		= White
					obj.ColorFull	= WhiteFull
					obj.Intensity 	= WhiteI * EnableGI
				End If
			End If
		Next
	Next
	For Each coll In Array(GIRightTop)
		For Each obj in coll
			obj.State = LightStateOn
			If Right(obj.Name,1) = "r" Then
				obj.IntensityScale 	= IIF(isGIRedOn,1,0)
				obj.Color			= Red
				obj.ColorFull		= RedFull
				obj.Intensity 		= RedI * EnableGI
			Else
				obj.IntensityScale 	= IIF(isGIWhiteOn,1,0)
				If GIColorMod = 1 And Right(obj.Name,1) = "b" Then
					obj.Color		= IceBlue
					obj.ColorFull	= IceBlueFull
					obj.Intensity 	= IceBlueI * EnableGI
				Else
					obj.Color		= White
					obj.ColorFull	= WhiteFull
					obj.Intensity 	= WhiteI * EnableGI*.5
				End If
			End If
		Next
	Next
	' init GI plastics lights
	For Each coll In Array(GIPlasticsLeft,GIPlasticsRight)
		For Each obj In coll
			obj.State = LightStateOn
			If Right(obj.Name,1) = "r" Then
				obj.IntensityScale 	= IIF(isGIRedOn,1,0)
				obj.Color			= Red
				obj.ColorFull		= RedFull
				obj.Intensity 		= RedPlasticI * EnableGI
			Else
				obj.IntensityScale 	= IIF(isGIWhiteOn,1,0)
				If GIColorMod = 1 And Right(obj.Name,1) = "b" Then
					obj.Color		= White
					obj.ColorFull	= WhiteFull
					obj.Intensity 	= WhitePlasticI * EnableGI
				ElseIf GIColorMod = 2 Then
					obj.Color		= IceBlue
					obj.ColorFull	= IceBlueFull
					obj.Intensity 	= IceBluePlasticI * EnableGI
				End If
				' reduce the intensity a bit for second level plastics
				If obj.Surface = "72h" Then obj.Intensity = obj.Intensity / 3
			End If
		Next
	Next
	' init additional flasher for lights
	For Each obj In InsertFlasher
		obj.IntensityScale 	= 0
	Next
	l85.Opacity = 0
	' init flasher bulbs
	For Each obj In FlasherBulbs
		obj.IntensityScale 	= 0
		obj.State			= LightStateOn
		obj.Color			= WhiteBulbs
		obj.ColorFull 		= WhiteBulbsFull
		obj.Intensity 		= WhiteBulbsI * 2 * EnableGI
	Next
	' init bumper lights
	For Each obj In GIBumperLights
		If BumperColorMod = 0 Or BumperColorMod = 1 Then
			obj.Color		= RedBumper
			obj.ColorFull	= RedBumperFull
			obj.Intensity	= RedBumperI
		ElseIf BumperColorMod = 2 Then
			obj.Color		= WhiteBumper
			obj.ColorFull	= WhiteBumperFull
			obj.Intensity	= WhiteBumperI * 2
		ElseIf BumperColorMod = 3 Then
			obj.Color		= WhiteBumper
			obj.ColorFull	= WhiteBumperFull
			obj.Intensity	= WhiteBumperI
		End If
	Next
End Sub

Dim GIWhiteDir  : GIWhiteDir = 0
Dim GIWhiteStep : GIWhiteStep = 0
Dim GIRedDir    : GIRedDir = 0
Dim GIRedStep   : GIRedStep = 0
dim gilvl:gilvl = 1

Sub SetGI()
   If EnableGI = 0 Then 
	  gilvl = 0
	  Exit Sub
   Else
	' white GI goes on or off
	dim soundPlayed : soundPlayed = False

	If isGIWhiteOn And GIWhiteStep < 4 Then
		GIWhiteDir = 1
		If Not GIWhiteTimer.Enabled Or GIWhiteDir <> 1 Then
			Sound_GI_Relay soundPlayed, Relay_GI
			soundPlayed = True
			DOF 101, DOFOn
			GIWhiteTimer_Timer
		End If
	ElseIf Not isGIWhiteOn And GIWhiteStep > 0 Then
		GIWhiteDir = -1
		If Not GIWhiteTimer.Enabled Or GIWhiteDir <> -1 Then
			Sound_GI_Relay soundPlayed, Relay_GI
			soundPlayed = True
			DOF 101, DOFOff
			GIWhiteTimer_Timer
		End If
	End If
	' red GI goes on or off
	If isGIRedOn And GIRedStep < 4 Then
		GIRedDir = 1
		If Not GIRedTimer.Enabled Or GIRedDir <> 1 Then
			If Not soundPlayed Then Sound_GI_Relay soundPlayed, Relay_GI
			DOF 101, DOFOn
			GIRedTimer_Timer
		End If
	ElseIf Not isGIRedOn And GIRedStep > 0 Then
		GIRedDir = -1
		If Not GIRedTimer.Enabled Or GIRedDir <> -1 Then
			If Not soundPlayed Then Sound_GI_Relay soundPlayed, Relay_GI
			DOF 101, DOFOff
			GIRedTimer_Timer
		End If
	End If
   End If
End Sub
Sub GIWhiteTimer_Timer()
	If Not GIWhiteTimer.Enabled Then GIWhiteTimer.Enabled = True
	GIWhiteStep = GIWhiteStep + GIWhiteDir
	' set opacity of the shadow overlays
	SetShadowOpacityAndGIOverhead()
	' set white GI illumination
	SetGIIllumination True
	' set bumper lights
	SetBumperLights
	' set material of targets, posts, pegs, ramps etc
	SetMaterials
	' set GI Brightness for Flasher Domes
	SetDomeLights
	' set GI Brightness for Ball
	SetBallGI
	' white GI on/off goes in 4 steps so maybe stop timer
	If (GIWhiteDir = 1 And isGIWhiteOn And GIWhiteStep >= 4) Or (GIWhiteDir = -1 And GIWhiteStep <= 0) Or GIWhiteDir = 0 Then
		GIWhiteTimer.Enabled = False
	End If
End Sub
Sub GIRedTimer_Timer()
	If Not GIRedTimer.Enabled Then GIRedTimer.Enabled = True
	GIRedStep = GIRedStep + GIRedDir
	' set opacity of the shadow overlays
	SetShadowOpacityAndGIOverhead()
	' set red GI illumination
	SetGIIllumination False
	' set bumper lights
	SetBumperLights
	' set material of targets, posts, pegs, ramps etc
	SetMaterials
	' set GI Brightness for Flasher Domes
	SetDomeLights
	' set GI Brightness for Ball
	SetBallGI
	' red GI on/off goes in 4 steps so maybe stop timer
	If (GIRedDir = 1 And isGIRedOn And GIRedStep >= 4) Or (GIRedDir = -1 And GIRedStep <= 0) Or GIRedDir = 0 Then
		GIRedTimer.Enabled = False
	End If
End Sub
Sub SetShadowOpacityAndGIOverhead()
	Dim currentGIStep
	currentGIStep 	= Maximum(GIWhiteStep,GIRedStep)
	' shadow layers
	fGIOff.Opacity 	= (ShadowOpacityGIOff / 4) * (4 - currentGIStep)
	fGIOn.Opacity 	= (ShadowOpacityGIOn / 4) * currentGIStep
	' overhead GI
	With GIOverhead
		If isGIRedOn Then
			.Color		= RedOverhead
		Else
			If GIColorMod = 1 Then
				.Color		= IceBlueOverhead
			Else
				.Color		= WhiteOverhead
			End If
		End If
		.IntensityScale = currentGIStep/4
	End With
	With GIOverheadUpperPF
		.Color			= IIF(isGIRedOn, RedOverhead, IceBlueOverhead)
		.IntensityScale = IIF(GIColorMod=1 And isGIWhiteOn, currentGIStep/4, 0)
	End With
End Sub
Sub SetGIIllumination(setWhite)
	Dim coll, obj
	If setWhite Then
		For Each coll In Array(GILeft,GIPlasticsLeft,GIRight,GIRightTop,GIPlasticsRight,GIBulbs)
			For Each obj In coll
				If obj.TimerInterval <> -1 Then
					If EnableGIInlane = 0 And Right(obj.Name,1) = "x" Then
						obj.IntensityScale = 0
					ElseIf Right(obj.Name,1) <> "r" Then
						obj.IntensityScale = GIWhiteStep/4
					End If
				End If
			Next
		Next
	Else
		For Each coll In Array(GILeft,GIPlasticsLeft,GIRight,GIRightTop,GIPlasticsRight,GIBulbs)
			For Each obj In coll
				If obj.TimerInterval <> -1 Then
					If EnableGIInlane = 0 And Right(obj.Name,1) = "x" Then
						obj.IntensityScale = 0
					ElseIf Right(obj.Name,1) = "r" Then 
						obj.IntensityScale = GIRedStep/4
					End If
				End If
			Next
		Next
	End If
End Sub
Sub SetBumperLights()
	Dim obj, currentGIStep
	currentGIStep = Maximum(GIWhiteStep,GIRedStep)
	For Each obj In GIBumperLights
		obj.IntensityScale 	= currentGIStep/4 * 0.6
		obj.State 			= IIF(currentGIStep <= 0, LightstateOff, LightStateOn)
	Next
End Sub

Sub SetBallGI()
	Dim obj, currentGIStep
	currentGIStep = Maximum(GIWhiteStep,GIRedStep)
		bbgi= IIF(currentGIStep <= 0, 0, 1)
End Sub

Sub InitDomeLights
	Flasherbase1.blenddisablelighting=FlasherOffBrightness/6
	Flasherbase2.blenddisablelighting=FlasherOffBrightness/6
	Flasherbase3.blenddisablelighting=FlasherOffBrightness/6
	Flasherbase4.blenddisablelighting=FlasherOffBrightness/6
	Flasherbase5.blenddisablelighting=FlasherOffBrightness/6
	pApron.blenddisablelighting=FlasherOffBrightness/12
	pLiftRampLowerFlap.blenddisablelighting=FlasherOffBrightness/12
	pLiftRampTopFlapDown.blenddisablelighting=FlasherOffBrightness/12
	pLiftRampTopFlapMid.blenddisablelighting=FlasherOffBrightness/12
	pLiftRampTopFlapUp.blenddisablelighting=FlasherOffBrightness/12
End Sub

Sub SetDomeLights()
	Dim obj, currentGIStep
	currentGIStep = Maximum(GIWhiteStep,GIRedStep)
	Flasherbase1.blenddisablelighting=currentgiStep/6
	Flasherbase2.blenddisablelighting=currentgiStep/6
	Flasherbase3.blenddisablelighting=currentgiStep/6
	Flasherbase4.blenddisablelighting=currentgiStep/6
	Flasherbase5.blenddisablelighting=currentgiStep/6
	pApron.blenddisablelighting=currentgiStep/12
	pLiftRampLowerFlap.blenddisablelighting=currentgiStep/24
	pLiftRampTopFlapDown.blenddisablelighting=currentgiStep/24
	pLiftRampTopFlapMid.blenddisablelighting=currentgiStep/24
	pLiftRampTopFlapUp.blenddisablelighting=currentgiStep/24
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
	Dim obj, currentGIStep
	currentGIStep = Maximum(GIWhiteStep,GIRedStep)
	UseGIMaterial GIWireTrigger, "Metal0.8", currentGIStep
	UseGIMaterial GILocknuts, "Metal0.8", currentGIStep
	UseGIMaterial GIScrews, "Metal0.8", currentGIStep
	UseGIMaterial GIMetalPrims, "Metal0.8", currentGIStep
	UseGIMaterial GIYellowPosts, "Plastic White", currentGIStep
	UseGIMaterial GIPlasticScrews, "Plastic White", currentGIStep
	UseGIMaterial GIWhiteAndBlackPosts, "Plastic White", currentGIStep
	UseGIMaterial GITargets, "Plastic with an image", currentGIStep
	UseGIMaterial GIRubbers, "Rubber White", currentGIStep
	UseGIMaterial GIPlasticPegs, "TransparentPlasticRed", currentGIStep
	UseGIMaterial GIGates, "Metal Wires", currentGIStep
	UseGIMaterial GIBumperTops, "BumperTopRed", currentGIStep
	UseGIMaterial Array(pLeftFlipperBally,pRightFlipperBally,pSubmarine,pBumperTop17a,pBumperTop18a,pBumperTop19a), "Plastic with an image", currentGIStep
	UseGIMaterial Array(pLiftRampMetal,pLevelPlate), "Metal0.8", currentGIStep
	UseGIMaterial Array(pMetalWallsPlayfield,pMetalWallsLift,sw32P,sw42P,sw43P), "Metal", currentGIStep
	If EnableGIInlane = 1 Then UseGIMaterial Array(pLeftInlane,pRightInlane), "Metal S34", currentGIStep
	UseGIMaterial Array(rBBMountR1,rBBMountR2,rBBMountR3,rBBMountL1,rBBMountL2,rBBMountL3), "Metal Chrome S34", currentGIStep
	UseGIMaterial Array(pSubPerspective,pSubBake), "Prims platt", currentGIStep
	UseGIMaterial Array(pSpinner), "Plastic with an image V", currentGIStep
	wBackboard.SideMaterial = CreateMaterialName("Plastics Light", currentGIStep)
	For Each obj In GIMetalPosts: obj.TopMaterial = CreateMaterialName("Metal0.8",currentGIStep) : obj.SideMaterial = CreateMaterialName("Metal0.8",currentGIStep) : Next
	If GIWhiteStep = 4 Or GIRedStep = 4 Then
		For Each obj In Array(Bumper17,Bumper18,Bumper19) : obj.SkirtMaterial	= IIF(BumperColorMod=1 Or BumperColorMod=2,"Plastic White Dark","Plastic Cobalt Blue") : Next
		For Each obj In Array(pSubPerspective,pSubBake) : obj.Material = "Prims platt" & IIF(isGIRedOn," Red","") : Next
		For Each obj In Array(LeftFlipper,RightFlipper) : obj.Material = "Plastic Yellow" : obj.RubberMaterial = "Red Rubber" : Next
		wBackboard.IsDropped 		= Not (GIWhiteStep = 4 And GIColorMod = 0)
		wBackboardIceBlue.IsDropped = Not (GIWhiteStep = 4 And GIColorMod = 1)
		wBackboardRed.IsDropped 	= Not (GIRedStep = 4)
		wBackboardDark.IsDropped 	= True
	ElseIf GIWhiteStep = 0 And GIRedStep = 0 Then
		For Each obj In Array(Bumper17,Bumper18,Bumper19) : obj.SkirtMaterial	= IIF(BumperColorMod=1 Or BumperColorMod=2,"Plastic White Very Dark","Plastic Cobalt Blue Dark") : Next
		For Each obj In Array(pLeftInlane,pRightInlane) : obj.Material = "Metal S34 Dark" : Next
		For Each obj In Array(pBumperTop17a,pBumperTop18a,pBumperTop19a) : obj.Material = "Plastic with an image Very Dark" : Next
		For Each obj In Array(LeftFlipper,RightFlipper) : obj.Material = "Plastic Yellow Dark" : obj.RubberMaterial = "Red Rubber Dark" : Next
		wBackboard.IsDropped 		= True
		wBackboardIceBlue.IsDropped = True
		wBackboardRed.IsDropped 	= True
		wBackboardDark.IsDropped 	= False
	End If
	' ramps
	If isGIWhiteOn Then
		pPlasticRamp.Material = "rampsGI" & (GIWhiteStep * 2)
	ElseIf isGIRedOn Then
		pPlasticRamp.Material = "rampsGI8Red"
	End If
	pWireRamp.Material = "wireramp" & IIF(WireColorMod = 1, " Blue", "") & IIF(GIWhiteStep = 0 And GIRedStep = 0, " Dark", "")
	UpdateGILightsBehindDropTargets
End Sub
Sub UseGIMaterial(coll, matName, currentGIStep)
	Dim obj, mat
	mat = CreateMaterialName(matName, currentGIStep)
	For Each obj In coll : obj.Material = mat : Next
End Sub
Function CreateMaterialName(matName, currentGIStep)
	CreateMaterialName = matName & IIF(currentGIStep=0, " Dark", IIF(currentGIStep<4, " Dark" & currentGIStep,""))
End Function
Sub UpdateGILightsBehindDropTargets()
	GILightDT1b.IntensityScale 	= IIF(isTargetDown(1) And isGIWhiteOn, GIWhiteStep/4, 0)
	GILightDT2b.IntensityScale 	= IIF(isTargetDown(2) And isGIWhiteOn, GIWhiteStep/4, 0)
	GILightDT3b.IntensityScale 	= IIF(isTargetDown(3) And isGIWhiteOn, GIWhiteStep/4, 0)
	GILightDT4b.IntensityScale 	= IIF(isTargetDown(4) And isGIWhiteOn, GIWhiteStep/4, 0)
	GILightDT1r.IntensityScale 	= IIF(isTargetDown(1) And isGIRedOn, GIRedStep/4, 0)
	GILightDT2r.IntensityScale 	= IIF(isTargetDown(2) And isGIRedOn, GIRedStep/4, 0)
	GILightDT3r.IntensityScale 	= IIF(isTargetDown(3) And isGIRedOn, GIRedStep/4, 0)
	GILightDT4r.IntensityScale 	= IIF(isTargetDown(4) And isGIRedOn, GIRedStep/4, 0)
End Sub

Dim giStep : giStep = 0
Sub GITimer_Timer()
	giStep = giStep + 1
	If Not GITimer.Enabled Then
		giStep = 0
		gilvl = 0
		If GITimer.Interval >= 1000 Then
			' nothing to do
		Else
			isGIWhiteOn = False : isGIRedOn = False : SetGI
		End If
		GITimer.Enabled = True
	Else
		gilvl = 1
		If GITimer.Interval >= 1000 Then
			If giStep = 1 Then
				isGIWhiteOn = False : isGIRedOn = False : SetGI
			ElseIf giStep >= 3 Then
				isGIWhiteOn = True : isGIRedOn = False : SetGI
				GITimer.Enabled = False
			End If
		Else
			isGIWhiteOn = Not isGIWhiteOn : isGIRedOn = False : SetGI
			If giStep >= 5 Then
				isGIWhiteOn = True : isGIRedOn = False : SetGI
				GITimer.Enabled = False
			End If
		End If
	End If
End Sub


' *********************************************************************
' colors
' *********************************************************************
Dim White, WhiteFull, WhiteI, WhiteP, WhitePlastic, WhitePlasticFull, WhitePlasticI, WhiteBumper, WhiteBumperFull, WhiteBumperI, WhiteBulbs, WhiteBulbsFull, WhiteBulbsI, WhiteOverheadFull, WhiteOverhead, WhiteOverheadI
WhiteFull = rgb(255,255,255) 
White = rgb(255,255,180)
WhiteI = 20
WhitePlasticFull = rgb(255,255,180) 
WhitePlastic = rgb(255,255,180)
WhitePlasticI = 25
WhiteBumperFull = rgb(255,255,180) 
WhiteBumper = rgb(255,255,180)
WhiteBumperI = 25
WhiteBulbsFull = rgb(255,255,180)
WhiteBulbs = rgb(255,255,180)
WhiteBulbsI = 100 * ShadowOpacityGIOff
WhiteOverheadFull = rgb(255,255,180)
WhiteOverhead = rgb(255,255,180)
WhiteOverheadI = .25

Dim Red, RedFull, RedI, RedPlastic, RedPlasticFull, RedPlasticI, RedBumper, RedBumperFull, RedBumperI, RedBulbs, RedBulbsFull, RedBulbsI, RedOverheadFull, RedOverhead, RedOverheadI
RedFull = rgb(255,75,75)
Red = rgb(255,75,75)
RedI = 50
RedPlasticFull = rgb(255,75,75)
RedPlastic = rgb(255,75,75)
RedPlasticI = 25
RedBumperFull = rgb(255,75,75)
RedBumper = rgb(255,128,32)
RedBumperI = 75
RedBulbsFull = rgb(255,75,75)
RedBulbs = rgb(255,75,75)
RedBulbsI = 250 * ShadowOpacityGIOff
RedOverheadFull = rgb(255,10,10)
RedOverhead = rgb(255,10,10)
RedOverheadI = 0.25

Dim IceBlue, IceBlueFull, IceBlueI, IceBluePlastic, IceBluePlasticFull, IceBluePlasticI, IceBlueBumper, IceBlueBumperFull, IceBlueBumperI, IceBlueBulbs, IceBlueBulbsFull, IceBlueBulbsI, IceBlueOverheadFull, IceBlueOverhead, IceBlueOverheadI
IceBlueFull = rgb(224,255,255)
IceBlue = rgb(224,255,255)
IceBlueI = 15
IceBluePlasticFull = rgb(224,255,255)
IceBluePlastic = rgb(224,255,255)
IceBluePlasticI = 25
IceBlueBumperFull = rgb(224,255,255)
IceBlueBumper = rgb(224,255,255)
IceBlueBumperI = 50
IceBlueBulbsFull = rgb(224,255,255)
IceBlueBulbs = rgb(224,255,255)
IceBlueBulbsI = 100
IceBlueOverheadFull = rgb(224,255,255)
IceBlueOverhead = rgb(224,255,255)
IceBlueOverheadI = .5

Dim Blue, BlueFull, BlueI, BluePlastic, BluePlasticFull, BluePlasticI, BlueBumper, BlueBumperFull, BlueBumperI, BlueBulbs, BlueBulbsFull, BlueBulbsI,  BlueOverheadFull, BlueOverhead, BlueOverheadI
BlueFull = rgb(75,75,255)
Blue = rgb(75,75,255)
BlueI = 50
BluePlasticFull = rgb(75,75,255)
BluePlastic = rgb(75,75,255)
BluePlasticI = 25
BlueBumperFull = rgb(0,0,255)
BlueBumper = rgb(0,0,255)
BlueBumperI = 50
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
		Object.Opacity = 10
	Else
		Object.Color = RGB(1,1,1)
		Object.Opacity = 12
	End If
End Sub

Dim Digits(28)
Digits(0) = Array(LED1x0,LED1x1,LED1x2,LED1x3,LED1x4,LED1x5,LED1x6,LED1x7,LED1x8,LED1x9)
Digits(1) = Array(LED2x0,LED2x1,LED2x2,LED2x3,LED2x4,LED2x5,LED2x6,LED2x7,LED2x8,LED2x9)
Digits(2) = Array(LED3x0,LED3x1,LED3x2,LED3x3,LED3x4,LED3x5,LED3x6,LED3x7,LED3x8,LED3x9)
Digits(3) = Array(LED4x0,LED4x1,LED4x2,LED4x3,LED4x4,LED4x5,LED4x6,LED4x7,LED4x8,LED4x9)
Digits(4) = Array(LED5x0,LED5x1,LED5x2,LED5x3,LED5x4,LED5x5,LED5x6,LED5x7,LED5x8,LED5x9)
Digits(5) = Array(LED6x0,LED6x1,LED6x2,LED6x3,LED6x4,LED6x5,LED6x6,LED6x7,LED6x8,LED6x9)
Digits(6) = Array(LED7x0,LED7x1,LED7x2,LED7x3,LED7x4,LED7x5,LED7x6,LED7x7,LED7x8,LED7x9)

Digits(7) = Array(LED8x0,LED8x1,LED8x2,LED8x3,LED8x4,LED8x5,LED8x6,LED8x7,LED8x8,LED8x9)
Digits(8) = Array(LED9x0,LED9x1,LED9x2,LED9x3,LED9x4,LED9x5,LED9x6,LED9x7,LED9x8,LED9x9)
Digits(9) = Array(LED10x0,LED10x1,LED10x2,LED10x3,LED10x4,LED10x5,LED10x6,LED10x7,LED10x8,LED10x9)
Digits(10) = Array(LED11x0,LED11x1,LED11x2,LED11x3,LED11x4,LED11x5,LED11x6,LED11x7,LED11x8,LED11x9)
Digits(11) = Array(LED12x0,LED12x1,LED12x2,LED12x3,LED12x4,LED12x5,LED12x6,LED12x7,LED12x8,LED12x9)
Digits(12) = Array(LED13x0,LED13x1,LED13x2,LED13x3,LED13x4,LED13x5,LED13x6,LED13x7,LED13x8,LED13x9)
Digits(13) = Array(LED14x0,LED14x1,LED14x2,LED14x3,LED14x4,LED14x5,LED14x6,LED14x7,LED14x8,LED14x9)

Digits(14) = Array(LED1x000,LED1x001,LED1x002,LED1x003,LED1x004,LED1x005,LED1x006,LED1x007,LED1x008,LED1x009)
Digits(15) = Array(LED1x100,LED1x101,LED1x102,LED1x103,LED1x104,LED1x105,LED1x106,LED1x107,LED1x108,LED1x109)
Digits(16) = Array(LED1x200,LED1x201,LED1x202,LED1x203,LED1x204,LED1x205,LED1x206,LED1x207,LED1x208,LED1x209)
Digits(17) = Array(LED1x300,LED1x301,LED1x302,LED1x303,LED1x304,LED1x305,LED1x306,LED1x307,LED1x308,LED1x309)
Digits(18) = Array(LED1x400,LED1x401,LED1x402,LED1x403,LED1x404,LED1x405,LED1x406,LED1x407,LED1x408,LED1x409)
Digits(19) = Array(LED1x500,LED1x501,LED1x502,LED1x503,LED1x504,LED1x505,LED1x506,LED1x507,LED1x508,LED1x509)
Digits(20) = Array(LED1x600,LED1x601,LED1x602,LED1x603,LED1x604,LED1x605,LED1x606,LED1x607,LED1x608,LED1x609)

Digits(21) = Array(LED2x000,LED2x001,LED2x002,LED2x003,LED2x004,LED2x005,LED2x006,LED2x007,LED2x008,LED2x009)
Digits(22) = Array(LED2x100,LED2x101,LED2x102,LED2x103,LED2x104,LED2x105,LED2x106,LED2x107,LED2x108,LED2x109)
Digits(23) = Array(LED2x200,LED2x201,LED2x202,LED2x203,LED2x204,LED2x205,LED2x206,LED2x207,LED2x208,LED2x209)
Digits(24) = Array(LED2x300,LED2x301,LED2x302,LED2x303,LED2x304,LED2x305,LED2x306,LED2x307,LED2x308,LED2x309)
Digits(25) = Array(LED2x400,LED2x401,LED2x402,LED2x403,LED2x404,LED2x405,LED2x406,LED2x407,LED2x408,LED2x409)
Digits(26) = Array(LED2x500,LED2x501,LED2x502,LED2x503,LED2x504,LED2x505,LED2x506,LED2x507,LED2x508,LED2x509)
Digits(27) = Array(LED2x600,LED2x601,LED2x602,LED2x603,LED2x604,LED2x605,LED2x606,LED2x607,LED2x608,LED2x609)


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


' *********************************************************************
' digital display
' *********************************************************************

Dim DTDigits(28)
DTDigits(0)		= Array(a00,a01,a02,a03,a04,a05,a06,a07,a08)
DTDigits(1)		= Array(a10,a11,a12,a13,a14,a15,a16,a17,a18)
DTDigits(2)		= Array(a20,a21,a22,a23,a24,a25,a26,a27,a28)
DTDigits(3)		= Array(a30,a31,a32,a33,a34,a35,a36,a37,a38)
DTDigits(4)		= Array(a40,a41,a42,a43,a44,a45,a46,a47,a48)
DTDigits(5)		= Array(a50,a51,a52,a53,a54,a55,a56,a57,a58)
DTDigits(6)		= Array(a60,a61,a62,a63,a64,a65,a66,a67,a68)

DTDigits(7)		= Array(b00,b01,b02,b03,b04,b05,b06,b07,b08)
DTDigits(8)		= Array(b10,b11,b12,b13,b14,b15,b16,b17,b18)
DTDigits(9)		= Array(b20,b21,b22,b23,b24,b25,b26,b27,b28)
DTDigits(10)	= Array(b30,b31,b32,b33,b34,b35,b36,b37,b38)
DTDigits(11)	= Array(b40,b41,b42,b43,b44,b45,b46,b47,b48)
DTDigits(12)	= Array(b50,b51,b52,b53,b54,b55,b56,b57,b58)
DTDigits(13)	= Array(b60,b61,b62,b63,b64,b65,b66,b67,b68)

DTDigits(14)	= Array(c00,c01,c02,c03,c04,c05,c06,c07,c08)
DTDigits(15)	= Array(c10,c11,c12,c13,c14,c15,c16,c17,c18)
DTDigits(16)	= Array(c20,c21,c22,c23,c24,c25,c26,c27,c28)
DTDigits(17)	= Array(c30,c31,c32,c33,c34,c35,c36,c37,c38)
DTDigits(18)	= Array(c40,c41,c42,c43,c44,c45,c46,c47,c48)
DTDigits(19)	= Array(c50,c51,c52,c53,c54,c55,c56,c57,c58)
DTDigits(20)	= Array(c60,c61,c62,c63,c64,c65,c66,c67,c68)

DTDigits(21)	= Array(d00,d01,d02,d03,d04,d05,d06,d07,d08)
DTDigits(22)	= Array(d10,d11,d12,d13,d14,d15,d16,d17,d18)
DTDigits(23)	= Array(d20,d21,d22,d23,d24,d25,d26,d27,d28)
DTDigits(24)	= Array(d30,d31,d32,d33,d34,d35,d36,d37,d38)
DTDigits(25)	= Array(d40,d41,d42,d43,d44,d45,d46,d47,d48)
DTDigits(26)	= Array(d50,d51,d52,d53,d54,d55,d56,d57,d58)
DTDigits(27)	= Array(d60,d61,d62,d63,d64,d65,d66,d67,d68)

Sub DisplayTimerDT
    Dim chgLED, ii, num, chg, stat, obj
	chgLED = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If Not IsEmpty(chgLED) Then
			For ii = 0 To UBound(chgLED)
				num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
				if (num < 28) then
					For Each obj In DTDigits(num)
						If chg And 1 Then obj.State = stat And 1
						chg = chg\2 : stat = stat\2
					Next
				End If
			Next
    End If
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
		dim str, b, highestID

		for each b in gBOT
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)	'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)	'set bounds

		for each b in gBOT
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
'****  DROP TARGETS by Rothbauerw
'******************************************************


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
Dim DT1, DT2, DT3, DT4

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

Set DT1 = (new DropTarget)(sw1, sw1a, sw1p, 1, 0, false)
Set DT2 = (new DropTarget)(sw2, sw2a, sw2p, 2, 0, false)
Set DT3 = (new DropTarget)(sw3, sw3a, sw3p, 3, 0, false)
Set DT4 = (new DropTarget)(sw4, sw4a, sw4p, 4, 0, false)

Dim DTArray
DTArray = Array(DT1, DT2, DT3, DT4)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 42.5 'VP units primitive drops so top of at or below the playfield
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

' added to control the GI and shadows of the drop targets 
		isTargetDown(switch) = True
		UpdateGILightsBehindDropTargets

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
				DTAction switchid
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
			Dim BOT, b
			BOT = GetBalls

			For b = 0 to UBound(BOT)
				If InRotRect(BOT(b).x,BOT(b).y,prim.x, prim.y, prim.rotz, -25,-10,25,-10,25,25,-25,25) and BOT(b).z < prim.z+DTDropUnits+25 Then
					BOT(b).velz = 20
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

Sub DTAction(switchid)
	Select Case switchid
		Case 1:
			ShadowDT(0).visible=False
		Case 2:
			ShadowDT(1).visible=False
		Case 3:
			ShadowDT(2).visible=False
	End Select
End Sub


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
Dim ST25, ST26, ST27, ST28, ST29, ST30, ST33, ST34, ST35

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
Set ST33 = (new StandupTarget)(sw33, psw33,33, 0)
Set ST34 = (new StandupTarget)(sw34, psw34,34, 0)
Set ST35 = (new StandupTarget)(sw35, psw35,35, 0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST25, ST26, ST27, ST28, ST29, ST30, ST33, ST34, ST35)

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

Sub STAction(Switch)
	Select Case Switch
		Case 11:
			Addscore 1000
			Flash1 True						'Demo of the flasher
		Case 12:
			Addscore 1000
			Flash2 True						'Demo of the flasher
		Case 13:
			Addscore 1000
			Flash3 True						'Demo of the flasher
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

Const RelayFlashSoundLevel = .15 '0.315									'volume level; range [0, 1];
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

if BallBrightness = 0 Then
	Atlantis.BallImage="ball-dark"
	Atlantis.BallFrontDecal="JPBall-Scratches"
elseif BallBrightness = 1 Then
	Atlantis.BallImage="ball best2"
	Atlantis.BallFrontDecal="g5kscratchedmorelight"
elseif BallBrightness = 2 Then
	Atlantis.BallImage="ball-light-hf"
	Atlantis.BallFrontDecal="g5kscratchedmorelight"
else
	Atlantis.BallImage="ball-lighter-hf"
	Atlantis.BallFrontDecal="g5kscratchedmorelight"
End if


'*******************************************
'  Custom Side Blades
'*******************************************

If sideblades = 1 Then 'Use black plywood grain sideblades
	PinCab_Blades.image = "PinCab_Blades" 
Else
	PinCab_Blades.image = "Black" 
End If


'**********************************************************
'*******	Set Up Backglass and Backglass Flashers	*******
'**********************************************************
' this is for lining up the backglass flashers on top of a backglass image

Sub SetBackglass()
	Dim obj

	For Each obj In BackglassLow
		obj.x = obj.x
		obj.height = - obj.y + 0
		obj.y = -30 'adjusts the distance from the backglass towards the user
	Next

	For Each obj In BackglassMid
		obj.x = obj.x
		obj.height = - obj.y + 0
		obj.y = -47 'adjusts the distance from the backglass towards the user
	Next

	For Each obj In BackglassHigh
		obj.x = obj.x
		obj.height = - obj.y + 0
		obj.y = -60 'adjusts the distance from the backglass towards the user
	Next

	setup_backglass

End Sub


'******************************
' Setup Backglass
'******************************

Dim xoff,yoff1, zoff, xrot, zscale, xcen, ycen

Sub setup_backglass()

	xoff = -0
	yoff1 = -24 ' this is where you adjust the forward/backward position for player scores
	zoff = 495
	xrot = -90


	Dim ix, xx, yy, yfact, xfact, xobj

	zscale = 0.0000001

	xcen = (130 /2) - (92 / 2)
	ycen = (780 /2 ) + (203 /2)

	for ix = 0 to 27
		For Each xobj In Digits(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff1 

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next


end sub


'**********************************************


' ***************************************************************************
'                                  LAMP CALLBACK
' ****************************************************************************

if VR_Room = 1 Then
	Set LampCallback = GetRef("UpdateMultipleLamps")
Else
	Set LampCallback = GetRef("UpdateDTLamp")
End If

Sub UpdateMultipleLamps()
' Jackpot lights
	If Controller.Lamp(57) = 0 Then: BGFl57.visible  =0: else: BGFl57.visible  =1
	If Controller.Lamp(73) = 0 Then: BGFl73.visible  =0: else: BGFl73.visible  =1
	If Controller.Lamp(89) = 0 Then: BGFl89.visible  =0: else: BGFl89.visible  =1
	If Controller.Lamp(58) = 0 Then: BGFl58.visible  =0: else: BGFl58.visible  =1
	If Controller.Lamp(74) = 0 Then: BGFl74.visible  =0: else: BGFl74.visible  =1
	If Controller.Lamp(90) = 0 Then: BGFl90.visible  =0: else: BGFl90.visible  =1
	If Controller.Lamp(59) = 0 Then: BGFl59.visible  =0: else: BGFl59.visible  =1
	If Controller.Lamp(75) = 0 Then: BGFl75.visible  =0: else: BGFl75.visible  =1

' Backglass Flasher Lamps
	If Controller.Lamp(76) = 0 Then: BGFl76.visible  =0: else: BGFl76.visible  =1
	If Controller.Lamp(77) = 0 Then: BGFl77.visible  =0: else: BGFl77.visible  =1
	If Controller.Lamp(13) = 0 Then: BGFl13.visible  =0: else: BGFl13.visible  =1
	If Controller.Lamp(91) = 0 Then: BGFl91.visible  =0: else: BGFl91.visible  =1
	If Controller.Lamp(43) = 0 Then: BGFl43.visible  =0: else: BGFl43.visible  =1
	If Controller.Lamp(62) = 0 Then: BGFl62.visible  =0: else: BGFl62.visible  =1
	If Controller.Lamp(49) = 0 Then: BGFl49.visible  =1: else: BGFl49.visible  =0

End Sub

Sub UpdateDTLamp()
	If Controller.Lamp(49) = 0 Then: DTBackglassLight.state  =1: else: DTBackglassLight.state  =0
End Sub


'******************************************************
'*****   FLUPPER DOMES 
'******************************************************
' Based on FlupperDomes2.2

 Sub Flash1(Enabled)
	If Enabled Then		
		Objlevel(1) = 1 : FlasherFlash1_Timer
	End If
	Sound_Flash_Relay enabled, Flasherbase1
 End Sub

 Sub Flash2(Enabled)
	If Enabled Then
		Objlevel(2) = 1 : FlasherFlash2_Timer
	End If
	Sound_Flash_Relay enabled, Flasherbase2
 End Sub

 Sub Flash3(Enabled)
	If Enabled Then		
		Objlevel(3) = 1 : FlasherFlash3_Timer
	End If
	Sound_Flash_Relay enabled, Flasherbase3
 End Sub

 Sub Flash4(Enabled)
	If Enabled Then
		Objlevel(4) = 1 : FlasherFlash4_Timer
	End If
	Sound_Flash_Relay enabled, Flasherbase1
 End Sub


Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Atlantis  		' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.1		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = .1 '.3  ' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherBloomIntensity = 0.2		' *** lower this, if the blooms are too bright (i.e. 0.1)			***	
FlasherOffBrightness = 0.5		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20)
'Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"

InitFlasher 1, "white"
InitFlasher 2, "red"
InitFlasher 3, "white"
InitFlasher 4, "yellow"
InitFlasher 5, "yellow"

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
			Case 17,33,65,81
				If chglamp(x,0) = 17 Then
					isGIRedOn = (chgLamp(x,1) = 1)
				ElseIf chglamp(x,0) = 33 Then
					isGIWhiteOn = (chgLamp(x,1) = 0)
				ElseIf chglamp(x,0) = 65 Then
					isGIRedOn = (chgLamp(x,1) = 1)
				ElseIf chglamp(x,0) = 81 Then
					isGIWhiteOn = (chgLamp(x,1) = 0)
				End If
				SetGI
			Case 13,43,76,91,92
				' playfield flasher
				If chglamp(x,1) = 1 Then
					If chglamp(x,0) = 13 Then
						Flash 1, 0
					ElseIf chglamp(x,0) = 43 Then
						ObjLevel(4) = 1 : FlasherFlash4_Timer
						Sound_Flash_Relay 1, Flasherbase4
					ElseIf chglamp(x,0) = 76 Then
						ObjLevel(5) = 1 : FlasherFlash5_Timer
						Sound_Flash_Relay 1, Flasherbase5
					ElseIf chglamp(x,0) = 91 Then
						Flash 4, 0
					ElseIf chglamp(x,0) = 92 Then
						Flash 5, 0
					End If
				End If
			Case 45,62,77
				' backboard popper, 'Lane B' and 'Lane A' flasher
				If chglamp(x,1) = 1 Then
					If chglamp(x,0) = 77 Then
						ObjLevel(1) = 1 : FlasherFlash1_Timer
						Sound_Flash_Relay 1, Flasherbase1
					ElseIf chglamp(x,0) = 45 Then
						ObjLevel(2) = 1 : FlasherFlash2_Timer
						Sound_Flash_Relay 1, Flasherbase2
					ElseIf chglamp(x,0) = 62 Then
						ObjLevel(3) = 1 : FlasherFlash3_Timer
						Sound_Flash_Relay 1, Flasherbase3
					End If
				End If
			Case 60
				' jackpot flasher
				l60.State = chgLamp(x,1)
				l34.State = chgLamp(x,1)
'			Case 78
'				' drain post
'				isDrainPostLightOn	= chgLamp(x,1)
'				UpdateLight4DrainPost isDrainPostLightOn
			Case 85
				' backboard lamp "WHEN LIT"
				If chgLamp(x,1) = 1 Then
					l85.Opacity = 800
					l85.Visible = True
				Else
					l85.Opacity = l85.Opacity / 2
					BackboardLampTimer.Enabled = True
				End If
			End Select
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

	If centerpost.blenddisablelighting > 0.1 Then
		CenterPost.image = "pp_popupON"
	Else
		CenterPost.image = "pp_popupOFF"
	End If

	If centerpost.transz = 25 Then
		lp1.state = 1 
		lp1.IntensityScale = lp2.IntensityScale
	else
		lp1.state = 0
	End If

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


const insert_dl_on_default = 130
const insert_dl_on_red = 60
const insert_dl_on_green = 5

Sub InitLampsNF()

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (1 / full MS fading time)
	dim x : for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/3 : Lampz.FadeSpeedDown(x) = 1/8 : next
	Lampz.FadeSpeedUp(102) = 1/4 : Lampz.FadeSpeedDown(102) = 1/16

	'Lampz Assignments
	'  In a ROM based table, the lamp ID is used to set the state of the Lampz objects

	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays
	Lampz.MassAssign(2)= l2
	Lampz.MassAssign(2)= l2a
	Lampz.MassAssign(2)= l2b
	Lampz.Callback(2) = "DisableLighting p2, insert_dl_on_default,"
	Lampz.MassAssign(3)= l3
	Lampz.MassAssign(3)= l3a
	Lampz.Callback(3) = "DisableLighting p3, insert_dl_on_default,"
	Lampz.MassAssign(4)= l4
	Lampz.MassAssign(4)= l4a
	Lampz.Callback(4) = "DisableLighting p4, insert_dl_on_default,"
	Lampz.MassAssign(5)= l5
	Lampz.MassAssign(5)= l5a
	Lampz.Callback(5) = "DisableLighting p5, insert_dl_on_red,"
	Lampz.MassAssign(6)= l6
	Lampz.MassAssign(6)= l6a
	Lampz.Callback(6) = "DisableLighting p6, insert_dl_on_red,"
	Lampz.MassAssign(7)= l7
	Lampz.MassAssign(7)= l7a
	Lampz.Callback(7) = "DisableLighting p7, insert_dl_on_green,"
	Lampz.MassAssign(8)= l8
	Lampz.MassAssign(8)= l8a
	Lampz.Callback(8) = "DisableLighting p8, insert_dl_on_default,"
	Lampz.MassAssign(9)= l9
	Lampz.MassAssign(9)= l9a
	Lampz.MassAssign(9)= l9b
	Lampz.Callback(9) = "DisableLighting p9, insert_dl_on_red,"
	Lampz.MassAssign(10)= l10
	Lampz.MassAssign(10)= l10a
	Lampz.MassAssign(10)= l10b
	Lampz.Callback(10) = "DisableLighting p10, insert_dl_on_red,"
	Lampz.MassAssign(11)= l11
	Lampz.MassAssign(11)= l11a
	Lampz.Callback(11) = "DisableLighting p11, insert_dl_on_red,"
	Lampz.MassAssign(12)= l12
	Lampz.MassAssign(12)= l12a
	Lampz.MassAssign(12)= l12b
	Lampz.MassAssign(15)= l15
	Lampz.MassAssign(15)= l15a
	Lampz.Callback(15) = "DisableLighting p15, insert_dl_on_default,"
	Lampz.MassAssign(18)= l18
	Lampz.MassAssign(18)= l18a
	Lampz.MassAssign(18)= l18b
	Lampz.Callback(18) = "DisableLighting p18, insert_dl_on_default,"
	Lampz.MassAssign(19)= l19
	Lampz.MassAssign(19)= l19a
	Lampz.Callback(19) = "DisableLighting p19, insert_dl_on_default,"
	Lampz.MassAssign(20)= l20
	Lampz.MassAssign(20)= l20a
	Lampz.Callback(20) = "DisableLighting p20, insert_dl_on_default,"
	Lampz.MassAssign(21)= l21
	Lampz.MassAssign(21)= l21a
	Lampz.Callback(21) = "DisableLighting p21, insert_dl_on_red,"
	Lampz.MassAssign(22)= l22
	Lampz.MassAssign(22)= l22a
	Lampz.Callback(22) = "DisableLighting p22, insert_dl_on_red,"
	Lampz.MassAssign(23)= l23
	Lampz.MassAssign(23)= l23a
	Lampz.Callback(23) = "DisableLighting p23, insert_dl_on_green,"
	Lampz.MassAssign(24)= l24
	Lampz.MassAssign(24)= l24a
	Lampz.Callback(24) = "DisableLighting p24, insert_dl_on_default,"
	Lampz.MassAssign(25)= l25
	Lampz.MassAssign(25)= l25a
	Lampz.MassAssign(25)= l25b
	Lampz.Callback(25) = "DisableLighting p25, insert_dl_on_red,"
	Lampz.MassAssign(26)= l26
	Lampz.MassAssign(26)= l26a
	Lampz.MassAssign(26)= l26b
	Lampz.Callback(26) = "DisableLighting p26, insert_dl_on_red,"
	Lampz.MassAssign(27)= l27
	Lampz.MassAssign(27)= l27a
	Lampz.Callback(27) = "DisableLighting p27, insert_dl_on_red,"
	Lampz.MassAssign(30)= l30
	Lampz.MassAssign(30)= l30a
	Lampz.Callback(30) = "DisableLighting p30, insert_dl_on_default,"
	Lampz.MassAssign(31)= l31
	Lampz.MassAssign(31)= l31a
	Lampz.Callback(31) = "DisableLighting p31, insert_dl_on_default,"
	Lampz.MassAssign(34)= l34
	Lampz.MassAssign(34)= l34a
	Lampz.Callback(34) = "DisableLighting p34, insert_dl_on_red,"
	Lampz.MassAssign(35)= l35
	Lampz.MassAssign(35)= l35a
	Lampz.Callback(35) = "DisableLighting p35, insert_dl_on_default,"
	Lampz.MassAssign(36)= l36
	Lampz.MassAssign(36)= l36a
	Lampz.Callback(36) = "DisableLighting p36, insert_dl_on_default,"
	Lampz.MassAssign(37)= l37
	Lampz.MassAssign(37)= l37a
	Lampz.Callback(37) = "DisableLighting p37, insert_dl_on_red,"
	Lampz.MassAssign(38)= l38
	Lampz.MassAssign(38)= l38a
	Lampz.Callback(38) = "DisableLighting p38, insert_dl_on_default,"
	Lampz.MassAssign(39)= l39
	Lampz.MassAssign(39)= l39a
	Lampz.Callback(39) = "DisableLighting p39, insert_dl_on_green,"
	Lampz.MassAssign(40)= l40
	Lampz.MassAssign(40)= l40a
	Lampz.Callback(40) = "DisableLighting p40, insert_dl_on_default,"
	Lampz.MassAssign(41)= l41
	Lampz.MassAssign(41)= l41a
	Lampz.MassAssign(41)= l41b
	Lampz.Callback(41) = "DisableLighting p41, insert_dl_on_red,"
	Lampz.MassAssign(42)= l42
	Lampz.MassAssign(42)= l42a
	Lampz.Callback(42) = "DisableLighting p42, insert_dl_on_red,"
	Lampz.MassAssign(46)= l1
	Lampz.MassAssign(46)= l1a
	Lampz.Callback(46) = "DisableLighting p1, insert_dl_on_default,"
	Lampz.MassAssign(46)= l46
	Lampz.MassAssign(46)= l46a
	Lampz.Callback(46) = "DisableLighting p46, insert_dl_on_default,"
	Lampz.MassAssign(47)= l47
	Lampz.MassAssign(47)= l47a
	Lampz.Callback(47) = "DisableLighting p47, insert_dl_on_default,"
	Lampz.MassAssign(51)= l51
	Lampz.MassAssign(51)= l51a
	Lampz.Callback(51) = "DisableLighting p51, insert_dl_on_default,"
	Lampz.MassAssign(52)= l52
	Lampz.MassAssign(52)= l52a
	Lampz.Callback(52) = "DisableLighting p52, insert_dl_on_default,"
	Lampz.MassAssign(53)= l53
	Lampz.MassAssign(53)= l53a
	Lampz.Callback(53) = "DisableLighting p53, insert_dl_on_default,"
	Lampz.MassAssign(55)= l55
	Lampz.MassAssign(55)= l55a
	Lampz.Callback(55) = "DisableLighting p55, insert_dl_on_default,"
	Lampz.MassAssign(56)= l56
	Lampz.MassAssign(56)= l56a
	Lampz.Callback(56) = "DisableLighting p56, insert_dl_on_default,"
	Lampz.MassAssign(57)= l57
	Lampz.MassAssign(58)= l58
	Lampz.MassAssign(59)= l59
	Lampz.MassAssign(66)= l66
	Lampz.MassAssign(66)= l66a
	Lampz.MassAssign(66)= l66b
	Lampz.MassAssign(66)= l66c
	Lampz.Callback(66) = "DisableLighting p66, insert_dl_on_default,"
	Lampz.MassAssign(67)= l67
	Lampz.MassAssign(67)= l67a
	Lampz.Callback(67) = "DisableLighting p67, insert_dl_on_default,"
	Lampz.MassAssign(68)= l68
	Lampz.MassAssign(68)= l68a
	Lampz.Callback(68) = "DisableLighting p68, insert_dl_on_default,"
	Lampz.MassAssign(69)= l69
	Lampz.MassAssign(69)= l69a
	Lampz.Callback(69) = "DisableLighting p69, insert_dl_on_default,"
	Lampz.MassAssign(70)= l70
	Lampz.MassAssign(70)= l70a
	Lampz.Callback(70) = "DisableLighting p70, insert_dl_on_red,"
	Lampz.MassAssign(71)= l71
	Lampz.MassAssign(71)= l71a
	Lampz.Callback(71) = "DisableLighting p71, insert_dl_on_default,"
	Lampz.MassAssign(72)= l72
	Lampz.MassAssign(72)= l72a
	Lampz.Callback(72) = "DisableLighting p72, insert_dl_on_red,"
	Lampz.MassAssign(73)= l73
	Lampz.MassAssign(74)= l74
	Lampz.MassAssign(75)= l75
	Lampz.MassAssign(78)= CPFlash1
	Lampz.MassAssign(78)= CPFlash2
	Lampz.MassAssign(78)= lp2
	Lampz.Callback(78) = "DisableLighting centerpost, 0.15,"
	Lampz.MassAssign(82)= l82
	Lampz.MassAssign(82)= l82a
	Lampz.Callback(82) = "DisableLighting p82, insert_dl_on_red,"
	Lampz.MassAssign(83)= l83
	Lampz.MassAssign(83)= l83a
	Lampz.Callback(83) = "DisableLighting p83, insert_dl_on_red,"
	Lampz.MassAssign(84)= l84
	Lampz.MassAssign(84)= l84a
	Lampz.Callback(84) = "DisableLighting p84, insert_dl_on_red,"
	Lampz.MassAssign(86)= l86
	Lampz.MassAssign(86)= l86a
	Lampz.Callback(86) = "DisableLighting p86, insert_dl_on_default,"
	Lampz.MassAssign(87)= l87
	Lampz.MassAssign(87)= l87a
	Lampz.Callback(87) = "DisableLighting p87, insert_dl_on_default,"
	Lampz.MassAssign(88)= l88
	Lampz.MassAssign(88)= l88a
	Lampz.Callback(88) = "DisableLighting p88, insert_dl_on_default,"
	Lampz.MassAssign(89)= l89
	Lampz.MassAssign(90)= l90


	Lampz.Callback(102) = "InsertIntensityUpdate"

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

