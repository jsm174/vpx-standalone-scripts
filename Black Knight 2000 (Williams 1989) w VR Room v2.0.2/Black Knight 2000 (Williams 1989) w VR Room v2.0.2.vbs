'*****************************************************************************************
'****************************** Williams, Black Knight 2000, 1989 ************************
'*****************************************************************************************

' Black Knight 2000, ver 1.3 by Flupper
' Based on VP8 version of Lio
' upper and lower playfield redraw by Tomasaco
' Parts of the script/table taken from Totan, AFM, Dirty Harry
' Script review / DOF changes / some soundfx by Ninuzzu
' Several plastics photos from Johngreve
' So big thanks to Lio, Ninuzzu, Tomasaco, Johngreve, JPSalas, Dozer, Knorr
' Version 2.0 VR and Hybrid - UnclePaulie

'******************************************************************************************
'******************************************************************************************
'******************************************************************************************

' release notes

' version 1.0 : initial version

' version 1.1
' SliderPoint's new plunger lane (no more magic balls!)
' Ninuzzu's fix for sounds for rolling on metal and balldrop
' nFozzy's physics changes (rubbers, kickers, FlipperTricks code, ...)
' extensive testing by JohnGreve
' small visual fixes
' added Glowball code for upper wireramps and different switches in beginning of script
' added FS siderails switch
' fixed ball entering trough when falling back down plunger lane
' fixed potential ball hang before left ramp to upf

' version 1.2:
' NOTE:  This release looks its best on a 4k cabinet. If you change the day-night slider, 
'		 please adjust the InsertBrightness value in the script options to your liking.
' New HDR ball, with shadow
' New wireramps, with HDR texture and fake reflections
' new Williams bats, new bumpertops, new LUT

'version 1.3:
' all inserts replaced by 3D prerendered inserts
' fixed ball shadow (thanks Pmax65!)
' replaced magna save bulb
' bumpertops tweaked
' lighting tweaked

'******** Revisions done by UnclePaulie on Hybrid version 0.1 - 2.0 *********
' v0.1 	Added hybrid mode for VR room, cabinet, and desktop
'		Added a rails material1VR to turn off active for VR mode.  The rail light reflections were bad in VR.
'		Added a different backwall primitive for VR.  It was too high.
'		Animated the flippers, start button, and plunger in VR
' v0.2	Animated the backglass in VR 
'		Added an optional topper in VR
' v0.3	Added Updated/more recent Roth/nFozzy physics and Flipper physics and Fleep Sounds
' 		Added dynamic shadows
'		Added updated ramprolling sounds 
'		Modified shape of Light8, as it was very bright and reflected on side wall.
'		Added option for desktop/cab users to remove significant "glow" on rails
'		Added additional ball options without the ball reflectivity, as well as a couple brighter balls from hauntfreaks
'		Reduced reflection of playfield on ball from 6 to 0.5
'		Changed depth bias on Prim6 and 7 to 100.  Huge glow on middle transparent plastics
'		Adjusted the height and rotation of metal triggers.  Too high and incorrect rotation... could really see in VR.
'		Changed the rubber physics guide right of the right sling to a physics wall.
' v0.4	Adjusted physics on bumpers to hit threshold of 1.6, scatter 2.  
'		Adjusted physcis on slings.  They weren't the same.  Made the same and adjusted to zCol_Slings: 
'		Slight update to playfield friction to better match VPW, and some of Flupper's.
'		Adjusted flipper strength to 2600.  
'		Adjusted VR Metalshiny material to check metal material
'		Adjusted shapes of some of the inset lights on layer 2 slightly, as you could see edges in VR.  Needed to align better with the text flasher.
'		Added a black side image material to the apron walls, so you can't see through in VR.
'		Added a upper plyfield VR wall just under the playfield to block being able to see the ball through the insets when on lower playfield.
'		Adjusted the plunger pull speed, as it was too slow
'		Adjusted GI22b,10,9,11 from 25 to 10.  Way too bright.  
'		Changed intenisty of display lights on backglass for desktop mode from 3 to 0.8.  It was too intense/radiated.
'		Added code to adjust some light intensities based off cabinet mode or VR mode or desktop mode:  Too bright in VR and desktop, but looks good on cab.
'		  GI34 from 40 to 25.  GI12,23 from 150 to 50.  Lights 25b,26b,27b from 400 to 200.  GI10,11,19,20 from 75 to 50
'		Cut holes in the playfield for the slings and the rollover targets.  Added plywood insets for more authentic look.
'		Added sling walls and new sling prim.
'		Adjusted the left sling rubber and top peg to the left to aling with actual table.  
'		Adjusted UPPF wall slightly to not intersect the triggers (could see the Wall)
'		Added code to turn ransom off on vr and cab by default... unless a user specifies it...  Desktop always on.
'		Reduced the intensity of lights under slings for VR Mode
'		bumpertop2 had "render backfacing transparent checked.  Unchecked it and you can see though it now.
' v0.5	Added new drop targets by Rothbauerw
'		Added a couple screws to the sling plastics for realism.
'		Changed the live catch constant slightly for flipper physics realism
' v0.6	A few updates to material. Also adjusted the apron wall height down by 3 units.  You could see in the apron card.
'		Adjusted flipper coil ramp up to 2.5 (mode = 0).  Also changed rubberizer to 0... too bouncy on flippers.  
'		Change LUT for cab vs VR.  cab = lut1.1.  VR is consat.
'		Flupper provided a new floor for VR.
' v0.7	Flippers were activating before and after game.  Made correction in keyup and down subs.
'		Added Flupper's original light intensities to cab_mode and desktop mode, unless user chooses to lower.  (VR mode is lower)
' v0.8	Reduced bloom strength for desktop and VR mode - issue mostly in desktop mode where ball had a huge glow over insertlights 
'		Changed option for higher cabinet mode siderails
' v0.9	Updated VR backglass flashers to more closely match real backglass
' v0.10	Added option for bloom to be forced off in cabinet mode.
' v0.11	Changed ball default options for VR mode.  Added choice for default balls in all modes.
'		Changed rail textures for VR provided by Flupper.
'		A couple light intensities needed to be adjusted in the script.
' v0.12	Made _noextrashadingvr acitve
'		Fixed the "I" inset lighting... top wasn't showing.  Also fixed the falloff of the jackpot and ransom insets on UPF.  It was cutting off the edges of the insets.
'		Added grooves for plunger lane to emulate the ball sitting in the groove.  Adjusted BallRelease strength from 4 to 8 to get into groove.
' v0.13	Added a ballrealease wall for the kicker to help get onto grove.
' v0.14 Turned wirerampoff sounds off.  Fleep ramp sounds were enough.  
'		Adjusted the kickback trigger forward to enable the kickback quicker. (thanks Rothbauerw for tip)
'		Adjusted the flipper size to 150 (thanks Rothbauerw for tip)
' 		Reduced kickback timer to 300.  Stayed active too long.
' v0.15	Adjusted dynamic shadow code to eliminate stutter on some slower CPU machines.  Removed all getballs calls and changed to global gBOT
'		Changed the trough script to never delete balls.  The trough now updates like the real machine without destroying and creating new balls.
' 		Added two walls to ensure ball launch goes into wire ramp.  Correcting corner condition where ball got inside table, behind back wall.
' 		Adjustedthe WAR and extra ball inset lights slightly.  Could see some light leaks
' v0.16 Plywood cutouts were missing material.  Added.
'		Changed relay sounds to new Fleep sounds, and positioned for SSF.
'		Reduced the ballrelease strength.
'		Changed ballpopper, and reject saucer code and moved to Fleep sounds.
' v0.17	Removed vlock external call, and handled the upper lock code in the script.  Added Fleep sounds to those saucers.
'		Changed bk2000drawbridge motor to SSF.
'		Added Knocker to SSF
' v0.18	Changed dynamic sources to an array to improve performance for shadows
'		Adjusted the bottom WAR triggers on UPF down and a little smaller.
'		Set hit height of all triggers to 50.
'		Corrected sound of UPF Targets
' 		Added ballrolling sounds to upper playfield
'		Corrected ramprolling sound on UPF.  It would continue if rolled back down UPF ramp.
' v0.19	Rothbauerw recommended updating the rubberizer code for more natural bounces.
' v0.20	Moved options up in script.
'		Couldn't hear drop target reset sounds, corrected.
' v0.21	Modified some of the drop target sounds to make a little more random sounds.
' v0.22	Corrected glowball code for new gBOT
'		Updated coordinates for CheckBallLocations
'
' v2.0	Release Version
' v2.0.1 Small dynamic shadows update.  The second DSSources is referencing 0 (X position) when it should be 1 (Y)
' v2.0.1 Added Magna Save button on the right, and animated it in VR.
'		 retro27 Made some VR improvements including an alternate topper, a flyer, a power cord, side rail plates, and a couple other small VR tweaks.  

Option Explicit
Randomize

Dim ShowRansom, GlowAmount, InsertBrightness, AmbienceCategory
Dim GlowBall, ChooseBall, CustomBulbIntensity(10), red(10), green(10), Blue(10)
Dim CustomBallImage(10), CustomBallLogoMode(10), CustomBallDecal(10), CustomBallGlow(10)
Dim ForceSiderailsFS, Bats


'******************************************************************************************
'* TABLE OPTIONS **************************************************************************
'******************************************************************************************

' *** Desktop, VR, or Full Cabinet Options *** These HAVE to be selected depending on the mode you are playing.

const VR_Room = 1 ' 1 = VR Room; 0 = desktop or cab mode, 
const cab_mode = 0 ' 1 = cabinet mode (Turns off the desktop backglass score lights; also sets Fluppers 4k cabinet light intensity, and allows for higher cab side rail option)

' *** If using cabinet mode: ***
ForceSiderailsFS = 1 ' 1 = higher siderails in cabinet mode: 0 = does not show higher sides


' *** If using VR Room: ***

const CustomWalls = 0 	'set to 0 for Modern Minimal Walls, floor, and roof, 1 for Sixtoe's original walls and floor
const WallClock = 1	  	'1 Shows the clock in the VR room only   
const topper = 1		'0 = Off, 1 = Topper 1, 2 = Topper 2 visible in VR Room only
const poster = 0		'1 Shows the flyer poster in the VR room only 

' *** LUT Lighting Options ***

' 0 = Desktop / Cab default darker setting.  You can override to 1 if you wish.  
' 1 = VR brighter setting (default), Can use for cab/desktop if you want to override to dark
' 2 = VR darker LUT override.

const LUT = 0 


' *** Rail Reflection / Glow Option:  If you experience signifcant "Reflection" or "Glow" on metal rails in desktop or cab ***
const remove_rail_glow = 0 ' 0 = Rail reflection/glow on; 1 = turns it off ' VR Mode turned off automatically


' *** Lower Brightness Option:  If you feel the lighting under the plastics and top lanes are too bright in desktop or cab ***
const lower_light_brightness = 0 ' 0 = Flupper original higher intensity under plastics; 1 = turns it lower ' VR Mode turned off automatically

' *** Remove Bloom Option:  If you feel the lighting seems to bloom too high up in desktop or cab ***
const force_bloom_off = 0 ' 0 = Flupper original higher bloom intensity; 1 = turns it to zero ' Desktop and VR Mode turned off automatically

'*** Flipper Bat Options: ***
Bats = 2'  0 = Lightning bolt bats; 1 = Normal (VPX) bats; 2 = new primitive Williams bats


'*** Show Ransom Option:  ***
' Don't need for VR or cab users, as it's on the backglass.  Most useful for desktop users, and cab users that have a 3 screen backglass setup.
' Code was added to have the showransom OFF by default on cab and on VR (since it's on the backglass).  Desktop is always on.

ShowRansom = False ' True = Overwrites the default setting and shows Ransom Plate on the playfield backwall for VR and cab_mode; False = does not show ransom plate


' *** Ambience Category: on = true, off = false, only works in DeskTop mode (adds a cat sleeping in background) ***
AmbienceCategory = False


'----- Shadow Options -----
Const DynamicBallShadowsOn 	= 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn 	= 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's)
									'2 = flasher image shadow, but it moves like ninuzzu's
Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 1 	'0 to 1, higher is darker (default is 0.95)
Const AmbientBSFactor 		= 1		'0 to 1, higher is darker (default is 0.7)
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source

'----- General Sound Options -----
Const VolumeDial = 0.8				' Recommended values should be no greater than 1.

'/////////////////////-----BallRoll Sound Amplification -----/////////////////////
Const BallRollAmpFactor = 1 ' 0 = no amplification, 1 = 2.5db amplification, 2 = 5db amplification, 3 = 7.5db amplification, 4 = 9db amplification (aka: Tie Fighter)


'-----RampRoll Sound Amplification -----/////////////////////
Const RampRollAmpFactor = 4 ' 0 = no amplification, 1 = 2.5db amplification, 2 = 5db amplification, 3 = 7.5db amplification, 4 = 9db amplification (aka: Tie Fighter)


'----- Phsyics Mods -----
Const Rubberizer = 1			'0 - disable, 1 - rothbauerw version, 2 - iaakki version    ' Enhances micro bounces on flippers
Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets, 2 = orig TargetBouncer
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7 when TargetBouncerEnabled=1, and 1.1 when TargetBouncerEnabled=2



' *** How to reduce jaggies on the wireramps or increase performance **********************
' The wireramps look best at 4K resolution with 4x MSAA; if you need more performance 
' you can make the wireramps and wireguides "static" so they will have less jaggies without MSAA, 
' but you will lose realtime reflections of the lights
' to do this, go to layer 2, select the wireramps and the wireguides (near the apron)
' and enable "static rendering"

'********************************** Insert Lighting settings ******************************
'The 2 settings below together with Night->Day cycle setting change the lighting looks.
'Night time: Night->day cycle completely left, GlowAmount = 3.0, InsertBrightness = 2.0
'Day time:   Night->day cycle 2 notches from left, GlowAmount = 0.5, InsertBrightness = 0.5
'Mixed:      Night->day cycle 1 notch from left, GlowAmount = 1.0, InsertBrightness = 0.5
'******************************************************************************************

'1.0 = default, useful range 0.1 - 3

GlowAmount = 0.6

'0.75 = default, useful range 0.25 - 2	

InsertBrightness = 2

'********************************** Ball settings *****************************************

' ***  VR USERS: It's recommended to use ball 7,8,9, or 10 ****

' 0 = normal ball
' 1 = white GlowBall
' 2 = magma GlowBall
' 3 = blue GlowBall
' 4 = another normal ball
' 5 = earth
' 6 = HDR ball
' 7 = normal without ball reflections
' 8 = HDR without ball reflections
' 9 = Hauntfreaks brighter ball without ball reflections
' 10 = Hauntfreaks really bright ball without ball reflections
'******************************************************************************************

const Ball_Choice = 6 		'Ball for Desktop and Cabinet users.  Default is 6.
const VR_Ball_Choice = 9	'Ball choice for VR users.  Default is 9.


'********************************************
' END TABLE OPTIONS 
'********************************************


'********************************************
' Ball Choice Code
'********************************************

If VR_Room = 1 Then
	ChooseBall = VR_Ball_Choice
Else
	ChooseBall = Ball_Choice
End If

' default Ball
CustomBallGlow(0) = 		False
CustomBallImage(0) = 		"TTMMball"
CustomBallLogoMode(0) = 	False
CustomBallDecal(0) = 		"scratches"
CustomBulbIntensity(0) = 	10
Red(0) = 0 : Green(0)	= 0 : Blue(0) = 0

' white GlowBall
CustomBallGlow(1) = 		True
CustomBallImage(1) = 		"white"
CustomBallLogoMode(1) = 	True
CustomBallDecal(1) = 		""
CustomBulbIntensity(1) = 	0
Red(1) = 255 : Green(1)	= 255 : Blue(1) = 255

' Magma GlowBall
CustomBallGlow(2) = 		True
CustomBallImage(2) = 		"ballblack"
CustomBallLogoMode(2) = 	True
CustomBallDecal(2) = 		"ballmagma"
CustomBulbIntensity(2) = 	0
Red(2) = 255 : Green(2)	= 180 : Blue(2) = 100

' Blue ball
CustomBallGlow(3) = 		True
CustomBallImage(3) = 		"blueball2"
CustomBallLogoMode(3) = 	False
CustomBallDecal(3) = 		""
CustomBulbIntensity(3) = 	0
Red(3) = 30 : Green(3)	= 40 : Blue(3) = 200

' Pin ball
CustomBallGlow(4) = 		False
CustomBallImage(4) = 		"pinball"
CustomBallLogoMode(4) = 	False
CustomBallDecal(4) = 		"scratch2"
CustomBulbIntensity(4) = 	10
Red(4) = 0 : Green(4)	= 0 : Blue(4) = 0

' Earth
CustomBallGlow(5) = 		True
CustomBallImage(5) = 		"ballblack"
CustomBallLogoMode(5) = 	True
CustomBallDecal(5) = 		"earth"
CustomBulbIntensity(5) = 	0
Red(5) = 100 : Green(5)	= 100 : Blue(5) = 100

' HDR ball
CustomBallGlow(6) = 		False
CustomBallImage(6) = 		"MRBallDark2"
CustomBallLogoMode(6) = 	False
CustomBallDecal(6) = 		"swirl2"
CustomBulbIntensity(6) = 	2
Red(6) = 0 : Green(6)	= 0 : Blue(6) = 0

' Normal Ball without relection of playfield on ball
CustomBallGlow(7) = 		False
CustomBallImage(7) = 		"TTMMball"
CustomBallLogoMode(7) = 	False
CustomBallDecal(7) = 		"scratches"
CustomBulbIntensity(7) = 	0
Red(7) = 0 : Green(7)	= 0 : Blue(7) = 0

' HDR ball without relection of playfield on ball
CustomBallGlow(8) = 		False
CustomBallImage(8) = 		"MRBallDark2"
CustomBallLogoMode(8) = 	False
CustomBallDecal(8) = 		"swirl2"
CustomBulbIntensity(8) = 	0
Red(8) = 0 : Green(8)	= 0 : Blue(8) = 0

' Hauntfreak's brighter ball without relection of playfield on ball
CustomBallGlow(9) = 		False
CustomBallImage(9) = 		"ball-light-hf"
CustomBallLogoMode(9) = 	False
CustomBallDecal(9) = 		"scratches"
CustomBulbIntensity(9) = 	0
Red(9) = 0 : Green(9)	= 0 : Blue(9) = 0

' Hauntfreak's really bright Ball without relection of playfield on ball
CustomBallGlow(10) = 		False
CustomBallImage(10) = 		"ball-lighter-hf"
CustomBallLogoMode(10) = 	False
CustomBallDecal(10) = 		"scratches"
CustomBulbIntensity(10) = 	0
Red(10) = 0 : Green(10)	= 0 : Blue(10) = 0




'********************************************
' Standard definitions
'********************************************

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const UsingROM = True
Const BallSize = 50
Const BallMass = 1
Const tnob = 3 ' total number of balls
Const lob = 0   'number of locked balls

LoadVPM "00990300", "S11.VBS", 3.10

Const UseSolenoids = 2
Const UseLamps = 0
Const UseSync = 0
Const HandleMech = 0
Const cGameName = "bk2k_l4"

Dim tablewidth: tablewidth = BK2K.width
Dim tableheight: tableheight = BK2K.height
Dim BIPL : BIPL = False				'Ball in plunger lane
Dim BK2KBall1, BK2KBall2, BK2KBall3, gBOT
																	

Dim dtKNI,dtGHT, Mech3Bank, MagnaSave							


'********************************************
' Light definitions
'********************************************

Dim LightType(65), LightObjectFirst(65), LightObjectSecond(65)
Dim GlowLow(10), GlowMed(10), GlowHigh(10)

' *** LightType 0 = normal
' *** LightType 1 = MagnaSave
' *** LightType 2 = Ransom

LightType(1)  = 2 : Set LightObjectFirst(1)  = FlasherR										  '*R*ANSOM (Backbox)
LightType(2)  = 2 : Set LightObjectFirst(2)  = FlasherA 		  							  'R*A*NSOM (Backbox)
LightType(3)  = 2 : Set LightObjectFirst(3)  = FlasherN 	      							  'RA*N*SOM (Backbox)
LightType(4)  = 2 : Set LightObjectFirst(4)  = FlasherS 	      							  'RAN*S*OM (Backbox)
LightType(6)  = 2 : Set LightObjectFirst(6)  = FlasherO 		  							  'RANS*O*M (Backbox)
LightType(7)  = 2 : Set LightObjectFirst(7)  = FlasherM 		  							  'RANSO*M* (Backbox)
LightType(19) = 1																			  'Magna Save
LightType(5)  = 0 : Set LightObjectFirst(5)  = light5b  : Set LightObjectSecond(5)  = Light5  'Bolt Circle Center
LightType(8)  = 0 : Set LightObjectFirst(8)  = Light8b  : Set LightObjectSecond(8)  = Light8  'LAST CHANCE (L. Outlane)
LightType(9)  = 0 : Set LightObjectFirst(9)  = Light9b  : Set LightObjectSecond(9)  = Light9  'U-Turn Bolt (Right)
LightType(10) = 0 : Set LightObjectFirst(10) = Light10b : Set LightObjectSecond(10) = Light10 'Spin Bolt (Ball Popper)
LightType(11) = 0 : Set LightObjectFirst(11) = Light11b : Set LightObjectSecond(11) = Light11 'Lock Bolt (R. Eject)
LightType(12) = 0 : Set LightObjectFirst(12) = Light12b : Set LightObjectSecond(12) = Light12 '*B* in BLACK
LightType(13) = 0 : Set LightObjectFirst(13) = Light13b : Set LightObjectSecond(13) = Light13 '*L* in BLACK
LightType(14) = 0 : Set LightObjectFirst(14) = Light14b : Set LightObjectSecond(14) = Light14 '*A* in BLACK
LightType(15) = 0 : Set LightObjectFirst(15) = Light15b : Set LightObjectSecond(15) = Light15 '*C* in BLACK
LightType(16) = 0 : Set LightObjectFirst(16) = Light16b : Set LightObjectSecond(16) = Light16 '*K* in BLACK
LightType(17) = 0 : Set LightObjectFirst(17) = Light17b : Set LightObjectSecond(17) = Light17 'Extra Ball Bolt
LightType(18) = 0 : Set LightObjectFirst(18) = Light18b : Set LightObjectSecond(18) = Light18 'Red Bolt (UPF right)
LightType(20) = 0 : Set LightObjectFirst(20) = Light20b : Set LightObjectSecond(20) = Light20 'Blue Bolt (UPF left)
LightType(21) = 0 : Set LightObjectFirst(21) = Light21b : Set LightObjectSecond(21) = Light21 'Last Chance (R. Outlane)
LightType(22) = 0 : Set LightObjectFirst(22) = Light22b : Set LightObjectSecond(22) = Light22 'U-Turn Bolt (Left)
LightType(23) = 0 : Set LightObjectFirst(23) = light23b : Set LightObjectSecond(23) = Light23 'KICKBACK (L. Outlane)
LightType(24) = 0 : Set LightObjectFirst(24) = Light24b : Set LightObjectSecond(24) = Light24 'Shoot Again
LightType(25) = 0 : Set LightObjectFirst(25) = Light25b : Set LightObjectSecond(25) = Light25 'W-Lane (Top UPF)
LightType(26) = 0 : Set LightObjectFirst(26) = Light26b : Set LightObjectSecond(26) = Light26 'I-Lane (Top UPF)
LightType(27) = 0 : Set LightObjectFirst(27) = Light27b : Set LightObjectSecond(27) = Light27 'N-Lane (Top UPF)
LightType(28) = 0 : Set LightObjectFirst(28) = Light28b : Set LightObjectSecond(28) = Light28 'W-Lane (Btm UPF)
LightType(29) = 0 : Set LightObjectFirst(29) = Light29b : Set LightObjectSecond(29) = Light29 'A-Lane (Btm UPF)
LightType(30) = 0 : Set LightObjectFirst(30) = Light30b : Set LightObjectSecond(30) = Light30 'R-Lane (Btm UPF)
LightType(31) = 0 : Set LightObjectFirst(31) = Light31b : Set LightObjectSecond(31) = Light31 'JACKPOT Bolt
LightType(32) = 0 : Set LightObjectFirst(32) = Light32b : Set LightObjectSecond(32) = Light32 'ADVANCE RANSOM Bolt
LightType(33) = 0 : Set LightObjectFirst(33) = Light33b : Set LightObjectSecond(33) = Light33 'Motor Target Bolt 3 (L UPF)
LightType(34) = 0 : Set LightObjectFirst(34) = Light34b : Set LightObjectSecond(34) = Light34 'Motor Target Bolt 2 (L UPF)
LightType(35) = 0 : Set LightObjectFirst(35) = Light35b : Set LightObjectSecond(35) = Light35 'Motor Target Bolt 1 (L UPF)
LightType(36) = 0 : Set LightObjectFirst(36) = Light36b : Set LightObjectSecond(36) = light36 '2X (Bonus Lights LPF)
LightType(37) = 0 : Set LightObjectFirst(37) = Light37b : Set LightObjectSecond(37) = Light37 '3X (Bonus Lights LPF)
LightType(38) = 0 : Set LightObjectFirst(38) = Light38b : Set LightObjectSecond(38) = Light38 'BONUS HOLD (Bonus Lights LPF)
LightType(39) = 0 : Set LightObjectFirst(39) = Light39b : Set LightObjectSecond(39) = Light39 '4X (Bonus Lights LPF)
LightType(40) = 0 : Set LightObjectFirst(40) = Light40b : Set LightObjectSecond(40) = Light40 '5X (Bonus Lights LPF)
LightType(41) = 0 : Set LightObjectFirst(41) = Light41b : Set LightObjectSecond(41) = Light41 '*G* in KNIGHT (R 3-Bank Dr Tgt)
LightType(42) = 0 : Set LightObjectFirst(42) = Light42b : Set LightObjectSecond(42) = Light42 '*H* in KNIGHT (R 3-Bank Dr Tgt)
LightType(43) = 0 : Set LightObjectFirst(43) = Light43b : Set LightObjectSecond(43) = Light43 '*T* in KNIGHT (R 3-Bank Dr Tgt)
LightType(44) = 0 : Set LightObjectFirst(44) = Light44b : Set LightObjectSecond(44) = Light44 '*K* in KNIGHT (L 3-Bank Dr Tgt)
LightType(45) = 0 : Set LightObjectFirst(45) = Light45b : Set LightObjectSecond(45) = Light45 '*N* in KNIGHT (L 3-Bank Dr Tgt)
LightType(46) = 0 : Set LightObjectFirst(46) = Light46b : Set LightObjectSecond(46) = Light46 '*I* in KNIGHT (L 3-Bank Dr Tgt)
LightType(47) = 0 : Set LightObjectFirst(47) = Light47b : Set LightObjectSecond(47) = Light47 'SKYWAY Bolt
LightType(48) = 0 : Set LightObjectFirst(48) = Light48b : Set LightObjectSecond(48) = Light48 'HURRY-UP Bolt
LightType(49) = 0 : Set LightObjectFirst(49) = Light49b : Set LightObjectSecond(49) = Light49 'EXTRA BALL (Bolt Circle)
LightType(50) = 0 : Set LightObjectFirst(50) = Light50b : Set LightObjectSecond(50) = Light50 '50,000 (Bolt Circle)
LightType(51) = 0 : Set LightObjectFirst(51) = Light51b : Set LightObjectSecond(51) = Light51 'MAGNA SAVE (Bolt Circle)
LightType(52) = 0 : Set LightObjectFirst(52) = Light52b : Set LightObjectSecond(52) = Light52 '10,000 (Bolt Circle)
LightType(53) = 0 : Set LightObjectFirst(53) = Light53b : Set LightObjectSecond(53) = Light53 'MULTI-BALL (Bolt Circle)
LightType(54) = 0 : Set LightObjectFirst(54) = Light54b : Set LightObjectSecond(54) = Light54 '100,000 (Bolt Circle)
LightType(55) = 0 : Set LightObjectFirst(55) = Light55b : Set LightObjectSecond(55) = Light55 'RANSOM (Bolt Circle)
LightType(56) = 0 : Set LightObjectFirst(56) = Light56b : Set LightObjectSecond(56) = Light56 '200,000 (Bolt Circle)
LightType(57) = 0 : Set LightObjectFirst(57) = Light57b : Set LightObjectSecond(57) = Light57 'SPECIAL (Bolt Circle)
LightType(58) = 0 : Set LightObjectFirst(58) = Light58b : Set LightObjectSecond(58) = Light58 '20,000 (Bolt Circle)
LightType(59) = 0 : Set LightObjectFirst(59) = Light59b : Set LightObjectSecond(59) = Light59 'KICKBACK (Bolt Circle)
LightType(60) = 0 : Set LightObjectFirst(60) = Light60b : Set LightObjectSecond(60) = Light60 '150,000 (Bolt Circle)
LightType(61) = 0 : Set LightObjectFirst(61) = Light61b : Set LightObjectSecond(61) = Light61 'DRAWBRIDGE (Bolt Circle)
LightType(62) = 0 : Set LightObjectFirst(62) = Light62b : Set LightObjectSecond(62) = Light62 '75,000 (Bolt Circle)
LightType(63) = 0 : Set LightObjectFirst(63) = Light63b : Set LightObjectSecond(63) = Light63 'HURRY-UP (Bolt Circle)
LightType(64) = 0 : Set LightObjectFirst(64) = Light64b : Set LightObjectSecond(64) = Light64 '250,000 (Bolt Circle)

Set GlowLow(0) = Glowball1low : Set GlowMed(0) = Glowball1med : Set GlowHigh(0) = Glowball1high
Set GlowLow(1) = Glowball2low : Set GlowMed(1) = Glowball2med : Set GlowHigh(1) = Glowball2high
Set GlowLow(2) = Glowball3low : Set GlowMed(2) = Glowball3med : Set GlowHigh(2) = Glowball3high


'*******************************************
'  Timers
'*******************************************

' The game timer interval is 10 ms
Sub GameTimer_Timer()
	Cor.Update 						'update ball tracking
	RollingUpdate					'update rolling sounds
	DoDTAnim 						'handle drop target animations
End Sub

' The frame timer interval is -1, so executes at the display frame rate
Sub FrameTimer_Timer()
	FlipperVisualUpdate				'update flipper shadows and primitives
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate
	CheckBallLocations
End Sub


'*******************************************
'  Flipper Shadow and Gate Subs
'*******************************************

' This subroutine updates the flipper shadows and visual primitives
Sub FlipperVisualUpdate
	If Bats = 2 Then
		LeftBat.ObjRotZ = LeftFlipper.CurrentAngle : batleftshadow.ObjRotZ = LeftFlipper.CurrentAngle
		RightBat.ObjRotZ = RightFlipper.CurrentAngle : batrightshadow.ObjRotZ = RightFlipper.CurrentAngle
		URightBat.ObjRotZ = uRightFlipper.CurrentAngle : baturightshadow.ObjRotZ = uRightFlipper.CurrentAngle
	End If
End Sub



'*******************************************
' Table init.
'*******************************************

Sub BK2K_Init
	Dim Light

	vpmInit Me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Black Knight 2000 - Williams 1989" & vbNewLine & "VPX by Flupper"
        .Games(cGameName).Settings.Value("rol") = 0 'set it to 1 to rotate the DMD to the left
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = 0
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
        .Switch(22) = 1 'close coin door
        .Switch(24) = 1 'and keep it close
    End With

	'*** option settings ***

	If AmbienceCategory and BK2K.ShowDT Then Flasher1.Visible = 1 Else Flasher1.Visible = 0 End If
	If Bats = 0 Then LeftFlipper.image = "flipperaoleft" : RightFlipper.image = "flipperaoright" : URightFlipper.image = "flipperaoright" End If
	If Bats < 2 Then LeftFlipper.visible = 1 : RightFlipper.visible = 1: URightFlipper.visible = 1 : leftbat.visible = 0 : rightbat.visible = 0 : urightbat.visible = 0 : End If
	For Each Light in GlowLights : Light.IntensityScale = GlowAmount: Light.FadeSpeedUp = Light.Intensity /  10 : Light.FadeSpeedDown = Light.FadeSpeedUp : Next
	For Each Light in InsertLights : Light.IntensityScale = InsertBrightness : Light.FadeSpeedUp = Light.Intensity / 10 : Light.FadeSpeedDown = Light.FadeSpeedUp : Next
	ChangeBall(ChooseBall)

If VR_Room = 1 or cab_mode = 1 Then
	If not ShowRansom Then ransomplate.visible = 0 : FlasherR.visible = 0 : FlasherA.visible = 0 : FlasherN.visible = 0 : FlasherS.visible = 0 : FlasherO.visible = 0 : FlasherM.visible = 0 end If
End If
		
    ' ### Nudging ###
    vpmNudge.TiltSwitch = 1 'was 14 nFozzy
    vpmNudge.Sensitivity = 1
	vpmNudge.TiltObj = Array(Bumper1,Bumper2,Bumper3,LSling,RSling)    
	    
	' ### Main Timer init ###
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1
	
	' autoplunger pullback
    LockPlunger.PullBack
        

	'************  Trough	**************
	Set BK2KBall1 = sw11.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set BK2KBall2 = sw12.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set BK2KBall3 = sw13.CreateSizedballWithMass(Ballsize/2,Ballmass)
	gBOT = Array(BK2KBall1,BK2KBall2,BK2KBall3)
	
	Controller.Switch(10) = 0
	Controller.Switch(11) = 1
	Controller.Switch(12) = 1
	Controller.Switch(13) = 1

	CheckBallLocations


	'3 Targets Bank (copied from AFM)
    Set Mech3Bank = new cvpmMech
    With Mech3Bank
        .Sol1 = sMTargets
        .Mtype = vpmMechLinear + vpmMechReverse + vpmMechOneSol
        .Length = 60
        .Steps = 50
        .AddSw swMTargetsUp, 0, 0
        .AddSw swMTargetsDn, 50, 50
        .Callback = GetRef("Update3Bank")
        .Start
    End With

	'Setup magnets
	Set MagnaSave = New cvpmMagnet
	With MagnaSave
		.InitMagnet MSave, 12
		.Solenoid = sMagnaSave
		.GrabCenter = False 	'Try to set to true it if you like...i prefer it off as that´s closer to the real bk2k magnet
	End With


	if VR_Room = 1 Then
		setup_backglass()
	End If

	PinCab_Backglass.blenddisablelighting = .3
	PinCab_Backbox.blenddisablelighting = 2

End Sub


'*******************************************
'  VR Plunger Code
'*******************************************

Sub TimerVRPlunger_Timer
  If PinCab_Shooter.Y < -30 then
       PinCab_Shooter.Y = PinCab_Shooter.Y + 5
  End If
End Sub

Sub TimerVRPlunger1_Timer
  PinCab_Shooter.Y = -113.0579 + (5* Plunger.Position) -20
End Sub


'********************************************
' keyboard handlers
'********************************************

Sub BK2K_KeyDown(ByVal keycode)
	If keycode = LeftTiltKey Then Nudge 90, 5 : SoundNudgeLeft
	If keycode = RightTiltKey Then Nudge 270, 5 : SoundNudgeRight
	If keycode = CenterTiltKey Then Nudge 0, 3 : SoundNudgeCenter

	If keycode = RightMagnaSave Then
		Controller.Switch(59) = 1
		VRMagnaButtonRight.X = 2085.186 - 8
	End If  

	If keycode = LeftFlipperKey Then 
		FlipperActivate LeftFlipper, LFPress
		VRFlipperButtonLeft.X = 2112.2 + 8
	End If

	If keycode = RightFlipperKey Then 
		FlipperActivate RightFlipper, RFPress
		VRFlipperButtonRight.X = 2085.186 - 8
	End If

	If KeyCode = PlungerKey Then
		Plunger.PullBack
		Plunger.Pullback:SoundPlungerPull()
		TimerVRPlunger.Enabled = True
		TimerVRPlunger1.Enabled = False
		PinCab_Shooter.Y = -113.0579
	End If

	If keycode = StartGameKey Then
		StartButton.y = 877.1433 - 5
		SoundStartButton
	End If


	'If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then 'Use this for ROM based games
	If keycode = AddCreditKey or keycode = AddCreditKey2 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If

	If vpmKeyDown(keycode) Then Exit Sub

End Sub

Sub BK2K_KeyUp(ByVal keycode)

	If keycode = RightMagnaSave Then
		Controller.Switch(59) = False
		VRMagnaButtonRight.X = 2085.186
	End If

	If keycode = PlungerKey Then 
		Plunger.Fire
		If BIPL = 1 Then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
		TimerVRPlunger.Enabled = False
        TimerVRPlunger1.Enabled = True
		PinCab_Shooter.Y = -113.0579
	End If

	If keycode = LeftFlipperKey Then 
		FlipperDeActivate LeftFlipper, LFPress
		VRFlipperButtonLeft.X = 2112.2
	End If

	If keycode = RightFlipperKey Then 
		FlipperDeActivate RightFlipper, RFPress
		VRFlipperButtonRight.X = 2085.186
	End If

	If keycode = StartGameKey Then
		StartButton.y = 877.1433
	End If

	If vpmKeyUp(keycode) Then Exit Sub

End Sub

Sub BK2K_Paused: Controller.Pause = 1:End Sub
Sub BK2K_UnPaused: Controller.Pause = 0:End Sub
Sub BK2K_Exit(): Controller.Stop:End Sub

'********************************************
'  Switches 
'********************************************

Const swSwitch14   = 14 ' not used
Const swSwitch15   = 15 ' not used
Const swMTargetsUp = 16 ' UP Motor Targets
Const swJet1       = 17 ' Left Jet Bumper
Const swLSling     = 18 ' Left Slingshot
Const swJet2       = 19 ' Right Jet Bumper
Const swRSling     = 20 ' Left Slingshot
Const swJet3       = 21 ' Bottom Jet Bumper
Const swLoop1      = 22 ' Left UPF Loop End
Const swLoop2      = 23 ' Right UPF Loop End
Const swMTargetsDn = 24 ' Down Motor Targets
Const swWin1       = 25 ' W-Lane in WIN
Const swWin2       = 26 ' I-Lane in WIN
Const swWin3       = 27 ' N-Lane in WIN
Const swWar1       = 28 ' W-Lane in WAR
Const swWar2       = 29 ' W-Lane in WAR
Const swWar3       = 30 ' W-Lane in WAR
Const swURampEntry = 31 ' Upper Ramp Entry
Const swLRampExit  = 32 ' Lower Ramp Exit (skyway)
Const swBridge1    = 33 ' Motor Targets (Upper Target)
Const swBridge2    = 34 ' Motor Targets (Middle Target)
Const swBridge3    = 35 ' Motor Targets (Lower Target)
Const swLock1      = 36 ' UPF Lock Switch (Lower)
Const swLock2      = 37 ' UPF Lock Switch (Middle)
Const swLock3      = 38 ' UPF Lock Switch (Upper)
Const swLOutlane   = 39 ' Left Outlane
Const swREject     = 40 ' Right Eject (Challenge Hole)
Const swKNIGHT4    = 41 ' *G* Target in KNIGHT (Right Dt Bank - left)
Const swKNIGHT5    = 42 ' *H* Target in KNIGHT (Right Dt Bank - middle)
Const swKNIGHT6    = 43 ' *T* Target in KNIGHT (Right Dt Bank - right)
Const swKNIGHT1    = 44 ' *K* Target in KNIGHT (Left Dt Bank - lower)
Const swKNIGHT2    = 45 ' *N* Target in KNIGHT (Left Dt Bank - middle)
Const swKNIGHT3    = 46 ' *I* Target in KNIGHT (Left Dt Bank - upper)
Const swBallPopper = 47 ' Ball Popper
Const swROutlane   = 48 ' Right Outlane
Const swUTurn1     = 49 ' U-Turn 1 (Left Entrance lower switch)
Const swUTurn2     = 50 ' U-Turn 2 (Left Entrance upper switch)
Const swUTurn3     = 51 ' U-Turn 3 (Right Entrance upper switch)
Const swUTurn4     = 52 ' U-Turn 4 (Right Entrance lower switch)
Const swShooter    = 53 ' Ball Shooter
Const swRRetLane   = 54 ' Right Return Lane
Const swLRetLane   = 55 ' Left Return Lane
Const swSwitch55   = 56 ' not used
Const swRLaneChg   = 57 ' Flipper Right
Const swLLaneChg   = 58 ' Flipper Left
Const swMagnaSave  = 59 ' Magna Save
Const swSwitch60   = 60 ' not used
Const swSwitch61   = 61 ' not used
Const swSwitch62   = 62 ' not used
Const swSwitch63   = 63 ' not used
Const swSwitch64   = 64 ' not used


'********************************************
' Solenoids
'********************************************

'Solenoids A-SIDE
Const sLDropBank    = 3 ' Left 3-Bank Drop Target Reset
Const sRDropBank    = 4 ' Right 3-Bank Drop Target Reset
'Const sSolenoid5    = 5 ' not used
Const sBallPopper   = 6 ' Ball Popper
Const sLockPlunger  = 7 ' UPF Lockup Kickback
Const sREject       = 8 ' Right Eject (Challenge hole)

'Solenoids C-SIDE
Const sUPFRedBoltFlash  = 25 '1c (UPF Big Red Bolt Flasher)
Const sUPFBlueBoltFlash = 26 '2c (UPF Big Blue Bolt Flasher)
Const sKnightHeadFlash  = 27 '3c (Bolt Circle Center Flasher)
Const sFlipLaneFlash    = 28 '4c (Flipper Lane Flasher)
Const sDTFlash          = 29 '5c (Drop Target Flasher)
Const sSkyWayFlash      = 30 '6c (LPF Ramp Flasher)
Const sREjectFlash      = 31 '7c (Right Eject/Upr Flipper Flashers)
Const sLockFlash        = 32 '8c (UPF Lockup Kickback Flasher)

'And The Rest ;)
Const sIGIRelay     = 9  ' (Insert Board GI-Relay)
Const sUPFGIRelay   = 10 ' (UPF GI-Relay)
Const sLPFGIRelay   = 11 ' (LPF GI-Relay)
Const sACselect     = 12 ' (A/C Select Relay)
Const sKickback     = 13 ' (Kickback - Left Outlane)
Const sKnocker      = 14 ' (Knocker)
Const sMagnaSave    = 15 ' (Magna Save Driver)
Const sMTargets     = 16 ' (Motor Targets[UPF] Relay)
Const sLJet         = 17 ' (Left Jet Bumper)
Const sLSling       = 18 ' (Left Slingshot)
Const sRJet         = 19 ' (Right Jet Bumper)
Const sRSling       = 20 ' (Right SlingShot)
Const sBJet         = 21 ' (Bottom Jet Bumper)
'Const sSolenoid     = 22 ' not used

'Solenoid CallBacks (A-Side)
SolCallback(1)	= "SolOuthole" ' Outhole Kicker
SolCallback(2)	= "SolRelease" ' Ball Release
SolCallback(sLDropBank)    = "SolLDropBank"
SolCallback(sRDropBank)    = "SolRDropBank"
SolCallback(sBallPopper)   = "SolBallPopper"
SolCallback(sLockPlunger)  = "SolLockPlunger"
SolCallback(sREject)       = "SolREject"

'Solenoid CallBacks (C-Side)
SolCallback(sUPFRedBoltFlash)  = "SolRedBoltFlash"
SolCallback(sUPFBlueBoltFlash) = "SolBlueBoltFlash"
SolCallback(sKnightHeadFlash)  = "SolKnightHeadFlash"
SolCallback(sFlipLaneFlash)    = "SolFlipLaneFlash"
SolCallback(sDTFlash)          = "SolDTFlash" 
SolCallback(sSkyWayFlash)      = "SolSkyWayFlash"
SolCallback(sREjectFlash)      = "SolREjectFlash"
SolCallback(sLockFlash)        = "SolLockFlash"

'Solenoid CallBacks (The Rest)
SolCallback(sIGIRelay)   = ""
SolCallback(sUPFGIRelay) = "SolUPFGIRelay"
SolCallback(sLPFGIRelay) = "SolLPFGIRelay"
SolCallback(sACselect)   = "SolACSelect"
SolCallback(sKickback)   = "SolKickBack"
SolCallback(sKnocker)    = "SolKnocker"
SolCallback(sMTargets)   = "SolMTargets" 
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"


'VR Backglass
SolCallback(25)   = "Sol25"
SolCallback(26)   = "Sol26"
SolCallback(27)   = "Sol27"
SolCallback(28)   = "Sol28"
SolCallback(29)   = "Sol29"
SolCallback(30)   = "Sol30"
SolCallback(31)   = "Sol31"
SolCallback(32)   = "Sol32"


'********************************************
'SWITCH HANDLING (Lower PF)
'********************************************

'Shooter Lane
Sub Shooter_Hit()       : Controller.Switch(swShooter) = 1 : End Sub
Sub Shooter_UnHit()     : Controller.Switch(swShooter) = 0 : End Sub
'In/Out Lanes
Sub LOutLane_Hit()	    : Controller.Switch(swLOutLane) = 1 : End Sub
Sub LOutLane_UnHit()	: Controller.Switch(swLOutLane) = 0 : End Sub
Sub LRetLane_Hit()		: Controller.Switch(swLRetLane) = 1 : End Sub
Sub LRetLane_UnHit()	: Controller.Switch(swLRetLane) = 0 : End Sub
Sub ROutLane_Hit()	    : Controller.Switch(swROutLane) = 1 : End Sub
Sub ROutLane_UnHit()	: Controller.Switch(swROutLane) = 0 : End Sub
Sub RRetLane_Hit()		: Controller.Switch(swRRetLane) = 1 : End Sub
Sub RRetLane_UnHit()	: Controller.Switch(swRRetLane) = 0 : End Sub
'Skyway
Sub Skyway_Hit()        : Controller.Switch(swLRampExit) = 1 : End Sub
Sub Skyway_UnHit()      : Controller.Switch(swLRampExit) = 0 : End Sub
'U-Turn
Sub UTurn1_Hit()        : Controller.Switch(swUTurn1) = 1 : If ActiveBall.VelY < -10 Then ActiveBall.VelY = -10 End If : End Sub
Sub UTurn1_UnHit()      : Controller.Switch(swUTurn1) = 0 : End Sub
Sub UTurn2_Hit()        : Controller.Switch(swUTurn2) = 1 : End Sub
Sub UTurn2_UnHit()      : Controller.Switch(swUTurn2) = 0 : End Sub
Sub UTurn3_Hit()        : Controller.Switch(swUTurn3) = 1 : If ActiveBall.VelY > 10 Then ActiveBall.VelY = 10 End If : End Sub
Sub UTurn3_UnHit()      : Controller.Switch(swUTurn3) = 0 : End Sub
Sub UTurn4_Hit()        : Controller.Switch(swUTurn4) = 1 : End Sub
Sub UTurn4_UnHit()      : Controller.Switch(swUTurn4) = 0 : End Sub

'********************************************
'SWITCH HANDLING (Upper PF)
'********************************************

'UPF Wire Ramp (Lock)
Sub URampEntry_Hit()
	Controller.Switch(swURampEntry) = 1
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub URampEntry_UnHit()
	Controller.Switch(swURampEntry) = 0
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound 
End Sub

Sub URampExit_Hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

'W-I-N Lanes
Sub Win1_Hit()          : Controller.Switch(swWin1) = 1 : End Sub
Sub Win1_UnHit()        : Controller.Switch(swWin1) = 0 : End Sub
Sub Win2_Hit()          : Controller.Switch(swWin2) = 1 : End Sub
Sub Win2_UnHit()        : Controller.Switch(swWin2) = 0 : End Sub
Sub Win3_Hit()          : Controller.Switch(swWin3) = 1 : End Sub
Sub Win3_UnHit()        : Controller.Switch(swWin3) = 0 : End Sub
'W-A-R Lanes
Sub War1_Hit()          : Controller.Switch(swWar1) = 1 : End Sub
Sub War1_UnHit()        : Controller.Switch(swWar1) = 0 : End Sub
Sub War2_Hit()          : Controller.Switch(swWar2) = 1 : End Sub
Sub War2_UnHit()        : Controller.Switch(swWar2) = 0 : End Sub
Sub War3_Hit()          : Controller.Switch(swWar3) = 1 : End Sub
Sub War3_UnHit()        : Controller.Switch(swWar3) = 0 : End Sub
'Loop
Sub Loop1_Hit()         : Controller.Switch(swLoop1) = 1 : End Sub
Sub Loop1_UnHit()       : Controller.Switch(swLoop1) = 0 : End Sub
Sub Loop2_Hit()         : Controller.Switch(swLoop2) = 1 : If ActiveBall.VelY > 10 Then ActiveBall.VelY = 5 End If:End Sub
Sub Loop2_UnHit()       : Controller.Switch(swLoop2) = 0 : End Sub

'Drawbridge Targets
Sub DBTrgt1_Slingshot()
	vpmTimer.PulseSwitch (swBridge1), 0, ""
	PlayTargetSound
End Sub

Sub DBTrgt2_Slingshot()
	vpmTimer.PulseSwitch (swBridge2), 0, ""
	PlayTargetSound
End Sub

Sub DBTrgt3_Slingshot()
	vpmTimer.PulseSwitch (swBridge3), 0, ""
	PlayTargetSound
End Sub

'********************************************
' Bumpers
'********************************************
Sub Bumper1_Hit()
	vpmTimer.PulseSwitch (swJet1), 0, "" 
	RandomSoundBumperMiddle Bumper1
End Sub

Sub Bumper2_Hit()
	vpmTimer.PulseSwitch (swJet2), 0, ""
	RandomSoundBumperTop Bumper2
End Sub

Sub Bumper3_Hit()
	vpmTimer.PulseSwitch (swJet3), 0, ""
	RandomSoundBumperBottom Bumper3
End Sub


'********************************************
' Trough
'********************************************

Sub sw13_Hit():Controller.Switch(13) = 1:UpdateTrough:End Sub
Sub sw13_UnHit():Controller.Switch(13) = 0:UpdateTrough:End Sub
Sub sw12_Hit():Controller.Switch(12) = 1:UpdateTrough:End Sub
Sub sw12_UnHit():Controller.Switch(12) = 0:UpdateTrough:End Sub
Sub sw11_Hit():Controller.Switch(11) = 1:UpdateTrough:End Sub
Sub sw11_UnHit():Controller.Switch(11) = 0:UpdateTrough:End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If sw11.BallCntOver = 0 Then sw12.kick 45, 8
	If sw12.BallCntOver = 0 Then sw13.kick 45, 8
	Me.Enabled = 0
End Sub


'********************************************
' Drain and kickers
'********************************************

Sub Drain_Hit()
	RandomSoundDrain Drain
	UpdateTrough
	Controller.Switch(10) = 1
End Sub

Sub Drain_UnHit()
	Controller.Switch(10) = 0
End Sub

Sub SolOuthole(enabled)
	If enabled Then 
		Drain.kick 60,15
	End If
End Sub

Sub SolRelease(enabled)
	If enabled Then 
		RandomSoundBallRelease sw11
		sw11.kick 60, 12
		UpdateTrough
	End If
End Sub

Sub SolREject(enabled)
	If enabled Then
		If Controller.Switch(swREject) = True Then
			SoundSaucerKick 1, REject
		Else
			SoundSaucerKick 0, REject
		End If
		REject.Kick 250, 40, 0
		Controller.Switch(swREject) = 0
	End If
End Sub

Sub REject_Hit()
	SoundSaucerLock
	Controller.Switch(swREject) = 1
End Sub

Sub SolBallPopper(enabled)
	If enabled Then
		If Controller.Switch(swBallPopper) = True Then
			SoundSaucerKick 1, BallPopper1
		Else
			SoundSaucerKick 0, BallPopper1
		End If

		BallPopper1.Kick 220, 60, 1.56
		Controller.Switch(swBallPopper) = 0

	End If
End Sub

Sub Ballpopper1_Hit()
	SoundSaucerLock
	Controller.Switch(swBallPopper) = 1
End Sub


Sub SolKickBack(enabled)
    if Enabled then
		SoundSaucerKick 1, Kickback
		KickbackTimer.Interval = 300 '500
    	KickbackTimer.Enabled = True
    	Kickback.Enabled = True
	End If
End Sub  


Sub SolLockPlunger(enabled)
    if Enabled then
		LockKicker1.Kick 0, 1
		LockKicker2.Kick 0, 1
		LockKicker3.Kick 0, 1
		SoundSaucerKick 1, LockKicker1
		SoundSaucerKick 1, LockKicker2
		SoundSaucerKick 1, LockKicker3
		LockPlunger.Fire
		Controller.Switch(swLock1) = 0
		Controller.Switch(swLock2) = 0
		Controller.Switch(swLock3) = 0
		LockKicker2.enabled=0
		LockKicker3.enabled=0
	else
		LockPlunger.PullBack
	end If
End Sub  

Sub LockKicker1_Hit()
	SoundSaucerLock
	Controller.Switch(swLock1) = 1
	LockKicker2.enabled=1
End Sub

Sub LockKicker2_Hit()
	SoundSaucerLock
	Controller.Switch(swLock2) = 1
	LockKicker3.enabled=1
End Sub

Sub LockKicker3_Hit()
	SoundSaucerLock
	Controller.Switch(swLock3) = 1
End Sub


'********************************************
' Ramp Sounds
'********************************************

Sub Trigger1_Hit()
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound 
End Sub

Sub Trigger2_Hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub Trigger3a_Hit()
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound 
End Sub 

Sub Trigger3_Hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub Trigger4a_Hit()
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound 
End Sub 

Sub Trigger4_Hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub Trigger5_Hit()
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound 
End Sub

Sub TriggerR3Enter_Hit()
	WireRampOn True ' On Metal Ramp Play Ramp Sound 
End Sub

Sub TriggerR3Exit_Hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub TriggerR30Enter_Hit()  
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound 
End Sub

Sub TriggerR10Enter_Hit()
	WireRampOn True ' On Metal Ramp Play Ramp Sound 
End Sub

Sub TriggerR10Enter_Hit()  
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub



'********************************************
'  Drop Target Controls
'********************************************

' Drop targets
Sub sw1_Hit
	DTHit 41
	SoundDropTargetDrop sw1p
End Sub

Sub sw2_Hit
	DTHit 42
	SoundDropTargetDrop sw2p
End Sub

Sub sw3_Hit
	DTHit 43
	SoundDropTargetDrop sw3p
End Sub

Sub sw4_Hit
	DTHit 44
	SoundDropTargetDrop sw4p
End Sub

Sub sw5_Hit
	DTHit 45
	SoundDropTargetDrop sw5p
End Sub

Sub sw6_Hit
	DTHit 46
	SoundDropTargetDrop sw6p
End Sub

' Drop Target left Solenoid
Sub SolLDropBank(enabled)
	If enabled Then
		PlaySoundAtLevelStatic DTResetSound, 1, sw4p
		DTRaise 44
		DTRaise 45
		DTRaise 46
	End If
End Sub

' Drop Target top Solenoid
Sub SolRDropBank(enabled)
	If enabled Then
		PlaySoundAtLevelStatic DTResetSound, 1, sw1p
		DTRaise 41
		DTRaise 42
		DTRaise 43
	End If
End Sub



'******************************************************
'					KNOCKER
'******************************************************
Sub SolKnocker(enabled)
	If enabled then 
		KnockerSolenoid
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
		URightFlipper.RotateToEnd

		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If

	Else
		RightFlipper.RotateToStart
		URightFlipper.RotateToStart
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



'******************************************************************************************
' Sling Shot Animations: Rstep and Lstep  are the variables that increment the animation
'******************************************************************************************

Dim RStep, Lstep

Sub RSling_Slingshot
	vpmTimer.PulseSwitch (swRSling), 0, ""
	RandomSoundSlingshotRight Sling1
    RSling3.Visible = 0
    RSling1.Visible = 1
    sling1.RotX = 20
    RStep = 0
    RSling.TimerEnabled = 1
	'gi1.State = 0:Gi2.State = 0
End Sub

Sub RSling_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.RotX = 10
        Case 4:RSLing2.Visible = 0:RSling3.Visible = 1:sling1.RotX = -10:RSling.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LSling_Slingshot
	vpmTimer.PulseSwitch (swLSling), 0, ""
	RandomSoundSlingshotLeft Sling2
    LSling3.Visible = 0
    LSling1.Visible = 1
    sling2.RotX = 20
    LStep = 0
    LSling.TimerEnabled = 1
End Sub

Sub LSling_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.RotX = 10
        Case 4:LSLing2.Visible = 0:LSling3.Visible = 1:sling2.RotX = -10:LSling.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub


'********************************************
' Solenoid Callback handlers
'********************************************

Sub SolACSelect(enabled)
	Sound_GI_Relay enabled, Relay_GI_AC
End Sub


'********************************************
' "FlipLaneFlashers" (LPF)
'********************************************

Sub SolFlipLaneFlash(enabled)
	if enabled then	
		GI14.state = LightStateOn : GI14b.state = LightStateOn : GI15.state = LightStateOn : GI15b.state = LightStateOn
	Else
		GI14.state = LightStateOff : GI14b.state = LightStateOff : GI15.state = LightStateOff : GI15b.state = LightStateOff
	End If
	Sound_Flash_Relay enabled, Relay_1
End Sub    

Sub SolDTFlash(enabled)
	if enabled then	
		GI17.state = LightStateOn : GI17b.state = LightStateOn
		PegPlasticT18.DisableLighting = 1 : PegPlasticT17.DisableLighting = 1 : PegPlasticT16.DisableLighting = 1  : PegPlasticT15.DisableLighting = 1 
	Else
		GI17.state = LightStateOff : GI17b.state = LightStateOff
		PegPlasticT18.DisableLighting = 0 : PegPlasticT17.DisableLighting = 0 : PegPlasticT16.DisableLighting = 0  : PegPlasticT15.DisableLighting = 0
	End If
	Sound_Flash_Relay enabled, Relay_2
End Sub 

'********************************************
' Light-Flashers
'********************************************

Sub SolREjectFlash(enabled) 
	if enabled then 
		GI24b.state = LightStateOn 
	Else 
		GI24b.state = LightStateOff 
	End If
	Sound_Flash_Relay enabled, Relay_3
End Sub

Sub SolBlueBoltFlash(enabled)
	if enabled then	
		Light20.State=LightStateOn
		BlueBoltFlash.State=LightStateOn 
	else 
		Light20.State=LightStateOff
		BlueBoltFlash.State=LightStateOff 
	end if
	Sound_Flash_Relay enabled, Relay_4
End Sub  

Sub SolRedBoltFlash(enabled)
	if enabled then 
		Light18.State=LightStateOn
		RedBoltFlash.State=LightStateOn 
	else 
		Light18.State=LightStateOff
		RedBoltFlash.State=LightStateOff 
	end if
	Sound_Flash_Relay enabled, Relay_5
End Sub  

Sub SolKnightHeadFlash(enabled)
	if enabled then 
		KnightHeadFlash.State=LightStateOn
	else 
		KnightHeadFlash.State=LightStateOff 
	end if
	Sound_Flash_Relay enabled, Relay_6
End Sub    

Sub SolSkyWayFlash(enabled)
	if enabled then 
		SkyWayFlash.State=LightStateOn 
	else 
		SkyWayFlash.State=LightStateOff 
	end if
	Sound_Flash_Relay enabled, Relay_7
End Sub

Sub SolLockFlash(enabled)
	if enabled then	
		LockFlash.State=LightStateOn 
	else 
		LockFlash.State=LightStateOff 
	Sound_Flash_Relay enabled, Relay_8
	end if
End Sub

'********************************************
' MagnaSave!                      
'********************************************

Sub MSave_Hit(): MagnaSave.AddBall ActiveBall:End Sub
Sub MSave_UnHit(): MagnaSave.RemoveBall ActiveBall:End Sub

'********************************************
' Motor Bank Up Down (from AFM)
'********************************************

Sub Update3Bank(currpos, currspeed, lastpos)
    Dim zpos
    zpos = 135 - currpos
    If currpos <> lastpos Then BackBank.Z = zpos - 10 End If

    If currpos >= 49 Then
        DBTrgt1.Isdropped = 1 : DBTrgt2.Isdropped = 1 : DBTrgt3.Isdropped = 1
    End If

    If currpos = 0 Then
        DBTrgt1.Isdropped = 0 : DBTrgt2.Isdropped = 0 : DBTrgt3.Isdropped = 0
    End If
	
End Sub

Sub SolMTargets(enabled)
    If enabled Then
		PlaySoundAtLevelStaticLoop SoundFX("bk2k_drawbridge",DOFGear), 1, backbank
	Else
		StopSound ("bk2k_drawbridge")
	End If
End Sub



'********************************************
' ### General Illumination-UPF ###
'********************************************

Sub SolUPFGIRelay(enabled)
    Dim Prim, Light   
    If enabled Then
		For Each Light in LightsGIupf : Light.State = LightStateOff : Next
		For Each Prim in primitivesGIupf : Prim.DisableLighting = 0 : Next
		For Each Prim in primitivesGIupfpegs : Prim.image = "peg" : Prim.material = "peg3": Next
		Primitive98.material = "RubberOff" : Primitive99.material = "RubberOff" : Primitive100.material = "RubberOff" : Primitive101.material = "RubberOff"
		FlBumperFadeTarget(1) = 0 : FlBumperFadeTarget(2) = 0 : FlBumperFadeTarget(3) = 0
		MaterialColor "UPFbat", RGB(50,50,50)
		

	If VR_Room = 1 Then
		BGFlasher10a.visible = 0 
		BGFlasher10a1.visible = 0 
		BGFlasher10a2.visible = 0 
		BGFlasher10a3.visible = 0 
		BGFlasher10a4.visible = 0 
		BGFlasher10a5.visible = 0
		BGFlasher10a6.visible = 0 
		BGFlasher10b.visible = 0 
		BGFlasher10b1.visible = 0 
		BGFlasher10b2.visible = 0 
		BGFlasher10c.visible = 0
		BGFlasher10c1.visible = 0
		BGFlasher10d.visible = 0
		BGFlasher10d1.visible = 0
		BGFlasher10e.visible = 0 
		BGFlasher10e1.visible = 0 
		BGFlasher10e2.visible = 0 
		BGFlasher10g.visible = 0 
		BGFlasher10g1.visible = 0 
	End If


	Else
		For Each Light in LightsGIupf : Light.State = LightStateOn : Next
		For Each Prim in primitivesGIupfpegs : Prim.image = "peglight" : Prim.material = "peg3light": Next		
		For Each Prim in primitivesGIupf : Prim.DisableLighting = 1 : Next
		Primitive98.material = "RubberOn" : Primitive99.material = "RubberOn" : Primitive100.material = "RubberOn" : Primitive101.material = "RubberOn"
		FlBumperFadeTarget(1) = 1 : FlBumperFadeTarget(2) = 1 : FlBumperFadeTarget(3) = 1
		MaterialColor "UPFbat", RGB(100,100,100)

	If VR_Room = 1 Then
		BGFlasher10a.visible = 1 
		BGFlasher10a.visible = 1
		BGFlasher10a1.visible = 1 
		BGFlasher10a2.visible = 1 
		BGFlasher10a3.visible = 1 
		BGFlasher10a4.visible = 1 
		BGFlasher10a5.visible = 1 
		BGFlasher10a6.visible = 1 
		BGFlasher10b.visible = 1 
		BGFlasher10b1.visible = 1 
		BGFlasher10b2.visible = 1
		BGFlasher10c.visible = 1 
		BGFlasher10c1.visible = 1
		BGFlasher10d.visible = 1 
		BGFlasher10d1.visible = 1
		BGFlasher10e.visible = 1 
		BGFlasher10e1.visible = 1 
		BGFlasher10e2.visible = 1
		BGFlasher10g.visible = 1 
		BGFlasher10g1.visible = 1
	End If

    End If
	Sound_GI_Relay enabled, Relay_GI_UPF

	If VR_Room = 1 Then
		Sound_Flash_Relay enabled, Relay_12
	End If

End Sub

'********************************************
' ### General Illumination-LPF ###
'********************************************

dim gilvl:gilvl = 1
Sub SolLPFGIRelay(enabled)
	Dim Prim, Light
	If enabled Then
		For Each Light in LightsGIlpf : Light.State = LightStateOff : Next
		For Each Prim in primitivesGIlpf : Prim.DisableLighting = 0 : Next
		For Each Prim in primitivesGIlpfpegs : Prim.image = "peg" : Prim.material = "peg3": Next
		MaterialColor "LPFbat", RGB(50,50,50)
		gilvl = 0

	If VR_Room = 1 Then
		BGFlasher11.visible = 0
		BGFlasher11a.visible = 0 
		BGFlasher11b.visible = 0
		BGFlasher11c.visible = 0
		BGFlasher11d.visible = 0
		BGFlasher11e.visible = 0
		BGFlasher11f.visible = 0
		BGFlasher11g.visible = 0
		BGFlasher11h.visible = 0
		BGFlasher11i.visible = 0
		BGFlasher11j.visible = 0
		BGFlasher11k.visible = 0
		BGFlasher11m.visible = 0
		BGFlasher11n.visible = 0
		BGFlasher11o.visible = 0
		BGFlasher11p.visible = 0
		BGFlasher11q.visible = 0
	End If

	Else
		For Each Light in LightsGIlpf : Light.State = LightStateOn : Next
		For Each Prim in primitivesGIlpf : Prim.DisableLighting = 1 : Next
		For Each Prim in primitivesGIlpfpegs : Prim.image = "peglight" : Prim.material = "peg3light": Next
		MaterialColor "LPFbat", RGB(100,100,100)
		gilvl = 1

	If VR_Room = 1 Then
		BGFlasher11.visible = 1 
		BGFlasher11a.visible = 1 
		BGFlasher11b.visible = 1 
		BGFlasher11c.visible = 1
		BGFlasher11d.visible = 1
		BGFlasher11e.visible = 1
		BGFlasher11f.visible = 1
		BGFlasher11g.visible = 1
		BGFlasher11h.visible = 1
		BGFlasher11i.visible = 1
		BGFlasher11j.visible = 1
		BGFlasher11k.visible = 1
		BGFlasher11m.visible = 1
		BGFlasher11n.visible = 1
		BGFlasher11o.visible = 1
		BGFlasher11p.visible = 1
		BGFlasher11q.visible = 1
	End If

	End If
	Sound_GI_Relay enabled, Relay_GI_LPF

	If VR_Room = 1 Then
		Sound_Flash_Relay enabled, Relay_13
	End If
End Sub  


'********************************************
' WorkArounds
'********************************************

Sub KickbackTimer_Timer
	' Turn off kickback after a certain period
	KickbackTimer.Enabled = False
	Kickback.Enabled = False
End Sub

Sub Kickback_Hit()
	' Kick ball back up, with random unpredictable force (like real life)
	Kickback.Kick 0, (33 + ((Rnd - 0.5) * 3)), 15
	'change the 33 to something else to raise or lower the kickback strength
	Kickback.Enabled = False
End Sub

Sub NoJump_Hit() : ActiveBall.VelZ = 0 : End Sub

sub MagnetHelper_unhit
	if MagnaSave.MagnetOn = 1 then	'if magnet is on...
		activeball.vely = activeball.vely * -0.25	'reverse direction of the ball
		activeball.velx = activeball.velx * -0.25
	end if
End Sub


'********************************************
' Lamps & Flashers 
'********************************************

LampTimer.Enabled = 1

Sub LampTimer_Timer()
    Dim chgLamp, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
			Select Case LightType(chgLamp(ii, 0))
				Case 0
					If LightObjectFirst(chgLamp(ii, 0)).state <> chgLamp(ii, 1) Then
						LightObjectFirst(chgLamp(ii, 0)).state = chgLamp(ii, 1) : LightObjectSecond(chgLamp(ii, 0)).state = chgLamp(ii, 1)
					End If
				Case 1 : 
						If Light19.State <> chgLamp(ii, 1) Then
							Select Case chgLamp(ii, 1)
								Case 0
									Light19.state = 0:Light19b.state = 0
									PegPlasticT23.Disablelighting = 0 : PegPlasticT23.image = "peg" : PegPlasticT23.material = "peg3"
									PegPlasticT24.Disablelighting = 0 : PegPlasticT24.image = "peg" : PegPlasticT24.material = "peg3"
									bulb0.image = "simplelightyellow1" : bulb0.blenddisablelighting = 0.05
								Case 1
									Light19.state = 1:Light19b.state = 1
									PegPlasticT23.Disablelighting = 1 : PegPlasticT23.image = "peglight" : PegPlasticT23.material = "peg3light"
									PegPlasticT24.Disablelighting = 1 : PegPlasticT24.image = "peglight" : PegPlasticT24.material = "peg3light"
									bulb0.image = "simplelightyellow8" : bulb0.blenddisablelighting = 1
							 End Select
						End If
				Case 2 
						Select Case chgLamp(ii, 1)
							Case 0: LightObjectFirst(chgLamp(ii, 0)).IntensityScale = 0.5
							Case 1: LightObjectFirst(chgLamp(ii, 0)).IntensityScale = 20.0
						End Select
			End Select
        Next
    End If

    If VR_Room = 0 and cab_mode = 0 Then
        UpdateLeds
    End If

	If VR_Room=1 Then
		DisplayTimer
	End If

End Sub

'********************************************
' Change Ball appearance
'********************************************

Sub ChangeBall(ballnr)
	Dim ii, col
	Bk2k.BallDecalMode = CustomBallLogoMode(ballnr)
	Bk2k.BallFrontDecal = CustomBallDecal(ballnr)
	Bk2k.DefaultBulbIntensityScale = CustomBulbIntensity(ballnr)
	Bk2k.BallImage = CustomBallImage(ballnr)
	GlowBall = CustomBallGlow(ballnr)
	For ii = 0 to 2
		col = RGB(red(ballnr), green(ballnr), Blue(ballnr))
		GlowLow(ii).color = col : GlowMed(ii).color = col : GlowHigh(ii).color = col
		GlowLow(ii).colorfull = col : GlowMed(ii).colorfull = col : GlowHigh(ii).colorfull = col
	Next
End Sub


'********************************************
' Bumper graphics
'********************************************

' prepare some global vars to dim/brighten objects when using day-night slider
Dim DayNightAdjust , DNA30, DNA45, DNA90, fact : fact = 1
If NightDay < 10 Then
	DNA30 = 0 : DNA45 = fact * (NightDay-10)/20 : DNA90 = 0 : DayNightAdjust = fact *  0.4
Else
	DNA30 = fact * (NightDay-10)/30 : DNA45 = fact * (NightDay-10)/45 : DNA90 = fact * (NightDay-10)/90 : DayNightAdjust = fact * NightDay/25
End If

Dim FlBumperFadeActual(6), FlBumperFadeTarget(6), FlBumperColor(6), FlBumperTop(6), FlBumperSmallLight(6), Flbumperbiglight(6)
Dim FlBumperDisk(6), FlBumperBase(6), FlBumperBulb(6), FlBumperscrews(6), FlBumperActive(6), FlBumperHighlight(6), FlBumperBaseInside(6)
Dim cnt : For cnt = 1 to 6 : FlBumperActive(cnt) = False : Next

FlInitBumper 1, "red"
FlInitBumper 2, "red"
FlInitBumper 3, "red"

Sub FlInitBumper(nr, col)
	FlBumperActive(nr) = True
	FlBumperFadeActual(nr) = 1 : FlBumperFadeTarget(nr) = 0: FlBumperColor(nr) = col
	Set FlBumperTop(nr) = Eval("bumpertop" & nr) : FlBumperTop(nr).material = "bumpertopmat" & nr
	Set FlBumperSmallLight(nr) = Eval("bumpersmalllight" & nr) : Set Flbumperbiglight(nr) = Eval("bumperbiglight" & nr)
	Set FlBumperDisk(nr) = Eval("bumperdisk" & nr) : Set FlBumperBase(nr) = Eval("bumperbase" & nr)
	Set FlBumperBulb(nr) = Eval("bumperbulb" & nr) : FlBumperBulb(nr).material = "bumperbulbmat" & nr
	Set FlBumperscrews(nr) = Eval("bumperscrews" & nr): FlBumperscrews(nr).material = "bumperscrew" & col
	Set FlBumperHighlight(nr) = Eval("bumperhighlight" & nr)
	Set FlBumperBaseInside(nr) = Eval("bumperbaseinside" & nr)
	select case col
		Case "red"
			FlBumperSmallLight(nr).color = RGB(255,16,0) : FlBumperSmallLight(nr).colorfull = RGB(255,48,0)
			'FlBumperBigLight(nr).color = RGB(255,2,0) : FlBumperBigLight(nr).colorfull = RGB(255,2,0)
			FlBumperBigLight(nr).color = RGB(255,180,100) : FlBumperBigLight(nr).colorfull = RGB(255,255,255)
			FlBumperHighlight(nr).color = RGB(255,255,0)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.99
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperDisk(nr).BlendDisableLighting = 0.2
	end select
End Sub

Sub FlFadeBumper(nr, Z)
	FlBumperBase(nr).BlendDisableLighting = 2 + 4* Z * DayNightAdjust
	FlBumperBaseInside(nr).BlendDisableLighting = 0.4 * Z * DayNightAdjust

	select case FlBumperColor(nr)
		
		Case "red" 
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.95 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 6 + Z*28,3 + z*8), RGB(255,255,255), RGB(255,255,255), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 15 + 50 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 12 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 20 * DayNightAdjust + 2500 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 150 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,16 - Z*8,8 - Z*8)
			MaterialColor "bumperbase", RGB(64+64*Z,48+48*Z,20+20*Z) 'RGB(255-100*z,255, 255-180*z)
			MaterialColor "bumperdisk", RGB(30+10*Z,20+10*Z,0)

	end select

End Sub

Sub BumperTimer_Timer
	dim nr
	For nr = 1 to 6
		If FlBumperFadeActual(nr) < FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.999
			If FlBumperFadeActual(nr) > 0.99 Then FlBumperFadeActual(nr) = 1 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
		If FlBumperFadeActual(nr) > FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.2 / (FlBumperFadeActual(nr) + 0.1)
			If FlBumperFadeActual(nr) < 0.01 Then FlBumperFadeActual(nr) = 0 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
	next
End Sub

'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

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
sourcenames = Array ("","","","")
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
							objrtx1(s).rotz = AnglePP(DSSources(AnotherSource)(0),DSSources(AnotherSource)(1), gBOT(s).X, gBOT(s).Y) + 90
							ShadowOpacity = (falloff-DistanceFast((gBOT(s).x-DSSources(AnotherSource)(0)),(gBOT(s).y-DSSources(AnotherSource)(1))))/falloff
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

							currentMat = objrtx2(s).material
							objrtx2(s).visible = 1 : objrtx2(s).X = gBOT(s).X : objrtx2(s).Y = gBOT(s).Y + fovY
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
		If Flipper1.currentangle <> EndAngle1 then 
			EOSNudge1 = 0
		end if
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

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 1 'EM's to late 80's
'Const EOSTnew = 0.8 '90's and later
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

Const LiveCatch = 12
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
RubbersD.addpoint 0, 0, 0.96        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.96
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

'######################### Add Dampenf to Dampener Class 
'#########################    Only applies dampener when abs(velx) < 2 and vely < 0 and vely > -3.75  


Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold 	'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
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
		RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	public sub Dampenf(aBall, parm, ver)
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


	Public Sub Report() 	'debug, reports all coords in tbPL.text
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

	Public Sub Update()        'tracks in-ball-velocity
		dim str, b, highestID

		for each b in gBOT
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)        'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)        'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)        'set bounds

		for each b in gBOT
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class


Dim bBallInTrough(3)
Sub CheckBallLocations
	Dim b
	For b = 0 to UBound(gBOT)
		'Check if ball is in the trough
		If InRect(gBOT(b).X, gBOT(b).Y, 856,1717, 856,1796, 470,2008, 420,1967) Then
			bBallInTrough(b) = True
		Else
			bBallInTrough(b) = False
		End If
	Next
End Sub


'******************************************************
'****  END PHYSICS DAMPENERS
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
'       isDropped:  Boolean which determines whether a drop target is dropped. Set to false if they are initially raised, true if initially dropped.


Dim DT1,DT2,DT3,DT4,DT5,DT6

Set DT1 = (new DropTarget)(sw1, sw1a, sw1p, 41, 0, false)
Set DT2 = (new DropTarget)(sw2, sw2a, sw2p, 42, 0, false)
Set DT3 = (new DropTarget)(sw3, sw3a, sw3p, 43, 0, false)
Set DT4 = (new DropTarget)(sw4, sw4a, sw4p, 44, 0, false)
Set DT5 = (new DropTarget)(sw5, sw5a, sw5p, 45, 0, false)
Set DT6 = (new DropTarget)(sw6, sw6a, sw6p, 46, 0, false)

Dim DTArray
DTArray = Array(DT1,DT2,DT3,DT4,DT5,DT6)


'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 44 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 5 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 2 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 100 '40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30 'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "Target_Hit_1" 'Drop Target Hit sound
Const DTDropSound = "DropTarget_Down" 'Drop Target Drop sound
Const DTResetSound = "DropTarget_Up" 'Drop Target reset sound

Const DTMass = 0.2 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance


'******************************************************
'  DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)
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
'		PlaySoundAt SoundFX(DTDropSound,DOFDropTargets),prim
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
			if UsingROM then controller.Switch(Switchid) = 1
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
'****  BALL ROLLING AND DROP SOUNDS
'******************************************************

Const anglecompensate = 15
Dim rolling
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
		If GlowBall And b <3 AND NOT bBallInTrough(b) Then GlowLow(b).state = 0 : GlowMed(b).state = 0 : GlowHigh(b).state = 0 :  End If
	Next

	' exit the sub if no balls on the table
	If UBound(gBOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(gBOT)
		If BallVel(gBOT(b)) > 1 AND gBOT(b).z < 30 AND NOT bBallInTrough(b) Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b & ampFactor), -1, VolPlayfieldRoll(gBOT(b)) * 1.1 * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b)) 

		ElseIf BallVel(gBOT(b)) > 1 AND gBOT(b).z > 110 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b & ampFactor), -1, VolPlayfieldRoll(gBOT(b)) * 1.1 * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b & ampFactor)
				rolling(b) = False
			End If
		End If

		If GlowBall and NOT bBallInTrough(b) Then
			If gBOT(b).z > 160 Then
				If GlowHigh(b).state = 0 Then GlowLow(b).state = 0 : GlowMed(b).state = 0 : GlowHigh(b).state = 1 : End If
				GlowHigh(b).x = gBOT(b).x : GlowHigh(b).y = gBOT(b).y + anglecompensate
			Else
				If gBOT(b).z > 50 Then
					If GlowMed(b).state = 0 Then GlowLow(b).state = 0 : GlowHigh(b).state = 0 : GlowMed(b).state = 1 : End If
					GlowMed(b).x = gBOT(b).x : GlowMed(b).y = gBOT(b).y + anglecompensate
				Else
					If GlowLow(b).state = 0 Then GlowMed(b).state = 0 : GlowHigh(b).state = 0 : GlowLow(b).state = 1 : End If
					GlowLow(b).x = gBOT(b).x : GlowLow(b).y = gBOT(b).y + anglecompensate
				End If
			End if
		Else
			GlowLow(b).state = 0 : GlowMed(b).state = 0 : GlowHigh(b).state = 0
		End If

		'***Ball Drop Sounds***
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
dim RampType(3)	

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

'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1														'volume level; range [0, 1]
NudgeLeftSoundLevel = 1													'volume level; range [0, 1]
NudgeRightSoundLevel = 1												'volume level; range [0, 1]
NudgeCenterSoundLevel = 1												'volume level; range [0, 1]
StartButtonSoundLevel = 0.1												'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 											'volume level; range [0, 1]
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
RubberFlipperSoundFactor = .06 '0.075/5										'volume multiplier; must not be zero
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
TargetSoundFactor = .2 '0.0025 * 10											'volume multiplier; must not be zero
DTSoundLevel = 0.25														'volume multiplier; must not be zero
RolloverSoundLevel = .5 '0.25                              					'volume level; range [0, 1]
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
'****  End FLEEP MECHANICAL SOUNDS
'******************************************************




'********************************************
' VR Backglass Digit code
'********************************************

dim DisplayColor
DisplayColor =  RGB(255,40,1)

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

Sub DisplayTimer
    Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
    ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
       For ii=0 To UBound(chgLED)
          num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
			if (num < 32) then
              For Each obj In Digits(num)
'                   If chg And 1 Then obj.visible=stat And 1    'if you use the object color for off; turn the display object visible to not visible on the playfield, and uncomment this line out.
				   If chg And 1 Then FadeDisplay obj, stat And 1	
                   chg=chg\2 : stat=stat\2
                  Next
			Else
			       end if
        Next
    End If
 End Sub

Sub FadeDisplay(object, onoff)
	If OnOff = 1 Then
		object.color = DisplayColor
		Object.Opacity = 1750
	Else
		Object.Color = RGB(1,1,1)
		Object.Opacity = 750
	End If
End Sub


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

InitDigits

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
	for each VRThings in aReels:VRThings.visible = 0: Next
	for each VRThings in DTRails:VRThings.visible = 0:Next
Else
	for each VRThings in VRStuff:VRThings.visible = 1:Next
	for each VRThings in VRClock:VRThings.visible = WallClock:Next
	for each VRThings in aReels:VRThings.visible = 0: Next
	for each VRThings in DTRails:VRThings.visible = 0:Next
'Custom Walls, Floor, and Roof
	if CustomWalls = 1 Then
		VR_Wall_Left.image = "VR_Wall_Left"
		VR_Wall_Right.image = "VR_Wall_Right"
		VR_Floor.image = "VR_Floor"
		VR_Roof.image = "VR_Roof"
	end if

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
' Rail Glow adjustments
'********************************************

if VR_Room = 1 Then
	Primitive001.material = "Material1VR"
	Primitive001.image = "VR Rail Textures"
	Primitive10.material = "rampVR"
	Primitive71.visible = 0
	Primitive71vr.visible = 1
	If topper = 1 Then
		Primary_topper.visible = 1
		Primary_topper2.visible = 0
	Else
		Primary_topper.visible = 0
	End If

	If topper = 2 Then
		Primary_topper2.visible = 1
		Primary_topper.visible = 0
	Else
		Primary_topper2.visible = 0
	End If

	If poster = 1 Then
		VRposter.visible = 1
	Else
		VRposter.visible = 0
	End If

Else
	If remove_rail_glow = 1 Then
		primitive001.material = "Material1VR"
		Primitive10.material = "rampVR"
	Else
		primitive001.material = "Material1"
		Primitive10.material = "ramp"
	End If
	Primitive71.visible = 1
	Primitive71vr.visible = 0
	Primary_topper.visible = 0
end If


'********************************************
' Keep the light intenisty to the 4k cab settings Flupper had; lower settings for VR or by choice
'********************************************

if VR_Room = 1 OR lower_light_brightness = 1 Then
	GI12.intensity = 20
	GI23.intensity = 20
	GI12b.intensity = 20
	GI10.intensity = 50
	GI11.intensity = 50
	GI19.intensity = 50
	GI20.intensity = 50
	GI34.intensity = 20
	Light25b.intensity = 200
	Light26b.intensity = 200
	Light27b.intensity = 200
	GI1.intensity = 50
	GI2.intensity = 50
	GI3.intensity = 50
	GI4.intensity = 50
	GI22b.intensity = 10
	GI10b.intensity = 10
	GI9b.intensity = 10
	GI11b.intensity = 10
	GI22.intensity = 50
	GI10.intensity = 50
	GI9.intensity = 50
	GI11.intensity = 50
Else
	GI12.intensity = 150
	GI23.intensity = 150
	GI12b.intensity = 100
	GI10.intensity = 75
	GI11.intensity = 75
	GI19.intensity = 200
	GI20.intensity = 280
	GI34.intensity = 40
	Light25b.intensity = 400
	Light26b.intensity = 400
	Light27b.intensity = 400
	GI1.intensity = 200
	GI2.intensity = 200
	GI3.intensity = 200
	GI4.intensity = 200
	GI22b.intensity = 25
	GI10b.intensity = 25
	GI9b.intensity = 25
	GI11b.intensity = 25
	GI22.intensity = 75
	GI10.intensity = 75
	GI9.intensity = 75
	GI11.intensity = 75
End If


'********************************************
' Bloom Strength adjustments 
'   (lowered for desktop and VR... could see glow on balls too much)
' Also higher side rails for cab mode
'********************************************

if cab_mode = 1 Then
	If force_bloom_off = 1 Then
		BK2K.bloomstrength = 0
	Else
		BK2K.bloomstrength = 1
	End If

	If ForceSiderailsFS = 1 then 
		wallleftFS.isdropped = 0 
		wallrightFS.isdropped = 0 
	Else
		wallleftFS.isdropped = 1
		wallrightFS.isdropped = 1 
	End If
Else
	BK2K.bloomstrength = 0
	wallleftFS.isdropped = 1
	wallrightFS.isdropped = 1 
End if


'********************************************
' LED display on Desktop
' Based on the Eala's rutine
'********************************************

Dim DTDigits(32)
DTDigits(0) = Array(a00, a05, a0c, a0d, a08, a01, a06, a0f, a02, a03, a04, a07, a0b, a0a, a09, a0e)
DTDigits(1) = Array(a10, a15, a1c, a1d, a18, a11, a16, a1f, a12, a13, a14, a17, a1b, a1a, a19, a1e)
DTDigits(2) = Array(a20, a25, a2c, a2d, a28, a21, a26, a2f, a22, a23, a24, a27, a2b, a2a, a29, a2e)
DTDigits(3) = Array(a30, a35, a3c, a3d, a38, a31, a36, a3f, a32, a33, a34, a37, a3b, a3a, a39, a3e)
DTDigits(4) = Array(a40, a45, a4c, a4d, a48, a41, a46, a4f, a42, a43, a44, a47, a4b, a4a, a49, a4e)
DTDigits(5) = Array(a50, a55, a5c, a5d, a58, a51, a56, a5f, a52, a53, a54, a57, a5b, a5a, a59, a5e)
DTDigits(6) = Array(a60, a65, a6c, a6d, a68, a61, a66, a6f, a62, a63, a64, a67, a6b, a6a, a69, a6e)
DTDigits(7) = Array(a70, a75, a7c, a7d, a78, a71, a76, a7f, a72, a73, a74, a77, a7b, a7a, a79, a7e)
DTDigits(8) = Array(a80, a85, a8c, a8d, a88, a81, a86, a8f, a82, a83, a84, a87, a8b, a8a, a89, a8e)
DTDigits(9) = Array(a90, a95, a9c, a9d, a98, a91, a96, a9f, a92, a93, a94, a97, a9b, a9a, a99, a9e)
DTDigits(10) = Array(aa0, aa5, aac, aad, aa8, aa1, aa6, aaf, aa2, aa3, aa4, aa7, aab, aaa, aa9, aae)
DTDigits(11) = Array(ab0, ab5, abc, abd, ab8, ab1, ab6, abf, ab2, ab3, ab4, ab7, abb, aba, ab9, abe)
DTDigits(12) = Array(ac0, ac5, acc, acd, ac8, ac1, ac6, acf, ac2, ac3, ac4, ac7, acb, aca, ac9, ace)
DTDigits(13) = Array(ad0, ad5, adc, add, ad8, ad1, ad6, adf, ad2, ad3, ad4, ad7, adb, ada, ad9, ade)
DTDigits(14) = Array(ae0, ae5, aec, aed, ae8, ae1, ae6, aef, ae2, ae3, ae4, ae7, aeb, aea, ae9, aee)
DTDigits(15) = Array(af0, af5, afc, afd, af8, af1, af6, aff, af2, af3, af4, af7, afb, afa, af9, afe)

DTDigits(16) = Array(b00, b05, b0c, b0d, b08, b01, b06, b0f, b02, b03, b04, b07, b0b, b0a, b09, b0e)
DTDigits(17) = Array(b10, b15, b1c, b1d, b18, b11, b16, b1f, b12, b13, b14, b17, b1b, b1a, b19, b1e)
DTDigits(18) = Array(b20, b25, b2c, b2d, b28, b21, b26, b2f, b22, b23, b24, b27, b2b, b2a, b29, b2e)
DTDigits(19) = Array(b30, b35, b3c, b3d, b38, b31, b36, b3f, b32, b33, b34, b37, b3b, b3a, b39, b3e)
DTDigits(20) = Array(b40, b45, b4c, b4d, b48, b41, b46, b4f, b42, b43, b44, b47, b4b, b4a, b49, b4e)
DTDigits(21) = Array(b50, b55, b5c, b5d, b58, b51, b56, b5f, b52, b53, b54, b57, b5b, b5a, b59, b5e)
DTDigits(22) = Array(b60, b65, b6c, b6d, b68, b61, b66, b6f, b62, b63, b64, b67, b6b, b6a, b69, b6e)
DTDigits(23) = Array(b70, b75, b7c, b7d, b78, b71, b76, b7f, b72, b73, b74, b77, b7b, b7a, b79, b7e)
DTDigits(24) = Array(b80, b85, b8c, b8d, b88, b81, b86, b8f, b82, b83, b84, b87, b8b, b8a, b89, b8e)
DTDigits(25) = Array(b90, b95, b9c, b9d, b98, b91, b96, b9f, b92, b93, b94, b97, b9b, b9a, b99, b9e)
DTDigits(26) = Array(ba0, ba5, bac, bad, ba8, ba1, ba6, baf, ba2, ba3, ba4, ba7, bab, baa, ba9, bae)
DTDigits(27) = Array(bb0, bb5, bbc, bbd, bb8, bb1, bb6, bbf, bb2, bb3, bb4, bb7, bbb, bba, bb9, bbe)
DTDigits(28) = Array(bc0, bc5, bcc, bcd, bc8, bc1, bc6, bcf, bc2, bc3, bc4, bc7, bcb, bca, bc9, bce)
DTDigits(29) = Array(bd0, bd5, bdc, bdd, bd8, bd1, bd6, bdf, bd2, bd3, bd4, bd7, bdb, bda, bd9, bde)
DTDigits(30) = Array(be0, be5, bec, bed, be8, be1, be6, bef, be2, be3, be4, be7, beb, bea, be9, bee)
DTDigits(31) = Array(bf0, bf5, bfc, bfd, bf8, bf1, bf6, bff, bf2, bf3, bf4, bf7, bfb, bfa, bf9, bfe)

Sub UpdateLeds
    Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
    ChgLED = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
        For ii = 0 To UBound(chgLED)
            num = chgLED(ii, 0):chg = chgLED(ii, 1):stat = chgLED(ii, 2)
            For Each obj In DTDigits(num)
                If chg And 1 Then obj.State = stat And 1
                chg = chg \ 2:stat = stat \ 2
            Next
        Next
    End If
End Sub


'********************************************
' LAMP CALLBACK for the bckglass flasher lamps
'********************************************

if VR_Room = 1 Then
	Set LampCallback = GetRef("UpdateMultipleLamps")
End If

Sub UpdateMultipleLamps()

	If Controller.Lamp(1) = 0 Then: BGFlasherR.visible=0: else: BGFlasherR.visible=1 ' R
	If Controller.Lamp(2) = 0 Then: BGFlasherA.visible=0: else: BGFlasherA.visible=1 ' A
	If Controller.Lamp(3) = 0 Then: BGFlasherN.visible=0: else: BGFlasherN.visible=1 ' N
	If Controller.Lamp(4) = 0 Then: BGFlasherS.visible=0: else: BGFlasherS.visible=1 ' S
	If Controller.Lamp(6) = 0 Then: BGFlasherO.visible=0: else: BGFlasherO.visible=1 ' O
	If Controller.Lamp(7) = 0 Then: BGFlasherM.visible=0: else: BGFlasherM.visible=1 ' M

End Sub



'********************************************
' Set Up Backglass Flashers
'   (this is for lining up the backglass flashers on top of a backglass image)
'********************************************


Sub Setup_backglass()
	Dim obj

	For Each obj In VRBackglassFlashTop
		obj.x = obj.x
		obj.height = - obj.y
		obj.y = -110 'adjusts the distance from the backglass towards the user
	Next

	For Each obj In VRBackglassFlashMed
		obj.x = obj.x
		obj.height = - obj.y
		obj.y = -95 'adjusts the distance from the backglass towards the user
	Next

	For Each obj In VRBackglassFlashlow
		obj.x = obj.x
		obj.height = - obj.y
		obj.y = -75 'adjusts the distance from the backglass towards the user
	Next

	For Each obj In VRDMDFlash
		obj.x = obj.x
		obj.height = - obj.y
		obj.y = -18 'adjusts the distance from the backglass towards the user
	Next

End Sub


'********************************************
' Backglass Flasher Objects
'********************************************


Sub Sol25(Enabled)
	If Enabled Then
		BGFlasher25.visible = 1 
		BGFlasher25a.visible = 1
	Else
		BGFlasher25.visible = 0	
		BGFlasher25a.visible = 0
	End If 
End Sub

Sub Sol26(Enabled)
	If Enabled Then
		BGFlasher26.visible = 1 
		BGFlasher26a.visible = 1 
	Else
		BGFlasher26.visible = 0	
		BGFlasher26a.visible = 0	
	End If 
End Sub

Sub Sol27(Enabled)
	If Enabled Then
		BGFlasher27.visible = 1 
		BGFlasher27a.visible = 1 
	Else
		BGFlasher27.visible = 0	
		BGFlasher27a.visible = 0	
	End If 
End Sub

Sub Sol28(Enabled)
	If Enabled Then
		BGFlasher28.visible = 1 
		BGFlasher28a.visible = 1 
	Else
		BGFlasher28.visible = 0	
		BGFlasher28a.visible = 0
	End If 
End Sub

Sub Sol29(Enabled)
	If Enabled Then
		BGFlasher29.visible = 1 
		BGFlasher29a.visible = 1 
	Else
		BGFlasher29.visible = 0	
		BGFlasher29a.visible = 0
	End If 
End Sub

Sub Sol30(Enabled)
	If Enabled Then
		BGFlasher30.visible = 1 
		BGFlasher30a.visible = 1 
	Else
		BGFlasher30.visible = 0	
		BGFlasher30a.visible = 0
	End If 
End Sub

Sub Sol31(Enabled)
	If Enabled Then
		BGFlasher31.visible = 1 
		BGFlasher31a.visible = 1 
	Else
		BGFlasher31.visible = 0	
		BGFlasher31a.visible = 0
	End If 
End Sub

Sub Sol32(Enabled)
	If Enabled Then
		BGFlasher32.visible = 1 
		BGFlasher32a.visible = 1 
	Else
		BGFlasher32.visible = 0	
		BGFlasher32a.visible = 0
	End If 
End Sub



'********************************************
' LUT Code
'********************************************

If VR_Room = 1 Then
	If LUT = 2 Then
		BK2K.ColorGradeImage = "LUT1_1"
	Else
		BK2K.ColorGradeImage = "ColorGradeLUT256x16_ConSat"
	End If
Else
	If LUT = 1 Then
		BK2K.ColorGradeImage = "ColorGradeLUT256x16_ConSat"
	Else
		BK2K.ColorGradeImage = "LUT1_1"
	End If
End If

