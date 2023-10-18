'*****************************************************************************************
'*****************************************************************************************
'****************************** Diner, Williams 1990 *************************************
'*****************************************************************************************

' Diner by Flupper
' Based on VP9 version by Tamoore which was based on Pac-Dude's version
' Playfield redraw by Bodydump
' Plastics redraws by Ben Logan
' Several plastics photos from George
' standup target by Dark
' many sounds from Knorr's sound package
' testing by Clark Kent and Ben Logan
' Shadow ball code by Ninuzzu

' VPW CHANGE LOG
' v01 - Fluffhead35  - Removed Duplicate Functions.  Enabled flipper calls in keyup. Changed UseSolenoids to 2. Fixed Bumpers, slings, and gates to be in line with Doc.  Removed slings from rubbers collection.
'                    - Changed Gameplay Difficult to 56.  Changed min/max slope of table to 6 and 7. Created a rubbers Layer and disabled all rubbers.  Created sleeves layer and moved sleeves and rubber post primitives there
'                    - Reshaping the flipper triggers. Added all rubber walls and corners to the table, Created Rubber posts and sleeve primitives. Reshapped rubber at slings and corrected corners.  Fixed sling position
'                    - Changed all old sleves to be toys. 
' v01a               - Gave slope to Ramp7 so ball would not get suck.  Added Materials to missing collidable objects.
' v01b               - Reorganize Code. Uncomment AmbientBallShadows code. ReEnabled the AlternateDroptargets. Deleted new Lut and restored original lut setting. Deleted BallShadow Code.
' v01c               - Changed the ramp drop hit triggers to stars, put them at 60 hight, moved wireramp_off sound to hit trigger.  Trigger is hit height of 0, might need to make more. 
'                    - Reimported the sounds as the rolling sounds were not playing correctly.  Adjusted the cup values to make it spin more.  Fixed ballshadows.  Updated SoundCode to latest.
'                    - Updated Physics to latest.  Changed switch a to be asw, it was conflicting with a redim.  
' v01d               - Updated collections to have correct sound values.  Added physics materials to collidables where they were missing.
' v02                - Commented out bumper sounds and wired up kicker sound
' v02a               - added fluppper domes
' v02b               - Adjusting drop target locations for hamburger.  Changed long rubber on left side to middles.
' v02c               - Roth drop targets and new sling correction code
' v03  -Leojreimroc  - VR Room, Animated Backglass
' v04 - Fluffhead35  - Adjusted Lights for Customers and Diner Inserts.  Adjusted Flasher postion and size on backwall, Adjusted Flasher fading based on apophis recomendations.
' v05 - fluffhead35  - Setting SSR to .1.  Adjusting Materials on walls to be metal from wood. Reverted right ramp beginning to orignal settings.  Reverted Gate3 to original settings.  Fixed Flasher bloom
' v05a - Rawd        - Added Burger and Fries to VR Room
' v06 - fluffhead35  - Adjusting Digital Nudge
' v07 - Leojreimroc  - Various VR adjustements, reduced size for backglass images, turned ball reflection on, changed Beer to Coke in VR
' v08 - fluffhead35  - Changed DT POV based on Apophis suggistion to remove black borde in desktop.  Removed commented out code.  Removed unused sound files.  Adjusted Tilt Sensitivity.
' v08a - fluffhead35 - Added in BountyBob pov
' v08b               - added Blacksad b2s code and flag to enable it
' v09 -              - merged in leojreimroc alphanumerics for desktop
' v10 - Leojreimroc  - Fixed Alphanumerics
' v11 - fluffhead35  - updating fs pov.   Adjusted flasher bloom brightness.
' v12 - Leojreimroc  - Set VR objects to non-visible, non static rendering
' v13 - sixtoe       - FIxed left Ramp
' v14 - fluffhead35  - Adjusted cup speed along with right ramp physic materials.
' v15 - fluffhead35  - Adjusted cupspeed logic and added CupSpeedValue variable.
' v15a               - Set CoffeCup to material of plastic.
' v16 - fluffhead35  - Changed Tabler slope to 6.2/6.2   Changed CupSpeedValue to 3.  Added collidable walls for wires under flippers
'                    - Added ability to set the cupspinner logic to a random value between min and max, or use static cupspeed value of 3
'                    - Added in Rawds vr code for foam and set geight to -224
' v17 - skitso       - Updated GI, LUT, Few Textures.  
' v18 - fluffhead35  - merge of v17 in with v16 because of table corruption.
' v18a  fluffhead35  - Removed Static rendering from plastics as missed this chagne from v17 merge
' v18c               - Updated lights from skitso and updated droptargets from apophis
' v18d  skitoe       - fixed lights and inserts
' v18e  fluffhead35  - Added logic to enable displaytimer if in desktop.  Updated cupspeed to make more random.
' v19   fluffhead35  - Updated falloff for light10 and 11.  Added in new flipper bats.
' v19a  fluffhead35  - Changed bats to .5 dl as they were too dark.
' v20	iaakki		 - Tied flip prims to GI, adjusted flip rest angle 1.5 degree lower and reworked flip trigger areas, smoothed out left ramp at curve
' v21	Sixtoe		 - Various VR fixes, dropped and lengthened trigger wires, dropped lower right flasher height, dropped flipper heights.
' v22	apophis		 - Realigned the playfieldlights flasher on layer_3, modified DINER light shapes and falloff on layer_2
' v1.0 Release
' v1.0.1 fluffhead35 - Rotated Flasher on back wall 180 degrees.  Fixed flipper shadow overlaying serve again light.  Increased apron wall height to prevent lost ball. Fixed droptargets alternative decals from not showing up.
' v1.0.2 fluffhead35 - Updated Lut to load durring it, so default contrast settings are applied at table init.

Option Explicit
Randomize


Dim GlowAmountDay, InsertBrightnessDay, GlowAmountCutoutDay, InsertBrightnessCutoutDay
Dim GlowAmountNight, InsertBrightnessNight, GlowAmountCutoutNight, InsertBrightnessCutoutNight
Dim GlowBall, ChooseBall, CustomBulbIntensity(10), red(10), green(10), Blue(10)
Dim CustomBallImage(10), CustomBallLogoMode(10), CustomBallDecal(10), CustomBallGlow(10)
Dim ForceSiderailsFS, ChooseBats, LightsDemo, ShowClock, ContrastSetting
Dim AlternateDroptargets


'******************************************************************************************
'* TABLE OPTIONS **************************************************************************
'******************************************************************************************


'///////////////////////-----General Sound Options-----///////////////////////
'// VolumeDial:
'// VolumeDial is the actual global volume multiplier for the mechanical sounds.
'// Values smaller than 1 will decrease mechanical sounds volume.
'// Recommended values should be no greater than 1.
Const VolumeDial = 0.8
Const BallRollVolume = 0.5 			'Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 1 			'Level of ramp rolling volume. Value between 0 and 1

'----- VR -----
Const VRRoom = 0 					'0 = OFF, 1 = BaSti/Rawd Room, 2 = Minimal Room, 3 = Ultra Minimal Room

'----- Enable Backsad Backglass code
'                - Enable this if using Blacksad backglass.Downloaded from here https://vpuniverse.com/files/file/4892-diner-williams-1990-2-3-screens-db2s/
Const EnableBlacksadBackglass = 0   '1 = Enabled , 0 = (default) disabled

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									'2 = flasher image shadow, but it moves like ninuzzu's

' *** 0=default flipper, 1=primitive flipper, 2=glow green, 3=glow blue, 4=glow orange ****
ChooseBats = 1

' *** Show siderails in fullscreen mode: True = show siderails, False = do not show *******
ForceSiderailsFS = False

' *** Show clock on back wall *************************************************************
ShowClock = false

' *** Contrast level, possible values are 0 - 7, can be done in game with magnasave keys **
' *** 0: bright, good for desktop view, daytime settings in insert lighting below *********
' *** 1: same as 0, but with nighttime settings in insert lighting below ******************
' *** 2: darker, better contrast, daytime settings in insert lighting below ***************
' *** 3: same as 2, but with nighttime settings in insert lighting below ******************
' *** etc for 4-7; default is 3 ***********************************************************
ContrastSetting = 3


' *** relative sound level for all mechanical sounds **************************************
' *** 1: if you have separate sound system for mech sounds, 2/3/etc when you do not *******
GlobalSoundLevel = 3

' *** Other decals for the droptargets ****************************************************
AlternateDroptargets = False

' *** Insert Lighting settings ************************************************************
' *** The settings below together with ContrastSetting determines how the lighting looks **
' *** for all values: 1.0 = default, useful range 0.1 - 5 *********************************
GlowAmountDay = 0.05
InsertBrightnessDay = 0.8 
GlowAmountCutoutDay = 0 
InsertBrightnessCutoutDay = 0.6
GlowAmountNight = 0.5
InsertBrightnessNight = 0.6
GlowAmountCutoutNight = 0.25
InsertBrightnessCutoutNight = 0.75

' *** Ball settings ***********************************************************************
' *** 0 = normal ball
' *** 1 = white GlowBall
' *** 2 = magma GlowBall
' *** 3 = blue GlowBall
' *** 4 = HDR ball
' *** 5 = earth
' *** 6 = green glowball (matching green GlowBat)
' *** 7 = blue glowball (matching blue GlowBat)
' *** 8 = orange glowball (matching orange GlowBat)
' *** 9 = shiny Ball

ChooseBall = 4

' default Ball
CustomBallGlow(0) = 		False
CustomBallImage(0) = 		"TTMMball"
CustomBallLogoMode(0) = 	False
CustomBallDecal(0) = 		"scratches"
CustomBulbIntensity(0) = 	0.01
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

' HDR ball
CustomBallGlow(4) = 		False
CustomBallImage(4) = 		"ball_HDR"
CustomBallLogoMode(4) = 	False
CustomBallDecal(4) = 		"JPBall-Scratches"
CustomBulbIntensity(4) = 	0.01
Red(4) = 0 : Green(4)	= 0 : Blue(4) = 0

' Earth
CustomBallGlow(5) = 		True
CustomBallImage(5) = 		"ballblack"
CustomBallLogoMode(5) = 	True
CustomBallDecal(5) = 		"earth"
CustomBulbIntensity(5) = 	0
Red(5) = 100 : Green(5)	= 100 : Blue(5) = 100

' green GlowBall
CustomBallGlow(6) = 		True
CustomBallImage(6) = 		"glowball green"
CustomBallLogoMode(6) = 	True
CustomBallDecal(6) = 		""
CustomBulbIntensity(6) = 	0
Red(6) = 100 : Green(6)	= 255 : Blue(6) = 100

' blue GlowBall
CustomBallGlow(7) = 		True
CustomBallImage(7) = 		"glowball blue"
CustomBallLogoMode(7) = 	True
CustomBallDecal(7) = 		""
CustomBulbIntensity(7) = 	0
Red(7) = 100 : Green(7)	= 100 : Blue(7) = 255

' orange GlowBall
CustomBallGlow(8) = 		True
CustomBallImage(8) = 		"glowball orange"
CustomBallLogoMode(8) = 	True
CustomBallDecal(8) = 		""
CustomBulbIntensity(8) = 	0
Red(8) = 255 : Green(8)	= 255 : Blue(8) = 100

' shiny Ball
CustomBallGlow(9) = 		False
CustomBallImage(9) = 		"pinball3"
CustomBallLogoMode(9) = 	False
CustomBallDecal(9) = 		"JPBall-Scratches"
CustomBulbIntensity(9) = 	0.01
Red(9) = 0 : Green(9)	= 0 : Blue(9) = 0

' *** Developer switch: Show all lights, no gameplay **************************************
LightsDemo = false


'******************************************************************************************
'* END OF TABLE OPTIONS *******************************************************************
'******************************************************************************************

' release notes

' version 1.0 : initial version
' version 1.1 : merged 2 lights-layers, improving performance by 8%
'				added flipperbats shadows for the primitive Bats
'				added flasher fade effect (afterglow on the flasher primitive)
'				several DOF related changes proposed by Arngrim
'				slingshot rom sound fix
'				reduced some texture sizes without visual impact
'				some small visual fixes
' version 1.2 : fix for GI not on at first game
'				fix for inserts showing only black
'				potential fix for GI not on (issue similar to black inserts)

' explanation of standard constants (info by Jpsalas and others on VpForums): 
' These constants are for the vpinmame emulation, and they tell vpinmame what it is supposed 
' to emulate.
' UseSolenoids=1 means the vpinmame will use the solenoids, so in the script there are calls
' 				 for a solenoid to do different things (like reset droptargets, kick a ball, etc)
' UseLamps=0 	 means the vpinmame won't bother updating the lights, but done in script
' UseSync=0      (unclear) but probably is to enable or disable the sync in the vpinmame window 
' 				 (or dmd). So it syncs with the screen or not.
' HandleMech=0   means vpinmame won't handle  special animations, they will have to be done 
'				 manually in Scripts
' UseGI=1        If 1 and used together with "Set GiCallback2 = GetRef("UpdateGI")" where 
'				 UpdateGI is the sub routine that sets the Global Illumination lights 
' 				 only the Williams wpc tables have Gi circuitry support (so you can use GICallback)
'				 for other tables a solenoid has to be used
' SFlipperOn     - Flipper activate sound
' SFlipperOff    - Flipper deactivate sound
' SSolenoidOn    - Solenoid activate sound
' SSolenoidOff   - Solenoid deactivate sound
' SCoin          - Coin Sound 



Const BallSize = 50					'Ball size must be 50
Const BallMass = 1					'Ball mass must be 1


'*******************************************
'  Timers
'*******************************************

' The game timer interval is 10 ms
Sub GameTimer_Timer()
	Cor.Update 						'update ball tracking (this sometimes goes in the RDampen_Timer sub)
	RollingUpdate					'update rolling sounds
	DoDTAnim                        'Dropt Targets
End Sub

' The frame timer interval is -1, so executes at the display frame rate
Sub FrameTimer_Timer()
	FlipperVisualUpdate				'update flipper shadows and primitives
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
End Sub
'********************************************

' *** Global constants ***
Const UseSolenoids = 2
Const UseLamps = 0
Const UseSync = 0
Const HandleMech = 0
Const UseGI=0
Const cGameName="diner_l4" 
Const cCredits="Diner, Williams 1990"

' *** Global variables ***
Dim FlippersEnabled		' Used to enable/disable flippers based on tilt status
Dim Leftflash, Rightflash ' Used to detect which flasher is flashing
Dim CupCounter 'Used for influencing speed in the cup
Dim LRAindex, LRA(10) ' angle of lockramp
Dim DVindex, DV(10), DVdir ' angle of diverter
Dim OptionOpacity 'opacity of the option primive
OptionOpacity = 8
Dim relaylevel ' Sound level op relay clicking
relaylevel = 0.5 * GlobalSoundLevel
LRA(0) = -9 : LRA(1) = -6: LRA(2) = -1: LRA(3) = 3: LRA(4) = 5: LRA(5) = 2: LRA(6) = 0
DV(0) = 179 : DV(1) = 180 : DV(2) = 181 : DV(3) = 182 : DV(4) = 184 : DV(5) = 186 : DV(6) = 201
LRAindex = 0 : DVindex = 0 : DVdir = 0

' *** game specific constants ***
Const swOuthole=9,swUpDownRamp=10,swTrough1=11,swTrough2=12,swTrough3=13,swShooterLane=14,swSubPlfdShooter1=15
Const swSubPlfdShooter2=16,swCup=17,swGrillBonus=18,swE=19,swA=20,swT=21,swHotdog=22,swBurger=23,swChili=24
Const swRightRampEntry=27,swRightRampExit=28,swCupEntry=29,swRootBeer=30,swFries=31,swIcedTea=32
Const swLeftRampExit=36,swLeftOutlane=37,swLeftReturnLane=38,swRightReturnLane=39,swRightOutlane=40
Const swUpperLeftEject=49,swLowerLeftEject=50,swLeftJetBumper=51
Const swRightJetBumper=52,swLowerJetBumper=53,swBRKicker=54,swBLKicker=55,swSpinner=56,swFlipR=57,swFlipL=58,swClockWheel=59
Const sOuthole=1,sRampDown=2,sC3BDTReset=3,sRampUp=4,sUpperLeftEject=5,sSubPlfdShooter=6,sKnocker=7,sLowerLeftEject=8
Const sRightRampFlasher=9,sBackBoxRelay=10,sLeftRampFlasher=11,sL3BDTReset=13,sClockWheel=15 
Const sACselectrelay=12, sLeftJetBumper=17,sRightJetBumper=19,sLowerJetBumper=21,sShooterLaneFeeder=22,sDineTF=32
Const sLSling=18,sRSling=20,swLSling=55,swRSling=54

Dim tablewidth, tableheight : tablewidth = Diner.width : tableheight = Diner.height
Dim DesktopMode:DesktopMode = Diner.ShowDT

' *** mechanic handlers ***
Dim dtright, dtleft, bsTrough, bsLowKicker, bsUpperEject, bsSubShooter, WheelMech, bsSubWay

' *** creating lights arrays for playfield inserts ***

' *** LightType 0 = normal insert lighting with two lights
' *** LightType 1 = one object only for visible on/off
' *** LightType 2 = EAT multiple Lights
' *** LightType 3 = cutout flashers (pepe, babs, boris, haji, buck)
' *** LightType 4 = not connected

Dim LightType(65), LightObjectFirst(65), LightObjectSecond(65), LightObjectThird(65), Glowing(10), LightState(65)

LightType(1)  = 1 : Set LightObjectFirst(1)  = register20k  								  		' 20k register
LightType(2)  = 1 : Set LightObjectFirst(2)  = register40k  								  		' 40k register
LightType(3)  = 1 : Set LightObjectFirst(3)  = register60k  								  		' 60k register
LightType(4)  = 1 : Set LightObjectFirst(4)  = register80k  								  		' 80k register
LightType(5)  = 1 : Set LightObjectFirst(5)  = register100k  								  		' 100k register
LightType(6)  = 0 : Set LightObjectFirst(6)  = Light6b  : Set LightObjectSecond(6)  = Light6  		' left adv dine time
LightType(7)  = 0 : Set LightObjectFirst(7)  = Light7b  : Set LightObjectSecond(7)  = Light7  		' right adv dine time
LightType(8)  = 0 : Set LightObjectFirst(8)  = Light8b  : Set LightObjectSecond(8)  = Light8  		' extra ball right
LightType(9)  = 0 : Set LightObjectFirst(9)  = Light9b  : Set LightObjectSecond(9)  = Light9  		' serve again
LightType(10)  = 0 : Set LightObjectFirst(10)  = Light10b  : Set LightObjectSecond(10)  = Light10  	' left ramp scores
LightType(11)  = 0 : Set LightObjectFirst(11)  = Light11b  : Set LightObjectSecond(11)  = Light11  	' right ramp scores
LightType(12)  = 0 : Set LightObjectFirst(12)  = Light12b  : Set LightObjectSecond(12)  = Light12  	' Lock
LightType(13)  = 0 : Set LightObjectFirst(13)  = Light13b  : Set LightObjectSecond(13)  = Light13  	' Release
LightType(14)  = 0 : Set LightObjectFirst(14)  = Light14b  : Set LightObjectSecond(14)  = Light14  	' Rush1
LightType(15)  = 0 : Set LightObjectFirst(15)  = Light15b  : Set LightObjectSecond(15)  = Light15  	' Rush2
LightType(16)  = 0 : Set LightObjectFirst(16)  = Light16b  : Set LightObjectSecond(16)  = Light16  	' Spinner
LightType(17)  = 0 : Set LightObjectFirst(17)  = Light17b  : Set LightObjectSecond(17)  = Light17  	' D
LightType(18)  = 0 : Set LightObjectFirst(18)  = Light18b  : Set LightObjectSecond(18)  = Light18  	' I
LightType(19)  = 0 : Set LightObjectFirst(19)  = Light19b  : Set LightObjectSecond(19)  = Light19  	' N
LightType(20)  = 0 : Set LightObjectFirst(20)  = Light20b  : Set LightObjectSecond(20)  = Light20  	' E
LightType(21)  = 0 : Set LightObjectFirst(21)  = Light21b  : Set LightObjectSecond(21)  = Light21  	' R
LightType(22)  = 2 : Set LightObjectFirst(22)  = Light22b  : Set LightObjectSecond(22)  = Light22 : Set LightObjectThird(22)  = circleE  	' E
LightType(23)  = 2 : Set LightObjectFirst(23)  = Light23b  : Set LightObjectSecond(23)  = Light23 : Set LightObjectThird(23)  = circleA 	' A
LightType(24)  = 2 : Set LightObjectFirst(24)  = Light24b  : Set LightObjectSecond(24)  = Light24 : Set LightObjectThird(24)  = circleT 	' T
LightType(25)  = 1 : Set LightObjectFirst(25)  = juke150k  										   	' 150k jukebox
LightType(26)  = 1 : Set LightObjectFirst(26)  = juke100k   									   	' 100k jukebox
LightType(27)  = 1 : Set LightObjectFirst(27)  = juke75k 										   	' 75k jukebox
LightType(28)  = 1 : Set LightObjectFirst(28)  = juke50k 										   	' 50k jukebox
LightType(29)  = 1 : Set LightObjectFirst(29)  = juke25k 										   	' 25k jukebox
LightType(30)  = 0 : Set LightObjectFirst(30)  = Light30b  : Set LightObjectSecond(30)  = Light30  	' hotdog
LightType(31)  = 0 : Set LightObjectFirst(31)  = Light31b  : Set LightObjectSecond(31)  = Light31  	' Burger
LightType(32)  = 0 : Set LightObjectFirst(32)  = Light32b  : Set LightObjectSecond(32)  = Light32  	' soup
LightType(33)  = 3 : Set LightObjectFirst(33)  = Light33b  : Set LightObjectSecond(33)  = Light33  	' Haji
LightType(34)  = 3 : Set LightObjectFirst(34)  = Light34b  : Set LightObjectSecond(34)  = Light34  	' Babs
LightType(35)  = 3 : Set LightObjectFirst(35)  = Light35b  : Set LightObjectSecond(35)  = Light35  	' Boris
LightType(36)  = 3 : Set LightObjectFirst(36)  = Light36b  : Set LightObjectSecond(36)  = Light36  	' Pepe
LightType(37)  = 3 : Set LightObjectFirst(37)  = Light37b  : Set LightObjectSecond(37)  = Light37  	' Buck
LightType(38)  = 0 : Set LightObjectFirst(38)  = Light38b  : Set LightObjectSecond(38)  = Light38  	' Beer
LightType(39)  = 0 : Set LightObjectFirst(39)  = Light39b  : Set LightObjectSecond(39)  = Light39  	' Fries
LightType(40)  = 0 : Set LightObjectFirst(40)  = Light40b  : Set LightObjectSecond(40)  = Light40  	' Lemonade
LightType(41)  = 0 : Set LightObjectFirst(41)  = Light41b  : Set LightObjectSecond(41)  = Light41  	' 100k
LightType(42)  = 0 : Set LightObjectFirst(42)  = Light42b  : Set LightObjectSecond(42)  = Light42  	' 150k
LightType(43)  = 0 : Set LightObjectFirst(43)  = Light43b  : Set LightObjectSecond(43)  = Light43  	' 250k
LightType(44)  = 0 : Set LightObjectFirst(44)  = Light44b  : Set LightObjectSecond(44)  = Light44  	' 1M
LightType(45)  = 0 : Set LightObjectFirst(45)  = Light45b  : Set LightObjectSecond(45)  = Light45  	' extra ball
LightType(46)  = 0 : Set LightObjectFirst(46)  = Light46b  : Set LightObjectSecond(46)  = Light46  	' food
LightType(47)  = 0 : Set LightObjectFirst(47)  = Light47b  : Set LightObjectSecond(47)  = Light47  	' cup scores
LightType(48)  = 0 : Set LightObjectFirst(48)  = Light48b  : Set LightObjectSecond(48)  = Light48  	' extra ball left
If VRRoom > 0 Then
	LightType(49)  = 1 : Set LightObjectFirst(49) = BGClockLight1 : Set LightObjectFirst(49) = BGClockLight1_2	' 49 - 60 are 1-12 on clock lights
	LightType(50)  = 1 : Set LightObjectFirst(50) = BGClockLight2 :  Set LightObjectFirst(50) = BGClockLight2_2
	LightType(51)  = 1 : Set LightObjectFirst(51) = BGClockLight3 : Set LightObjectFirst(51) = BGClockLight3_2
	LightType(52)  = 1 : Set LightObjectFirst(52) = BGClockLight4 : Set LightObjectFirst(52) = BGClockLight4_2
	LightType(53)  = 1 : Set LightObjectFirst(53) = BGClockLight5 : Set LightObjectFirst(53) = BGClockLight5_2
	LightType(54)  = 1 : Set LightObjectFirst(54) = BGClockLight6 : Set LightObjectFirst(54) = BGClockLight6_2
	LightType(55)  = 1 : Set LightObjectFirst(55) = BGClockLight7 : Set LightObjectFirst(55) = BGClockLight7_2
	LightType(56)  = 1 : Set LightObjectFirst(56) = BGClockLight8 : Set LightObjectFirst(56) = BGClockLight8_2
	LightType(57)  = 1 : Set LightObjectFirst(57) = BGClockLight9 : Set LightObjectFirst(57) = BGClockLight9_2
	LightType(58)  = 1 : Set LightObjectFirst(58) = BGClockLight10 : Set LightObjectFirst(58) = BGClockLight10_2
	LightType(59)  = 1 : Set LightObjectFirst(59) = BGClockLight11 : Set LightObjectFirst(59) = BGClockLight11_2
	LightType(60)  = 1 : Set LightObjectFirst(60) = BGClockLight12 : Set LightObjectFirst(60) = BGClockLight12_2
Else
	LightType(49)  = 4 : 																				
	LightType(50)  = 4 : 
	LightType(51)  = 4 : 
	LightType(52)  = 4 : 
	LightType(53)  = 4 : 
	LightType(54)  = 4 : 
	LightType(55)  = 4 : 
	LightType(56)  = 4 : 
	LightType(57)  = 4 : 
	LightType(58)  = 4 : 
	LightType(59)  = 4 : 
	LightType(60)  = 4 : 
End If
LightType(61)  = 0 : Set LightObjectFirst(61)  = Light61b  : Set LightObjectSecond(61)  = Light61  	' TOP 5
LightType(62)  = 0 : Set LightObjectFirst(62)  = Light62b  : Set LightObjectSecond(62)  = Light62  	' special
LightType(63)  = 0 : Set LightObjectFirst(63)  = Light63b  : Set LightObjectSecond(63)  = Light63  	' dinetime
LightType(64)  = 0 : Set LightObjectFirst(64)  = Light64b  : Set LightObjectSecond(64)  = Light64  	' food
LightType(65)  = 4 : 

' *** prepare the variable with references to three lights for glow ball ***

Set Glowing(0) = Glowball1 : Set Glowing(1) = Glowball2 : Set Glowing(2) = Glowball3

Dim stat
Sub B2SSTAT
	if EnableBlacksadBackglass Then
		If B2SOn Then
			Controller.B2SSetData 100+stat, 0
			stat = INT(rnd*6)+1
			Controller.B2SSetData 100+stat, 1
		End If
	End If
End Sub


' *** Start VPM ***

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0
LoadVPM "01560000", "S11.VBS", 3.26

' MotorCallback	 Function called after every update of solenoids and lamps (i.e. as often as 
'				 possible). Main purpose is to update table based on playfield motors. It can also 
'				 be used to override the standard solenoid callback handler
'Set MotorCallback = GetRef("RollingUpdate") 'realtime updates for rolling sound

'*** solenoid definitions ***

SolCallback(23)="TiltSol"
SolCallback(sOuthole)="bsTrough.SolIn"
SolCallback(sShooterLaneFeeder)="bsTrough.SolOut"
SolCallback(sKnocker) = "SolKnocker"
'SolCallback(sKnocker) = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
'SolCallback(sLeftJetBumper)="vpmSolSound SoundFX(""bumperleft"",DOFContactors),"
'SolCallback(sRightJetBumper)="vpmSolSound SoundFX(""bumperright"",DOFContactors),"
'SolCallback(sLowerJetBumper)="vpmSolSound SoundFX(""bumpermiddle"",DOFContactors),"
SolCallback(sLowerLeftEject) = "SolLowKicker"
SolCallback(sUpperLeftEject) = "SolUpperEject"
SolCallback(sRightRampFlasher) 	= "SolFlash9"			'BG Moon
SolCallback(sLeftRampFlasher)   = "SolFlash11"			'BG Diner
SolCallback(sSubPlfdShooter) = "SolSubPlfdShooter"
SolCallback(25) = "SolHajiF"							'BG Haji
SolCallback(26) = "SolBabsF"							'BG Babs
SolCallback(27) = "SolBorisF"							'BG Boris
SolCallback(28) = "SolPepeF"							'BG Pepe
SolCallback(29) = "SolBuckF"							'BG Buck
SolCallback(30) = "SolCupF"
SolCallback(sBackBoxRelay) = "SolGIRelay"
SolCallback(sACselectrelay)   = "SolACSelect"
SolCallback(sKnocker)    = "SolKnocker"
SolCallback(sC3BDTReset) = "SolDTBankMiddle"
SolCallback(sL3BDTReset) = "SolDTBankLeft"
SolCallback(sRampUp)="SolRampUp"
SolCallback(sRampDown)="SolRampDown"
SolCallback(14)="Sol14Diverter"
SolCallback(31) = "Sol31"								'BG clock (2)
SolCallback(32) = "Sol32"								'BG	Dine Time (2)

Sub SolKnocker(Enabled)
	If enabled Then
		KnockerSolenoid 'Add knocker position object
	End If
End Sub

'*** flipper solenoids disabled for faster response in KeyDown ***
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

' *** table init ***

Sub Diner_Init
	Dim Light, Prim
	vpmInit Me
	If LightsDemo Then
		For Each Light in GlowLights : Light.state = 1 : Next 
		For Each Light in InsertLights : Light.state = 1 : Next
		For Each Light in glowlightscutout : Light.state = 1 : Next 
		For Each Light in insertlightscutout : Light.state = 1 : Next
		For Each Light in LightsGI : Light.State = LightStateOn : Next
		For Each Prim in primitivesGI : Prim.DisableLighting = 1 : Next
		For Each Prim in primitivesGIpegs : Prim.image = "peglight" : Prim.material = "peg3light": Next
		'SolFlash11(True)
		'SolFlash9(true)
		'solCupF(True)
	Else
		With Controller
			' *** explanation of controller properties
			' .HandleKeyboard	if set to 1, VPinMAME will process the keyboard. Standard MAME
			'					keys can be used in VPinMAME output window. Also the internal 
			'					ball simulator is enabled. If set to 0, the scripting language 
			'					will process the keyboard.
			' ShowTitle			if set to 1, Show title bar on VPinMAME output window (to move 
			'					it around) 
			' ShowDMDOnly		if set to 1, does not show VPinMAME status matrices.
			' ShowFrame			if set to 1, Enables the window border, only works if ShowTitle 
			'					is set to 0
			' SplashInfoLine	Game credits to display in startup splash screen.
			' Hidden			If set to 1, hides the vpinmame display. This is useful for 
			'					applications that want to do the display drawing on their own. 
			' HandleMechanics	If the game has a PinMAME simulator it can be used to simulate 
			'					hardware "toys".
			.GameName = cGameName
			If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
			.SplashInfoLine = "Diner - Williams 1990" & vbNewLine & "VPX by Flupper"
			.Games(cGameName).Settings.Value("rol") = 0 'set it to 1 to rotate the DMD to the left
			.Games(cGameName).Settings.Value("ror") = 0
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
		End With
	End if

	'*** Nudging ***
    vpmNudge.TiltSwitch = 1 
    vpmNudge.Sensitivity = 3
	vpmNudge.TiltObj = Array(LeftJetBumper,RightJetBumper,LowerJetBumper,LSling,RSling) 
	    
	' *** Main Timer init ***
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1
	
	'*** option settings ***
	If ContrastSetting=0 or ContrastSetting=2 or ContrastSetting=4 or ContrastSetting=6 Then ChangeGlow(True) Else ChangeGlow(False) End If
	ChangeBats(ChooseBats)
	ChangeBall(ChooseBall)
	If Diner.ShowDT or ForceSiderailsFS then sidewallFS.visible = 0 : sidewallDT.visible = 1 Else lockbar.Visible = 0 : sidewallDT.visible = 0 : sidewallFS.visible = 1 : End If
	If ShowClock Then clock.visible = 1 : clockwijzer.visible = 1 : end If
	If GlowBall Then GraphicsTimer.enabled = True End If
	IF not AlternateDroptargets Then
		hotdog_p.image = "DropTargetyellow" : burger_p.image = "DropTargetyellow" : chili_p.image = "DropTargetyellow"
		icedtea_p.image = "DropTargetred" : rootbeer_p.image = "DropTargetred" : fries_p.image = "DropTargetred"
	End If

	' *** Desktop specific changes for better 3D view ***
	If Diner.ShowDT then 
		Flasher4light.BulbHaloHeight = 250
		rampsDT.visible = 1
		rampsFS.visible = 0
	Else
		rampsDT.visible = 0
		rampsFS.visible = 1
	end If
     
	' *** mechanic for clock hand ***
	Set WheelMech = New cvpmMech
	With WheelMech : 
		.MType = vpmMechStepSol + vpmMechCircle + vpmMechLinear + vpmMechFast 
		.Sol1 = 15 : .Sol2 = 16 : .Length = 200 : .Steps = 60
		.CallBack = GetRef("UpdateClockHand")
		.AddSw 59, 0, 0
		.Start 
	End With

    ' *** Trough ***
    Set bsTrough = New cvpmTrough
    With bsTrough
		.size = 3
		.initSwitches Array(swTrough1,swTrough2,swTrough3)
        .Initexit BallRelease, 90, 4
        .InitEntrySounds "drain", "", ""
        '.InitExitSounds SoundFX("fx_Solenoid",DOFContactors), SoundFX("fx_Ballrel",DOFContactors)
        .Balls = 3
		.EntrySw = 10
    End With


' *** lock ramp saucer ***
	Set bsLowKicker = New cvpmSaucer
	With bsLowKicker
		.InitKicker LowKicker, swLowerLeftEject, 45, 6, 0
		.InitSounds "Saucer_Enter_2", SoundFX("Saucer_Kick",DOFContactors), SoundFX("Saucer_Kick",DOFContactors)
	End With 

	' *** upper left saucer ***
	Set bsUpperEject = New cvpmSaucer
	With bsUpperEject
		.InitKicker UpperEject, swUpperLeftEject, 90, 6 , 0
		.InitExitVariance 0,2
		.InitSounds "Saucer_Enter_2", SoundFX("Saucer_Kick",DOFContactors), SoundFX("Saucer_Kick",DOFContactors)
	End With 

	' *** subway kicker ***
	Set bsSubWay = New cvpmTrough
    With bsSubWay
		.size = 2
		.initSwitches Array(swSubPlfdShooter1,swSubPlfdShooter2)
        .Initexit SubWaypopper, 0, 100
        .InitEntrySounds "WireRamp_Hit", "", ""
        .InitExitSounds SoundFX("fx_Solenoid",DOFContactors), SoundFX("BallRelease",DOFContactors)
        .Balls = 0
		.EntrySw = 0
		.CreateEvents "bsSubWay", SubWaypopper
    End With
	
	' *** workaround: on some setups, the GI is not by default on ***
	SolGIRelay(False)

	InitDropTargets

	ColorGradeFlash()

End Sub

Sub InitDropTargets
	DTRaise swHotdog
    DTRaise swBurger
    DTRaise swChili
    DTRaise swRootBeer
    DTRaise swFries
    DTRaise swIcedTea
End Sub

'*** change insert glow appearance ***

Sub ChangeGlow(day)
	Dim Light
	If day Then
		For Each Light in GlowLights : Light.IntensityScale = GlowAmountDay: Light.FadeSpeedUp = Light.Intensity * GlowAmountDay / 2: Light.FadeSpeedDown = Light.FadeSpeedUp / 25: Next
		For Each Light in InsertLights : Light.IntensityScale = InsertBrightnessDay : Light.FadeSpeedUp = Light.Intensity * InsertBrightnessDay / 2 : Light.FadeSpeedDown = Light.FadeSpeedUp / 25 : Next
		For Each Light in glowlightscutout : Light.IntensityScale = GlowAmountCutoutDay : Light.FadeSpeedUp = Light.Intensity /  2 : Light.FadeSpeedDown = Light.FadeSpeedUp / 25 : Next
		For Each Light in insertlightscutout : Light.IntensityScale = InsertBrightnessCutoutDay : Light.FadeSpeedUp = Light.Intensity / 2 : Light.FadeSpeedDown = Light.FadeSpeedUp / 25: Next	
		For Each Light in eaters: Light.FadeSpeedUp = 5000 : Light.FadeSpeedDown = 5000 : Next
	Else
		For Each Light in GlowLights : Light.IntensityScale = GlowAmountNight: Light.FadeSpeedUp = Light.Intensity * GlowAmountNight / 2: Light.FadeSpeedDown = Light.FadeSpeedUp / 25: Next
		For Each Light in InsertLights : Light.IntensityScale = InsertBrightnessNight : Light.FadeSpeedUp = Light.Intensity * InsertBrightnessNight / 2 : Light.FadeSpeedDown = Light.FadeSpeedUp / 25 : Next
		For Each Light in glowlightscutout : Light.IntensityScale = GlowAmountCutoutNight : Light.FadeSpeedUp = Light.Intensity /  2 : Light.FadeSpeedDown = Light.FadeSpeedUp / 25 : Next
		For Each Light in insertlightscutout : Light.IntensityScale = InsertBrightnessCutoutNight : Light.FadeSpeedUp = Light.Intensity / 2 : Light.FadeSpeedDown = Light.FadeSpeedUp / 25: Next	
		For Each Light in eaters: Light.FadeSpeedUp = 5000 : Light.FadeSpeedDown = 5000 : Next
	End If
End Sub

'*** change bat appearance ***

Sub ChangeBats(Bats)
	Select Case Bats
		Case 0
			glowbatleft.visible = 0 : glowbatright.visible = 0 : GlowBatLightLeft.visible = 0 : GlowBatLightRight.visible = 0 
			batleft.visible = 0 : batright.visible = 0 : LeftFlipper.visible = 1 : RightFlipper.visible = 1
			batleftshadow.visible = 0 : batrightshadow.visible = 0 : GraphicsTimer.enabled = False
		Case 1
			glowbatleft.visible = 0 : glowbatright.visible = 0 : GlowBatLightLeft.visible = 0 : GlowBatLightRight.visible = 0 
			batleft.visible = 1 : batright.visible = 1 : LeftFlipper.visible = 0 : RightFlipper.visible = 0
			batleftshadow.visible = 1 : batrightshadow.visible = 1 : GraphicsTimer.enabled = True
		Case 2
			glowbatleft.visible = 1 : glowbatright.visible = 1 : GlowBatLightLeft.visible = 1 : GlowBatLightRight.visible = 1 
			batleft.visible = 0 : batright.visible = 0 : LeftFlipper.visible = 0 : RightFlipper.visible = 0
			glowbatleft.image = "glowbat green" : glowbatright.image = "glowbat green" 
			GlowBatLightLeft.color = RGB(0,255,0) : GlowBatLightRight.color = RGB(0,255,0) 
			batleftshadow.visible = 0 : batrightshadow.visible = 0 : GraphicsTimer.enabled = True
		Case 3
			glowbatleft.visible = 1 : glowbatright.visible = 1 : GlowBatLightLeft.visible = 1 : GlowBatLightRight.visible = 1 
			batleft.visible = 0 : batright.visible = 0 : LeftFlipper.visible = 0 : RightFlipper.visible = 0
			glowbatleft.image = "glowbat blue" : glowbatright.image = "glowbat blue" 
			GlowBatLightLeft.color = RGB(0,0,255) : GlowBatLightRight.color = RGB(0,0,255) 
			batleftshadow.visible = 0 : batrightshadow.visible = 0 : GraphicsTimer.enabled = True
		Case 4
			glowbatleft.visible = 1 : glowbatright.visible = 1 : GlowBatLightLeft.visible = 1 : GlowBatLightRight.visible = 1 
			batleft.visible = 0 : batright.visible = 0 : LeftFlipper.visible = 0 : RightFlipper.visible = 0
			glowbatleft.image = "glowbat orange" : glowbatright.image = "glowbat orange"
			GlowBatLightLeft.color = RGB(255,0,0) : GlowBatLightRight.color = RGB(255,0,0) 
			batleftshadow.visible = 0 : batrightshadow.visible = 0 : GraphicsTimer.enabled = True
	End Select
End Sub

'*** change ball appearance ***

Sub ChangeBall(ballnr)
	Dim BOT, ii, col
	Diner.BallDecalMode = CustomBallLogoMode(ballnr)
	Diner.BallFrontDecal = CustomBallDecal(ballnr)
	Diner.DefaultBulbIntensityScale = CustomBulbIntensity(ballnr)
	Diner.BallImage = CustomBallImage(ballnr)
	GlowBall = CustomBallGlow(ballnr)
	For ii = 0 to 2
		col = RGB(red(ballnr), green(ballnr), Blue(ballnr))
		Glowing(ii).color = col : Glowing(ii).colorfull = col 
	Next
End Sub

'************************************************************
'*         Drop Targets
'************************************************************

'Targets Left
Sub rootbeer_hit
	DTHit swRootBeer
	TargetBouncer Activeball, 1
	If VRRoom > 0 Then
		VRInsertSmallMove 35, 35, 35, 35, 35, 35
	End If
End Sub
Sub fries_hit
	DTHit swFries
	TargetBouncer Activeball, 1
	If VRRoom > 0 Then
		VRInsertSmallMove 40, 40, 40, 40, 40, 40
	End If
End Sub
Sub icedtea_hit
	DTHit swIcedTea
	TargetBouncer Activeball, 1
	If VRRoom > 0 Then
		VRInsertSmallMove 35, 35, 35, 35, 35, 35
	End If
End Sub

' Targets Middle
Sub hotdog_hit
	DTHit swHotdog
	TargetBouncer Activeball, 1
	If VRRoom > 0 Then
		VRInsertSmallMove 35, 35, 35, 35, 35, 35
	End If
End Sub
Sub burger_hit
	DTHit swBurger
	TargetBouncer Activeball, 1
	If VRRoom > 0 Then
		VRInsertSmallMove 40, 40, 40, 40, 40, 40
	End If
End Sub
Sub chili_hit
	DTHit swChili
	TargetBouncer Activeball, 1
	If VRRoom > 0 Then
		VRInsertSmallMove 35, 35, 35, 35, 35, 35
	End If
End Sub

Sub SolDTBankLeft(enabled)
	if enabled then
		RandomSoundDropTargetReset rootbeer_p
		DTRaise swRootBeer
		DTRaise swFries
		DTRaise swIcedTea
	end if
End Sub

Sub SolDTBankMiddle(enabled)
	if enabled then
		RandomSoundDropTargetReset hotdog_p
		DTRaise swHotdog
		DTRaise swBurger
		DTRaise swChili
	end if
End Sub

' *** various hit event handling ***

Sub ShooterLane_Hit(): B2SSTAT: Controller.Switch(swShooterLane)=1:End Sub
Sub ShooterLane_UnHit():Controller.Switch(swShooterLane)=0:End Sub

Sub LeftJetBumper_Hit()
	B2SSTAT
	vpmTimer.PulseSwitch (swLeftJetBumper), 0, ""
	RandomSoundBumperTop LeftJetBumper
	If VRRoom > 0 Then
		VRInsertMove 20, 20, 20, 20, 20, 20
	End If
End Sub
Sub RightJetBumper_Hit()
	B2SSTAT
	vpmTimer.PulseSwitch (swRightJetBumper), 0, ""
	RandomSoundBumperMiddle RightJetBumper
	If VRRoom > 0 Then
		VRInsertMove 20, 20, 20, 20, 20, 20
	End If
End Sub
Sub LowerJetBumper_Hit()
	B2SSTAT
	vpmTimer.PulseSwitch (swLowerJetBumper), 0, ""
	RandomSoundBumperBottom LowerJetBumper
	If VRRoom > 0 Then
		VRInsertMove 20, 20, 20, 20, 20, 20
	End If
End Sub

Sub LeftReturnLane_Hit():Controller.Switch(swLeftReturnLane)=1:End Sub
Sub LeftReturnLane_UnHit():Controller.Switch(swLeftReturnLane)=0:End Sub
Sub RightReturnLane_Hit():Controller.Switch(swRightReturnLane)=1:End Sub
Sub RightReturnLane_UnHit():Controller.Switch(swRightReturnLane)=0:End Sub
Sub LeftOutlane_Hit():Controller.Switch(swLeftOutLane)=1:End Sub
Sub LeftOutlane_UnHit():Controller.Switch(swLeftOutLane)=0:End Sub
Sub RightOutlane_Hit():Controller.Switch(swRightOutlane)=1:End Sub
Sub RightOutlane_UnHit():Controller.Switch(swRightOutlane)=0:End Sub
Sub E_Hit():B2SSTAT : Controller.Switch(swE)=1:End Sub
Sub E_UnHit():Controller.Switch(swE)=0:End Sub
Sub Asw_Hit():B2SSTAT :Controller.Switch(swA)=1:End Sub
Sub Asw_UnHit():Controller.Switch(swA)=0:End Sub
Sub T_Hit():B2SSTAT :Controller.Switch(swT)=1:End Sub
Sub T_UnHit():Controller.Switch(swT)=0:End Sub
Sub grillbonus_Hit()
	VpmTimer.PulseSw swGrillBonus
	If VRRoom > 0 Then
		VRInsertMove 30, 30, 30, 30, 30, 30
	End If
End Sub
Sub Spinner_Spin():vpmTimer.PulseSwitch(swSpinner),0,"": SoundSpinner Spinner : End Sub
Sub RightRampEntry_Hit() : Controller.Switch(swRightRampEntry)=1:End Sub
Sub RightRampEntry_UnHit():Controller.Switch(swRightRampEntry)=0:End Sub
Sub LeftRampExit_Hit():Controller.Switch(swLeftRampExit)=1:End Sub
Sub LeftRampExit_UnHit():Controller.Switch(swLeftRampExit)=0:End Sub
Sub RightRampExit_Hit():Controller.Switch(swRightRampExit)=1:End Sub
Sub RightRampExit_UnHit():Controller.Switch(swRightRampExit)=0:End Sub
Sub UpperEject_Hit() : bsUpperEject.AddBall Me : SoundSaucerLock : End Sub
Sub UpperEject_UnHit: SoundSaucerKick 1, UpperEject : End Sub
Sub LowKicker_Hit() : bsLowKicker.AddBall Me : End Sub
Sub subwayenter_Hit() : SoundSaucerLock : End Sub
Sub subwayenter_UnHit() : SoundSaucerKick 1, subwayenter : End Sub
Sub Outhole_Hit() : vpmTimer.PulseSwitch(swOutHole), 100, "HandleOutHole" : RandomSoundDrain OutHole : End Sub
Sub HandleOutHole(swNo) : bsTrough.AddBall Outhole : End Sub
Sub TiltSol(Enabled) : FlippersEnabled = Enabled : SolLFlipper(False) : SolRFlipper(False) : End Sub
Sub UpdateClockHand(aNewPos, aSpeed, aLastPos)
	clockwijzer.ObjrotY = anewpos * 6 : DOF 101, DOFPulse
	If VRRoom > 0 Then
		VRClockHand.RotZ = anewpos * 6
	End If
end Sub
Sub SolUpperEject(enabled) : if enabled then bsUpperEject.SolOut Enabled : end if : end sub 'Solout: Fire the exit kicker and ejects ball if one is present.
Sub SolLowKicker(enabled) : if enabled then bsLowKicker.SolOut Enabled : end if : end sub 'Solout: Fire the exit kicker and ejects ball if one is present.
Sub BallRelease_UnHit : RandomSoundBallRelease BallRelease : End Sub
'*** Cup handling ***

dim CupEntrySpeed

Dim   CupSpeedValue : CupSpeedValue = 3
Const RandomCupSpeed = 1            ' 0 = Disabled, 1 = Enabled : Use a random threshold number between the min/max vlues of RandomCupSpeedMin and RandomCupSpeedMax
Const RandomCupSpeedMin = 2.5
Const RandomCupSpeedMax = 4.5


Sub CupEntry_Hit()
	vpmTimer.PulseSw swCupEntry
	'Stopsound "plasticrolling"
	If RandomCupSpeed Then
		CupSpeedValue = ((RandomCupSpeedMax - RandomCupSpeedMin) * Rnd + RandomCupSpeedMin)
	End If
	'Debug.Print "CupSpeedValue = " & CupSpeedValue
	CupExit.Enabled = 1 : CupCounter = 25 : cupblocker.collidable = False 
	ActiveBall.VelZ = 0
	CupEntrySpeed =  sqr(ActiveBall.VelX ^2 + ActiveBall.VelY ^2) * 1.25
	'Debug.print "Cup Entry Speed = " & CupEntrySpeed

End Sub

Sub Cup_Hit() : Cupspeeder : vpmTimer.PulseSw swCup : cupblocker.collidable = True : End Sub
Sub Cup2_Hit() : Cupspeeder : end sub
Sub Cup_UnHit() : Cupspeeder : end sub
Sub Cup2_UnHit() : Cupspeeder : end sub

Sub Cupspeeder()
	'Debug.print "1 = VelY = " & abs(ActiveBall.VelY) & " Velx = " & abs(ActiveBall.Velx)
	'Debug.print "rand value : " & Int((4-2+1)*Rnd+2)
	If abs(ActiveBall.VelY) > CupSpeedValue And abs(ActiveBall.VelX) > CupSpeedValue Then
		'Debug.print "2 = VelY = " & abs(ActiveBall.VelY) & " Velx = " & abs(ActiveBall.Velx)
		Dim BallSpeed, BallSpeedReduction
		BallSpeed = sqr(ActiveBall.VelX ^2 + ActiveBall.VelY ^2)
		BallSpeedReduction = (CupEntrySpeed / BallSpeed) * 30 / ( sqr(2) * CupCounter )
		ActiveBall.VelY =  ActiveBall.VelY * BallSpeedReduction
		ActiveBall.VelX =  ActiveBall.VelX * BallSpeedReduction
		'Debug.print "3 = VelY = " & abs(ActiveBall.VelY) & " Velx = " & abs(ActiveBall.Velx)
		cupcounter = cupcounter + 1
	End If
End Sub

'*** Lockramp handling ***

Sub SolRampDown(Enabled)
	If ramp1.collidable Then
		Controller.Switch(swUpDownRamp)=1
	Else
		ramp1.collidable = True : LRAindex = 6 : lockramptimer.enabled = True
	End If
End Sub

Sub SolRampUp(Enabled)
	If Ramp1.collidable Then
		ramp1.collidable = False : lockramptimer.enabled = True : LRAindex = 0
	Else
		Controller.Switch(swUpDownRamp)=0
	End If
End Sub

Sub lockramptimer_timer
	If ramp1.collidable Then
		lockramp.ObjRotX = LRA(LRAindex) : LRAindex = LRAindex - 1
		If LRAindex < 0 then LRAindex = 0 : lockramptimer.enabled = False : Controller.Switch(swUpDownRamp)=1 : playsoundAt "TrapDoorLow", lockramp : end If
	Else
		lockramp.ObjRotX = LRA(LRAindex) : LRAindex = LRAindex + 1
		If LRAindex > 6 then LRAindex = 6 : lockramptimer.enabled = False : Controller.Switch(swUpDownRamp)=0 : playsoundat "TrapDoorHigh", lockramp : end If
	End If
end Sub

'*** diverter handling ***

Sub Sol14Diverter(Enabled)
	if Enabled then
		Sol14Closed.collidable = False : DVdir = 1 : divertertimer.enabled = True : playsoundat "TopDiverterOn", Sol14Closed_Snd 'playsound SoundFX("TopDiverterOn",DOFContactors) 
	else
		Sol14Closed.collidable = True : Dvdir = -1 : divertertimer.enabled = True : playsoundAt "TopDiverterOff", Sol14Closed_Snd 'playsound SoundFX("TopDiverterOff",DOFContactors) 
	end if
End Sub

Sub divertertimer_timer
	If DVdir = 0 Then
		divertertimer.enabled = False 
	Else
		DVindex = DVindex + DVdir
		If DVindex < 0 Then DVindex = 0 : DVdir = 0 : end If
		IF DVindex > 6 Then Dvindex = 6:  DVdir = 0 : end If
		divider179to201.RotY = DV(DVindex)
	End If
end Sub

' *** drophole sub playfield shooter ***

Sub SolSubPlfdShooter(enabled)
    If enabled and bsSubWay.balls > 0 Then
		bsSubWay.ExitSol_On : rightflap.RotX = 80 : rightflaptimer.enabled = True
	End If
End Sub

Sub rightflaptimer_timer : rightflap.RotX = 90 : rightflaptimer.enabled = False : end Sub

' *** character flasher handling ***

dim FlashTargetLevel25, FlashLevel25, FlashTargetLevel26, FlashLevel26, FlashTargetLevel27, FlashLevel27, FlashTargetLevel28, FlashLevel28, FlashTargetLevel29, FlashLevel29

Sub solHajiF(enabled)
	FlashCutout 33,enabled
	if Enabled then
		FlashTargetLevel25 = 1
	Else
		FlashTargetLevel25 = 0
	end if
	F25_Timer
End Sub

Sub solBabsF(enabled)
	FlashCutout 34,enabled
	if Enabled then
		FlashTargetLevel26 = 1
	Else
		FlashTargetLevel26 = 0
	end if
	F26_Timer	
End Sub

Sub solBorisF(enabled)
	FlashCutout 35,enabled
	if Enabled then
		FlashTargetLevel27 = 1
	Else
		FlashTargetLevel27 = 0
	end if
	F27_Timer
End Sub

Sub solPepeF(enabled)
	FlashCutout 36,enabled
	if Enabled then
		FlashTargetLevel28 = 1
	Else
		FlashTargetLevel28 = 0
	end if
	F28_Timer
End Sub

Sub solBuckF(enabled)
	FlashCutout 37,enabled
	if Enabled then
		FlashTargetLevel29 = 1
	Else
		FlashTargetLevel29 = 0
	end if
	F29_Timer
End Sub

Sub FlashCutout(nr,enabled)
	Dim GlowCutout, InsertCutout
	If ContrastSetting=0 or ContrastSetting=2 or ContrastSetting=4 or ContrastSetting=6 Then
		GlowCutout = GlowAmountCutoutDay : InsertCutout = InsertBrightnessCutoutDay
	Else
		GlowCutout = GlowAmountCutoutNight : InsertCutout = InsertBrightnessCutoutNight
	End If
	if enabled then
		LightObjectFirst(nr).IntensityScale = GlowCutout * 4 : LightObjectFirst(nr).state = LightStateOn 
		LightObjectSecond(nr).IntensityScale = InsertCutout * 12 : LightObjectSecond(nr).state = LightStateOn
	Else
		LightObjectFirst(nr).IntensityScale = GlowCutout : LightObjectFirst(nr).state = LightState(nr)
		LightObjectSecond(nr).IntensityScale = InsertCutout : LightObjectSecond(nr).state = LightState(nr)
	End If
End Sub

' *** character backglass flasher handling ***

'Haji
sub F25_Timer()
	If not F25.Enabled Then 
		F25.Enabled = True
		If VRRoom > 0 Then
			VRBGFL25_1.visible = true
			VRBGFL25_2.visible = true
			VRBGFL25_3.visible = true
			VRBGFL25_4.visible = true
		End If
	End If

	If VRRoom > 0 Then
		VRBGFL25_1.opacity = 35 * FlashLevel25^2
		VRBGFL25_2.opacity = 35 * FlashLevel25^2
		VRBGFL25_3.opacity = 35 * FlashLevel25^2
		VRBGFL25_4.opacity = 90 * FlashLevel25^3
	End If
	
	if round(FlashTargetLevel25,1) > round(FlashLevel25,1) Then
		FlashLevel25 = FlashLevel25 + 0.3
		if FlashLevel25 > 1 then FlashLevel25 = 1
	Elseif round(FlashTargetLevel25,1) < round(FlashLevel25,1) Then
		FlashLevel25 = FlashLevel25 * 0.8 - 0.01
		if FlashLevel25 < 0 then FlashLevel25 = 0
	Else
		FlashLevel25 = round(FlashTargetLevel25,1)
		F25.Enabled = False
	end if

	If FlashLevel25 <= 0 Then
		F25.Enabled = False
		If VRRoom > 0 Then
			VRBGFL25_1.visible = False
			VRBGFL25_2.visible = False
			VRBGFL25_3.visible = False
			VRBGFL25_4.visible = False
		End If
	End If
end sub

'Babs
sub F26_Timer()
	If not F26.Enabled Then 
		F26.Enabled = True
		If VRRoom > 0 Then
			VRBGFL26_1.visible = true
			VRBGFL26_2.visible = true
			VRBGFL26_3.visible = true
			VRBGFL26_4.visible = true
		End If
	End If

	If VRRoom > 0 Then
		VRBGFL26_1.opacity = 35 * FlashLevel26^2
		VRBGFL26_2.opacity = 35 * FlashLevel26^2
		VRBGFL26_3.opacity = 35 * FlashLevel26^2
		VRBGFL26_4.opacity = 90 * FlashLevel26^3
	End If
	
	if round(FlashTargetLevel26,1) > round(FlashLevel26,1) Then
		FlashLevel26 = FlashLevel26 + 0.3
		if FlashLevel26 > 1 then FlashLevel26 = 1
	Elseif round(FlashTargetLevel26,1) < round(FlashLevel26,1) Then
		FlashLevel26 = FlashLevel26 * 0.8 - 0.01
		if FlashLevel26 < 0 then FlashLevel26 = 0
	Else
		FlashLevel26 = round(FlashTargetLevel26,1)
		F26.Enabled = False
	end if

	If FlashLevel26 <= 0 Then
		F26.Enabled = False
		If VRRoom > 0 Then
			VRBGFL26_1.visible = False
			VRBGFL26_2.visible = False
			VRBGFL26_3.visible = False
			VRBGFL26_4.visible = False
		End If
	End If
end sub

'Boris
sub F27_Timer()
	If not F27.Enabled Then 
		F27.Enabled = True
		If VRRoom > 0 Then
			VRBGFL27_1.visible = true
			VRBGFL27_2.visible = true
			VRBGFL27_3.visible = true
			VRBGFL27_4.visible = true
		End If
	End If

	If VRRoom > 0 Then
		VRBGFL27_1.opacity = 35 * FlashLevel27^2
		VRBGFL27_2.opacity = 35 * FlashLevel27^2
		VRBGFL27_3.opacity = 35 * FlashLevel27^2
		VRBGFL27_4.opacity = 90 * FlashLevel27^3
	End If
	
	if round(FlashTargetLevel27,1) > round(FlashLevel27,1) Then
		FlashLevel27 = FlashLevel27 + 0.3
		if FlashLevel27 > 1 then FlashLevel27 = 1
	Elseif round(FlashTargetLevel27,1) < round(FlashLevel27,1) Then
		FlashLevel27 = FlashLevel27 * 0.8 - 0.01
		if FlashLevel27 < 0 then FlashLevel27 = 0
	Else
		FlashLevel27 = round(FlashTargetLevel27,1)
		F27.Enabled = False
	end if

	If FlashLevel27 <= 0 Then
		F27.Enabled = False
		If VRRoom > 0 Then
			VRBGFL27_1.visible = False
			VRBGFL27_2.visible = False
			VRBGFL27_3.visible = False
			VRBGFL27_4.visible = False
		End If
	End If
end sub

'Pepe
sub F28_Timer()
	If not F28.Enabled Then 
		F28.Enabled = True
		If VRRoom > 0 Then
			VRBGFL28_1.visible = true
			VRBGFL28_2.visible = true
			VRBGFL28_3.visible = true
			VRBGFL28_4.visible = true
		End If
	End If

	If VRRoom > 0 Then
		VRBGFL28_1.opacity = 35 * FlashLevel28^2
		VRBGFL28_2.opacity = 35 * FlashLevel28^2
		VRBGFL28_3.opacity = 35 * FlashLevel28^2
		VRBGFL28_4.opacity = 90 * FlashLevel28^3
	End If
	
	if round(FlashTargetLevel28,1) > round(FlashLevel28,1) Then
		FlashLevel28 = FlashLevel28 + 0.3
		if FlashLevel28 > 1 then FlashLevel28 = 1
	Elseif round(FlashTargetLevel28,1) < round(FlashLevel28,1) Then
		FlashLevel28 = FlashLevel28 * 0.8 - 0.01
		if FlashLevel28 < 0 then FlashLevel28 = 0
	Else
		FlashLevel28 = round(FlashTargetLevel28,1)
		F28.Enabled = False
	end if

	If FlashLevel28 <= 0 Then
		F28.Enabled = False
		If VRRoom > 0 Then
			VRBGFL28_1.visible = False
			VRBGFL28_2.visible = False
			VRBGFL28_3.visible = False
			VRBGFL28_4.visible = False
		End If
	End If
end sub

'Buck
sub F29_Timer()
	If not F29.Enabled Then 
		F29.Enabled = True
		If VRRoom > 0 Then
			VRBGFL29_1.visible = true
			VRBGFL29_2.visible = true
			VRBGFL29_3.visible = true
			VRBGFL29_4.visible = true
		End If
	End If

	If VRRoom > 0 Then
		VRBGFL29_1.opacity = 35 * FlashLevel29^2
		VRBGFL29_2.opacity = 35 * FlashLevel29^2
		VRBGFL29_3.opacity = 35 * FlashLevel29^2
		VRBGFL29_4.opacity = 90 * FlashLevel29^3
	End If
	
	if round(FlashTargetLevel29,1) > round(FlashLevel29,1) Then
		FlashLevel29 = FlashLevel29 + 0.3
		if FlashLevel29 > 1 then FlashLevel29 = 1
	Elseif round(FlashTargetLevel29,1) < round(FlashLevel29,1) Then
		FlashLevel29 = FlashLevel29 * 0.8 - 0.01
		if FlashLevel29 < 0 then FlashLevel29 = 0
	Else
		FlashLevel29 = round(FlashTargetLevel29,1)
		F29.Enabled = False
	end if

	If FlashLevel29 <= 0 Then
		F29.Enabled = False
		If VRRoom > 0 Then
			VRBGFL29_1.visible = False
			VRBGFL29_2.visible = False
			VRBGFL29_3.visible = False
			VRBGFL29_4.visible = False
		End If
	End If
end sub



' *** cup Flasher ***

Sub solCupF(enabled)
    if enabled then
		PlaySound "fx_relay_on",0,relaylevel : StopSound "fx_relay_off" 
		Flasher5light.state = LightStateOn : Flasher5lightsec.state = LightStateOn
		'ObjTargetLevel(5) = 1
		'FlasherFlash5_Timer
	Else
		PlaySound "fx_relay_off",0,relaylevel : StopSound "fx_relay_on" 
		Flasher5light.state = LightStateOff : Flasher5lightsec.state = LightStateOff
		'ObjTargetLevel(5) = 0
	End If
End Sub

' *** Rightrampflasher ***

Sub SolFlash9(Enabled)
	Rightflash = enabled
	if enabled then
		PlaySound "fx_relay_on",0,relaylevel : StopSound "fx_relay_off"
		ObjTargetLevel(1) = 1
		ObjTargetLevel(2) = 1
		FlasherFlash1_Timer
		FlasherFlash2_Timer
		If Leftflash Then
			sidewallFS.image = "sidewallsFSbothflash" : backwall.image = "backwallbothflash" : sidewallDT.image = "sidewallsFSbothflash"
			sidewallFS.material = "sidewallsbothflash" : sidewallDT.material = "sidewallsbothflash" 
			ColorGradeFlash()
		Else
			sidewallFS.image = "sidewallsFSrightflash" : backwall.image = "backwallrightflash" : sidewallDT.image = "sidewallsFSrightflash"
			sidewallFS.material = "sidewallsrightflash" : sidewallDT.material = "sidewallsrightflash" 
			ColorGradeFlash()
		End If	
	Else
		PlaySound "fx_relay_off",0,relaylevel : StopSound "fx_relay_on"
		ObjTargetLevel(1) = 0
		ObjTargetLevel(2) = 0
		If Leftflash Then
			sidewallFS.image = "sidewallsFSleftflash" : backwall.image = "backwallleftflash" : sidewallDT.image = "sidewallsFSleftflash"
			sidewallFS.material = "sidewallsleftflash" : sidewallDT.material = "sidewallsleftflash" 
			ColorGradeFlash()
		Else
			sidewallFS.image = "sidewallsFS" : backwall.image = "backwallnoflash" : sidewallDT.image = "sidewallsFS"
			sidewallFS.material = "sidewallsnoflash" : sidewallDT.material = "sidewallsnoflash" 
			ColorGradeFlash()
		End If
		
	End If
End sub



Dim FlashStepRight

Sub FlashTimerRight_timer()
	Flasher1.image = "flasherfade" & FlashStepRight
	Flasher2.image = "flasherfade" & FlashStepRight
	FlashStepRight = FlashStepRight + 1
	If FlashStepRight = 6 Then
		Flasher1.image = "domelightoff" : Flasher2.image = "domelightoff"
		Flasher1.disablelighting = False : Flasher2.disablelighting = False
		FlashTimerRight.enabled = False
	End If
End Sub

' *** Leftrampflasher ***

Sub SolFlash11(Enabled)
	Leftflash = enabled
	if enabled then
		PlaySound "fx_relay_on",0,relaylevel : StopSound "fx_relay_off"
		ObjTargetLevel(3) = 1
		ObjTargetLevel(4) = 1
		FlasherFlash3_Timer
		FlasherFlash4_Timer
		If Rightflash Then
			sidewallFS.image = "sidewallsFSbothflash" : backwall.image = "backwallbothflash" : sidewallDT.image = "sidewallsFSbothflash"
			sidewallFS.material = "sidewallsbothflash" : sidewallDT.material = "sidewallsbothflash" 
			ColorGradeFlash()
		Else
			sidewallFS.image = "sidewallsFSleftflash" : backwall.image = "backwallleftflash" : sidewallDT.image = "sidewallsFSleftflash"
			sidewallFS.material = "sidewallsleftflash" : sidewallDT.material = "sidewallsleftflash" 
			ColorGradeFlash()
		End If
	Else
		PlaySound "fx_relay_off",0,relaylevel : StopSound "fx_relay_on"
		ObjTargetLevel(3) = 0
		ObjTargetLevel(4) = 0
		If Rightflash Then
			sidewallFS.image = "sidewallsFSrightflash" : backwall.image = "backwallrightflash" : sidewallDT.image = "sidewallsFSrightflash"
			sidewallFS.material = "sidewallsrightflash" : sidewallDT.material = "sidewallsrightflash" 
			ColorGradeFlash()
		Else
			sidewallFS.image = "sidewallsFS" : backwall.image = "backwallnoflash" : sidewallDT.image = "sidewallsFS"
			sidewallFS.material = "sidewallsnoflash" : sidewallDT.material = "sidewallsnoflash" 
			ColorGradeFlash()
		End If
	End If
End sub


Dim FlashStepLeft

Sub FlashTimerLeft_timer()
	Flasher3.image = "flasherfade" & FlashStepLeft
	Flasher4.image = "flasherfade" & FlashStepLeft
	FlashStepLeft = FlashStepLeft + 1
	If FlashStepLeft = 6 Then
		Flasher3.image = "domelightoff" : Flasher4.image = "domeshadowoff"
		Flasher3.disablelighting = False : Flasher4.disablelighting = False
		FlashTimerLeft.enabled = False
	End If
End Sub

' *** flashes the whole screen by changing colorgrade LUT ***

Sub ColorGradeFlash()
	Dim lutlevel, ContrastLut
	If VRRoom = 0 Then
		Lutlevel = 0
		If Leftflash Then Lutlevel = lutlevel + 1 End If
		If Rightflash Then Lutlevel = lutlevel + 1 End If
		If ContrastSetting=0 or ContrastSetting=2 or ContrastSetting=4 or ContrastSetting=6 Then
			ContrastLut = ContrastSetting / 2
		Else
			ContrastLut = ContrastSetting / 2 - 0.5
		End If
		diner.ColorGradeImage = "LUT" & ContrastLut & "_" & lutlevel
	End If
End Sub


' *** VR Backglass flashers ***

dim FlashLevel31, FlashTargetLevel31, FlashLevel32, FlashTargetLevel32

'Clock Flasher
Sub sol31(enabled)
	If VRRoom > 0 Then
		if Enabled then
			FlashTargetLevel31 = 1
		Else
			FlashTargetLevel31 = 0
		end if
		F31_Timer
	End If
End Sub

sub F31_Timer()
	If not F31.Enabled Then 
		F31.Enabled = True
		If VRRoom > 0 Then
			VRBGFL31_1.visible = true
			VRBGFL31_2.visible = true
			VRBGFL31_3.visible = true
			VRBGFL31_4.visible = true
			VRBGFL31_5.visible = true
			VRBGFL31_6.visible = true
			VRBGFL31_7.visible = true
			VRBGFL31_8.visible = true
		End If
	End If

	If VRRoom > 0 Then
		VRBGFL31_1.opacity = 100 * FlashLevel31^2
		VRBGFL31_2.opacity = 100 * FlashLevel31^2
		VRBGFL31_3.opacity = 100 * FlashLevel31^2
		VRBGFL31_4.opacity = 85 * FlashLevel31^3
		VRBGFL31_5.opacity = 100 * FlashLevel31^2
		VRBGFL31_6.opacity = 100 * FlashLevel31^2
		VRBGFL31_7.opacity = 100 * FlashLevel31^2
		VRBGFL31_8.opacity = 85 * FlashLevel31^3
	End If
	
	if round(FlashTargetLevel31,1) > round(FlashLevel31,1) Then
		FlashLevel31 = FlashLevel31 + 0.3
		if FlashLevel31 > 1 then FlashLevel31 = 1
	Elseif round(FlashTargetLevel31,1) < round(FlashLevel31,1) Then
		FlashLevel31 = FlashLevel31 * 0.85 - 0.01
		if FlashLevel31 < 0 then FlashLevel31 = 0
	Else
		FlashLevel31 = round(FlashTargetLevel31,1)
		F31.Enabled = False
	end if

	If FlashLevel31 <= 0 Then
		F31.Enabled = False
		If VRRoom > 0 Then
			VRBGFL31_1.visible = False
			VRBGFL31_2.visible = False
			VRBGFL31_3.visible = False
			VRBGFL31_4.visible = False
			VRBGFL31_5.visible = False
			VRBGFL31_6.visible = False
			VRBGFL31_7.visible = False
			VRBGFL31_8.visible = False
		End If
	End If
end sub

'Dine Time Flasher
Sub sol32(enabled)
	If VRRoom > 0 Then
		if Enabled then
			FlashTargetLevel32 = 1
		Else
			FlashTargetLevel32 = 0
		end if
		F32_Timer
	End If
End Sub

sub F32_Timer()
	If not F32.Enabled Then 
		F32.Enabled = True
		If VRRoom > 0 Then
			VRBGFL32_1.visible = true
			VRBGFL32_2.visible = true
			VRBGFL32_3.visible = true
			VRBGFL32_4.visible = true
			VRBGFL32_5.visible = true
			VRBGFL32_6.visible = true
			VRBGFL32_7.visible = true
			VRBGFL32_8.visible = true
		End If
	End If

	If VRRoom > 0 Then
		VRBGFL32_1.opacity = 100 * FlashLevel32^2
		VRBGFL32_2.opacity = 100 * FlashLevel32^2
		VRBGFL32_3.opacity = 100 * FlashLevel32^2
		VRBGFL32_4.opacity = 100 * FlashLevel32^3
		VRBGFL32_5.opacity = 100 * FlashLevel32^2
		VRBGFL32_6.opacity = 100 * FlashLevel32^2
		VRBGFL32_7.opacity = 100 * FlashLevel32^2
		VRBGFL32_8.opacity = 100 * FlashLevel32^3
	End If
	
	if round(FlashTargetLevel32,1) > round(FlashLevel32,1) Then
		FlashLevel32 = FlashLevel32 + 0.3
		if FlashLevel32 > 1 then FlashLevel32 = 1
	Elseif round(FlashTargetLevel32,1) < round(FlashLevel32,1) Then
		FlashLevel32 = FlashLevel32 * 0.85 - 0.01
		if FlashLevel32 < 0 then FlashLevel32 = 0
	Else
		FlashLevel32 = round(FlashTargetLevel32,1)
		F32.Enabled = False
	end if

	If FlashLevel32 <= 0 Then
		F32.Enabled = False
		If VRRoom > 0 Then
			VRBGFL32_1.visible = False
			VRBGFL32_2.visible = False
			VRBGFL32_3.visible = False
			VRBGFL32_4.visible = False
			VRBGFL32_5.visible = False
			VRBGFL32_6.visible = False
			VRBGFL32_7.visible = False
			VRBGFL32_8.visible = False
		End If
	End If
end sub


'****************************************************************
'  GI
'****************************************************************
dim gilvl:gilvl = 1
Sub ToggleGI(Enabled)
	dim xx
	If enabled Then
		for each xx in GI:xx.state = 1:Next
		gilvl = 1
	Else
		for each xx in GI:xx.state = 0:Next	
		GITimer.enabled = True
		gilvl = 0
	End If
	Sound_GI_Relay enabled, LowerJetBumper
End Sub

Sub GITimer_Timer()
	me.enabled = False
	ToggleGI 1
End Sub


' *** keyboard handlers ***

Sub Diner_KeyDown(ByVal keycode)
	If keycode = RightFlipperKey Then B2SSTAT : FlipperActivate RightFlipper, RFPress End If
    If keycode = LeftFlipperKey Then B2SSTAT : FlipperActivate LeftFlipper, LFPress End If
    If keycode = LeftTiltKey Then Nudge 90, 1:SoundNudgeLeft()
    If keycode = RightTiltKey Then Nudge 270, 1:SoundNudgeRight()
    If keycode = CenterTiltKey Then Nudge 0, 1:SoundNudgeCenter()
	If keycode = RightMagnaSave Then 
		ContrastSetting = ContrastSetting + 1
		If ContrastSetting > 7 Then ContrastSetting = 7 End If
		If ContrastSetting=0 or ContrastSetting=2 or ContrastSetting=4 or ContrastSetting=6 Then ChangeGlow(True) Else ChangeGlow(False) End If
		ColorGradeFlash()
		OptionPrim.image = "contrastsetting" & ContrastSetting : OptionPrim.visible = 1 : OptionOpacity = 0 : OptionTimer.enabled = True
	End If
	If keycode = LeftMagnaSave Then 
		ContrastSetting = ContrastSetting - 1
		If ContrastSetting < 0 Then ContrastSetting = 0 End If
		If ContrastSetting=0 or ContrastSetting=2 or ContrastSetting=4 or ContrastSetting=6 Then ChangeGlow(True) Else ChangeGlow(False) End If
		ColorGradeFlash()
		OptionPrim.image = "contrastsetting" & ContrastSetting : OptionPrim.visible = 1 : OptionOpacity = 0 : OptionTimer.enabled = True
	End If
     If keycode = PlungerKey Then Plunger.Pullback : SoundPlungerPull()

		If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
                Select Case Int(rnd*3)
                        Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
                        Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
                        Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25

                End Select
        End If
	If VRRoom = 1 or VRRoom = 2 Then
		If keycode = LeftFlipperKey Then
			VR_FB_Left.x = VR_FB_Left.x + 5
		End If
		If keycode = RightFlipperKey Then
			VR_FB_Right.x = VR_FB_Right.x - 5
		End If
		If keycode = PlungerKey Then
			TimerVRPlunger.Enabled = True
			TimerVRPlunger2.Enabled = False
		End If
		If keycode = LeftTiltKey Then 
			VRInsertMove 10, 10, 10, 10, 10, 10
		End If
		If keycode = RightTiltKey Then
			VRInsertMove 10, 10, 10, 10, 10, 10
		End If
		If keycode = CenterTiltKey Then
			VRInsertMove 10, 10, 10, 10, 10, 10
		End If
	End If

      if keycode=StartGameKey then soundStartButton()


	If vpmKeyDown(keycode) Then Exit Sub
End Sub


Sub Diner_KeyUp(ByVal keycode)
     If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress End If
     If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress End If
	'if keycode = LeftFlipperKey and FlippersEnabled Then SolLFlipper(False) 
	'if keycode = RightFlipperKey and FlippersEnabled Then SolRflipper(False)
	 'If keycode = PlungerKey Then PLaySound "fx_plunger", 0, 1, 0.1, 0.25:Plunger.Fire

	If keycode = PlungerKey Then 
		B2SSTAT
		Plunger.Fire
		If controller.switch(swShooterLane) = True Then 	'If true then ball in shooter lane, else no ball is shooter lane
			'SoundPlungerPullStop()
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			'SoundPlungerPullStop()
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
		If VRRoom > 0 Then
			VRInsertSmallMove 40, 40, 40, 40, 40, 40
		End If
	End If

	If VRRoom = 1 or VRRoom = 2 Then
		If keycode = LeftFlipperKey Then
			VR_FB_Left.x = VR_FB_Left.x - 5
		End If
		If keycode = RightFlipperKey Then
			VR_FB_Right.x = VR_FB_Right.x + 5
		End If
		If keycode = PlungerKey Then
			TimerVRPlunger.Enabled = False
			TimerVRPlunger2.Enabled = True
		End If		
	End If

	 If vpmKeyUp(keycode) Then Exit Sub
End Sub

Sub Diner_Paused: Controller.Pause = 1:End Sub
Sub Diner_UnPaused: Controller.Pause = 0:End Sub
Sub Diner_Exit(): Controller.Stop:End Sub


'****************************

Sub OptionTimer_timer()
	If OptionOpacity = 8 Then
		OptionPrim.visible = 0
		OptionTimer.enabled = False
	Else
		OptionPrim.material = "PrimOption" & OptionOpacity
		OptionOpacity = OptionOpacity + 1
	End If
End Sub

'******************************************************************************************
' Sling Shot Animations: Rstep and Lstep  are the variables that increment the animation
'******************************************************************************************

Dim RStep, Lstep

Sub RSling_Slingshot
	RS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSwitch (swRSling), 0, "" : RandomSoundSlingshotRight sling1 
    RSling3.Visible = 0 : RSling1.Visible = 1 : sling1.TransZ = -20 : RStep = 0 : RSling.TimerEnabled = 1
	If VRRoom > 0 Then
		VRInsertMove 40, 40, 40, 40, 40, 40
	End If
End Sub

Sub RSling_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSling3.Visible = 1:sling1.TransZ = 0:RSling.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LSling_Slingshot
	LS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSwitch (swLSling), 0, "" : RandomSoundSlingshotLeft sling2 
    LSling3.Visible = 0 : LSling1.Visible = 1 : sling2.TransZ = -20 : LStep = 0 : LSling.TimerEnabled = 1
	If VRRoom > 0 Then
		VRInsertMove 40, 40, 40, 40, 40, 40
	End If
End Sub

Sub LSling_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSling3.Visible = 1:sling2.TransZ = 0:LSling.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub SolBallRelease(enabled)
	if enabled then
		if bsTrough.Balls then vpmTimer.PulseSwitch(swTroughEject),0,""
		bsTrough.ExitSol_On
	End if
End Sub

' *** General Illumination ***

Sub SolGIRelay(enabled)
	Dim Prim, Light
	If enabled Then
		PlaySound "fx_relay_off",0,relaylevel : StopSound "fx_relay_on"
		For Each Light in LightsGI: Light.State = LightStateOff : Next
		For Each Prim in primitivesGI : Prim.DisableLighting = 0 : Next
		For Each Prim in primitivesGIpegs : Prim.image = "peg" : Prim.material = "peg3": Next
		batleft.blenddisablelighting = 0.1
		batright.blenddisablelighting = 0.1
		If VRRoom > 0 Then
			For Each Light in VRBGGI : Light.visible = 0 : Next
			BGBrightBackground.Color = RGB(65,65,65)
		End If
		If Diner.ShowDT then rampsDT.image = "rampsoffDT" Else rampsFS.image = "rampsoffFS"	End If
		cupprim.image = "cupoff"
	Else
		PlaySound "fx_relay_on",0,relaylevel : StopSound "fx_relay_off"
		For Each Light in LightsGI : Light.State = LightStateOn : Next
		For Each Prim in primitivesGI : Prim.DisableLighting = 1 : Next
		For Each Prim in primitivesGIpegs : Prim.image = "peglight" : Prim.material = "peg3light": Next
		batleft.blenddisablelighting = 0.4
		batright.blenddisablelighting = 0.4
		If VRRoom > 0 Then
			For Each Light in VRBGGI : Light.visible = 1 : Next
			BGBrightBackground.Color = RGB(255,255,255)
		End If
		If Diner.ShowDT then rampsDT.image = "rampsDT" Else	rampsFS.image = "rampsFS" End If
		cupprim.image = "cup"
	End If
End Sub  

'*** Lighting system (mainly inserts) ***

Sub LampTimer_Timer()
    Dim chgLamp, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
			Select Case LightType(chgLamp(ii, 0))
				Case 0
					' *** LightType 0 = normal insert lighting with two lights
					If LightObjectFirst(chgLamp(ii, 0)).state <> chgLamp(ii, 1) Then
						LightObjectFirst(chgLamp(ii, 0)).state = chgLamp(ii, 1) : LightObjectSecond(chgLamp(ii, 0)).state = chgLamp(ii, 1)
					End If
				Case 1
					' *** LightType 1 = one object only for visible on/off
					If LightObjectFirst(chgLamp(ii, 0)).visible <> chgLamp(ii, 1) Then
						LightObjectFirst(chgLamp(ii, 0)).visible = chgLamp(ii, 1)
					End If
				Case 2
					' *** LightType 2 = EAT multiple Lights
					If LightObjectFirst(chgLamp(ii, 0)).state <> chgLamp(ii, 1) Then
						LightObjectFirst(chgLamp(ii, 0)).state = chgLamp(ii, 1) : LightObjectSecond(chgLamp(ii, 0)).state = chgLamp(ii, 1)
						LightObjectThird(chgLamp(ii, 0)).visible = chgLamp(ii, 1)
					End If
				Case 3
					' *** LightType 3 = cutout flashers (pepe, babs, boris, haji, buck)
					If LightObjectFirst(chgLamp(ii, 0)).state <> chgLamp(ii, 1) Then
						LightObjectFirst(chgLamp(ii, 0)).state = chgLamp(ii, 1) : LightObjectSecond(chgLamp(ii, 0)).state = chgLamp(ii, 1)
					End If
					LightState(chgLamp(ii, 0)) = chgLamp(ii, 1)
			End Select
        Next
    End If
End Sub

' *** Collection Hit Sounds ***

Sub Trigger1_Hit() : WireRampOff : WireRampOn False : End Sub
Sub SolACSelect(enabled) : If Enabled Then PlaySound "fx_relay_on",0,GlobalSoundLevel  * relaylevel : StopSound "fx_relay_off" Else PlaySound "fx_relay_off",0, GlobalSoundLevel * relaylevel : StopSound "fx_relay_on": End If : End Sub
Sub RRentrysound_hit() : If Activeball.VelY < 0 Then WireRampOn True Else WireRampOff : End If : End Sub
Sub RRentrysound_UnHit() : If Activeball.VelY > 0 Then WireRampOff : End If : End Sub
Sub LRentrysound_hit() : If Activeball.VelY < 0 and Ramp1.collidable Then WireRampOn True Else	WireRampOff : End If : End Sub
Sub LRentrysound_UnHit() : If Activeball.VelY > 0 Then WireRampOff : End If : End Sub

'*******************************************
'  Ramp Triggers
'*******************************************

'*************** Left Ramp *****************

Sub RightRampDrop_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
	RandomSoundWireRampStop RightRampDrop 
End Sub

'Sub RightRampDrop_unhit()
'	Debug.Print "RightRampDrop"
'	PlaySoundAt "WireRamp_Stop", RightRampDrop 
'End Sub

'*************** Right Ramp *****************

Sub RightRampExit001_hit()
	WireRampOff ' Turn off the Plastic Ramp Sound
	RandomSoundRampStop RightRampExit001
End Sub

Sub CupExit_Hit()
    WireRampOff  : RandomSoundRampStop CupExit : CupExit.Enabled = 0 : End Sub





' *** Ball Shadow code / Glow Ball code / Primitive Flipper Update ***

Const anglecompensate = 15

Sub GraphicsTimer_Timer()
	Dim BOT, b
    BOT = GetBalls

	' switch off glowlight for removed Balls
	IF GlowBall Then
		For b = UBound(BOT) + 1 to 2
			If GlowBall and Glowing(b).state = 1 Then Glowing(b).state = 0 End If
		Next
	End If

    For b = 0 to UBound(BOT)
		' *** move glowball light for max 3 balls ***
		If GlowBall and b < 3 Then
			If Glowing(b).state = 0 Then Glowing(b).state = 1 end if
			Glowing(b).BulbHaloHeight = BOT(b).z + 51
			Glowing(b).x = BOT(b).x : Glowing(b).y = BOT(b).y + anglecompensate
		End If
	Next
	If ChooseBats = 1 Then
		' *** move primitive bats ***
		batleft.objrotz = LeftFlipper.CurrentAngle + 1
		batleftshadow.objrotz = batleft.objrotz
		batright.objrotz = RightFlipper.CurrentAngle - 1
		batrightshadow.objrotz  = batright.objrotz 
	Else
		If ChooseBats > 1 Then
			' *** move glowbats ***
			GlowBatLightLeft.y = 1720 - 121 + LeftFlipper.CurrentAngle
			glowbatleft.objrotz = LeftFlipper.CurrentAngle
			GlowBatLightRight.y =1720 - 121 - RightFlipper.CurrentAngle
			glowbatright.objrotz = RightFlipper.CurrentAngle
		End If
	End If
End Sub

'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************
 Dim Digits(32)
 Digits(0)=Array(a00, a05, a0c, a0d, a08, a01, a06, a0f, a02, a03, a04, a07, a0b, a0a, a09, a0e)
 Digits(1)=Array(a10, a15, a1c, a1d, a18, a11, a16, a1f, a12, a13, a14, a17, a1b, a1a, a19, a1e)
 Digits(2)=Array(a20, a25, a2c, a2d, a28, a21, a26, a2f, a22, a23, a24, a27, a2b, a2a, a29, a2e)
 Digits(3)=Array(a30, a35, a3c, a3d, a38, a31, a36, a3f, a32, a33, a34, a37, a3b, a3a, a39, a3e)
 Digits(4)=Array(a40, a45, a4c, a4d, a48, a41, a46, a4f, a42, a43, a44, a47, a4b, a4a, a49, a4e)
 Digits(5)=Array(a50, a55, a5c, a5d, a58, a51, a56, a5f, a52, a53, a54, a57, a5b, a5a, a59, a5e)
 Digits(6)=Array(a60, a65, a6c, a6d, a68, a61, a66, a6f, a62, a63, a64, a67, a6b, a6a, a69, a6e)
 Digits(7)=Array(a70, a75, a7c, a7d, a78, a71, a76, a7f, a72, a73, a74, a77, a7b, a7a, a79, a7e)
 Digits(8)=Array(a80, a85, a8c, a8d, a88, a81, a86, a8f, a82, a83, a84, a87, a8b, a8a, a89, a8e)
 Digits(9)=Array(a90, a95, a9c, a9d, a98, a91, a96, a9f, a92, a93, a94, a97, a9b, a9a, a99, a9e)
 Digits(10)=Array(aa0, aa5, aac, aad, aa8, aa1, aa6, aaf, aa2, aa3, aa4, aa7, aab, aaa, aa9, aae)
 Digits(11)=Array(ab0, ab5, abc, abd, ab8, ab1, ab6, abf, ab2, ab3, ab4, ab7, abb, aba, ab9, abe)
 Digits(12)=Array(ac0, ac5, acc, acd, ac8, ac1, ac6, acf, ac2, ac3, ac4, ac7, acb, aca, ac9, ace)
 Digits(13)=Array(ad0, ad5, adc, add, ad8, ad1, ad6, adf, ad2, ad3, ad4, ad7, adb, ada, ad9, ade)
 Digits(14)=Array(ae0, ae5, aec, aed, ae8, ae1, ae6, aef, ae2, ae3, ae4, ae7, aeb, aea, ae9, aee)
 Digits(15)=Array(af0, af5, afc, afd, af8, af1, af6, aff, af2, af3, af4, af7, afb, afa, af9, afe)
 
 Digits(16)=Array(b00, b05, b0c, b0d, b08, b01, b06, b0f, b02, b03, b04, b07, b0b, b0a, b09, b0e)
 Digits(17)=Array(b10, b15, b1c, b1d, b18, b11, b16, b1f, b12, b13, b14, b17, b1b, b1a, b19, b1e)
 Digits(18)=Array(b20, b25, b2c, b2d, b28, b21, b26, b2f, b22, b23, b24, b27, b2b, b2a, b29, b2e)
 Digits(19)=Array(b30, b35, b3c, b3d, b38, b31, b36, b3f, b32, b33, b34, b37, b3b, b3a, b39, b3e)
 Digits(20)=Array(b40, b45, b4c, b4d, b48, b41, b46, b4f, b42, b43, b44, b47, b4b, b4a, b49, b4e)
 Digits(21)=Array(b50, b55, b5c, b5d, b58, b51, b56, b5f, b52, b53, b54, b57, b5b, b5a, b59, b5e)
 Digits(22)=Array(b60, b65, b6c, b6d, b68, b61, b66, b6f, b62, b63, b64, b67, b6b, b6a, b69, b6e)
 Digits(23)=Array(b70, b75, b7c, b7d, b78, b71, b76, b7f, b72, b73, b74, b77, b7b, b7a, b79, b7e)
 Digits(24)=Array(b80, b85, b8c, b8d, b88, b81, b86, b8f, b82, b83, b84, b87, b8b, b8a, b89, b8e)
 Digits(25)=Array(b90, b95, b9c, b9d, b98, b91, b96, b9f, b92, b93, b94, b97, b9b, b9a, b99, b9e)
 Digits(26)=Array(ba0, ba5, bac, bad, ba8, ba1, ba6, baf, ba2, ba3, ba4, ba7, bab, baa, ba9, bae)
 Digits(27)=Array(bb0, bb5, bbc, bbd, bb8, bb1, bb6, bbf, bb2, bb3, bb4, bb7, bbb, bba, bb9, bbe)
 Digits(28)=Array(bc0, bc5, bcc, bcd, bc8, bc1, bc6, bcf, bc2, bc3, bc4, bc7, bcb, bca, bc9, bce)
 Digits(29)=Array(bd0, bd5, bdc, bdd, bd8, bd1, bd6, bdf, bd2, bd3, bd4, bd7, bdb, bda, bd9, bde)
 Digits(30)=Array(be0, be5, bec, bed, be8, be1, be6, bef, be2, be3, be4, be7, beb, bea, be9, bee)
 Digits(31)=Array(bf0, bf5, bfc, bfd, bf8, bf1, bf6, bff, bf2, bf3, bf4, bf7, bfb, bfa, bf9, bfe)
 
 Sub DisplayTimer_Timer
	If VRRoom = 0 Then
		Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
		ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
		If Not IsEmpty(ChgLED)Then
			For ii=0 To UBound(chgLED)
			  num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
				if (num < 32) then
				  For Each obj In Digits(num)
					   If chg And 1 Then obj.intensity = 30 * (stat and 1) + 0.4 'obj.State=stat And 1
					   chg=chg\2 : stat=stat\2
					  Next
				Else
				end if
			Next
		End IF
    End If
 End Sub

dim xxxxxx
'If VRRoom = 0 Then
	If DesktopMode And VRRoom = 0 Then
		For each xxxxxx in DesktopDigits:xxxxxx.visible = 1: xxxxxx.state = 1 : xxxxxx.intensity = 0.4: Next
		DisplayTimer.enabled = True
	Else
		For each xxxxxx in DesktopDigits:xxxxxx.visible = 0: Next
		DisplayTimer.enabled = False
	End If
'End If




'//////////////////////////////////////////////////////////////////////
'// FLIPPERS 
'//////////////////////////////////////////////////////////////////////

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
        'LeftFlipper1.RotateToStart
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
	FlipperLSh.RotZ = LeftFlipper.CurrentAngle
	FlipperRSh.RotZ = RightFlipper.CurrentAngle
End Sub



'//////////////////////////////////////////////////////////////////////
'// Ball Rolling
'//////////////////////////////////////////////////////////////////////

Const tnob = 10 ' total number of balls
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
	Dim BOT, b
	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
		' Comment the next line if you are not implementing Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next

	' exit the sub if no balls on the table
	If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(BOT)
		If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If

		' Ball Drop Sounds
		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If BOT(b).velz > -7 Then
					RandomSoundBallBouncePlayfieldSoft BOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If				
			End If
		End If
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If

		' "Static" Ball Shadows
		' Comment the next If block, if you are not implementing the Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then
			If BOT(b).Z > 30 Then
				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
			Else
				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
			End If
			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + fovY
			BallShadowA(b).X = BOT(b).X
			BallShadowA(b).visible = 1
		End If
	Next
End Sub


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
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS 
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
        Dim BOT, b

        If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
                EOSNudge1 = 1
                'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
                If Flipper2.currentangle = EndAngle2 Then 
                        BOT = GetBalls
                        For b = 0 to Ubound(BOT)
                                If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
                                        'Debug.Print "ball in flip1. exit"
                                         exit Sub
                                end If
                        Next
                        For b = 0 to Ubound(BOT)
                                If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
                                        BOT(b).velx = BOT(b).velx / 1.3
                                        BOT(b).vely = BOT(b).vely - 0.5
                                end If
                        Next
                End If
        Else 
			If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then 
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

Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)

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
		Dim b, BOT
		BOT = GetBalls

		For b = 0 to UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
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

dim FlippersD : Set FlippersD = new Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False	
FlippersD.addpoint 0, 0, 1.1	
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

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
    public ballvel, ballvelx, ballvely, ballvelz, ballangmomx, ballangmomy, ballangmomz

    Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : redim ballvelz(0) : redim ballangmomx(0) : redim ballangmomy(0): redim ballangmomz(0): End Sub 

    Public Sub Update()    'tracks in-ball-velocity
        dim str, b, AllBalls, highestID : allBalls = getballs

        for each b in allballs
            if b.id >= HighestID then highestID = b.id
        Next

        if uBound(ballvel) < highestID then redim ballvel(highestID)    'set bounds
        if uBound(ballvelx) < highestID then redim ballvelx(highestID)    'set bounds
        if uBound(ballvely) < highestID then redim ballvely(highestID)    'set bounds
        if uBound(ballvelz) < highestID then redim ballvelz(highestID)    'set bounds
        if uBound(ballangmomx) < highestID then redim ballangmomx(highestID)    'set bounds
        if uBound(ballangmomy) < highestID then redim ballangmomy(highestID)    'set bounds
        if uBound(ballangmomz) < highestID then redim ballangmomz(highestID)    'set bounds

        for each b in allballs
            ballvel(b.id) = BallSpeed(b)
            ballvelx(b.id) = b.velx
            ballvely(b.id) = b.vely
            ballvelz(b.id) = b.velz
            ballangmomx(b.id) = b.angmomx
            ballangmomy(b.id) = b.angmomy
            ballangmomz(b.id) = b.angmomz
        Next
    End Sub
End Class



'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************

'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.

Dim BIPL : BIPL = False				'Ball in plunger lane

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
	TargetBouncer activeball, 1
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

Sub RandomSoundWireRampStop(obj)
	Select Case Int(rnd*6)
		Case 0: PlaySoundAtVol "wireramp_stop", obj, 0.2*volumedial
		Case 1: PlaySoundAtVol "wireramp_stop2", obj, 0.2*volumedial
		Case 2: PlaySoundAtVol "wireramp_stop3", obj, 0.2*volumedial
		Case 3: PlaySoundAtVol "wireramp_stop", obj, 0.1*volumedial
		Case 4: PlaySoundAtVol "wireramp_stop2", obj, 0.1*volumedial
		Case 5: PlaySoundAtVol "wireramp_stop3", obj, 0.1*volumedial
	End Select
End Sub

Sub RandomSoundRampStop(obj)
	Select Case Int(rnd*3)
		Case 0: PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 1: PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 2: PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
	End Select
End Sub


'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************


'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

'****** Part A:  Table Elements ******
'
' Import the "bsrtx7" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the BallShadowA flasher set and the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#
'	* with at least as many objects each as there can be balls, including locked balls
' Ensure you have a timer with a -1 interval that is always running

' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
'***These must be organized in order, so that lights that intersect on the table are adjacent in the collection***
'***If there are more than 3 lights that overlap in a playable area, exclude the less important lights***
' This is because the code will only project two shadows if they are coming from lights that are consecutive in the collection, and more than 3 will cause "jumping" between which shadows are drawn
' The easiest way to keep track of this is to start with the group on the right slingshot and move anticlockwise around the table
'	For example, if you use 6 lights: A & B on the left slingshot and C & D on the right, with E near A&B and F next to C&D, your collection would look like EBACDF
'
'G				H											^	E
'															^	B
'	A		 C												^	A
'	 B		D			your collection should look like	^	G		because E&B, B&A, etc. intersect; but B&D or E&F do not
'  E		  F												^	H
'															^	C
'															^	D
'															^	F
'		When selecting them, you'd shift+click in this order^^^^^

'****** End Part A:  Table Elements ******


'****** Part B:  Code and Functions ******

' *** Timer sub
' The "DynamicBSUpdate" sub should be called by a timer with an interval of -1 (framerate)
'Sub FrameTimer_Timer()
'	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
'End Sub

' *** These are usually defined elsewhere (ballrolling), but activate here if necessary
'Const tnob = 10 ' total number of balls
Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
'Dim tablewidth: tablewidth = Table1.width
'Dim tableheight: tableheight = Table1.height

' *** User Options - Uncomment here or move to top
'----- Shadow Options -----
'Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
'Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
'									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
'									'2 = flasher image shadow, but it moves like ninuzzu's

Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source


' *** This segment goes within the RollingUpdate sub, so that if Ambient...=0 and Dynamic...=0 the entire DynamicBSUpdate sub can be skipped for max performance
'	' stop the sound of deleted balls
'	For b = UBound(BOT) + 1 to tnob
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
'		...rolling(b) = False
'		...StopSound("BallRoll_" & b)
'	Next
'
'...rolling and drop sounds...

'		If DropCount(b) < 5 Then
'			DropCount(b) = DropCount(b) + 1
'		End If
'
'		' "Static" Ball Shadows
'		If AmbientBallShadowOn = 0 Then
'			If BOT(b).Z > 30 Then
'				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
'			Else
'				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
'			End If
'			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + fovY
'			BallShadowA(b).X = BOT(b).X
'			BallShadowA(b).visible = 1
'		End If

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
sourcenames = Array ("","","","","","","","","","","","")
currentShadowCount = Array (0,0,0,0,0,0,0,0,0,0,0,0)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!
dim objrtx1(12), objrtx2(12)
dim objBallShadow(12)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadowA5,BallShadowA6,BallShadowA7,BallShadowA8,BallShadowA9,BallShadowA10,BallShadowA11)

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
	Dim s, Source, LSd, currentMat, AnotherSource, BOT, iii
	BOT = GetBalls

	'Hide shadow of deleted balls
	For s = UBound(BOT) + 1 to tnob
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next

	If UBound(BOT) < lob Then Exit Sub		'No balls in play, exit

'The Magic happens now
	For s = lob to UBound(BOT)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

		If AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
			If BOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If BOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = BOT(s).Y + BallSize/10 + fovY
				objBallShadow(s).visible = 1

				BallShadowA(s).X = BOT(s).X
				BallShadowA(s).Y = BOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif BOT(s).Z <= 30 And BOT(s).Z > 20 Then	'On pf, primitive only
				objBallShadow(s).visible = 1
				If BOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = BOT(s).Y + fovY
				BallShadowA(s).visible = 0
			Else											'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If BOT(s).Z > 30 Then							'In a ramp
				BallShadowA(s).X = BOT(s).X
				BallShadowA(s).Y = BOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif BOT(s).Z <= 30 And BOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				If BOT(s).X < tablewidth/2 Then
					BallShadowA(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					BallShadowA(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				BallShadowA(s).Y = BOT(s).Y + Ballsize/10 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/2 + 5
			Else											'Under pf
				BallShadowA(s).visible = 0
			End If
		End If

' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If BOT(s).Z < 30 Then 'And BOT(s).Y < (TableHeight - 200) Then 'Or BOT(s).Z > 105 Then		'Defining when and where (on the table) you can have dynamic shadows
				For iii = 0 to numberofsources - 1 
					LSd=DistanceFast((BOT(s).x-DSSources(iii)(0)),(BOT(s).y-DSSources(iii)(1)))	'Calculating the Linear distance to the Source
					If LSd < falloff And gilvl > 0 Then						    'If the ball is within the falloff range of a light and light is on (we will set numberofsources to 0 when GI is off)
						currentShadowCount(s) = currentShadowCount(s) + 1		'Within range of 1 or 2
						if currentShadowCount(s) = 1 Then						'1 dynamic shadow source
							sourcenames(s) = iii
							currentMat = objrtx1(s).material
							objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
	'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01						'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), BOT(s).X, BOT(s).Y) + 90
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
							objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
	'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01							'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(DSSources(AnotherSource)(0),DSSources(AnotherSource)(1), BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity = (falloff-DistanceFast((BOT(s).x-DSSources(AnotherSource)(0)),(BOT(s).y-DSSources(AnotherSource)(1))))/falloff
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

							currentMat = objrtx2(s).material
							objrtx2(s).visible = 1 : objrtx2(s).X = BOT(s).X : objrtx2(s).Y = BOT(s).Y + fovY
	'						objrtx2(s).Z = BOT(s).Z - 25 + s/1000 + 0.02							'Uncomment if you want to add shadows to an upper/lower pf
							objrtx2(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), BOT(s).X, BOT(s).Y) + 90
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
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets, 2 = orig TargetBouncer
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7 when TargetBouncerEnabled=1, and 1.1 when TargetBouncerEnabled=2

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
		if aball.vely > 3 then	'only hard hits
			Select Case Int(Rnd * 4) + 1
				Case 1: zMultiplier = defvalue+1.1
				Case 2: zMultiplier = defvalue+1.05
				Case 3: zMultiplier = defvalue+0.7
				Case 4: zMultiplier = defvalue+0.3
			End Select
			aBall.velz = aBall.velz * zMultiplier * TargetBouncerFactor
			'debug.print "----> velz: " & activeball.velz
			'debug.print "conservation check: " & BallSpeed(aBall)/vel
		End If
	end if
end sub


'******************************************************
'*****   FLUPPER DOMES 
'******************************************************

dim SolDrawSpeed : SolDrawSpeed = 1.5


Sub FlashSol130(level)
'	debug.print "level status: " & level
	if level Then
		ObjTargetLevel(1) = 1
		FlasherFlash1_Timer
	Else
		ObjTargetLevel(1) = 0
	end if
End Sub

Sub FlashSol131(level)
	if level Then
		ObjTargetLevel(2) = 1
		FlasherFlash2_Timer
	Else
		ObjTargetLevel(2) = 0
	end if
End Sub

Sub FlashSol109(level)
	if level Then
		ObjTargetLevel(3) = 1
		ObjTargetLevel(4) = 1
		FlasherFlash3_Timer
		FlasherFlash4_Timer
	Else
		ObjTargetLevel(3) = 0
		ObjTargetLevel(4) = 0
	end if
End Sub

Sub FlashSol132(level)
	if level Then
		ObjTargetLevel(5) = 1
		FlasherFlash5_Timer
	Else
		ObjTargetLevel(5) = 0
	end if
End Sub


Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Diner			' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.1		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 0.1		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherBloomIntensity = 0.1		' *** lower this, if the blooms are too bright (i.e. 0.1)			***	
FlasherOffBrightness = 0.6		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20), ObjTargetLevel(20)
Dim FlashOffDL : FlashOffDL = FlasherOffBrightness

InitFlasher 1, "purple"
InitFlasher 2, "purple"
InitFlasher 3, "purple"
InitFlasher 4, "purple"
InitFlasher 5, "white"
RotateFlasher 4,90

'InitFlasher 5, "orange"

'rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 1,17 : RotateFlasher 2,0 : RotateFlasher 3,90 : RotateFlasher 4,90 

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	Set objbloom(nr) = Eval("Flasherbloom" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
'		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 40
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness

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
	If not objflasher(nr).TimerEnabled Then
		objflasher(nr).TimerEnabled = True
		objflasher(nr).visible = 1
		objbloom(nr).visible = 1
		objlit(nr).visible = 1
		Select Case nr
			case 1:	
			If VRRoom > 0 Then
				VRBGFL9_1.visible = 1
				VRBGFL9_2.visible = 1
				VRBGFL9_3.visible = 1
				VRBGFL9_4.visible = 1
			End If
			case 3:	
			If VRRoom > 0 Then
				VRBGFL11_1.visible = 1
				VRBGFL11_2.visible = 1
				VRBGFL11_3.visible = 1
				VRBGFL11_4.visible = 1
			End If
		End Select
	End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2
	objbloom(nr).opacity = 100 *  FlasherBloomIntensity * ObjLevel(nr)^2
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting = FlashOffDL + 10 * ObjLevel(nr)^3
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0
	Select Case nr
		case 1:	
			If VRRoom > 0 Then
				VRBGFL9_1.opacity = 100 * ObjLevel(nr)^2
				VRBGFL9_2.opacity = 100 * ObjLevel(nr)^2
				VRBGFL9_3.opacity = 100 * ObjLevel(nr)^2
				VRBGFL9_4.opacity = 100 * ObjLevel(nr)^3
			End If
		case 3:	
			If VRRoom > 0 Then
				VRBGFL11_1.opacity = 100 * ObjLevel(nr)^2
				VRBGFL11_2.opacity = 100 * ObjLevel(nr)^2
				VRBGFL11_3.opacity = 100 * ObjLevel(nr)^2
				VRBGFL11_4.opacity = 100 * ObjLevel(nr)^3
			End If
	End Select
	if ObjTargetLevel(nr) = 1 and ObjLevel(nr) < ObjTargetLevel(nr) Then			'solenoid ON happened
		ObjLevel(nr) = (ObjLevel(nr) + 0.10) * RndNum(1.05, 1.15)					'fadeup speed. ~4-5 frames * 30ms
		if ObjLevel(nr) > 1 then ObjLevel(nr) = 1
	Elseif ObjTargetLevel(nr) = 0 and ObjLevel(nr) > ObjTargetLevel(nr) Then		'solenoid OFF happened
		ObjLevel(nr) = ObjLevel(nr) * 0.85 - 0.01									'fadedown speed. ~16 frames * 30ms
		if ObjLevel(nr) < 0.02 then ObjLevel(nr) = 0								'slight perf optimization to cut the very tail of the fade
	Else																			'do nothing here
		ObjLevel(nr) = ObjTargetLevel(nr)
		'debug.print objTargetLevel(nr) &" = " & ObjLevel(nr)
	end if

	If ObjLevel(nr) <= 0 Then 
		objflasher(nr).TimerEnabled = False
		objflasher(nr).visible = 0
		objbloom(nr).visible = 0
		objlit(nr).visible = 0
		Select Case nr
			case 1:	
				If VRRoom > 0 Then
					VRBGFL9_1.visible = 0
					VRBGFL9_2.visible = 0
					VRBGFL9_3.visible = 0
					VRBGFL9_4.visible = 0
				End If
			case 3:	
				If VRRoom > 0 Then
					VRBGFL11_1.visible = 0
					VRBGFL11_2.visible = 0
					VRBGFL11_3.visible = 0
					VRBGFL11_4.visible = 0
				End If
		End Select
	End If
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
Dim DT1, DT2, DT3, DT4, DT5, DT6

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

Set DT1 = (new DropTarget)(rootbeer, rootbeer_a, rootbeer_p, swRootBeer, 0, false)
Set DT2 = (new DropTarget)(fries, fries_a, fries_p, swFries, 0, false)
Set DT3 = (new DropTarget)(icedtea, icedtea_a, icedtea_p, swIcedTea, 0, false)
Set DT4 = (new DropTarget)(hotdog, hotdog_a, hotdog_p, swHotdog, 0, false)
Set DT5 = (new DropTarget)(burger, burger_a, burger_p, swBurger, 0, false)
Set DT6 = (new DropTarget)(chili, chili_a, chili_p, swChili, 0, false)


Dim DTArray
DTArray = Array(DT1, DT2, DT3, DT4, DT5, DT6)

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
		controller.Switch(Switchid) = 0

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

	LS.Object = LSling
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS

	RS.Object = RSling
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

'****************************************************************
'		VR Mode
'****************************************************************
DIM VRThings
If VRRoom > 0 Then
	SetBackGlass
	BGDark.visible = 1
	Lockbar.visible = 0
	Rightrail.visible = 0
	Leftrail.visible = 0
	For each VRThings in VRBGGI:VRThings.visible = 1: Next
	For each VRThings in VRBGThings:VRThings.visible = 1: Next
	If VRRoom = 1 Then
		for each VRThings in VRCab:VRThings.visible = 1:Next
		for each VRThings in VR_BaSti:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		BeerTimer.enabled = 1
		ClockTimer.enabled = 1
	End If
	If VRRoom = 2 Then
		for each VRThings in VRCab:VRThings.visible = 1:Next
		for each VRThings in VR_BaSti:VRThings.visible = 0:Next
		for each VRThings in VR_Min:VRThings.visible = 1:Next
		BeerTimer.enabled = 0
		ClockTimer.enabled = 0
	End If
	If VRRoom = 3 Then
		for each VRThings in VRCab:VRThings.visible = 0:Next
		for each VRThings in VR_BaSti:VRThings.visible = 0:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next	
		PinCab_Backbox.visible = 1
		BeerTimer.enabled = 0
		ClockTimer.enabled = 0
	End If
Else
	for each VRThings in VRCab:VRThings.visible = 0:Next
	for each VRThings in VR_Min:VRThings.visible = 0:Next
	For each VRThings in VRBGGI:VRThings.visible = 0: Next
	For each VRThings in VRBGThings:VRThings.visible = 0: Next
	for each VRThings in VR_BaSti:VRThings.visible = 0:Next
	For each VRThings in VRDigitsTop:VRThings.visible = 0: Next
	For each VRThings in VRDigitsBottom:VRThings.visible = 0: Next
	VRBurger.visible = 1
	chair3.visible = 1
	liquid.visible = 1
	foam.visible = 1
	mug.visible = 1
	
End if


'******************************************************
'*******	Set Up Backglass	*******
'******************************************************

	Dim xoff,yoff,zoff,xrot,zscale, xcen,ycen

Sub SetBackglass()
	Dim obj


	For Each obj In VRBackglass
		obj.x = obj.x
		obj.height = - obj.y + 230
		obj.y = -80 'adjusts the distance from the backglass towards the user
		obj.rotx=-87
	Next

	For Each obj In VRBackglassInserts
		obj.x = obj.x
		obj.height = - obj.y + 230
		obj.y = -82 'adjusts the distance from the backglass towards the user
		obj.rotx=-87
	Next

	For Each obj In VRBackglassInsertFlashers
		obj.x = obj.x
		obj.height = - obj.y + 230
		obj.y = -120 'adjusts the distance from the backglass towards the user
		obj.rotx=-87
	Next

	For Each obj In VRDigitsTop
		obj.x = obj.x
		obj.height = - obj.y + 185
		obj.y = -27 'adjusts the distance from the backglass towards the user
		obj.rotx=-86
	Next

	For Each obj In VRDigitsBottom
		obj.x = obj.x
		obj.height = - obj.y + 183
		obj.y = -21 'adjusts the distance from the backglass towards the user
		obj.rotx=-86
	Next

	For Each obj In VRBackglassSpeaker
		obj.x = obj.x
		obj.height = - obj.y + 280
		obj.y = -30 'adjusts the distance from the backglass towards the user
		obj.rotx=-86
	Next

	BGBrightBackground.height = - BGBrightBackground.y + 230 : BGBrightBackground.y = -100
	VRClockHand2.height = - VRClockHand2.y + 230 : VRClockHand2.y = -90
	VRClockHand.height = - VRClockHand.y + 230 : VRClockHand.y = -88
End Sub


'******************************************************
'  END SLINGSHOT CORRECTION FUNCTIONS
'******************************************************


Dim DigitsVR(32)
DigitsVR(0)=Array(ax00, ax05, ax0c, ax0d, ax08, ax01, ax06, ax0f, ax02, ax03, ax04, ax07, ax0b, ax0a, ax09, ax0e)
DigitsVR(1)=Array(ax10, ax15, ax1c, ax1d, ax18, ax11, ax16, ax1f, ax12, ax13, ax14, ax17, ax1b, ax1a, ax19, ax1e)
DigitsVR(2)=Array(ax20, ax25, ax2c, ax2d, ax28, ax21, ax26, ax2f, ax22, ax23, ax24, ax27, ax2b, ax2a, ax29, ax2e)
DigitsVR(3)=Array(ax30, ax35, ax3c, ax3d, ax38, ax31, ax36, ax3f, ax32, ax33, ax34, ax37, ax3b, ax3a, ax39, ax3e)
DigitsVR(4)=Array(ax40, ax45, ax4c, ax4d, ax48, ax41, ax46, ax4f, ax42, ax43, ax44, ax47, ax4b, ax4a, ax49, ax4e)
DigitsVR(5)=Array(ax50, ax55, ax5c, ax5d, ax58, ax51, ax56, ax5f, ax52, ax53, ax54, ax57, ax5b, ax5a, ax59, ax5e)
DigitsVR(6)=Array(ax60, ax65, ax6c, ax6d, ax68, ax61, ax66, ax6f, ax62, ax63, ax64, ax67, ax6b, ax6a, ax69, ax6e)
DigitsVR(7)=Array(ax70, ax75, ax7c, ax7d, ax78, ax71, ax76, ax7f, ax72, ax73, ax74, ax77, ax7b, ax7a, ax79, ax7e)
DigitsVR(8)=Array(ax80, ax85, ax8c, ax8d, ax88, ax81, ax86, ax8f, ax82, ax83, ax84, ax87, ax8b, ax8a, ax89, ax8e)
DigitsVR(9)=Array(ax90, ax95, ax9c, ax9d, ax98, ax91, ax96, ax9f, ax92, ax93, ax94, ax97, ax9b, ax9a, ax99, ax9e)
DigitsVR(10)=Array(axa0, axa5, axac, axad, axa8, axa1, axa6, axaf, axa2, axa3, axa4, axa7, axab, axaa, axa9, axae)
DigitsVR(11)=Array(axb0, axb5, axbc, axbd, axb8, axb1, axb6, axbf, axb2, axb3, axb4, axb7, axbb, axba, axb9, axbe)
DigitsVR(12)=Array(axc0, axc5, axcc, axcd, axc8, axc1, axc6, axcf, axc2, axc3, axc4, axc7, axcb, axca, axc9, axce)
DigitsVR(13)=Array(axd0, axd5, axdc, axdd, axd8, axd1, axd6, axdf, axd2, axd3, axd4, axd7, axdb, axda, axd9, axde)
DigitsVR(14)=Array(axe0, axe5, axec, axed, axe8, axe1, axe6, axef, axe2, axe3, axe4, axe7, axeb, axea, axe9, axee)
DigitsVR(15)=Array(axf0, axf5, axfc, axfd, axf8, axf1, axf6, axff, axf2, axf3, axf4, axf7, axfb, axfa, axf9, axfe)

DigitsVR(16)=Array(a2ax00, a2ax05, a2ax0c, a2ax0d, a2ax08, a2ax01, a2ax06, a2ax0f, a2ax02, a2ax03, a2ax04, a2ax07, a2ax0b, a2ax0a, a2ax09, a2ax0e)
DigitsVR(17)=Array(a2ax10, a2ax15, a2ax1c, a2ax1d, a2ax18, a2ax11, a2ax16, a2ax1f, a2ax12, a2ax13, a2ax14, a2ax17, a2ax1b, a2ax1a, a2ax19, a2ax1e)
DigitsVR(18)=Array(a2ax20, a2ax25, a2ax2c, a2ax2d, a2ax28, a2ax21, a2ax26, a2ax2f, a2ax22, a2ax23, a2ax24, a2ax27, a2ax2b, a2ax2a, a2ax29, a2ax2e)
DigitsVR(19)=Array(a2ax30, a2ax35, a2ax3c, a2ax3d, a2ax38, a2ax31, a2ax36, a2ax3f, a2ax32, a2ax33, a2ax34, a2ax37, a2ax3b, a2ax3a, a2ax39, a2ax3e)
DigitsVR(20)=Array(a2ax40, a2ax45, a2ax4c, a2ax4d, a2ax48, a2ax41, a2ax46, a2ax4f, a2ax42, a2ax43, a2ax44, a2ax47, a2ax4b, a2ax4a, a2ax49, a2ax4e)
DigitsVR(21)=Array(a2ax50, a2ax55, a2ax5c, a2ax5d, a2ax58, a2ax51, a2ax56, a2ax5f, a2ax52, a2ax53, a2ax54, a2ax57, a2ax5b, a2ax5a, a2ax59, a2ax5e)
DigitsVR(22)=Array(a2ax60, a2ax65, a2ax6c, a2ax6d, a2ax68, a2ax61, a2ax66, a2ax6f, a2ax62, a2ax63, a2ax64, a2ax67, a2ax6b, a2ax6a, a2ax69, a2ax6e)
DigitsVR(23)=Array(a2ax70, a2ax75, a2ax7c, a2ax7d, a2ax78, a2ax71, a2ax76, a2ax7f, a2ax72, a2ax73, a2ax74, a2ax77, a2ax7b, a2ax7a, a2ax79, a2ax7e)
DigitsVR(24)=Array(a2ax80, a2ax85, a2ax8c, a2ax8d, a2ax88, a2ax81, a2ax86, a2ax8f, a2ax82, a2ax83, a2ax84, a2ax87, a2ax8b, a2ax8a, a2ax89, a2ax8e)
DigitsVR(25)=Array(a2ax90, a2ax95, a2ax9c, a2ax9d, a2ax98, a2ax91, a2ax96, a2ax9f, a2ax92, a2ax93, a2ax94, a2ax97, a2ax9b, a2ax9a, a2ax99, a2ax9e)
DigitsVR(26)=Array(a2axa0, a2axa5, a2axac, a2axad, a2axa8, a2axa1, a2axa6, a2axaf, a2axa2, a2axa3, a2axa4, a2axa7, a2axab, a2axaa, a2axa9, a2axae)
DigitsVR(27)=Array(a2axb0, a2axb5, a2axbc, a2axbd, a2axb8, a2axb1, a2axb6, a2axbf, a2axb2, a2axb3, a2axb4, a2axb7, a2axbb, a2axba, a2axb9, a2axbe)
DigitsVR(28)=Array(a2axc0, a2axc5, a2axcc, a2axcd, a2axc8, a2axc1, a2axc6, a2axcf, a2axc2, a2axc3, a2axc4, a2axc7, a2axcb, a2axca, a2axc9, a2axce)
DigitsVR(29)=Array(a2axd0, a2axd5, a2axdc, a2axdd, a2axd8, a2axd1, a2axd6, a2axdf, a2axd2, a2axd3, a2axd4, a2axd7, a2axdb, a2axda, a2axd9, a2axde)
DigitsVR(30)=Array(a2axe0, a2axe5, a2axec, a2axed, a2axe8, a2axe1, a2axe6, a2axef, a2axe2, a2axe3, a2axe4, a2axe7, a2axeb, a2axea, a2axe9, a2axee)
DigitsVR(31)=Array(a2axf0, a2axf5, a2axfc, a2axfd, a2axf8, a2axf1, a2axf6, a2axff, a2axf2, a2axf3, a2axf4, a2axf7, a2axfb, a2axfa, a2axf9, a2axfe)


Sub DisplayTimerVR_Timer
	If VRRoom > 0 Then
		Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
		ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
		If Not IsEmpty(ChgLED)Then

		   For ii=0 To UBound(chgLED)
			  num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
				'if (num < 32) then
				if (num < 32) then
				  For Each obj In DigitsVR(num)
					   If chg And 1 Then obj.visible=stat And 1
					   chg=chg\2 : stat=stat\2
					  Next
				Else
				end if
			Next
		end if
	End If
 End Sub

'******************* VR Plunger **********************

Sub TimerVRPlunger_Timer
  If TAFPlungerNew.Y < 1130 then
		TAFPlungerNew.Y = TAFPlungerNew.Y + 5
		TAFPlungerNew1.Y = TAFPlungerNew1.Y + 5
  End If
End Sub

Sub TimerVRPlunger2_Timer
	TAFPlungerNew.Y = 1000 + (5* Plunger.Position) -20
	TAFPlungerNew1.Y = 1000 + (5* Plunger.Position) -20
	timervrplunger2.enabled = 0
End Sub

' ***** Beer Bubble Code - Rawd *****
Sub BeerTimer_Timer()

Randomize(21)
    BeerBubble1.z = BeerBubble1.z + Rnd(1)*0.5
    if BeerBubble1.z > -785 then BeerBubble1.z = -975
    BeerBubble2.z = BeerBubble2.z + Rnd(1)*1
    if BeerBubble2.z > -787 then BeerBubble2.z = -975
    BeerBubble3.z = BeerBubble3.z + Rnd(1)*1
    if BeerBubble3.z > -782 then BeerBubble3.z = -975
    BeerBubble4.z = BeerBubble4.z + Rnd(1)*0.75
    if BeerBubble4.z > -788 then BeerBubble4.z = -975
    BeerBubble5.z = BeerBubble5.z + Rnd(1)*1
    if BeerBubble5.z > -785 then BeerBubble5.z = -975
    BeerBubble6.z = BeerBubble6.z + Rnd(1)*1
    if BeerBubble6.z > -790 then BeerBubble6.z = -975
    BeerBubble7.z = BeerBubble7.z + Rnd(1)*0.8
    if BeerBubble7.z > -791 then BeerBubble7.z = -975
    BeerBubble8.z = BeerBubble8.z + Rnd(1)*1
    if BeerBubble8.z > -786 then BeerBubble8.z = -975
End Sub


' ***************** VR Clock code below - THANKS RASCAL ******************
Dim CurrentMinute ' for VR clock 
' VR Clock code below....
Sub ClockTimer_Timer()
	Pminutes.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6
	Phours.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2)
    Pseconds.RotAndTra2 = (Second(Now()))*6
	CurrentMinute=Minute(Now())
End Sub
 ' ********************** END CLOCK CODE   *********************************




'***** Backglass Inserts************

Dim HajiDirection, BabsDirection, ChefDirection, BorisDirection, PepeDirection, BuckDirection, HajiSpeed, BabsSpeed, ChefSpeed, BorisSpeed, PepeSpeed, BuckSpeed
Dim hajis, babss, chefS, BorisS, PepeS, BuckS

Sub VRInsertMove (HajiS, BabsS, ChefS, BorisS, PepeS, BuckS)
	If HajiS = 0 Then
		HajiSpeed = 76
	Else
		HajiDirection = Int(rnd * 2) + 1
		Hajispeed = Int((HajiS * Rnd) + 1)
	End If
	If BabsS = 0 Then
		Babsspeed = 76
	Else
		BabsDirection = Int(rnd * 2) + 1
		Babsspeed = Int((BabsS * Rnd) + 1)
	End If
	If ChefS = 0 Then
		ChefSpeed = 76
	Else	
		ChefDirection = Int(rnd * 2) + 1
		Chefspeed = Int((ChefS * Rnd) + 1)
	End If
	If BorisS = 0 Then
		BorisSpeed = 76
	Else
		BorisDirection = Int(rnd * 2) + 1
		Borisspeed = Int((BorisS * Rnd) + 1)
	End If
	If PepeS = 0 Then
		PepeSpeed = 76
	Else
		PepeDirection = Int(rnd * 2) + 1
		Pepespeed = Int((PepeS * Rnd) + 1)
	End If
	If BuckS = 0 Then
		BuckSpeed = 76
	Else
		BuckDirection = Int(rnd * 2) + 1
		Buckspeed = Int((BuckS * Rnd) + 1)
	End If
	BGInsertTimer.interval = 30
	BGInserttimer.enabled = 1
End Sub	

Sub VRInsertSmallMove (HajiS, BabsS, ChefS, BorisS, PepeS, BuckS)
	If HajiS = 0 Then
		HajiSpeed = 76
	Else
		HajiDirection = Int(rnd * 2) + 1
		Hajispeed = Int((HajiS * Rnd) + 40)
	End If
	If BabsS = 0 Then
		Babsspeed = 76
	Else
		BabsDirection = Int(rnd * 2) + 1
		Babsspeed = Int((BabsS * Rnd) + 40)
	End If
	If ChefS = 0 Then
		ChefSpeed = 76
	Else	
		ChefDirection = Int(rnd * 2) + 1
		Chefspeed = Int((ChefS * Rnd) + 40)
	End If
	If BorisS = 0 Then
		BorisSpeed = 76
	Else
		BorisDirection = Int(rnd * 2) + 1
		Borisspeed = Int((BorisS * Rnd) + 40)
	End If
	If PepeS = 0 Then
		PepeSpeed = 76
	Else
		PepeDirection = Int(rnd * 2) + 1
		Pepespeed = Int((PepeS * Rnd) + 40)
	End If
	If BuckS = 0 Then
		BuckSpeed = 76
	Else
		BuckDirection = Int(rnd * 2) + 1
		Buckspeed = Int((BuckS * Rnd) + 40)
	End If
	BGInsertTimer.interval = 30
	BGInserttimer.enabled = 1
End Sub	
	

Sub BGInsertTimer_Timer()

	'Sets Haji
	If Hajispeed > 75  Then
		If BabsSpeed > 0 or ChefSpeed > 0 or BorisSpeed > 0 or PepeSpeed > 0 or BuckSpeed > 0 Then
		Else
			BGInsertTimer.enabled = False
		Exit Sub
		End If
	End If
	If Hajispeed > 45 and HajiSpeed < 76 Then
		If HajiDirection = 1 Then
			BGHaji.RotZ=BGHaji.RotZ+2
			If BGHaji.RotZ >= 2 then
				HajiDirection = 2
				HajiSpeed = HajiSpeed + 1
			End If
		End If
		If HajiDirection = 2 Then	
			BGHaji.RotZ=BGHaji.RotZ-2
			If BGHaji.RotZ <= -4 then
				HajiDirection = 1
				HajiSpeed = HajiSpeed + 1
			End If
		End If
	End If

	If Hajispeed > 15 and Hajispeed < 46 Then
		If HajiDirection = 1 Then
			BGHaji.RotZ=BGHaji.RotZ+4
			if BGHaji.RotZ >= 6 then
				HajiDirection = 2
				HajiSpeed = HajiSpeed + 1
			End If
		End If
		If HajiDirection = 2 Then	
			BGHaji.RotZ=BGHaji.RotZ-4
			if BGHaji.RotZ <= -7 then
				HajiDirection = 1
				HajiSpeed = HajiSpeed + 1
			End If
		End If
	End If
	If Hajispeed < 16 Then
		BGHaji.RotZ=BGHaji.RotZ+5
		If HajiDirection = 1 Then
			if BGHaji.RotZ >= 10 then
				HajiDirection = 2
				HajiSpeed = HajiSpeed + 1
			End If
		End If
		If HajiDirection = 2 Then
		BGHaji.RotZ=BGHaji.RotZ-8
			if BGHaji.RotZ <= -12 then
				HajiDirection = 1
				HajiSpeed = HajiSpeed + 1
			End If
		end if
	End If

	'Sets Babs
	If BabsSpeed > 75  Then
		If HajiSpeed > 0 or ChefSpeed > 0 or BorisSpeed > 0 or PepeSpeed > 0 or BuckSpeed > 0 Then
		Else
			BGInsertTimer.enabled = False
		Exit Sub
		End If
	End If
	If BabsSpeed >45 and BabsSpeed < 76 Then
		If BabsDirection = 1 Then
			BGBabs.RotZ=BGBabs.RotZ+2
			If BGBabs.RotZ >= 2 then
				BabsDirection = 2
				BabsSpeed = BabsSpeed + 1
			End If
		End If
		If BabsDirection = 2 Then	
			BGBabs.RotZ=BGBabs.RotZ-2
			If BGBabs.RotZ <= -4 then
				BabsDirection = 1
				BabsSpeed = BabsSpeed + 1
			End If
		End If
	End If

	If BabsSpeed > 15 and BabsSpeed < 46 Then
		If BabsDirection = 1 Then
			BGBabs.RotZ=BGBabs.RotZ+4
			if BGBabs.RotZ >= 6 then
				BabsDirection = 2
				BabsSpeed = BabsSpeed + 1
			End If
		End If
		If BabsDirection = 2 Then	
			BGBabs.RotZ=BGBabs.RotZ-4
			if BGBabs.RotZ <= -7 then
				BabsDirection = 1
				BabsSpeed = BabsSpeed + 1
			End If
		End If
	End If
	If BabsSpeed < 16 Then
		BGBabs.RotZ=BGBabs.RotZ+5
		If BabsDirection = 1 Then
			if BGBabs.RotZ >= 10 then
				BabsDirection = 2
				BabsSpeed = BabsSpeed + 1
			End If
		End If
		If BabsDirection = 2 Then
		BGBabs.RotZ=BGBabs.RotZ-8
			if BGBabs.RotZ <= -12 then
				BabsDirection = 1
				BabsSpeed = BabsSpeed + 1
			End If
		end if
	End If

	'Sets Chef
	If ChefSpeed > 75  Then
		If HajiSpeed > 0 or BabsSpeed > 0 or BorisSpeed > 0 or PepeSpeed > 0 or BuckSpeed > 0 Then
		Else
			BGInsertTimer.enabled = False
		Exit Sub
		End If
	End If
	If ChefSpeed > 45 and ChefSpeed < 76 Then
		If ChefDirection = 1 Then
			BGChef.RotZ=BGChef.RotZ+2
			If BGChef.RotZ >= 2 then
				ChefDirection = 2
				ChefSpeed = ChefSpeed + 1
			End If
		End If
		If ChefDirection = 2 Then	
			BGChef.RotZ=BGChef.RotZ-2
			If BGChef.RotZ <= -4 then
				ChefDirection = 1
				ChefSpeed = ChefSpeed + 1
			End If
		End If
	End If

	If ChefSpeed > 15 and ChefSpeed < 46 Then
		If ChefDirection = 1 Then
			BGChef.RotZ=BGChef.RotZ+4
			if BGChef.RotZ >= 6 then
				ChefDirection = 2
				ChefSpeed = ChefSpeed + 1
			End If
		End If
		If ChefDirection = 2 Then	
			BGChef.RotZ=BGChef.RotZ-4
			if BGChef.RotZ <= -7 then
				ChefDirection = 1
				ChefSpeed = ChefSpeed + 1
			End If
		End If
	End If
	If ChefSpeed < 16 Then
		BGChef.RotZ=BGChef.RotZ+5
		If ChefDirection = 1 Then
			if BGChef.RotZ >= 10 then
				ChefDirection = 2
				ChefSpeed = ChefSpeed + 1
			End If
		End If
		If ChefDirection = 2 Then
		BGChef.RotZ=BGChef.RotZ-8
			if BGChef.RotZ <= -12 then
				ChefDirection = 1
				ChefSpeed = ChefSpeed + 1
			End If
		end if
	End If

'Sets Boris
	If BorisSpeed > 75  Then
		If HajiSpeed > 0 or BabsSpeed > 0 or ChefSpeed > 0 or PepeSpeed > 0 or BuckSpeed > 0 Then
		Else
			BGInsertTimer.enabled = False
		Exit Sub
		End If
	End If
	If BorisSpeed > 45 and BorisSpeed < 76 Then
		If BorisDirection = 1 Then
			BGBoris.RotZ=BGBoris.RotZ+2
			If BGBoris.RotZ >= 2 then
				BorisDirection = 2
				BorisSpeed = BorisSpeed + 1
			End If
		End If
		If BorisDirection = 2 Then	
			BGBoris.RotZ=BGBoris.RotZ-2
			If BGBoris.RotZ <= -4 then
				BorisDirection = 1
				BorisSpeed = BorisSpeed + 1
			End If
		End If
	End If

	If BorisSpeed > 15 and BorisSpeed < 46 Then
		If BorisDirection = 1 Then
			BGBoris.RotZ=BGBoris.RotZ+4
			if BGBoris.RotZ >= 6 then
				BorisDirection = 2
				BorisSpeed = BorisSpeed + 1
			End If
		End If
		If BorisDirection = 2 Then	
			BGBoris.RotZ=BGBoris.RotZ-4
			if BGBoris.RotZ <= -7 then
				BorisDirection = 1
				BorisSpeed = BorisSpeed + 1
			End If
		End If
	End If
	If BorisSpeed < 16 Then
		BGBoris.RotZ=BGBoris.RotZ+5
		If BorisDirection = 1 Then
			if BGBoris.RotZ >= 10 then
				BorisDirection = 2
				BorisSpeed = BorisSpeed + 1
			End If
		End If
		If BorisDirection = 2 Then
		BGBoris.RotZ=BGBoris.RotZ-8
			if BGBoris.RotZ <= -12 then
				BorisDirection = 1
				BorisSpeed = BorisSpeed + 1
			End If
		end if
	End If

'Sets Pepe
	If PepeSpeed > 75  Then
		If HajiSpeed > 0 or BabsSpeed > 0 or ChefSpeed > 0 or BorisSpeed > 0 or BuckSpeed > 0 Then
		Else
			BGInsertTimer.enabled = False
		Exit Sub
		End If
	End If
	If PepeSpeed > 45 and PepeSpeed < 76 Then
		If PepeDirection = 1 Then
			BGPepe.RotZ=BGPepe.RotZ+2
			If BGPepe.RotZ >= 2 then
				PepeDirection = 2
				PepeSpeed = PepeSpeed + 1
			End If
		End If
		If PepeDirection = 2 Then	
			BGPepe.RotZ=BGPepe.RotZ-2
			If BGPepe.RotZ <= -4 then
				PepeDirection = 1
				PepeSpeed = PepeSpeed + 1
			End If
		End If
	End If

	If PepeSpeed > 15 and PepeSpeed < 46 Then
		If PepeDirection = 1 Then
			BGPepe.RotZ=BGPepe.RotZ+4
			if BGPepe.RotZ >= 6 then
				PepeDirection = 2
				PepeSpeed = PepeSpeed + 1
			End If
		End If
		If PepeDirection = 2 Then	
			BGPepe.RotZ=BGPepe.RotZ-4
			if BGPepe.RotZ <= -7 then
				PepeDirection = 1
				PepeSpeed = PepeSpeed + 1
			End If
		End If
	End If
	If PepeSpeed < 16 Then
		BGPepe.RotZ=BGPepe.RotZ+5
		If PepeDirection = 1 Then
			if BGPepe.RotZ >= 10 then
				PepeDirection = 2
				PepeSpeed = PepeSpeed + 1
			End If
		End If
		If PepeDirection = 2 Then
		BGPepe.RotZ=BGPepe.RotZ-8
			if BGPepe.RotZ <= -12 then
				PepeDirection = 1
				PepeSpeed = PepeSpeed + 1
			End If
		end if
	End If

	'Sets Buck
	If BuckSpeed > 75  Then
		If  HajiSpeed > 0 or BabsSpeed > 0 or ChefSpeed > 0 or BorisSpeed > 0 or PepeSpeed > 0 Then
		Else
			BGInsertTimer.enabled = False
		Exit Sub
		End If
	End If
	If BuckSpeed > 45 and BuckSpeed < 76 Then
		If BuckDirection = 1 Then
			BGBuck.RotZ=BGBuck.RotZ+2
			If BGBuck.RotZ >= 2 then
				BuckDirection = 2
				BuckSpeed = BuckSpeed + 1
			End If
		End If
		If BuckDirection = 2 Then	
			BGBuck.RotZ=BGBuck.RotZ-2
			If BGBuck.RotZ <= -4 then
				BuckDirection = 1
				BuckSpeed = BuckSpeed + 1
			End If
		End If
	End If

	If BuckSpeed > 15 and BuckSpeed < 46 Then
		If BuckDirection = 1 Then
			BGBuck.RotZ=BGBuck.RotZ+4
			if BGBuck.RotZ >= 6 then
				BuckDirection = 2
				BuckSpeed = BuckSpeed + 1
			End If
		End If
		If BuckDirection = 2 Then	
			BGBuck.RotZ=BGBuck.RotZ-4
			if BGBuck.RotZ <= -7 then
				BuckDirection = 1
				BuckSpeed = BuckSpeed + 1
			End If
		End If
	End If
	If BuckSpeed < 16 Then
		BGBuck.RotZ=BGBuck.RotZ+5
		If BuckDirection = 1 Then
			if BGBuck.RotZ >= 10 then
				BuckDirection = 2
				BuckSpeed = BuckSpeed + 1
			End If
		End If
		If BuckDirection = 2 Then
		BGBuck.RotZ=BGBuck.RotZ-8
			if BGBuck.RotZ <= -12 then
				BuckDirection = 1
				BuckSpeed = BuckSpeed + 1
			End If
		end if
	End If
End Sub
