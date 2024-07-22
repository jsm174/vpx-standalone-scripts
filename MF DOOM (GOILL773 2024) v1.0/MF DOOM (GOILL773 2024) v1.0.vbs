'           .-''-.                      .--.  .-. .--------.                      .-''-.
'          /      `'''--..              ||  \/  ||'=====. ||              ..--'''`      \
'         |               '.            || .  . ||  .---' ||            .'               |
'         |           ..''` '---.       || |\/|_||  '===. ||       .---' `''..           |
'         |        .``           ''''\  ||_|\/|.'       |_||  /''''           ``.        |
'         /'..   /`              /    /'|_.'            '._|'\    \              `\   ..'\
'         |   `:'         ___..  \   /  /                  \  \   /  ..___         ':`   |
'         '____'__...---'':::::\  '-' ./                    \. '-'  \:::::''---...__'____'
'           \::/\ \:::::::::::':  ___/                        \___  :':::::::::::/ /\::/
'            \'| \ '-:::--'`  .' /                                \ '.  `'--:::-' / |'/
'            /'|  \    ....''`__/                                  \__`''....    /  |'\
'            \ |   .   |  .-'`   .------. .------..------. .-. .--.   `'-.  |   .   | /
'             \/::.'   |  |      ||  _   V   _   ||   _   V  \/  ||      |  |   '.::\/
'             \':'_.---|  |      || | |  |  | |  ||  | |  | .  . ||      |  |---._':'/
'              \\      |  |      || | |  |   '|  ||  |'   | |\/| ||      |  |      //
'              \\      |  |      || |' .'|'.   .'||'.   .'|'|\/| ||      |  |      //
'               \\      | |      ||  .'.' '.'.'.'  '.'.'.' '|  | ||      | |      //
'               \\      | |      ||.'.'     '.'      '.'       |.||      | |      //
'                \\     |'       |_.'                          '._|       '|     //
'                                                            

'Change Log
' 01 goill773      - repositioned sounds, minor cosmetic changes 
' 02 goill773	   - NF physics, rubber dampening, compressed vids/audio   	
' 03 goill773	   - tweaked flashers
' 04 goill773	   - updated DOF calls, fixed hs, updated PuP vids, tweaked lighting, added new fx, cosmetic changes 
' 05 goill773	   - updated more DOF calls, fixed spamming start error, added MagnaSave LUT changer, tweaked lighting, lowered drop target heights, comsmetic changes 
' 06 goill773	   - full Fleep implementation, rubberizer, targetbouncer, added ball options, tweaked flashers, tweaked GI, reshaped some plastics and added lights
'                  - made ramps easier to hit, added static and animated primitives, fixed hs error, cosmetic changes
' 07 goill773	   - added dynamic ball shdow, fixed right orbit hitting slings, tweaked GI more, added flasherbloom/intensity to flupper domes, tweaked mech sounds 
' 08 goill773	   - added invisible walls to fix bumper2 ball jam and targetbouncer jumps , deleted dupe "cor.update", added flipper control of lane lights for skillshot  
' 				   - tweaked lightsequencer timings and colors, added rainbow/purple attract mode, added variable spin prim speeds, added Keyup/down lights to glowing hands next to flippers 
' 				   - added new "Arrow" & "?" lights under right ramp hit target for mystery missions, added mystery bonus & mission callouts, scripted some mystery alias bonus missions
' 				   - updated PuP callout and video priorities, added level primitive, added screws on gates, added ramp entry flap primitives, added additional animated smoke cloud primitive in bong  
' 				   - added PuP "smoke cloud popups" when bumper1 hit, added PuP "smoke cloud gifs" at ball release and drain events  
' 				   - added 2 Metal Fist primitives that randomly shake when slings hit, added a spinning bumper1 cap, weed prims shake on bumper3 hit, bong shakes on bumper1 hit, masks shake when hit  
' 				   - added lots of new playfield & plastics text and graphics, added new flipper graphics, added new plunger lane plastics with lights, added graffiti images to backwall square posts  
' 				   - fixed POV for DT and updated new DT background image, added/deleted some sound fx
' 08a hauntfreaks  - darkened DT background image
' 09a oqqsan 	   - Pure Flex DMD ... things start to look good.. need to enter all gifs names/lengths
' 09b goill773 	   - added Flex gif lengths, added Fire font/pngs
' 09c oqqsan 	   - added scripts for intro startup and end of game gifs, troubleshooting 
' 09e oqqsan       - added Flex Title at startup
' 09f oqqsan 	   - Flex font tweaks (colors, sizes, position, alignment)
' 09g goill773 	   - updated Fire fx pngs
' 09h goill773 	   - added more Flex animation scoring effects (checkerboard, vertical/horizontal/diagonal stripes, explosion, implosion, spirals, stars)  
' 10i oqqsan	   - fistbumps for slings
' 11 goill773 	   - updated pf art, added more inserts, added flipper prims, added DOOMSDAY lights, added cartoon smoke animations, added flasher lasers from mask rubies
'                  - added VR room, fixed positioning of ramps, outlane wire guides, blunt wrap & smoke prims 
' 12 goill773      - VR tweaks, added VR mask topper, Animated plunger  
' 13 iaakki 	   - Reworking how inserts are done
' 14 iaakki 	   - Bug fix and 17 new inserts reworked
' 15 iaakki		   - Added insert color mod. Some new inserts reworked
' 16 iaakki	 	   - Inserts done
' 17 iaakki	 	   - Fixed the GI bug I made and adjusted some levels
' 18 iaakki		   - Slings fixed, callout volume option added, something must be done for primitive25
' 19 RobbyKingPin  - Updated nFozzy/Rothbauerw physics and Fleep sounds, removed JP's ball rolling codes. Removed endpoints for the flippers and added endpoints on the slingshots
' 22 MerlinRTP 	   - Converted Pup from Orbital to pupevents, pup can be disabled to only use audio from pup, removed extraneous code and timers
'				   - Added sequence stopplay to the 4 long routines, converted vpmtimer to queue/tick timers
' 23 RobbyKingPin  - Added Rothbauerw Targets
'                  - Further inspection on the entire playfield to ensure all nFozzy and Fleep implementations are working as they should
'                  - Added VR Hybrid codes
' 24 MerlinRTP     - Added Audio Callouts that were missing, Added targets to Fleep target collection, changed how skill/lane light rotation works,
'				   - Fixed audio bugs where music would stop playing, Added highscore 4 to display rotation.
'				   - Moved Music to MFDOOM music folder for copywright issues
'				   - Added functions to dynamacially create the songs array based off what files are placed in music\MFDOOM folder
'				   - Fixed High Scores
'				   - Fixed Smoke clouds tied to bumpers, fixed flippers and solenoids from firing when game not started
' 25 Kemurro	   - Artwork for pup overlays, cabinet art, backgrounds, backglass

' RC1 MerlinRTP	   - Adjust ballrolling sounds, review code for release
' RC2 MerlinRTP	   - Changed audio file routines
' RC3 DGrimmReaper - Added VR flipper animations
' RC4 MerlinRTP	   - Added missing Drop_Target audio files, replaced a couple fx??? calls
' RC5 MerlinRTP	   - Added Scorbit	
' RC6 apophis	   - Fixed autoplunger. Made VR flipper buttons red. Set ScorbitActive = 0. Default Day Night set to 30%. Set most VR room prims disable lighting to zero. Updated desktop and cab pov. Updated Table Info Description with current credits


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	Option Explicit
	Randomize
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' This will dynamically create an array SONGS that has the mp3 files it finds in music\MFDOOM directory
'Dim re,fso,folder,files,Songs
'Songs = mp3PathArray
dim Songs(15)
Songs(1) = "MFDOOM01.mp3"
Songs(2) = "MFDOOM02.mp3"
Songs(3) = "MFDOOM03.mp3"
Songs(4) = "MFDOOM04.mp3"
Songs(5) = "MFDOOM05.mp3"
Songs(6) = "MFDOOM06.mp3"
Songs(7) = "MFDOOM07.mp3"
Songs(8) = "MFDOOM08.mp3"
Songs(9) = "MFDOOM09.mp3"
Songs(10) = "MFDOOM10.mp3"
Songs(11) = "MFDOOM11.mp3"
Songs(12) = "MFDOOM12.mp3"
Songs(13) = "MFDOOM13.mp3"
Songs(13) = "MFDOOM14.mp3"
Songs(14) = "Attract1.mp3"
Songs(15) = "Attract2.mp3"


'  USER OPTIONS
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'****** PuP Variables ******

Dim UsePuPEvents: Dim cPuPPack: Dim PUPStatus: PUPStatus=false ' dont edit this line!!!

'*************************** PuP Settings for this table ********************************

UsePuPEvents   = True               ' enable Pinup Player functions for this table
cPuPPack = "MFDOOM"    ' name of the PuP-Pack / PuPVideos folder for this table

'----- General Sound Options -----
	Const VolumeDial = 0.8				' Recommended values should be no greater than 1.

'----- Shadow Options -----
	Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow, 1 = enable dynamic ball shadow

'----- LUT Selector Using Magna Save Toggle On/Off-----
	DisableLUTSelector = 0              '0 = LUT selector using magna save enabled, 1 = LUT selector using magna save disabled
	
'----- PUP Topper Videos Toggle On/Off -----
	TopperVideo = 1      				'0 = turn off, 1 = turn on PUP topper videos

'----- Smoke Animations Toggle On/Off -----
	SmokeAnimation = 1      			'0 = turn off, 1 = turn on cartoon bong/blunt smoke animations

'----- Callouts Toggle On/Off -----
	EnableCallouts = 1      			'0 = turn off, 1 = turn on
	CalloutVol = 0.5					' Recommended values should be no greater than 1.

'----- Choose Ball Options -----
	Const ChooseBall = 0  				'Choose Ball Settings (select 0-4)
										' *** 0 = Normal Ball
										' *** 1 = Purple Green Swirl Ball
										' *** 2 = Purple Swirl Ball 
										' *** 3 = Purple Disco Ball
										' *** 4 = Purple Stripes Ball

'----- Flipper Color Options -----
	Const ChooseFlipper = 0  			'Choose Flipper Color (select 0-1)
										' *** 0 = Yellow
										' *** 1 = White

Dim BallRollVolume : BallRollVolume = 0.5   	' Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.5 		' Level of ramp rolling volume. Value between 0 and 1
Dim StagedFlippers : StagedFlippers = 0         ' Staged Flippers. 0 = Disabled, 1 = Enabled

'----- Music Options -----
Const fMusicVolume = 0.5			'Music volume. 0 = no music, 1 = full volume
COnst fAttractVolume = 0.3			'Attract mode music volume. 0 = no music, 1 = full volume


' SCORBIT SETTINGS
'//////////////////////////////////////////////////////////////////////
dim ScorbitActive
ScorbitActive					= 0 	' Is Scorbit Active	
Const     ScorbitShowClaimQR	= 1 	' If Scorbit is active this will show a QR Code  on ball 1 that allows player to claim the active player from the app

Const     ScorbitUploadLog		= 0 	' Store local log and upload after the game is over 
Const     ScorbitAlternateUUID  = 0 	' Force Alternate UUID from Windows Machine and saves it in VPX Users directory (C:\Visual Pinball\User\ScorbitUUID.dat)
Dim bOnTheFirstBallScorbit
'/////////////////////////////////////////////////////////////////////


'*******************************************
'  ZCON: Constants and Global Variables
'*******************************************

Const BallSize = 50				 'Ball diameter in VPX units; must be 50
Const BallMass = 1				  'Ball mass must be 1
Const tnob = 5					  'Total number of balls the table can hold
Const lob = 0					   'Locked balls

'----- VR Room Auto-Detect -----
Dim VRRoom, VR_Obj, VRMode
Const VRTest = 0				' 1 = Testing VR in Live View, 0 = Do not force VR mode.

If RenderingMode = 2 or Table1.ShowFSS = True or VRTest = 1 Then
	VRMode = True
	DMDbackbox.visible=true  ' flasher dmd
    PinCab_Rails.Visible = 1
	For Each VR_Obj in VRCabinet : VR_Obj.Visible = 1 : Next
	For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 1 : Next
Else
	VRMode = False
	For Each VR_Obj in VRCabinet : VR_Obj.Visible = 0 : Next
	For Each VR_Obj in VRMinimalRoom : VR_Obj.Visible = 0 : Next
End If

Dim tablewidth
tablewidth = Table1.width
Dim tableheight
tableheight = Table1.height
Dim BIP							 'Balls in play
BIP = 0
Dim BIPL							'Ball in plunger lane
BIPL = False
Dim CurrBall

Dim BallHandlingQueue : Set BallHandlingQueue = New vpwQueueManager
Dim AudioQueue : Set AudioQueue = New vpwQueueManager
Dim LightQueue : Set LightQueue = New vpwQueueManager
Dim GeneralPupQueue: Set GeneralPupQueue = New vpwQueueManager
																		
'*************************************************
'******** FLEXDMD ********************************
'*************************************************
' FlexDMD constants
Const 	FlexDMD_RenderMode_DMD_GRAY = 0, _
		FlexDMD_RenderMode_DMD_GRAY_4 = 1, _
		FlexDMD_RenderMode_DMD_RGB = 2, _
		FlexDMD_RenderMode_SEG_2x16Alpha = 3, _
		FlexDMD_RenderMode_SEG_2x20Alpha = 4, _
		FlexDMD_RenderMode_SEG_2x7Alpha_2x7Num = 5, _
		FlexDMD_RenderMode_SEG_2x7Alpha_2x7Num_4x1Num = 6, _
		FlexDMD_RenderMode_SEG_2x7Num_2x7Num_4x1Num = 7, _
		FlexDMD_RenderMode_SEG_2x7Num_2x7Num_10x1Num = 8, _
		FlexDMD_RenderMode_SEG_2x7Num_2x7Num_4x1Num_gen7 = 9, _
		FlexDMD_RenderMode_SEG_2x7Num10_2x7Num10_4x1Num = 10, _
		FlexDMD_RenderMode_SEG_2x6Num_2x6Num_4x1Num = 11, _
		FlexDMD_RenderMode_SEG_2x6Num10_2x6Num10_4x1Num = 12, _
		FlexDMD_RenderMode_SEG_4x7Num10 = 13, _
		FlexDMD_RenderMode_SEG_6x4Num_4x1Num = 14, _
		FlexDMD_RenderMode_SEG_2x7Num_4x1Num_1x16Alpha = 15, _
		FlexDMD_RenderMode_SEG_1x16Alpha_1x16Num_1x7Num = 16

Const 	FlexDMD_Align_TopLeft = 0, _
		FlexDMD_Align_Top = 1, _
		FlexDMD_Align_TopRight = 2, _
		FlexDMD_Align_Left = 3, _
		FlexDMD_Align_Center = 4, _
		FlexDMD_Align_Right = 5, _
		FlexDMD_Align_BottomLeft = 6, _
		FlexDMD_Align_Bottom = 7, _
		FlexDMD_Align_BottomRight = 8

Dim FlexDMD
Dim Frame
Dim DMDMode
: 

Sub FlexDMD_init
	Dim fso,curdir
	Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
    If FlexDMD is Nothing Then
        MsgBox "No FlexDMD found. This table will NOT run without it."
        Exit Sub
    End If
	SetLocale(1033)
	With FlexDMD
		.GameName = cGameName
		.TableFile = Table1.Filename & ".vpx"
		.Color = RGB(255, 88, 32)
		.RenderMode = FlexDMD_RenderMode_DMD_GRAY_4
		.Width = 128
		.Height = 32
		.Clear = True
		.ProjectFolder = "./MFDOOMDMD/"
		.Run = True
	End With

	CreateDMD_intro

	DMDTimer.interval=17
	DMDTimer.enabled=True

End Sub

Dim FontScoreActive
Dim FontScoreInactive

Dim FontFire1, FontFire2, FontFire3, FontFire4, FontFire5, FontFire6, FontFire7, FontFire8, FontFire9
Dim FontFireBG1, FontFireBG2, FontFireBG3, FontFireBG4, FontFireBG5, FontFireBG6, FontFireBG7, FontFireBG8, FontFireBG9
Dim FontCheckerboard1, FontCheckerboard2, FontCheckerboard3, FontCheckerboard4, FontCheckerboard5, FontCheckerboard6, FontCheckerboard7, FontCheckerboard8
Dim FontCheckerboardBG1, FontCheckerboardBG2, FontCheckerboardBG3, FontCheckerboardBG4, FontCheckerboardBG5, FontCheckerboardBG6, FontCheckerboardBG7, FontCheckerboardBG8
Dim FontExp1, FontExp2, FontExp3, FontExp4, FontExp5, FontExp6, FontExp7
Dim FontExpBG1, FontExpBG2, FontExpBG3, FontExpBG4, FontExpBG5, FontExpBG6, FontExpBG7
Dim FontImp1, FontImp2, FontImp3, FontImp4, FontImp5, FontImp6, FontImp7
Dim FontImpBG1, FontImpBG2, FontImpBG3, FontImpBG4, FontImpBG5, FontImpBG6, FontImpBG7
Dim FontVerticalThick1, FontVerticalThick2, FontVerticalThick3, FontVerticalThick4, FontVerticalThick5, FontVerticalThick6, FontVerticalThick7, FontVerticalThick8
Dim FontVerticalThickBG1, FontVerticalThickBG2, FontVerticalThickBG3, FontVerticalThickBG4, FontVerticalThickBG5, FontVerticalThickBG6, FontVerticalThickBG7, FontVerticalThickBG8
Dim FontHorizontalThick1, FontHorizontalThick2, FontHorizontalThick3, FontHorizontalThick4, FontHorizontalThick5, FontHorizontalThick6, FontHorizontalThick7, FontHorizontalThick8, FontHorizontalThick9, FontHorizontalThick10, FontHorizontalThick11, FontHorizontalThick12, FontHorizontalThick13, FontHorizontalThick14
Dim FontHorizontalThickBG1, FontHorizontalThickBG2, FontHorizontalThickBG3, FontHorizontalThickBG4, FontHorizontalThickBG5, FontHorizontalThickBG6, FontHorizontalThickBG7, FontHorizontalThickBG8, FontHorizontalThickBG9, FontHorizontalThickBG10, FontHorizontalThickBG11, FontHorizontalThickBG12, FontHorizontalThickBG13, FontHorizontalThickBG14
Dim FontSpiral1, FontSpiral2, FontSpiral3, FontSpiral4, FontSpiral5, FontSpiral6, FontSpiral7
Dim FontSpiralBG1, FontSpiralBG2, FontSpiralBG3, FontSpiralBG4, FontSpiralBG5, FontSpiralBG6, FontSpiralBG7
Dim FontSpiralL1, FontSpiralL2, FontSpiralL3, FontSpiralL4, FontSpiralL5, FontSpiralL6, FontSpiralL7
Dim FontSpiralLBG1, FontSpiralLBG2, FontSpiralLBG3, FontSpiralLBG4, FontSpiralLBG5, FontSpiralLBG6, FontSpiralLBG7
Dim FontStar1, FontStar2, FontStar3, FontStar4, FontStar5, FontStar6, FontStar7, FontStar8
Dim FontStarBG1, FontStarBG2, FontStarBG3, FontStarBG4, FontStarBG5, FontStarBG6, FontStarBG7, FontStarBG8
Dim FontDiag1, FontDiag2, FontDiag3, FontDiag4, FontDiag5, FontDiag6, FontDiag7, FontDiag8, FontDiag9, FontDiag10, FontDiag11, FontDiag12, FontDiag13, FontDiag14, FontDiag15, FontDiag16, FontDiag17
Dim FontDiagBG1, FontDiagBG2, FontDiagBG3, FontDiagBG4, FontDiagBG5, FontDiagBG6, FontDiagBG7, FontDiagBG8, FontDiagBG9, FontDiagBG10, FontDiagBG11, FontDiagBG12, FontDiagBG13, FontDiagBG14, FontDiagBG15, FontDiagBG16, FontDiagBG17
Dim FontDiagL1, FontDiagL2, FontDiagL3, FontDiagL4, FontDiagL5, FontDiagL6, FontDiagL7, FontDiagL8, FontDiagL9, FontDiagL10, FontDiagL11, FontDiagL12, FontDiagL13, FontDiagL14, FontDiagL15, FontDiagL16, FontDiagL17
Dim FontDiagLBG1, FontDiagLBG2, FontDiagLBG3, FontDiagLBG4, FontDiagLBG5, FontDiagLBG6, FontDiagLBG7, FontDiagLBG8, FontDiagLBG9, FontDiagLBG10, FontDiagLBG11, FontDiagLBG12, FontDiagLBG13, FontDiagLBG14, FontDiagLBG15, FontDiagLBG16, FontDiagLBG17
Dim FontMask1, FontMask2, FontMask3, FontMask4, FontMask5, FontMask6, FontMask7, FontMask8, FontMask9, FontMask10, FontMask11, FontMask12, FontMask13
Dim FontMaskBG1, FontMaskBG2, FontMaskBG3, FontMaskBG4, FontMaskBG5, FontMaskBG6, FontMaskBG7, FontMaskBG8, FontMaskBG9, FontMaskBG10, FontMaskBG11, FontMaskBG12, FontMaskBG13
Dim FontMaskCenter1, FontMaskCenter2, FontMaskCenter3, FontMaskCenter4, FontMaskCenter5, FontMaskCenter6, FontMaskCenter7, FontMaskCenter8, FontMaskCenter9, FontMaskCenter10
Dim FontMaskCenterBG1, FontMaskCenterBG2, FontMaskCenterBG3, FontMaskCenterBG4, FontMaskCenterBG5, FontMaskCenterBG6, FontMaskCenterBG7, FontMaskCenterBG8, FontMaskCenterBG9, FontMaskCenterBG10
Dim FontMaskBounce1, FontMaskBounce2, FontMaskBounce3, FontMaskBounce4, FontMaskBounce5, FontMaskBounce6, FontMaskBounce7, FontMaskBounce8, FontMaskBounce9, FontMaskBounce10, FontMaskBounce11
Dim FontMaskBounceBG1, FontMaskBounceBG2, FontMaskBounceBG3, FontMaskBounceBG4, FontMaskBounceBG5, FontMaskBounceBG6, FontMaskBounceBG7, FontMaskBounceBG8, FontMaskBounceBG9, FontMaskBounceBG10, FontMaskBounceBG11
Dim FontMaskBounceH1, FontMaskBounceH2, FontMaskBounceH3, FontMaskBounceH4, FontMaskBounceH5, FontMaskBounceH6, FontMaskBounceH7, FontMaskBounceH8, FontMaskBounceH9
Dim FontMaskBounceHBG1, FontMaskBounceHBG2, FontMaskBounceHBG3, FontMaskBounceHBG4, FontMaskBounceHBG5, FontMaskBounceHBG6, FontMaskBounceHBG7, FontMaskBounceHBG8,FontMaskBounceHBG9
Dim FontBig, FontBig2, FontBig3

Sub CreateDMD_intro

	Frame = 0

	Set FontFire1= FlexDMD.NewFont("udmd-f7by13-example1.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontFire2= FlexDMD.NewFont("udmd-f7by13-example2.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontFire3= FlexDMD.NewFont("udmd-f7by13-example3.fnt", RGB(255, 215, 00), vbWhite, 0)   'gold
	Set FontFire4= FlexDMD.NewFont("udmd-f7by13-example4.fnt", RGB(255, 165, 0), vbWhite, 0)    'orange
	Set FontFire5= FlexDMD.NewFont("udmd-f7by13-example5.fnt", RGB(255, 165, 0), vbWhite, 0)    'orange
	Set FontFire6= FlexDMD.NewFont("udmd-f7by13-example6.fnt", RGB(255, 140, 0), vbWhite, 0)    'darkorange
	Set FontFire7= FlexDMD.NewFont("udmd-f7by13-example7.fnt", RGB(255, 140, 0), vbWhite, 0)    'darkorange
	Set FontFire8= FlexDMD.NewFont("udmd-f7by13-example8.fnt", RGB(255, 0, 0), vbWhite, 0)      'red
	Set FontFire9= FlexDMD.NewFont("udmd-f7by13-example9.fnt", RGB(255, 0, 0), vbWhite, 0)      'red

	Set FontFireBG1= FlexDMD.NewFont("udmd-f7by13-exampleBG1.fnt", vbBlack, vbWhite, 0)    'black
	Set FontFireBG2= FlexDMD.NewFont("udmd-f7by13-exampleBG2.fnt", vbBlack, vbWhite, 0)    'black
	Set FontFireBG3= FlexDMD.NewFont("udmd-f7by13-exampleBG3.fnt", vbBlack, vbWhite, 0)    'black
	Set FontFireBG4= FlexDMD.NewFont("udmd-f7by13-exampleBG4.fnt", vbBlack, vbWhite, 0)    'black
	Set FontFireBG5= FlexDMD.NewFont("udmd-f7by13-exampleBG5.fnt", vbBlack, vbWhite, 0)    'black
	Set FontFireBG6= FlexDMD.NewFont("udmd-f7by13-exampleBG6.fnt", vbBlack, vbWhite, 0)    'black
	Set FontFireBG7= FlexDMD.NewFont("udmd-f7by13-exampleBG7.fnt", vbBlack, vbWhite, 0)    'black
	Set FontFireBG8= FlexDMD.NewFont("udmd-f7by13-exampleBG8.fnt", vbBlack, vbWhite, 0)    'black
	Set FontFireBG9= FlexDMD.NewFont("udmd-f7by13-exampleBG9.fnt", vbBlack, vbWhite, 0)    'black

	Set FontCheckerboard1= FlexDMD.NewFont("udmd-f7by13-checkerboard1.fnt", vbYellow, vbWhite, 0)     
	Set FontCheckerboard2= FlexDMD.NewFont("udmd-f7by13-checkerboard2.fnt", RGB(255, 0, 255), vbWhite, 0)   'magenta

	Set FontCheckerboardBG1= FlexDMD.NewFont("udmd-f7by13-checkerboardBG1.fnt", vbBlack, vbWhite, 0) 'black
	Set FontCheckerboardBG2= FlexDMD.NewFont("udmd-f7by13-checkerboardBG2.fnt", vbBlack, vbWhite, 0) 'black

	Set FontExp1= FlexDMD.NewFont("udmd-f7by13-exp1.fnt", RGB(0, 128, 0), vbWhite, 0)      'green
	Set FontExp2= FlexDMD.NewFont("udmd-f7by13-exp2.fnt", RGB(0, 128, 0), vbWhite, 0)      'green
	Set FontExp3= FlexDMD.NewFont("udmd-f7by13-exp3.fnt", RGB(0, 128, 0), vbWhite, 0)      'green
	Set FontExp4= FlexDMD.NewFont("udmd-f7by13-exp4.fnt", RGB(0, 255, 127), vbWhite, 0)   'spring green
	Set FontExp5= FlexDMD.NewFont("udmd-f7by13-exp5.fnt", RGB(0, 255, 127), vbWhite, 0)   'spring green
	Set FontExp6= FlexDMD.NewFont("udmd-f7by13-exp6.fnt", RGB(173, 255, 47), vbWhite, 0)    ' green yellow
	Set FontExp7= FlexDMD.NewFont("udmd-f7by13-exp7.fnt", RGB(173, 255, 47), vbWhite, 0)    ' green yellow

	Set FontExpBG1= FlexDMD.NewFont("udmd-f7by13-expBG1.fnt", vbBlack, vbWhite, 0)      'black
	Set FontExpBG2= FlexDMD.NewFont("udmd-f7by13-expBG2.fnt", vbBlack, vbWhite, 0)      'black
	Set FontExpBG3= FlexDMD.NewFont("udmd-f7by13-expBG3.fnt", vbBlack, vbWhite, 0)      'black
	Set FontExpBG4= FlexDMD.NewFont("udmd-f7by13-expBG4.fnt", vbBlack, vbWhite, 0)      'black
	Set FontExpBG5= FlexDMD.NewFont("udmd-f7by13-expBG5.fnt", vbBlack, vbWhite, 0)      'black
	Set FontExpBG6= FlexDMD.NewFont("udmd-f7by13-expBG6.fnt", vbBlack, vbWhite, 0)      'black
	Set FontExpBG7= FlexDMD.NewFont("udmd-f7by13-expBG7.fnt", vbBlack, vbWhite, 0)      'black

	Set FontImp1= FlexDMD.NewFont("udmd-f7by13-imp1.fnt", RGB(139, 0, 139), vbWhite, 0)      'dark magenta
	Set FontImp2= FlexDMD.NewFont("udmd-f7by13-imp2.fnt", RGB(139, 0, 139), vbWhite, 0)      'dark magenta
	Set FontImp3= FlexDMD.NewFont("udmd-f7by13-imp3.fnt", RGB(139, 0, 139), vbWhite, 0)      'dark magenta
	Set FontImp4= FlexDMD.NewFont("udmd-f7by13-imp4.fnt", RGB(255, 0, 255), vbWhite, 0)   'magenta
	Set FontImp5= FlexDMD.NewFont("udmd-f7by13-imp5.fnt", RGB(255, 0, 255), vbWhite, 0)   'magenta
	Set FontImp6= FlexDMD.NewFont("udmd-f7by13-imp6.fnt", RGB(238, 130, 238), vbWhite, 0)    'violet
	Set FontImp7= FlexDMD.NewFont("udmd-f7by13-imp7.fnt", RGB(238, 130, 238), vbWhite, 0)    'violet

	Set FontImpBG1= FlexDMD.NewFont("udmd-f7by13-impBG1.fnt", vbBlack, vbWhite, 0)      'black
	Set FontImpBG2= FlexDMD.NewFont("udmd-f7by13-impBG2.fnt", vbBlack, vbWhite, 0)      'black
	Set FontImpBG3= FlexDMD.NewFont("udmd-f7by13-impBG3.fnt", vbBlack, vbWhite, 0)      'black
	Set FontImpBG4= FlexDMD.NewFont("udmd-f7by13-impBG4.fnt", vbBlack, vbWhite, 0)      'black
	Set FontImpBG5= FlexDMD.NewFont("udmd-f7by13-impBG5.fnt", vbBlack, vbWhite, 0)      'black
	Set FontImpBG6= FlexDMD.NewFont("udmd-f7by13-impBG6.fnt", vbBlack, vbWhite, 0)      'black
	Set FontImpBG7= FlexDMD.NewFont("udmd-f7by13-impBG7.fnt", vbBlack, vbWhite, 0)      'black

	Set FontVerticalThick1= FlexDMD.NewFont("udmd-f7by13-verticalthick1.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontVerticalThick2= FlexDMD.NewFont("udmd-f7by13-verticalthick2.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontVerticalThick3= FlexDMD.NewFont("udmd-f7by13-verticalthick3.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontVerticalThick4= FlexDMD.NewFont("udmd-f7by13-verticalthick4.fnt", RGB(0, 255, 0), vbWhite, 0)      'lime
	Set FontVerticalThick5= FlexDMD.NewFont("udmd-f7by13-verticalthick5.fnt", RGB(0, 255, 0), vbWhite, 0)      'lime
	Set FontVerticalThick6= FlexDMD.NewFont("udmd-f7by13-verticalthick6.fnt", RGB(0, 128, 0), vbWhite, 0)      'green
	Set FontVerticalThick7= FlexDMD.NewFont("udmd-f7by13-verticalthick7.fnt", RGB(0, 128, 0), vbWhite, 0)      'green
	Set FontVerticalThick8= FlexDMD.NewFont("udmd-f7by13-verticalthick8.fnt", RGB(0, 128, 0), vbWhite, 0)      'green

	Set FontVerticalThickBG1= FlexDMD.NewFont("udmd-f7by13-verticalthickBG1.fnt", vbBlack, vbWhite, 0)    'black
	Set FontVerticalThickBG2= FlexDMD.NewFont("udmd-f7by13-verticalthickBG2.fnt", vbBlack, vbWhite, 0)    'black
	Set FontVerticalThickBG3= FlexDMD.NewFont("udmd-f7by13-verticalthickBG3.fnt", vbBlack, vbWhite, 0)    'black
	Set FontVerticalThickBG4= FlexDMD.NewFont("udmd-f7by13-verticalthickBG4.fnt", vbBlack, vbWhite, 0)    'black
	Set FontVerticalThickBG5= FlexDMD.NewFont("udmd-f7by13-verticalthickBG5.fnt", vbBlack, vbWhite, 0)    'black
	Set FontVerticalThickBG6= FlexDMD.NewFont("udmd-f7by13-verticalthickBG6.fnt", vbBlack, vbWhite, 0)    'black
	Set FontVerticalThickBG7= FlexDMD.NewFont("udmd-f7by13-verticalthickBG7.fnt", vbBlack, vbWhite, 0)    'black
	Set FontVerticalThickBG8= FlexDMD.NewFont("udmd-f7by13-verticalthickBG8.fnt", vbBlack, vbWhite, 0)    'black

	Set FontHorizontalThick1= FlexDMD.NewFont("udmd-f7by13-horizontalthick1.fnt", RGB(255, 0, 0), vbWhite, 0)      'red 
	Set FontHorizontalThick2= FlexDMD.NewFont("udmd-f7by13-horizontalthick2.fnt", RGB(255, 0, 0), vbWhite, 0)      'red
	Set FontHorizontalThick3= FlexDMD.NewFont("udmd-f7by13-horizontalthick3.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontHorizontalThick4= FlexDMD.NewFont("udmd-f7by13-horizontalthick4.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontHorizontalThick5= FlexDMD.NewFont("udmd-f7by13-horizontalthick5.fnt", RGB(0, 255, 0), vbWhite, 0)      'lime
	Set FontHorizontalThick6= FlexDMD.NewFont("udmd-f7by13-horizontalthick6.fnt", RGB(0, 128, 0), vbWhite, 0)      'green
	Set FontHorizontalThick7= FlexDMD.NewFont("udmd-f7by13-horizontalthick7.fnt", RGB(0, 128, 0), vbWhite, 0)      'green
	Set FontHorizontalThick8= FlexDMD.NewFont("udmd-f7by13-horizontalthick8.fnt",  RGB(255, 0, 0), vbWhite, 0)      'red 
	Set FontHorizontalThick9= FlexDMD.NewFont("udmd-f7by13-horizontalthick9.fnt",  RGB(255, 0, 0), vbWhite, 0)      'red 
	Set FontHorizontalThick10= FlexDMD.NewFont("udmd-f7by13-horizontalthick10.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontHorizontalThick11= FlexDMD.NewFont("udmd-f7by13-horizontalthick11.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontHorizontalThick12= FlexDMD.NewFont("udmd-f7by13-horizontalthick12.fnt", RGB(0, 255, 0), vbWhite, 0)      'lime
	Set FontHorizontalThick13= FlexDMD.NewFont("udmd-f7by13-horizontalthick13.fnt", RGB(0, 128, 0), vbWhite, 0)      'green
	Set FontHorizontalThick14= FlexDMD.NewFont("udmd-f7by13-horizontalthick14.fnt", RGB(0, 128, 0), vbWhite, 0)      'green

	Set FontHorizontalThickBG1= FlexDMD.NewFont("udmd-f7by13-horizontalthickBG1.fnt", vbBlack, vbWhite, 0)    'black
	Set FontHorizontalThickBG2= FlexDMD.NewFont("udmd-f7by13-horizontalthickBG2.fnt", vbBlack, vbWhite, 0)    'black
	Set FontHorizontalThickBG3= FlexDMD.NewFont("udmd-f7by13-horizontalthickBG3.fnt", vbBlack, vbWhite, 0)    'black
	Set FontHorizontalThickBG4= FlexDMD.NewFont("udmd-f7by13-horizontalthickBG4.fnt", vbBlack, vbWhite, 0)    'black
	Set FontHorizontalThickBG5= FlexDMD.NewFont("udmd-f7by13-horizontalthickBG5.fnt", vbBlack, vbWhite, 0)    'black
	Set FontHorizontalThickBG6= FlexDMD.NewFont("udmd-f7by13-horizontalthickBG6.fnt", vbBlack, vbWhite, 0)    'black
	Set FontHorizontalThickBG7= FlexDMD.NewFont("udmd-f7by13-horizontalthickBG7.fnt", vbBlack, vbWhite, 0)    'black
	Set FontHorizontalThickBG8= FlexDMD.NewFont("udmd-f7by13-horizontalthickBG8.fnt", vbBlack, vbWhite, 0)    'black
	Set FontHorizontalThickBG9= FlexDMD.NewFont("udmd-f7by13-horizontalthickBG9.fnt", vbBlack, vbWhite, 0)    'black
	Set FontHorizontalThickBG10= FlexDMD.NewFont("udmd-f7by13-horizontalthickBG10.fnt", vbBlack, vbWhite, 0)    'black
	Set FontHorizontalThickBG11= FlexDMD.NewFont("udmd-f7by13-horizontalthickBG11.fnt", vbBlack, vbWhite, 0)    'black
	Set FontHorizontalThickBG12= FlexDMD.NewFont("udmd-f7by13-horizontalthickBG12.fnt", vbBlack, vbWhite, 0)    'black
	Set FontHorizontalThickBG13= FlexDMD.NewFont("udmd-f7by13-horizontalthickBG13.fnt", vbBlack, vbWhite, 0)    'black
	Set FontHorizontalThickBG14= FlexDMD.NewFont("udmd-f7by13-horizontalthickBG14.fnt", vbBlack, vbWhite, 0)    'black

	Set FontSpiral1= FlexDMD.NewFont("udmd-f7by13-spiral1.fnt", RGB(255, 192, 203), vbWhite, 0)  'pink  
	Set FontSpiral2= FlexDMD.NewFont("udmd-f7by13-spiral2.fnt", RGB(255, 182, 193), vbWhite, 0)  'light pink
	Set FontSpiral3= FlexDMD.NewFont("udmd-f7by13-spiral3.fnt", RGB(219, 112, 147), vbWhite, 0)  'pale violet red
	Set FontSpiral4= FlexDMD.NewFont("udmd-f7by13-spiral4.fnt", RGB(255, 105, 180), vbWhite, 0)   'hot pink
	Set FontSpiral5= FlexDMD.NewFont("udmd-f7by13-spiral5.fnt", RGB(255, 20, 147), vbWhite, 0)    'deep pink
	Set FontSpiral6= FlexDMD.NewFont("udmd-f7by13-spiral6.fnt", RGB(199, 21, 133), vbWhite, 0)    'medium violet red
	Set FontSpiral7= FlexDMD.NewFont("udmd-f7by13-spiral7.fnt", RGB(255, 0, 255), vbWhite, 0)    'magenta

	Set FontSpiralBG1= FlexDMD.NewFont("udmd-f7by13-spiralBG1.fnt", vbBlack, vbWhite, 0)    'black
	Set FontSpiralBG2= FlexDMD.NewFont("udmd-f7by13-spiralBG2.fnt", vbBlack, vbWhite, 0)    'black
	Set FontSpiralBG3= FlexDMD.NewFont("udmd-f7by13-spiralBG3.fnt", vbBlack, vbWhite, 0)    'black
	Set FontSpiralBG4= FlexDMD.NewFont("udmd-f7by13-spiralBG4.fnt", vbBlack, vbWhite, 0)    'black
	Set FontSpiralBG5= FlexDMD.NewFont("udmd-f7by13-spiralBG5.fnt", vbBlack, vbWhite, 0)    'black
	Set FontSpiralBG6= FlexDMD.NewFont("udmd-f7by13-spiralBG6.fnt", vbBlack, vbWhite, 0)    'black
	Set FontSpiralBG7= FlexDMD.NewFont("udmd-f7by13-spiralBG7.fnt", vbBlack, vbWhite, 0)    'black

	Set FontSpiralL1= FlexDMD.NewFont("udmd-f7by13-spiralL1.fnt", RGB(255, 0, 255), vbWhite, 0)    'magenta  
	Set FontSpiralL2= FlexDMD.NewFont("udmd-f7by13-spiralL2.fnt", RGB(199, 21, 133), vbWhite, 0)    'medium violet red
	Set FontSpiralL3= FlexDMD.NewFont("udmd-f7by13-spiralL3.fnt", RGB(255, 20, 147), vbWhite, 0)    'deep pink
	Set FontSpiralL4= FlexDMD.NewFont("udmd-f7by13-spiralL4.fnt", RGB(255, 105, 180), vbWhite, 0)   'hot pink
	Set FontSpiralL5= FlexDMD.NewFont("udmd-f7by13-spiralL5.fnt", RGB(219, 112, 147), vbWhite, 0)  'pale violet red
	Set FontSpiralL6= FlexDMD.NewFont("udmd-f7by13-spiralL6.fnt", RGB(255, 182, 193), vbWhite, 0)  'light pink
	Set FontSpiralL7= FlexDMD.NewFont("udmd-f7by13-spiralL7.fnt", RGB(255, 192, 203), vbWhite, 0)  'pink 

	Set FontSpiralLBG1= FlexDMD.NewFont("udmd-f7by13-spiralLBG1.fnt", vbBlack, vbWhite, 0)    'black
	Set FontSpiralLBG2= FlexDMD.NewFont("udmd-f7by13-spiralLBG2.fnt", vbBlack, vbWhite, 0)    'black
	Set FontSpiralLBG3= FlexDMD.NewFont("udmd-f7by13-spiralLBG3.fnt", vbBlack, vbWhite, 0)    'black
	Set FontSpiralLBG4= FlexDMD.NewFont("udmd-f7by13-spiralLBG4.fnt", vbBlack, vbWhite, 0)    'black
	Set FontSpiralLBG5= FlexDMD.NewFont("udmd-f7by13-spiralLBG5.fnt", vbBlack, vbWhite, 0)    'black
	Set FontSpiralLBG6= FlexDMD.NewFont("udmd-f7by13-spiralLBG6.fnt", vbBlack, vbWhite, 0)    'black
	Set FontSpiralLBG7= FlexDMD.NewFont("udmd-f7by13-spiralLBG7.fnt", vbBlack, vbWhite, 0)    'black

	Set FontStar1= FlexDMD.NewFont("udmd-f7by13-star1.fnt", RGB(255, 192, 203), vbWhite, 0)  'pink  
 	Set FontStar2= FlexDMD.NewFont("udmd-f7by13-star2.fnt", RGB(255, 182, 193), vbWhite, 0)  'light pink
	Set FontStar3= FlexDMD.NewFont("udmd-f7by13-star3.fnt", RGB(219, 112, 147), vbWhite, 0)  'pale violet red
	Set FontStar4= FlexDMD.NewFont("udmd-f7by13-star4.fnt", RGB(255, 105, 180), vbWhite, 0)   'hot pink
	Set FontStar5= FlexDMD.NewFont("udmd-f7by13-star5.fnt", RGB(255, 20, 147), vbWhite, 0)    'deep pink
	Set FontStar6= FlexDMD.NewFont("udmd-f7by13-star6.fnt", RGB(199, 21, 133), vbWhite, 0)    'medium violet red
	Set FontStar7= FlexDMD.NewFont("udmd-f7by13-star7.fnt", RGB(255, 0, 255), vbWhite, 0)    'magenta
	Set FontStar8= FlexDMD.NewFont("udmd-f7by13-star8.fnt", RGB(153, 50, 204), vbWhite, 0)  'dark orchid

	Set FontStarBG1= FlexDMD.NewFont("udmd-f7by13-starBG1.fnt", vbBlack, vbWhite, 0)  'black  
 	Set FontStarBG2= FlexDMD.NewFont("udmd-f7by13-starBG2.fnt", vbBlack, vbWhite, 0)  'black
	Set FontStarBG3= FlexDMD.NewFont("udmd-f7by13-starBG3.fnt", vbBlack, vbWhite, 0)  'black
	Set FontStarBG4= FlexDMD.NewFont("udmd-f7by13-starBG4.fnt", vbBlack, vbWhite, 0)  'black
	Set FontStarBG5= FlexDMD.NewFont("udmd-f7by13-starBG5.fnt", vbBlack, vbWhite, 0)  'black
	Set FontStarBG6= FlexDMD.NewFont("udmd-f7by13-starBG6.fnt", vbBlack, vbWhite, 0)  'black
	Set FontStarBG7= FlexDMD.NewFont("udmd-f7by13-starBG7.fnt", vbBlack, vbWhite, 0)  'black
	Set FontStarBG8= FlexDMD.NewFont("udmd-f7by13-starBG8.fnt", vbBlack, vbWhite, 0)  'black

	Set FontDiag1= FlexDMD.NewFont("udmd-f7by13-diag1.fnt", RGB(255, 0, 0), vbWhite, 0)      'red 
	Set FontDiag2= FlexDMD.NewFont("udmd-f7by13-diag2.fnt", RGB(255, 0, 0), vbWhite, 0)      'red 
	Set FontDiag3= FlexDMD.NewFont("udmd-f7by13-diag3.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontDiag4= FlexDMD.NewFont("udmd-f7by13-diag4.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontDiag5= FlexDMD.NewFont("udmd-f7by13-diag5.fnt", RGB(0, 255, 0), vbWhite, 0)      'lime
	Set FontDiag6= FlexDMD.NewFont("udmd-f7by13-diag6.fnt", RGB(0, 128, 0), vbWhite, 0)      'green
	Set FontDiag7= FlexDMD.NewFont("udmd-f7by13-diag7.fnt", RGB(0, 128, 0), vbWhite, 0)      'green 
	Set FontDiag8= FlexDMD.NewFont("udmd-f7by13-diag8.fnt", RGB(255, 0, 0), vbWhite, 0)      'red 
	Set FontDiag9= FlexDMD.NewFont("udmd-f7by13-diag9.fnt", RGB(255, 0, 0), vbWhite, 0)      'red 
	Set FontDiag10= FlexDMD.NewFont("udmd-f7by13-diag10.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontDiag11= FlexDMD.NewFont("udmd-f7by13-diag11.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontDiag12= FlexDMD.NewFont("udmd-f7by13-diag12.fnt", RGB(0, 255, 0), vbWhite, 0)      'lime
	Set FontDiag13= FlexDMD.NewFont("udmd-f7by13-diag13.fnt", RGB(0, 255, 0), vbWhite, 0)      'lime
	Set FontDiag14= FlexDMD.NewFont("udmd-f7by13-diag14.fnt", RGB(0, 128, 0), vbWhite, 0)      'green
	Set FontDiag15= FlexDMD.NewFont("udmd-f7by13-diag15.fnt", RGB(0, 128, 0), vbWhite, 0)      'green
	Set FontDiag16= FlexDMD.NewFont("udmd-f7by13-diag16.fnt", RGB(0, 100, 0), vbWhite, 0)      'dark green
	Set FontDiag17= FlexDMD.NewFont("udmd-f7by13-diag17.fnt", RGB(0, 100, 0), vbWhite, 0)      'dark green

	Set FontDiagBG1= FlexDMD.NewFont("udmd-f7by13-diagBG1.fnt", vbBlack, vbWhite, 0)  'black  
	Set FontDiagBG2= FlexDMD.NewFont("udmd-f7by13-diagBG2.fnt", vbBlack, vbWhite, 0)  'black  
	Set FontDiagBG3= FlexDMD.NewFont("udmd-f7by13-diagBG3.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagBG4= FlexDMD.NewFont("udmd-f7by13-diagBG4.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagBG5= FlexDMD.NewFont("udmd-f7by13-diagBG5.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagBG6= FlexDMD.NewFont("udmd-f7by13-diagBG6.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagBG7= FlexDMD.NewFont("udmd-f7by13-diagBG7.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagBG8= FlexDMD.NewFont("udmd-f7by13-diagBG8.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagBG9= FlexDMD.NewFont("udmd-f7by13-diagBG9.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagBG10= FlexDMD.NewFont("udmd-f7by13-diagBG10.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagBG11= FlexDMD.NewFont("udmd-f7by13-diagBG11.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagBG12= FlexDMD.NewFont("udmd-f7by13-diagBG12.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagBG13= FlexDMD.NewFont("udmd-f7by13-diagBG13.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagBG14= FlexDMD.NewFont("udmd-f7by13-diagBG14.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagBG15= FlexDMD.NewFont("udmd-f7by13-diagBG15.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagBG16= FlexDMD.NewFont("udmd-f7by13-diagBG16.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagBG17= FlexDMD.NewFont("udmd-f7by13-diagBG17.fnt", vbBlack, vbWhite, 0)  'black 

	Set FontDiagL1= FlexDMD.NewFont("udmd-f7by13-diagL1.fnt", RGB(255, 0, 0), vbWhite, 0)      'red  
	Set FontDiagL2= FlexDMD.NewFont("udmd-f7by13-diagL2.fnt", RGB(255, 0, 0), vbWhite, 0)      'red 
	Set FontDiagL3= FlexDMD.NewFont("udmd-f7by13-diagL3.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontDiagL4= FlexDMD.NewFont("udmd-f7by13-diagL4.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontDiagL5= FlexDMD.NewFont("udmd-f7by13-diagL5.fnt", RGB(0, 255, 0), vbWhite, 0)      'lime
	Set FontDiagL6= FlexDMD.NewFont("udmd-f7by13-diagL6.fnt", RGB(0, 128, 0), vbWhite, 0)      'green
	Set FontDiagL7= FlexDMD.NewFont("udmd-f7by13-diagL7.fnt", RGB(0, 128, 0), vbWhite, 0)      'green 
	Set FontDiagL8= FlexDMD.NewFont("udmd-f7by13-diagL8.fnt", RGB(255, 0, 0), vbWhite, 0)      'red 
	Set FontDiagL9= FlexDMD.NewFont("udmd-f7by13-diagL9.fnt", RGB(255, 0, 0), vbWhite, 0)      'red   
	Set FontDiagL10= FlexDMD.NewFont("udmd-f7by13-diagL10.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow 
	Set FontDiagL11= FlexDMD.NewFont("udmd-f7by13-diagL11.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontDiagL12= FlexDMD.NewFont("udmd-f7by13-diagL12.fnt", RGB(0, 255, 0), vbWhite, 0)      'lime
	Set FontDiagL13= FlexDMD.NewFont("udmd-f7by13-diagL13.fnt", RGB(0, 255, 0), vbWhite, 0)      'lime
	Set FontDiagL14= FlexDMD.NewFont("udmd-f7by13-diagL14.fnt", RGB(0, 128, 0), vbWhite, 0)      'green
	Set FontDiagL15= FlexDMD.NewFont("udmd-f7by13-diagL15.fnt", RGB(0, 128, 0), vbWhite, 0)      'green
	Set FontDiagL16= FlexDMD.NewFont("udmd-f7by13-diagL16.fnt", RGB(0, 100, 0), vbWhite, 0)      'dark green
	Set FontDiagL17= FlexDMD.NewFont("udmd-f7by13-diagL17.fnt", RGB(0, 100, 0), vbWhite, 0)      'dark green

	Set FontDiagLBG1= FlexDMD.NewFont("udmd-f7by13-diagLBG1.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagLBG2= FlexDMD.NewFont("udmd-f7by13-diagLBG2.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagLBG3= FlexDMD.NewFont("udmd-f7by13-diagLBG3.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagLBG4= FlexDMD.NewFont("udmd-f7by13-diagLBG4.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagLBG5= FlexDMD.NewFont("udmd-f7by13-diagLBG5.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagLBG6= FlexDMD.NewFont("udmd-f7by13-diagLBG6.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagLBG7= FlexDMD.NewFont("udmd-f7by13-diagLBG7.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagLBG8= FlexDMD.NewFont("udmd-f7by13-diagLBG8.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagLBG9= FlexDMD.NewFont("udmd-f7by13-diagLBG9.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagLBG10= FlexDMD.NewFont("udmd-f7by13-diagLBG10.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagLBG11= FlexDMD.NewFont("udmd-f7by13-diagLBG11.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagLBG12= FlexDMD.NewFont("udmd-f7by13-diagLBG12.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagLBG13= FlexDMD.NewFont("udmd-f7by13-diagLBG13.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagLBG14= FlexDMD.NewFont("udmd-f7by13-diagLBG14.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagLBG15= FlexDMD.NewFont("udmd-f7by13-diagLBG15.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagLBG16= FlexDMD.NewFont("udmd-f7by13-diagLBG16.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontDiagLBG17= FlexDMD.NewFont("udmd-f7by13-diagLBG17.fnt", vbBlack, vbWhite, 0)  'black 

	Set FontMask1= FlexDMD.NewFont("udmd-f7by13-mask1.fnt", RGB(255, 0, 0), vbWhite, 0)      'red 
	Set FontMask2= FlexDMD.NewFont("udmd-f7by13-mask2.fnt", RGB(255, 0, 0), vbWhite, 0)      'red 
	Set FontMask3= FlexDMD.NewFont("udmd-f7by13-mask3.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontMask4= FlexDMD.NewFont("udmd-f7by13-mask4.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontMask5= FlexDMD.NewFont("udmd-f7by13-mask5.fnt", RGB(0, 255, 0), vbWhite, 0)      'lime
	Set FontMask6= FlexDMD.NewFont("udmd-f7by13-mask6.fnt", RGB(0, 128, 0), vbWhite, 0)      'green
	Set FontMask7= FlexDMD.NewFont("udmd-f7by13-mask7.fnt", RGB(0, 128, 0), vbWhite, 0)      'green
	Set FontMask8= FlexDMD.NewFont("udmd-f7by13-mask8.fnt", RGB(255, 0, 0), vbWhite, 0)      'red
	Set FontMask9= FlexDMD.NewFont("udmd-f7by13-mask9.fnt", RGB(255, 0, 0), vbWhite, 0)      'red
	Set FontMask10= FlexDMD.NewFont("udmd-f7by13-mask10.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontMask11= FlexDMD.NewFont("udmd-f7by13-mask11.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontMask12= FlexDMD.NewFont("udmd-f7by13-mask12.fnt", RGB(0, 255, 0), vbWhite, 0)      'lime
	Set FontMask13= FlexDMD.NewFont("udmd-f7by13-mask13.fnt", RGB(0, 128, 0), vbWhite, 0)      'green

	Set FontMaskBG1= FlexDMD.NewFont("udmd-f7by13-maskBG1.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBG2= FlexDMD.NewFont("udmd-f7by13-maskBG2.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBG3= FlexDMD.NewFont("udmd-f7by13-maskBG3.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBG4= FlexDMD.NewFont("udmd-f7by13-maskBG4.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBG5= FlexDMD.NewFont("udmd-f7by13-maskBG5.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBG6= FlexDMD.NewFont("udmd-f7by13-maskBG6.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBG7= FlexDMD.NewFont("udmd-f7by13-maskBG7.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBG8= FlexDMD.NewFont("udmd-f7by13-maskBG8.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBG9= FlexDMD.NewFont("udmd-f7by13-maskBG9.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBG10= FlexDMD.NewFont("udmd-f7by13-maskBG10.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBG11= FlexDMD.NewFont("udmd-f7by13-maskBG11.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBG12= FlexDMD.NewFont("udmd-f7by13-maskBG12.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBG13= FlexDMD.NewFont("udmd-f7by13-maskBG13.fnt", vbBlack, vbWhite, 0)  'black 

	Set FontMaskCenter1= FlexDMD.NewFont("udmd-f7by13-maskcenter1.fnt", RGB(255, 250, 205), vbWhite, 0)      'lemon chiffon
	Set FontMaskCenter2= FlexDMD.NewFont("udmd-f7by13-maskcenter2.fnt", RGB(255, 250, 205), vbWhite, 0)      'lemon chiffon
	Set FontMaskCenter3= FlexDMD.NewFont("udmd-f7by13-maskcenter3.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontMaskCenter4= FlexDMD.NewFont("udmd-f7by13-maskcenter4.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontMaskCenter5= FlexDMD.NewFont("udmd-f7by13-maskcenter5.fnt", RGB(255, 215, 0), vbWhite, 0)      'gold
	Set FontMaskCenter6= FlexDMD.NewFont("udmd-f7by13-maskcenter6.fnt", RGB(255, 215, 0), vbWhite, 0)      'gold
	Set FontMaskCenter7= FlexDMD.NewFont("udmd-f7by13-maskcenter7.fnt", RGB(255, 165, 0), vbWhite, 0)      'orange
	Set FontMaskCenter8= FlexDMD.NewFont("udmd-f7by13-maskcenter8.fnt", RGB(255, 165, 0), vbWhite, 0)      'orange
	Set FontMaskCenter9= FlexDMD.NewFont("udmd-f7by13-maskcenter9.fnt", RGB(255, 69, 0), vbWhite, 0)      'orange red
	Set FontMaskCenter10= FlexDMD.NewFont("udmd-f7by13-maskcenter10.fnt", RGB(255, 69, 0), vbWhite, 0)      'orange red

	Set FontMaskCenterBG1= FlexDMD.NewFont("udmd-f7by13-maskcenterBG1.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskCenterBG2= FlexDMD.NewFont("udmd-f7by13-maskcenterBG2.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskCenterBG3= FlexDMD.NewFont("udmd-f7by13-maskcenterBG3.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskCenterBG4= FlexDMD.NewFont("udmd-f7by13-maskcenterBG4.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskCenterBG5= FlexDMD.NewFont("udmd-f7by13-maskcenterBG5.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskCenterBG6= FlexDMD.NewFont("udmd-f7by13-maskcenterBG6.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskCenterBG7= FlexDMD.NewFont("udmd-f7by13-maskcenterBG7.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskCenterBG8= FlexDMD.NewFont("udmd-f7by13-maskcenterBG8.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskCenterBG9= FlexDMD.NewFont("udmd-f7by13-maskcenterBG9.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskCenterBG10= FlexDMD.NewFont("udmd-f7by13-maskcenterBG10.fnt", vbBlack, vbWhite, 0)  'black 

	Set FontMaskBounce1= FlexDMD.NewFont("udmd-f7by13-maskbounce1.fnt", RGB(0, 95, 0), vbWhite, 0)      ' dark green
	Set FontMaskBounce2= FlexDMD.NewFont("udmd-f7by13-maskbounce2.fnt", RGB(0, 128, 0), vbWhite, 0)      'green
	Set FontMaskBounce3= FlexDMD.NewFont("udmd-f7by13-maskbounce3.fnt", RGB(0, 128, 0), vbWhite, 0)      'green
	Set FontMaskBounce4= FlexDMD.NewFont("udmd-f7by13-maskbounce4.fnt", RGB(0, 255, 0), vbWhite, 0)      'lime
	Set FontMaskBounce5= FlexDMD.NewFont("udmd-f7by13-maskbounce5.fnt", RGB(0, 255, 0), vbWhite, 0)      'lime
	Set FontMaskBounce6= FlexDMD.NewFont("udmd-f7by13-maskbounce6.fnt", RGB(173, 255, 47), vbWhite, 0)      'green yellow
	Set FontMaskBounce7= FlexDMD.NewFont("udmd-f7by13-maskbounce7.fnt", RGB(173, 255, 47), vbWhite, 0)      'green yellow
	Set FontMaskBounce8= FlexDMD.NewFont("udmd-f7by13-maskbounce8.fnt", RGB(255, 255, 0), vbWhite, 0)    'yellow
	Set FontMaskBounce9= FlexDMD.NewFont("udmd-f7by13-maskbounce9.fnt", RGB(255, 215, 0), vbWhite, 0)      'gold
	Set FontMaskBounce10= FlexDMD.NewFont("udmd-f7by13-maskbounce10.fnt", RGB(255, 215, 0), vbWhite, 0)      'gold
	Set FontMaskBounce11= FlexDMD.NewFont("udmd-f7by13-maskbounce11.fnt", RGB(0, 95, 0), vbWhite, 0)      ' dark green

	Set FontMaskBounceBG1= FlexDMD.NewFont("udmd-f7by13-maskbounceBG1.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBounceBG2= FlexDMD.NewFont("udmd-f7by13-maskbounceBG2.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBounceBG3= FlexDMD.NewFont("udmd-f7by13-maskbounceBG3.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBounceBG4= FlexDMD.NewFont("udmd-f7by13-maskbounceBG4.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBounceBG5= FlexDMD.NewFont("udmd-f7by13-maskbounceBG5.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBounceBG6= FlexDMD.NewFont("udmd-f7by13-maskbounceBG6.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBounceBG7= FlexDMD.NewFont("udmd-f7by13-maskbounceBG7.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBounceBG8= FlexDMD.NewFont("udmd-f7by13-maskbounceBG8.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBounceBG9= FlexDMD.NewFont("udmd-f7by13-maskbounceBG9.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBounceBG10= FlexDMD.NewFont("udmd-f7by13-maskbounceBG10.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBounceBG11= FlexDMD.NewFont("udmd-f7by13-maskbounceBG11.fnt", vbBlack, vbWhite, 0)  'black 

	Set FontMaskBounceH1= FlexDMD.NewFont("udmd-f7by13-maskbounceh1.fnt", RGB(220, 220, 220), vbWhite, 0)      ' gainsboro
	Set FontMaskBounceH2= FlexDMD.NewFont("udmd-f7by13-maskbounceh2.fnt", RGB(211, 211, 211), vbWhite, 0)      ' light gray
	Set FontMaskBounceH3= FlexDMD.NewFont("udmd-f7by13-maskbounceh3.fnt", RGB(211, 211, 211), vbWhite, 0)      ' light gray
	Set FontMaskBounceH4= FlexDMD.NewFont("udmd-f7by13-maskbounceh4.fnt", RGB(192, 192, 192), vbWhite, 0)      ' silver
	Set FontMaskBounceH5= FlexDMD.NewFont("udmd-f7by13-maskbounceh5.fnt", RGB(128, 128, 128), vbWhite, 0)      ' gray
	Set FontMaskBounceH6= FlexDMD.NewFont("udmd-f7by13-maskbounceh6.fnt", RGB(119, 136, 153), vbWhite, 0)      ' light slate gray
	Set FontMaskBounceH7= FlexDMD.NewFont("udmd-f7by13-maskbounceh7.fnt", RGB(119, 136, 153), vbWhite, 0)      ' light slate gray
	Set FontMaskBounceH8= FlexDMD.NewFont("udmd-f7by13-maskbounceh8.fnt", RGB(112, 128, 144), vbWhite, 0)      ' slate gray
	Set FontMaskBounceH9= FlexDMD.NewFont("udmd-f7by13-maskbounceh9.fnt", RGB(112, 128, 144), vbWhite, 0)      ' slate gray

	Set FontMaskBounceHBG1= FlexDMD.NewFont("udmd-f7by13-maskbouncehBG1.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBounceHBG2= FlexDMD.NewFont("udmd-f7by13-maskbouncehBG2.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBounceHBG3= FlexDMD.NewFont("udmd-f7by13-maskbouncehBG3.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBounceHBG4= FlexDMD.NewFont("udmd-f7by13-maskbouncehBG4.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBounceHBG5= FlexDMD.NewFont("udmd-f7by13-maskbouncehBG5.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBounceHBG6= FlexDMD.NewFont("udmd-f7by13-maskbouncehBG6.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBounceHBG7= FlexDMD.NewFont("udmd-f7by13-maskbouncehBG7.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBounceHBG8= FlexDMD.NewFont("udmd-f7by13-maskbouncehBG8.fnt", vbBlack, vbWhite, 0)  'black 
	Set FontMaskBounceHBG9= FlexDMD.NewFont("udmd-f7by13-maskbouncehBG9.fnt", vbBlack, vbWhite, 0)  'black 

	' Default score Display
	Set FontScoreActive = FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", vbWhite, vbWhite, 0)
	Set FontScoreInactive = FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", RGB(128, 128, 128), vbWhite, 0)   'fuschia
	'Set FontBig = FlexDMD.NewFont("FlexDMD.Resources.udmd-f7by13.fnt", vbWhite, vbWhite, 0)
	Set FontBig = FlexDMD.NewFont("udmd-f7by13-color.fnt", vbWhite, vbWhite, 0)	'replaced above line with color gradient font
	Set FontBig2 = FlexDMD.NewFont("FlexDMD.Resources.udmd-f7by13.fnt", vbBlack, vbWhite , 0)	
	Set FontBig3 = FlexDMD.NewFont("FlexDMD.Resources.udmd-f14by26.fnt", RGB(0, 100, 0) , RGB(173, 255, 47), 1)

	Dim scene : Set scene = FlexDMD.NewGroup("Score")
		
	scene.AddActor FlexDMD.NewImage("BG1","bg1.jpg")
	scene.AddActor FlexDMD.NewImage("BG2","bg2.jpg")
	scene.GetImage("BG2").Visible = False
	scene.AddActor FlexDMD.NewImage("BG3","bg3.jpg")
	scene.GetImage("BG3").Visible = False
	scene.AddActor FlexDMD.NewImage("BG4","bg4.jpg")
	scene.GetImage("BG4").Visible = False

	For i = 1 to 4
		scene.AddActor FlexDMD.NewLabel("Score_" & i, FontScoreActive, "")
	Next

	scene.AddActor FlexDMD.NewGroup("Content")
	scene.GetGroup("Content").Clip = True
	scene.GetGroup("Content").SetBounds 47, 0, 81, 32

	Dim title 
	Set title = FlexDMD.NewLabel("Title2", FontBig2, " ")
	scene.GetGroup("Content").AddActor title
	Set title = FlexDMD.NewLabel("Title", FontBig, " ")
	scene.GetGroup("Content").AddActor title

	scene.GetGroup("Content").AddActor FlexDMD.NewLabel("Ball", FontScoreActive, "")
	scene.GetGroup("Content").AddActor FlexDMD.NewLabel("Player", FontScoreActive, "")

	scene.GetGroup("Content").AddActor FlexDMD.NewLabel("Topsplash", FontScoreActive, "")

	'scene.AddActor FlexDMD.NewImage("Mask1","mask1.png")
	'scene.GetImage("Mask1").Visible = False

	scene.AddActor FlexDMD.NewImage("Fist1","fist-facing_left.png")
	scene.GetImage("Fist1").Visible = False
	scene.AddActor FlexDMD.NewImage("Fist2","fist-facing_right.png")
	scene.GetImage("Fist2").Visible = False

	FlexDMD.LockRenderThread
	FlexDMD.RenderMode = 2
	FlexDMD.Stage.RemoveAll
	FlexDMD.Stage.AddActor scene

	FlexDMD.Show = True   'original commented out for now   XXXXXXXXXXXXXXXXXX
	'If VRroom > 0 Then FlexDMD.Show = False Else FlexDMD.Show = True     'XXXXXXXXXXXXXXXXX

	FlexDMD.UnlockRenderThread
End Sub

Dim DMD_Video(100,2)
Dim LastVideo : LastVideo=36 ' change as needed

DMD_Video(1,1)="feta1.gif" : DMD_Video(1,2)= 6360/17  
DMD_Video(2,1)="feta2.gif" : DMD_Video(2,2)= 6360/17
DMD_Video(3,1)="feta3.gif" : DMD_Video(3,2)= 6360/17
DMD_Video(4,1)="quas1.gif" : DMD_Video(4,2)= 1650/17
DMD_Video(5,1)="quas2.gif" : DMD_Video(5,2)= 1650/17
DMD_Video(6,1)="quas3.gif" : DMD_Video(6,2)= 1500/17
DMD_Video(7,1)="quas4.gif" : DMD_Video(7,2)= 1650/17
DMD_Video(8,1)="quas5.gif" : DMD_Video(8,2)= 1650/17
DMD_Video(9,1)="quas6.gif" : DMD_Video(9,2)= 1650/17
DMD_Video(10,1)="quas7.gif" : DMD_Video(10,2)= 1650/17
DMD_Video(11,1)="quas8.gif" : DMD_Video(11,2)= 1200/17
DMD_Video(12,1)="quas9.gif" : DMD_Video(12,2)= 1650/17
DMD_Video(13,1)="quas10.gif" : DMD_Video(13,2)= 1650/17
DMD_Video(14,1)="quas11.gif" : DMD_Video(14,2)= 1650/17
DMD_Video(15,1)="quas12.gif" : DMD_Video(15,2)= 1200/17
DMD_Video(16,1)="gasdrawls1.gif" : DMD_Video(16,2)= 6360/17 
DMD_Video(17,1)="gasdrawls2.gif" : DMD_Video(17,2)= 6360/17
DMD_Video(18,1)="gasdrawls3.gif" : DMD_Video(18,2)= 6360/17
DMD_Video(19,1)="gasdrawls4.gif" : DMD_Video(19,2)= 6360/17
DMD_Video(20,1)="gasdrawlsjackpot1.gif" : DMD_Video(20,2)= 6360/17
DMD_Video(21,1)="gasdrawlsjackpot2.gif" : DMD_Video(21,2)= 6360/17
DMD_Video(22,1)="gasdrawlsjackpot3.gif" : DMD_Video(22,2)= 6330/17
DMD_Video(23,1)="americasmostblunted1.gif" : DMD_Video(23,2)= 6360/17
DMD_Video(24,1)="americasmostblunted2.gif" : DMD_Video(24,2)= 6360/17
DMD_Video(25,1)="americasmostblunted3.gif" : DMD_Video(25,2)= 6360/17
DMD_Video(26,1)="americasmostblunted4.gif" : DMD_Video(26,2)= 6360/17
DMD_Video(27,1)="americasmostbluntedjackpot1.gif" : DMD_Video(27,2)= 6360/17
DMD_Video(28,1)="americasmostbluntedjackpot2.gif" : DMD_Video(28,2)= 6360/17
DMD_Video(29,1)="americasmostbluntedjackpot3.gif" : DMD_Video(29,2)= 6330/17
DMD_Video(30,1)="gazzillionear1.gif" : DMD_Video(30,2)= 6360/17
DMD_Video(31,1)="gazzillionear2.gif" : DMD_Video(31,2)= 6360/17
DMD_Video(32,1)="gazzillionear3.gif" : DMD_Video(32,2)= 6360/17
DMD_Video(33,1)="gazzillionear4.gif" : DMD_Video(33,2)= 6360/17
DMD_Video(34,1)="gazzillionearsuperjackpot1.gif" : DMD_Video(34,2)= 6360/17
DMD_Video(35,1)="gazzillionearsuperjackpot2.gif" : DMD_Video(35,2)= 6360/17
DMD_Video(36,1)="gazzillionearsuperjackpot3.gif" : DMD_Video(36,2)= 6330/17

DMD_Video(37,1)="intro1.gif" : DMD_Video(37,2)= 70290/17
DMD_Video(38,1)="intro2.gif" : DMD_Video(38,2)= 42930/17
DMD_Video(39,1)="intro3.gif" : DMD_Video(39,2)= 44310/17
DMD_Video(40,1)="intro4.gif" : DMD_Video(40,2)= 42000/17
DMD_Video(41,1)="intro5.gif" : DMD_Video(41,2)= 48060/17

Dim PlayVideo, OldVideo, DMD_VideoTimer, introtexton, introspeed,introblink
Dim label,i

Dim DMDFire 
Dim DMDFireBG 
Dim DMDCheckerboard  
Dim DMDCheckerboardBG     
Dim DMDExp
Dim DMDExpBG
Dim DMDImp
Dim DMDImpBG
Dim DMDVerticalThick 
Dim DMDVerticalThickBG 
Dim DMDVerticalThickL 
Dim DMDVerticalThickLBG 
Dim DMDHorizontalThick
Dim DMDHorizontalThickBG
Dim DMDHorizontalThickUp
Dim DMDHorizontalThickUpBG
Dim DMDSpiral
Dim DMDSpiralBG
Dim DMDSpiralL
Dim DMDSpiralLBG
Dim DMDStar
Dim DMDStarBG
Dim DMDStarL
Dim DMDStarLBG
Dim DMDDiag
Dim DMDDiagBG
Dim DMDDiagTwo
Dim DMDDiagTwoBG
Dim DMDDiagL
Dim DMDDiagLBG
Dim DMDDiagLTwo
Dim DMDDiagLTwoBG
Dim DMDMask
Dim DMDMaskBG
Dim DMDMaskL
Dim DMDMaskLBG
Dim DMDMaskCenter
Dim DMDMaskCenterBG
Dim DMDMaskBounce
Dim DMDMaskBounceBG
Dim DMDMaskBounceH
Dim DMDMaskBounceHBG
Dim DMDBgBlink
Dim DMDFistLeft
Dim DMDFistRight
Dim DMDMaskLeft
Dim DMDMaskRight
Dim DMDSmoke

Dim DMDTextOnScore
Dim DMDTextDisplayTime
Dim DMDTextEffect
Sub DMDBigText ( tex,time,effect)
	DMDTextOnScore=tex
	DMDTextDisplayTime = Frame+ time
	DMDTextEffect=effect
End Sub

Dim TopSplashText
Dim TopSplashTime
Dim TopSplashEffect
Sub DMDTopSplash ( tex,time,effect)
	TopSplashText=tex
	TopSplashTime=Frame+time
	TopSplashEffect=effect
End Sub

' Flex on vrroom and playfield runs this one    'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
Sub FlexFlasher
		Dim DMDp
		DMDp = FlexDMD.DmdColoredPixels
		If Not IsEmpty(DMDp) Then
			DMDWidth = FlexDMD.Width
			DMDHeight = FlexDMD.Height
			DMDColoredPixels = DMDp
		End If
End Sub                                          'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

Sub DMDTimer_Timer

	Frame=Frame+1

	FlexDMD.LockRenderThread

	If VRroom>0 then FlexFlasher  'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

if Not hsbModeActive Then
	If PlayVideo>0 Then
		If PLayvideo> OldVideo Then
			If Not OldVideo=0 Then 
				FlexDMD.Stage.GetVideo("vidvid1").remove()
				OldVideo=0
				If introtexton=1 Then
					introtexton=0
					FlexDMD.Stage.GetLabel("introtext").remove()
				End If
			End If
			If PlayVideo < 100 Then
				OldVideo=PlayVideo
				DMD_VideoTimer = Frame + DMD_Video(PlayVideo,2)
				FlexDMD.Stage.Getgroup("Score").AddActor FlexDMD.NewVideo("vidvid1",DMD_Video(PlayVideo,1))
				If playvideo>36 and introtexton=0 Then
					introtexton=1
					if introspeed=0 Then introspeed=32
					FlexDMD.Stage.Getgroup("Score").AddActor FlexDMD.NewLabel("introtext", FontBig3, "MF DOOM")
					FlexDMD.Stage.GetLabel("introtext").SetAlignedPosition 65, 17, FlexDMD_Align_Center
				End If
			Else
				introspeed=0
			End If
		End If
		PlayVideo=0
	End If
	If int(DMD_VideoTimer)=int(Frame) Then 
		If Not OldVideo=0 Then
			FlexDMD.Stage.GetVideo("vidvid1").remove()
			If OldVideo>36 Then
				Playvideo=37+int(rnd(1)*5)
				If introtexton=1 Then
					introtexton=0
					FlexDMD.Stage.GetLabel("introtext").remove()
				End If	
			End If
			OldVideo=0
		End If
	End If

	If introtexton=1 Then
		introblink=introblink+1 : if introblink>introspeed Then  introspeed=introspeed+5 : introblink=0
		If introblink=1 Then FlexDMD.Stage.GetLabel("introtext").Text = "MF DOOM"
		If introblink = int(introspeed/2) Then FlexDMD.Stage.GetLabel("introtext").Text = " "
	End If


'		debug.print "timer" & DMD_VideoTimer & "  FRAME=" & Frame & "   OldVideo=" & OldVideo

'	If PlayVideo>0 Then
'		debug.print "vidnr"=PlayVideo
'		If DMD_Video(PlayVideo,1) < Frame Then
'			DMD_Video(PlayVideo,1)= Frame+DMD_Video(PlayVideo,2)
'			FlexDMD.Stage.GetVideo("vidvid"&PlayVideo).Visible=True
'		End If
'		PlayVideo=0
'	End If
'	For i = 1 To LastVideo
'		If DMD_Video(i,1)=Frame Then FlexDMD.Stage.GetVideo("vidvid"&i).Visible=False
'	Next
End If


	If (Frame Mod 2) = 0 Then
		For i = 1 to 4
			if i <= PlayersPlayingGame Then
				Set label = FlexDMD.Stage.GetLabel("Score_" & i)
				label.visible=True
				If i = CurrentPlayer Then
					label.Font = FontScoreActive
				Else
					label.Font = FontScoreInactive
				End If
			Else
				Set label = FlexDMD.Stage.GetLabel("Score_" & i)
				label.visible=False					
			End If
			label.Text = FormatNumber(Score(i), 0, -1, 0, -1)
			label.SetAlignedPosition 45, 1 + (i - 1) * 6, FlexDMD_Align_TopRight
		Next

		'bigscore
		Set label = FlexDMD.Stage.GetLabel("Title2")

		If DMDFireBG > 0 Then
			DMDScoreAnimBG
		Elseif DMDCheckerboardBG > 0 Then
			DMDScoreAnim2BG
		Elseif DMDExpBG > 0 Then
			DMDScoreAnim3BG
		Elseif DMDImpBg > 0 Then
			DMDScoreAnim4BG
		Elseif DMDVerticalThickBG > 0 Then
			DMDScoreAnim5BG
		Elseif DMDVerticalThickLBG > 0 Then
			DMDScoreAnim6BG
		Elseif DMDHorizontalThickBG > 0 Then
			DMDScoreAnim7BG
		Elseif DMDHorizontalThickUpBG > 0 Then
			DMDScoreAnim8BG
		Elseif DMDSpiralBG > 0 Then
			DMDScoreAnim9BG
		Elseif DMDSpiralLBG > 0 Then
			DMDScoreAnim10BG
		Elseif DMDStarBG > 0 Then
			DMDScoreAnim11BG 
		Elseif DMDStarLBG > 0 Then
			DMDScoreAnim12BG
		Elseif DMDDiagBG > 0 Then
			DMDScoreAnim13BG
		Elseif DMDDiagTwoBG > 0 Then
			DMDScoreAnim14BG
		Elseif DMDDiagLBg > 0 Then
			DMDScoreAnim15BG
		Elseif DMDDiagLTwoBG > 0 Then
			DMDScoreAnim16BG
		Elseif DMDMaskBG > 0 Then
			DMDScoreAnim17BG
		Elseif DMDMaskLBG > 0 Then
			DMDScoreAnim18BG
		Elseif DMDMaskCenterBG > 0 Then
			DMDScoreAnim19BG
		Elseif DMDMaskBounceBG > 0 Then
			DMDScoreAnim20BG
		Elseif DMDMaskBounceHBG > 0 Then
			DMDScoreAnim21BG
		Else
			label.Font = FontBig2
		End If
		
		If DMDTextDisplayTime>Frame Then
			If DMDTextEffect=1 And (frame mod 20) >10 Then label.Text = " "  Else label.Text = DMDTextOnScore
		Else
			label.Text = FormatNumber(Score(currentplayer), 0, -1, 0, -1)
		End If
		label.SetAlignedPosition 41, 17, FlexDMD_Align_Center

		Set label = FlexDMD.Stage.GetLabel("Title")

		If DMDFire > 0 Then
			DMDScoreAnim
		Elseif DMDCheckerboard > 0 Then
			DMDScoreAnim2
		Elseif DMDExp > 0 Then
			DMDScoreAnim3
		Elseif DMDImp > 0 Then
			DMDScoreAnim4
		Elseif DMDVerticalThick > 0 Then
			DMDScoreAnim5
		Elseif DMDVerticalThickL > 0 Then
			DMDScoreAnim6
		Elseif DMDHorizontalThick > 0 Then
			DMDScoreAnim7
		Elseif DMDHorizontalThickUp > 0 Then
			DMDScoreAnim8
		Elseif DMDSpiral > 0 Then
			DMDScoreAnim9
		Elseif DMDSpiralL > 0 Then
			DMDScoreAnim10
		Elseif DMDStar > 0 Then
			DMDScoreAnim11 
		Elseif DMDStarL > 0 Then
			DMDScoreAnim12
		Elseif DMDDiag > 0 Then
			DMDScoreAnim13
		Elseif DMDDiagTwo > 0 Then
			DMDScoreAnim14
		Elseif DMDDiagL > 0 Then
			DMDScoreAnim15
		Elseif DMDDiagLTwo > 0 Then
			DMDScoreAnim16
		Elseif DMDMask > 0 Then
			DMDScoreAnim17
		Elseif DMDMaskL > 0 Then
			DMDScoreAnim18
		Elseif DMDMaskCenter > 0 Then
			DMDScoreAnim19
		Elseif DMDMaskBounce > 0 Then
			DMDScoreAnim20
		Elseif DMDMaskBounceH > 0 Then
			DMDScoreAnim21
		Else
			label.Font = FontBig
		End If

		If DMDBgBlink > 0 Then DMDBackgroundblink
		
		If DMDTextDisplayTime>Frame Then
			If DMDTextEffect=1 And (frame mod 20) >10 Then label.Text = " "  Else label.Text = DMDTextOnScore
		Else
			label.Text = FormatNumber(Score(currentplayer), 0, -1, 0, -1)
		End If

		label.SetAlignedPosition 40, 16, FlexDMD_Align_Center
	End If

	If TopSplashTime > Frame Then
'		If TopSplashEffect=2 Then FlexDMD.Stage.GetLabel("Topsplash").Font = ???
		If TopSplashEffect=1 And (frame mod 20)>10 Then
			FlexDMD.Stage.GetLabel("Topsplash").Text = " "
		Else
			FlexDMD.Stage.GetLabel("Topsplash").Text = TopSplashText
		End If
		FlexDMD.Stage.GetLabel("Topsplash").SetAlignedPosition 41, 4, FlexDMD_Align_Center  ' might need change the x (41) to center Item
		FlexDMD.Stage.GetLabel("Topsplash").Visible=True
	Else
		FlexDMD.Stage.GetLabel("Topsplash").Visible=False
	End If

	CurrBall = 4 - BallsRemaining(CurrentPlayer)
	if CurrBall > 3 Then CurrBall = 3
		FlexDMD.Stage.GetLabel("Ball").Text = "Ball " & CurrBall
		FlexDMD.Stage.GetLabel("Ball").SetAlignedPosition 0, 33, FlexDMD_Align_BottomLeft

		FlexDMD.Stage.GetLabel("Player").Text = "Villain " & (CurrentPlayer)
		FlexDMD.Stage.GetLabel("Player").SetAlignedPosition 81, 33, FlexDMD_Align_BottomRight

	If DMDFistLeft>0 Then FistLeft
	If DMDFistRight>0 Then FistRight

	FlexDMD.UnlockRenderThread
End Sub

Sub FistLeft
	Dim title,af,list
	DMDFistLeft=DMDFistLeft+1
	If DMDFistLeft=2 Then
		DMDFistLeft=3
		Set title= FlexDMD.Stage.GetImage("Fist2")
		Set af = title.ActionFactory
		Set list = af.Sequence()
		list.Add af.MoveTo(-90, 0, 0)
		list.Add af.Wait(0.1)
		list.add af.show(True)
		list.Add af.MoveTo(0, 0, 0.25) '  0.3 speed  lower = faster 
		list.Add af.Wait(0.1)
		list.Add af.MoveTo(-90, 0, 0.4) 
		list.add af.show(False)
		title.AddAction af.Repeat(list, 1)
	End If
	If DMDFistLeft>70 Then DMDFistLeft=0
End sub

Sub FistRight
	Dim title,af,list
	DMDFistRight=DMDFistRight+1
	If DMDFistRight=2 Then
		DMDFistRight=3
		Set title= FlexDMD.Stage.GetImage("Fist1")
		Set af = title.ActionFactory
		Set list = af.Sequence()
		list.Add af.MoveTo(90, 0, 0)
		list.Add af.Wait(0.1)
		list.add af.show(True)
		list.Add af.MoveTo(0, 0, 0.25)  '  0.3 speed  lower = faster 
		list.Add af.Wait(0.1)
		list.Add af.MoveTo(90, 0, 0.4)
		list.add af.show(False)
		title.AddAction af.Repeat(list, 1)
	End If
	If DMDFistRight>70 Then DMDFistRight=0
End sub

Sub DMDBackgroundblink
	DMDBgBlink=DMDBgBlink+1
	Select Case DMDBgBlink
		Case 2 : FlexDMD.stage.GetImage("BG2").visible = True :  FlexDMD.stage.GetImage("BG1").visible= False
		Case 3 : FlexDMD.stage.GetImage("BG3").visible = True :  FlexDMD.stage.GetImage("BG2").visible= False
		Case 4 : FlexDMD.stage.GetImage("BG4").visible = True :  FlexDMD.stage.GetImage("BG3").visible= False
		Case 7 : FlexDMD.stage.GetImage("BG3").visible = True :  FlexDMD.stage.GetImage("BG4").visible= False
		Case 8 : FlexDMD.stage.GetImage("BG2").visible = True :  FlexDMD.stage.GetImage("BG3").visible= False
		Case 9 : FlexDMD.stage.GetImage("BG1").visible = True :  FlexDMD.stage.GetImage("BG2").visible= False
	End Select
End Sub

Sub DMDScoreAnim    
	DMDFire=DMDFire-2
	If DMDFire < 0 Then DMDFire = 0
	i=Int(rnd(1)*9)+1
	Select Case i
		case 1: label.font = FontFire1
		case 2: label.font = FontFire2
		case 4: label.font = FontFire3
		case 3: label.font = FontFire4
		case 5: label.font = FontFire5
		case 6: label.font = FontFire6
		case 7: label.font = FontFire7
		case 8: label.font = FontFire8
		case 9: label.font = FontFire9
	End Select
End Sub

Sub DMDScoreAnimBG     
	DMDFireBG=DMDFireBG-2
	If DMDFireBG < 0 Then DMDFireBG = 0
	i=Int(rnd(1)*9)+1
	Select Case i
		case 1: label.font = FontFireBG1
		case 2: label.font = FontFireBG2
		case 4: label.font = FontFireBG3
		case 3: label.font = FontFireBG4
		case 5: label.font = FontFireBG5
		case 6: label.font = FontFireBG6
		case 7: label.font = FontFireBG7
		case 8: label.font = FontFireBG8
		case 9: label.font = FontFireBG9
	End Select
End Sub

Dim DMDCheckerCounter
Sub DMDScoreAnim2                                              
	DMDCheckerboard=DMDCheckerboard-2
	If DMDCheckerboard < 0 Then DMDCheckerboard = 0
	DMDCheckerCounter=DMDCheckerCounter+1
	If DMDCheckerCounter>2 THEN DMDCheckerCounter = 1
	Select Case DMDCheckerCounter
		case 1: label.font = FontCheckerboard1
		case 2: label.font = FontCheckerboard2
	End Select
End Sub

Dim DMDCheckerBGCounter
Sub DMDScoreAnim2BG                                              
	DMDCheckerboardBG=DMDCheckerboardBG-2
	If DMDCheckerboardBG < 0 Then DMDCheckerboardBG = 0
	DMDCheckerBGCounter=DMDCheckerBGCounter+1
	If DMDCheckerBGCounter>2 THEN DMDCheckerBGCounter = 1
	Select Case DMDCheckerBGCounter
		case 1: label.font = FontCheckerboardBG1
		case 2: label.font = FontCheckerboardBG2
	End Select
End Sub

Dim DMDExpCounter
Sub DMDScoreAnim3                                               
	DMDExp=DMDExp-2
	If DMDExp < 0 Then DMDExp = 0
	DMDExpCounter = DMDExpCounter +1
	If DMDExpCounter > 7 THEN DMDExpCounter = 1
	Select Case DMDExpCounter
		case 1: label.font = FontExp1
		case 2: label.font = FontExp2
		case 3: label.font = FontExp3
		case 4: label.font = FontExp4
		case 5: label.font = FontExp5
		case 6: label.font = FontExp6
		case 7: label.font = FontExp7
	End Select
End Sub

Dim DMDExpBGCounter
Sub DMDScoreAnim3BG                                               
	DMDExpBG=DMDExpBG-2
	If DMDExpBG < 0 Then DMDExpBG = 0
	DMDExpBGCounter = DMDExpBGCounter +1
	If DMDExpBGCounter > 7 THEN DMDExpBGCounter = 1
	Select Case DMDExpBGCounter
		case 1: label.font = FontExpBG1
		case 2: label.font = FontExpBG2
		case 3: label.font = FontExpBG3
		case 4: label.font = FontExpBG4
		case 5: label.font = FontExpBG5
		case 6: label.font = FontExpBG6
		case 7: label.font = FontExpBG7
	End Select
End Sub

Dim DMDImpCounter
Sub DMDScoreAnim4                                               
	DMDImp=DMDImp-2
	If DMDImp < 0 Then DMDImp = 0
	DMDImpCounter = DMDImpCounter +1
	If DMDImpCounter > 7 THEN DMDImpCounter = 1
	Select Case DMDImpCounter
		case 1: label.font = FontImp1
		case 2: label.font = FontImp2
		case 3: label.font = FontImp3
		case 4: label.font = FontImp4
		case 5: label.font = FontImp5
		case 6: label.font = FontImp6
		case 7: label.font = FontImp7
	End Select
End Sub

Dim DMDImpBGCounter
Sub DMDScoreAnim4BG                                               
	DMDImpBG=DMDImpBG-2
	If DMDImpBG < 0 Then DMDImpBG = 0
	DMDImpBGCounter = DMDImpBGCounter +1
	If DMDImpBGCounter > 7 THEN DMDImpBGCounter = 1
	Select Case DMDImpBGCounter
		case 1: label.font = FontImpBG1
		case 2: label.font = FontImpBG2
		case 3: label.font = FontImpBG3
		case 4: label.font = FontImpBG4
		case 5: label.font = FontImpBG5
		case 6: label.font = FontImpBG6
		case 7: label.font = FontImpBG7
	End Select
End Sub

Dim DMDVerticalThickCounter
Sub DMDScoreAnim5                                               
	DMDVerticalThick=DMDVerticalThick-2
	If DMDVerticalThick < 0 Then DMDVerticalThick = 0
	DMDVerticalThickCounter = DMDVerticalThickCounter +1
	If DMDVerticalThickCounter > 8 THEN DMDVerticalThickCounter = 1
	Select Case DMDVerticalThickCounter
		case 1: label.font = FontVerticalThick1
		case 2: label.font = FontVerticalThick2
		case 3: label.font = FontVerticalThick3
		case 4: label.font = FontVerticalThick4
		case 5: label.font = FontVerticalThick5
		case 6: label.font = FontVerticalThick6
		case 7: label.font = FontVerticalThick7
		case 8: label.font = FontVerticalThick8
	End Select
End Sub

Dim DMDVerticalThickBGCounter
Sub DMDScoreAnim5BG                                               
	DMDVerticalThickBG=DMDVerticalThickBG-2
	If DMDVerticalThickBG < 0 Then DMDVerticalThickBG = 0
	DMDVerticalThickBGCounter = DMDVerticalThickBGCounter +1
	If DMDVerticalThickBGCounter > 8 THEN DMDVerticalThickBGCounter = 1
	Select Case DMDVerticalThickBGCounter
		case 1: label.font = FontVerticalThickBG1
		case 2: label.font = FontVerticalThickBG2
		case 3: label.font = FontVerticalThickBG3
		case 4: label.font = FontVerticalThickBG4
		case 5: label.font = FontVerticalThickBG5
		case 6: label.font = FontVerticalThickBG6
		case 7: label.font = FontVerticalThickBG7
		case 8: label.font = FontVerticalThickBG8
	End Select
End Sub

Dim DMDVerticalThickLCounter
Sub DMDScoreAnim6                                               
	DMDVerticalThickL=DMDVerticalThickL-2
	If DMDVerticalThickL < 0 Then DMDVerticalThickL = 0
	DMDVerticalThickLCounter = DMDVerticalThickLCounter +1
	If DMDVerticalThickLCounter > 8 THEN DMDVerticalThickLCounter = 1
	Select Case DMDVerticalThickLCounter
		case 1: label.font = FontVerticalThick8
		case 2: label.font = FontVerticalThick7
		case 3: label.font = FontVerticalThick6
		case 4: label.font = FontVerticalThick5
		case 5: label.font = FontVerticalThick4
		case 6: label.font = FontVerticalThick3
		case 7: label.font = FontVerticalThick2
		case 8: label.font = FontVerticalThick1
	End Select
End Sub

Dim DMDVerticalThickLBGCounter
Sub DMDScoreAnim6BG                                               
	DMDVerticalThickLBG=DMDVerticalThickLBG-2
	If DMDVerticalThickLBG < 0 Then DMDVerticalThickLBG = 0
	DMDVerticalThickLBGCounter = DMDVerticalThickLBGCounter +1
	If DMDVerticalThickLBGCounter > 8 THEN DMDVerticalThickLBGCounter = 1
	Select Case DMDVerticalThickLBGCounter
		case 1: label.font = FontVerticalThickBG8
		case 2: label.font = FontVerticalThickBG7
		case 3: label.font = FontVerticalThickBG6
		case 4: label.font = FontVerticalThickBG5
		case 5: label.font = FontVerticalThickBG4
		case 6: label.font = FontVerticalThickBG3
		case 7: label.font = FontVerticalThickBG2
		case 8: label.font = FontVerticalThickBG1
	End Select
End Sub

Dim DMDHorizontalThickCounter
Sub DMDScoreAnim7                                               
	DMDHorizontalThick=DMDHorizontalThick-2
	If DMDHorizontalThick < 0 Then DMDHorizontalThick = 0
	DMDHorizontalThickCounter = DMDHorizontalThickCounter +1
	If DMDHorizontalThickCounter > 14 THEN DMDHorizontalThickCounter = 1
	Select Case DMDHorizontalThickCounter
		case 1: label.font = FontHorizontalThick1
		case 2: label.font = FontHorizontalThick2
		case 3: label.font = FontHorizontalThick3
		case 4: label.font = FontHorizontalThick4
		case 5: label.font = FontHorizontalThick5
		case 6: label.font = FontHorizontalThick6
		case 7: label.font = FontHorizontalThick7
		case 8: label.font = FontHorizontalThick8
		case 9: label.font = FontHorizontalThick9
		case 10: label.font = FontHorizontalThick10
		case 11: label.font = FontHorizontalThick11
		case 12: label.font = FontHorizontalThick12
		case 13: label.font = FontHorizontalThick13
		case 14: label.font = FontHorizontalThick14
	End Select
End Sub

Dim DMDHorizontalThickBGCounter
Sub DMDScoreAnim7BG                                               
	DMDHorizontalThickBG=DMDHorizontalThickBG-2
	If DMDHorizontalThickBG < 0 Then DMDHorizontalThickBG = 0
	DMDHorizontalThickBGCounter = DMDHorizontalThickBGCounter +1
	If DMDHorizontalThickBGCounter > 14 THEN DMDHorizontalThickBGCounter = 1
	Select Case DMDHorizontalThickBGCounter
		case 1: label.font = FontHorizontalThickBG1
		case 2: label.font = FontHorizontalThickBG2
		case 3: label.font = FontHorizontalThickBG3
		case 4: label.font = FontHorizontalThickBG4
		case 5: label.font = FontHorizontalThickBG5
		case 6: label.font = FontHorizontalThickBG6
		case 7: label.font = FontHorizontalThickBG7
		case 8: label.font = FontHorizontalThickBG8
		case 9: label.font = FontHorizontalThickBG9
		case 10: label.font = FontHorizontalThickBG10
		case 11: label.font = FontHorizontalThickBG11
		case 12: label.font = FontHorizontalThickBG12
		case 13: label.font = FontHorizontalThickBG13
		case 14: label.font = FontHorizontalThickBG14
	End Select
End Sub

Dim DMDHorizontalThickUpCounter
Sub DMDScoreAnim8                                               
	DMDHorizontalThickUp=DMDHorizontalThickUp-2
	If DMDHorizontalThickUp < 0 Then DMDHorizontalThickUp = 0
	DMDHorizontalThickUpCounter = DMDHorizontalThickUpCounter +1
	If DMDHorizontalThickUpCounter > 14 THEN DMDHorizontalThickUpCounter = 1
	Select Case DMDHorizontalThickUpCounter
		case 1: label.font = FontHorizontalThick14
		case 2: label.font = FontHorizontalThick13
		case 3: label.font = FontHorizontalThick12
		case 4: label.font = FontHorizontalThick11
		case 5: label.font = FontHorizontalThick10
		case 6: label.font = FontHorizontalThick9
		case 7: label.font = FontHorizontalThick8
		case 8: label.font = FontHorizontalThick7
		case 9: label.font = FontHorizontalThick6
		case 10: label.font = FontHorizontalThick5
		case 11: label.font = FontHorizontalThick4
		case 12: label.font = FontHorizontalThick3
		case 13: label.font = FontHorizontalThick2
		case 14: label.font = FontHorizontalThick1
	End Select
End Sub

Dim DMDHorizontalThickUpBGCounter
Sub DMDScoreAnim8BG                                               
	DMDHorizontalThickUpBG=DMDHorizontalThickUpBG-2
	If DMDHorizontalThickUpBG < 0 Then DMDHorizontalThickUpBG = 0
	DMDHorizontalThickUpBGCounter = DMDHorizontalThickUpBGCounter +1
	If DMDHorizontalThickUpBGCounter > 14 THEN DMDHorizontalThickUpBGCounter = 1
	Select Case DMDHorizontalThickUpBGCounter
		case 1: label.font = FontHorizontalThickBG14
		case 2: label.font = FontHorizontalThickBG13
		case 3: label.font = FontHorizontalThickBG12
		case 4: label.font = FontHorizontalThickBG11
		case 5: label.font = FontHorizontalThickBG10
		case 6: label.font = FontHorizontalThickBG9
		case 7: label.font = FontHorizontalThickBG8
		case 8: label.font = FontHorizontalThickBG7
		case 9: label.font = FontHorizontalThickBG6
		case 10: label.font = FontHorizontalThickBG5
		case 11: label.font = FontHorizontalThickBG4
		case 12: label.font = FontHorizontalThickBG3
		case 13: label.font = FontHorizontalThickBG2
		case 14: label.font = FontHorizontalThickBG1
	End Select
End Sub

Dim DMDSpiralCounter
Sub DMDScoreAnim9                                               
	DMDSpiral=DMDSpiral-2
	If DMDSpiral < 0 Then DMDSpiral = 0
	DMDSpiralCounter=DMDSpiralCounter +1
	If DMDSpiralCounter > 7 THEN DMDSpiralCounter = 1
	Select Case DMDSpiralCounter
		case 1: label.font = FontSpiral1
		case 2: label.font = FontSpiral2
		case 3: label.font = FontSpiral3
		case 4: label.font = FontSpiral4
		case 5: label.font = FontSpiral5
		case 6: label.font = FontSpiral6
		case 7: label.font = FontSpiral7
	End Select
End Sub

Dim DMDSpiralBGCounter
Sub DMDScoreAnim9BG                                               
	DMDSpiralBG=DMDSpiralBG-2
	If DMDSpiralBG < 0 Then DMDSpiralBG = 0
	DMDSpiralBGCounter=DMDSpiralBGCounter +1
	If DMDSpiralBGCounter > 7 THEN DMDSpiralBGCounter = 1
	Select Case DMDSpiralBGCounter
		case 1: label.font = FontSpiralBG1
		case 2: label.font = FontSpiralBG2
		case 3: label.font = FontSpiralBG3
		case 4: label.font = FontSpiralBG4
		case 5: label.font = FontSpiralBG5
		case 6: label.font = FontSpiralBG6
		case 7: label.font = FontSpiralBG7
	End Select
End Sub

Dim DMDSpiralLCounter
Sub DMDScoreAnim10                                               
	DMDSpiralL=DMDSpiralL-2
	If DMDSpiralL < 0 Then DMDSpiralL = 0
	DMDSpiralLCounter=DMDSpiralLCounter +1
	If DMDSpiralLCounter > 7 THEN DMDSpiralLCounter = 1
	Select Case DMDSpiralLCounter
		case 1: label.font = FontSpiralL1
		case 2: label.font = FontSpiralL2
		case 3: label.font = FontSpiralL3
		case 4: label.font = FontSpiralL4
		case 5: label.font = FontSpiralL5
		case 6: label.font = FontSpiralL6
		case 7: label.font = FontSpiralL7
	End Select
End Sub

Dim DMDSpiralLBGCounter
Sub DMDScoreAnim10BG                                               
	DMDSpiralLBG=DMDSpiralLBG-2
	If DMDSpiralLBG < 0 Then DMDSpiralLBG = 0
	DMDSpiralLBGCounter=DMDSpiralLBGCounter +1
	If DMDSpiralLBGCounter > 7 THEN DMDSpiralLBGCounter = 1
	Select Case DMDSpiralLBGCounter
		case 1: label.font = FontSpiralLBG1
		case 2: label.font = FontSpiralLBG2
		case 3: label.font = FontSpiralLBG3
		case 4: label.font = FontSpiralLBG4
		case 5: label.font = FontSpiralLBG5
		case 6: label.font = FontSpiralLBG6
		case 7: label.font = FontSpiralLBG7
	End Select
End Sub

Dim DMDStarCounter
Sub DMDScoreAnim11                                               
	DMDStar=DMDStar-2
	If DMDStar < 0 Then DMDStar = 0
	DMDStarCounter=DMDStarCounter+1
	If DMDStarCounter>8 THEN DMDStarCounter=1
	Select Case DMDStarCounter
		case 1: label.font = FontStar1
		case 2: label.font = FontStar2
		case 3: label.font = FontStar3
		case 4: label.font = FontStar4
		case 5: label.font = FontStar5
		case 6: label.font = FontStar6
		case 7: label.font = FontStar7
		case 8: label.font = FontStar8
	End Select
End Sub

Dim DMDStarBGCounter      
Sub DMDScoreAnim11BG                                               
	DMDStarBG=DMDStarBG-2
	If DMDStarBG < 0 Then DMDStarBG = 0
	DMDStarBGCounter=DMDStarBGCounter+1
	If DMDStarBGCounter>8 THEN DMDStarBGCounter=1
	Select Case DMDStarBGCounter
		case 1: label.font = FontStarBG1
		case 2: label.font = FontStarBG2
		case 3: label.font = FontStarBG3
		case 4: label.font = FontStarBG4
		case 5: label.font = FontStarBG5
		case 6: label.font = FontStarBG6
		case 7: label.font = FontStarBG7
		case 8: label.font = FontStarBG8
	End Select
End Sub

Dim DMDStarLCounter
Sub DMDScoreAnim12                                               
	DMDStarL=DMDStarL-2
	If DMDStarL < 0 Then DMDStarL = 0
	DMDStarLCounter=DMDStarLCounter+1
	If DMDStarLCounter>8 THEN DMDStarLCounter=1
	Select Case DMDStarLCounter
		case 1: label.font = FontStar8
		case 2: label.font = FontStar7
		case 3: label.font = FontStar6
		case 4: label.font = FontStar5
		case 5: label.font = FontStar4
		case 6: label.font = FontStar3
		case 7: label.font = FontStar2
		case 8: label.font = FontStar1
	End Select
End Sub

Dim DMDStarLBGCounter
Sub DMDScoreAnim12BG                                               
	DMDStarLBG=DMDStarLBG-2
	If DMDStarLBG < 0 Then DMDStarLBG = 0
	DMDStarLBGCounter=DMDStarLBGCounter+1
	If DMDStarLBGCounter>8 THEN DMDStarLBGCounter=1
	Select Case DMDStarLBGCounter
		case 1: label.font = FontStarBG8
		case 2: label.font = FontStarBG7
		case 3: label.font = FontStarBG6
		case 4: label.font = FontStarBG5
		case 5: label.font = FontStarBG4
		case 6: label.font = FontStarBG3
		case 7: label.font = FontStarBG2
		case 8: label.font = FontStarBG1
	End Select
End Sub

Dim DMDDiagCounter
Sub DMDScoreAnim13                                               
	DMDDiag=DMDDiag-2
	If DMDDiag < 0 Then DMDDiag = 0
	DMDDiagCounter=DMDDiagCounter+1
	If DMDDiagCounter>17 THEN DMDDiagCounter=1
	Select Case DMDDiagCounter
		case 1: label.font = FontDiag1
		case 2: label.font = FontDiag2
		case 3: label.font = FontDiag3
		case 4: label.font = FontDiag4
		case 5: label.font = FontDiag5
		case 6: label.font = FontDiag6
		case 7: label.font = FontDiag7
		case 8: label.font = FontDiag8
		case 9: label.font = FontDiag9
		case 10: label.font = FontDiag10
		case 11: label.font = FontDiag11
		case 12: label.font = FontDiag12
		case 13: label.font = FontDiag13
		case 14: label.font = FontDiag14
		case 15: label.font = FontDiag15
		case 16: label.font = FontDiag16
		case 17: label.font = FontDiag17
	End Select
End Sub

Dim DMDDiagBGCounter
Sub DMDScoreAnim13BG                                               
	DMDDiagBG=DMDDiagBG-2
	If DMDDiagBG < 0 Then DMDDiagBG = 0
	DMDDiagBGCounter=DMDDiagBGCounter+1
	If DMDDiagBGCounter>17 THEN DMDDiagBGCounter=1
	Select Case DMDDiagBGCounter
		case 1: label.font = FontDiagBG1
		case 2: label.font = FontDiagBG2
		case 3: label.font = FontDiagBG3
		case 4: label.font = FontDiagBG4
		case 5: label.font = FontDiagBG5
		case 6: label.font = FontDiagBG6
		case 7: label.font = FontDiagBG7
		case 8: label.font = FontDiagBG8
		case 9: label.font = FontDiagBG9
		case 10: label.font = FontDiagBG10
		case 11: label.font = FontDiagBG11
		case 12: label.font = FontDiagBG12
		case 13: label.font = FontDiagBG13
		case 14: label.font = FontDiagBG14
		case 15: label.font = FontDiagBG15
		case 16: label.font = FontDiagBG16
		case 17: label.font = FontDiagBG17
	End Select
End Sub

Dim DMDDiagTwoCounter
Sub DMDScoreAnim14                                               
	DMDDiagTwo=DMDDiagTwo-2
	If DMDDiagTwo < 0 Then DMDDiagTwo = 0
	DMDDiagTwoCounter=DMDDiagTwoCounter+1
	If DMDDiagTwoCounter>17 THEN DMDDiagTwoCounter=1
	Select Case DMDDiagTwoCounter
		case 1: label.font = FontDiag17
		case 2: label.font = FontDiag16
		case 3: label.font = FontDiag15
		case 4: label.font = FontDiag14
		case 5: label.font = FontDiag13
		case 6: label.font = FontDiag12
		case 7: label.font = FontDiag11
		case 8: label.font = FontDiag10
		case 9: label.font = FontDiag9
		case 10: label.font = FontDiag8
		case 11: label.font = FontDiag7
		case 12: label.font = FontDiag6
		case 13: label.font = FontDiag5
		case 14: label.font = FontDiag4
		case 15: label.font = FontDiag3
		case 16: label.font = FontDiag2
		case 17: label.font = FontDiag1
	End Select
End Sub

Dim DMDDiagTwoBGCounter
Sub DMDScoreAnim14BG                                               
	DMDDiagTwoBG=DMDDiagTwoBG-2
	If DMDDiagTwoBG < 0 Then DMDDiagTwoBG = 0
	DMDDiagTwoBGCounter=DMDDiagTwoBGCounter+1
	If DMDDiagTwoBGCounter>17 THEN DMDDiagTwoBGCounter=1
	Select Case DMDDiagTwoBGCounter
		case 1: label.font = FontDiagBG17
		case 2: label.font = FontDiagBG16
		case 3: label.font = FontDiagBG15
		case 4: label.font = FontDiagBG14
		case 5: label.font = FontDiagBG13
		case 6: label.font = FontDiagBG12
		case 7: label.font = FontDiagBG11
		case 8: label.font = FontDiagBG10
		case 9: label.font = FontDiagBG9
		case 10: label.font = FontDiagBG8
		case 11: label.font = FontDiagBG7
		case 12: label.font = FontDiagBG6
		case 13: label.font = FontDiagBG5
		case 14: label.font = FontDiagBG4
		case 15: label.font = FontDiagBG3
		case 16: label.font = FontDiagBG2
		case 17: label.font = FontDiagBG1
	End Select
End Sub

Dim DMDDiagLCounter
Sub DMDScoreAnim15                                               
	DMDDiagL=DMDDiagL-2
	If DMDDiagL < 0 Then DMDDiagL = 0
	DMDDiagLCounter=DMDDiagLCounter+1
	If DMDDiagLCounter>17 THEN DMDDiagLCounter=1
	Select Case DMDDiagLCounter
		case 1: label.font = FontDiagL1
		case 2: label.font = FontDiagL2
		case 3: label.font = FontDiagL3
		case 4: label.font = FontDiagL4
		case 5: label.font = FontDiagL5
		case 6: label.font = FontDiagL6
		case 7: label.font = FontDiagL7
		case 8: label.font = FontDiagL8
		case 9: label.font = FontDiagL9
		case 10: label.font = FontDiagL10
		case 11: label.font = FontDiagL11
		case 12: label.font = FontDiagL12
		case 13: label.font = FontDiagL13
		case 14: label.font = FontDiagL14
		case 15: label.font = FontDiagL15
		case 16: label.font = FontDiagL16
		case 17: label.font = FontDiagL17
	End Select
End Sub

Dim DMDDiagLBGCounter
Sub DMDScoreAnim15BG                                               
	DMDDiagLBG=DMDDiagLBG-2
	If DMDDiagLBG < 0 Then DMDDiagLBG = 0
	DMDDiagLBGCounter=DMDDiagLBGCounter+1
	If DMDDiagLBGCounter>17 THEN DMDDiagLBGCounter=1
	Select Case DMDDiagLBGCounter
		case 1: label.font = FontDiagLBG1
		case 2: label.font = FontDiagLBG2
		case 3: label.font = FontDiagLBG3
		case 4: label.font = FontDiagLBG4
		case 5: label.font = FontDiagLBG5
		case 6: label.font = FontDiagLBG6
		case 7: label.font = FontDiagLBG7
		case 8: label.font = FontDiagLBG8
		case 9: label.font = FontDiagLBG9
		case 10: label.font = FontDiagLBG10
		case 11: label.font = FontDiagLBG11
		case 12: label.font = FontDiagLBG12
		case 13: label.font = FontDiagLBG13
		case 14: label.font = FontDiagLBG14
		case 15: label.font = FontDiagLBG15
		case 16: label.font = FontDiagLBG16
		case 17: label.font = FontDiagLBG17
	End Select
End Sub

Dim DMDDiagLTwoCounter
Sub DMDScoreAnim16                                               
	DMDDiagLTwo=DMDDiagLTwo-2
	If DMDDiagLTwo < 0 Then DMDDiagLTwo = 0
	DMDDiagLTwoCounter=DMDDiagLTwoCounter+1
	If DMDDiagLTwoCounter>17 THEN DMDDiagLTwoCounter=1
	Select Case DMDDiagLTwoCounter
		case 1: label.font = FontDiagL17
		case 2: label.font = FontDiagL16
		case 3: label.font = FontDiagL15
		case 4: label.font = FontDiagL14
		case 5: label.font = FontDiagL13
		case 6: label.font = FontDiagL12
		case 7: label.font = FontDiagL11
		case 8: label.font = FontDiagL10
		case 9: label.font = FontDiagL9
		case 10: label.font = FontDiagL8
		case 11: label.font = FontDiagL7
		case 12: label.font = FontDiagL6
		case 13: label.font = FontDiagL5
		case 14: label.font = FontDiagL4
		case 15: label.font = FontDiagL3
		case 16: label.font = FontDiagL2
		case 17: label.font = FontDiagL1
	End Select
End Sub

Dim DMDDiagLTwoBGCounter
Sub DMDScoreAnim16BG                                               
	DMDDiagLTwoBG=DMDDiagLTwoBG-2
	If DMDDiagLTwoBG < 0 Then DMDDiagLTwoBG = 0
	DMDDiagLTwoBGCounter=DMDDiagLTwoBGCounter+1
	If DMDDiagLTwoBGCounter>17 THEN DMDDiagLTwoBGCounter=1
	Select Case DMDDiagLTwoBGCounter
		case 1: label.font = FontDiagLBG17
		case 2: label.font = FontDiagLBG16
		case 3: label.font = FontDiagLBG15
		case 4: label.font = FontDiagLBG14
		case 5: label.font = FontDiagLBG13
		case 6: label.font = FontDiagLBG12
		case 7: label.font = FontDiagLBG11
		case 8: label.font = FontDiagLBG10
		case 9: label.font = FontDiagLBG9
		case 10: label.font = FontDiagLBG8
		case 11: label.font = FontDiagLBG7
		case 12: label.font = FontDiagLBG6
		case 13: label.font = FontDiagLBG5
		case 14: label.font = FontDiagLBG4
		case 15: label.font = FontDiagLBG3
		case 16: label.font = FontDiagLBG2
		case 17: label.font = FontDiagLBG1
	End Select
End Sub

Dim DMDMaskCounter
Sub DMDScoreAnim17                                               
	DMDMask=DMDMask-2
	If DMDMask < 0 Then DMDMask= 0
	DMDMaskCounter=DMDMaskCounter+1
	If DMDMaskCounter>13 THEN DMDMaskCounter=1
	Select Case DMDMaskCounter
		case 1: label.font = FontMask1
		case 2: label.font = FontMask2
		case 3: label.font = FontMask3
		case 4: label.font = FontMask4
		case 5: label.font = FontMask5
		case 6: label.font = FontMask6
		case 7: label.font = FontMask7
		case 8: label.font = FontMask8
		case 9: label.font = FontMask9
		case 10: label.font = FontMask10
		case 11: label.font = FontMask11
		case 12: label.font = FontMask12
		case 13: label.font = FontMask13
	End Select
End Sub

Dim DMDMaskBGCounter
Sub DMDScoreAnim17BG                                               
	DMDMaskBG=DMDMaskBG-2
	If DMDMaskBG < 0 Then DMDMaskBG= 0
	DMDMaskBGCounter=DMDMaskBGCounter+1
	If DMDMaskBGCounter>13 THEN DMDMaskBGCounter=1
	Select Case DMDMaskBGCounter
		case 1: label.font = FontMaskBG1
		case 2: label.font = FontMaskBG2
		case 3: label.font = FontMaskBG3
		case 4: label.font = FontMaskBG4
		case 5: label.font = FontMaskBG5
		case 6: label.font = FontMaskBG6
		case 7: label.font = FontMaskBG7
		case 8: label.font = FontMaskBG8
		case 9: label.font = FontMaskBG9
		case 10: label.font = FontMaskBG10
		case 11: label.font = FontMaskBG11
		case 12: label.font = FontMaskBG12
		case 13: label.font = FontMaskBG13
	End Select
End Sub

Dim DMDMaskLCounter
Sub DMDScoreAnim18                                              
	DMDMaskL=DMDMaskL-2
	If DMDMaskL < 0 Then DMDMaskL= 0
	DMDMaskLCounter=DMDMaskLCounter+1
	If DMDMaskLCounter>13 THEN DMDMaskLCounter=1
	Select Case DMDMaskLCounter
		case 1: label.font = FontMask13
		case 2: label.font = FontMask12
		case 3: label.font = FontMask11
		case 4: label.font = FontMask10
		case 5: label.font = FontMask9
		case 6: label.font = FontMask8
		case 7: label.font = FontMask7
		case 8: label.font = FontMask6
		case 9: label.font = FontMask5
		case 10: label.font = FontMask4
		case 11: label.font = FontMask3
		case 12: label.font = FontMask2
		case 13: label.font = FontMask1
	End Select
End Sub

Dim DMDMaskLBGCounter
Sub DMDScoreAnim18BG                                              
	DMDMaskLBG=DMDMaskLBG-2
	If DMDMaskLBG < 0 Then DMDMaskLBG= 0
	DMDMaskLBGCounter=DMDMaskLBGCounter+1
	If DMDMaskLBGCounter>13 THEN DMDMaskLBGCounter=1
	Select Case DMDMaskLBGCounter
		case 1: label.font = FontMaskBG13
		case 2: label.font = FontMaskBG12
		case 3: label.font = FontMaskBG11
		case 4: label.font = FontMaskBG10
		case 5: label.font = FontMaskBG9
		case 6: label.font = FontMaskBG8
		case 7: label.font = FontMaskBG7
		case 8: label.font = FontMaskBG6
		case 9: label.font = FontMaskBG5
		case 10: label.font = FontMaskBG4
		case 11: label.font = FontMaskBG3
		case 12: label.font = FontMaskBG2
		case 13: label.font = FontMaskBG1
	End Select
End Sub

Dim DMDMaskCenterCounter
Sub DMDScoreAnim19                                              
	DMDMaskCenter=DMDMaskCenter-2
	If DMDMaskCenter < 0 Then DMDMaskCenter= 0
	DMDMaskCenterCounter=DMDMaskCenterCounter+1
	If DMDMaskCenterCounter>10 THEN DMDMaskCenterCounter=1
	Select Case DMDMaskCenterCounter
		case 1: label.font = FontMaskCenter1
		case 2: label.font = FontMaskCenter2
		case 3: label.font = FontMaskCenter3
		case 4: label.font = FontMaskCenter4
		case 5: label.font = FontMaskCenter5
		case 6: label.font = FontMaskCenter6
		case 7: label.font = FontMaskCenter7
		case 8: label.font = FontMaskCenter8
		case 9: label.font = FontMaskCenter9
		case 10: label.font = FontMaskCenter10
	End Select
End Sub

Dim DMDMaskCenterBGCounter
Sub DMDScoreAnim19BG                                              
	DMDMaskCenterBG=DMDMaskCenterBG-2
	If DMDMaskCenterBG < 0 Then DMDMaskCenterBG= 0
	DMDMaskCenterBGCounter=DMDMaskCenterBGCounter+1
	If DMDMaskCenterBGCounter>10 THEN DMDMaskCenterBGCounter=1
	Select Case DMDMaskCenterBGCounter
		case 1: label.font = FontMaskCenterBG1
		case 2: label.font = FontMaskCenterBG2
		case 3: label.font = FontMaskCenterBG3
		case 4: label.font = FontMaskCenterBG4
		case 5: label.font = FontMaskCenterBG5
		case 6: label.font = FontMaskCenterBG6
		case 7: label.font = FontMaskCenterBG7
		case 8: label.font = FontMaskCenterBG8
		case 9: label.font = FontMaskCenterBG9
		case 10: label.font = FontMaskCenterBG10
	End Select
End Sub

Dim DMDMaskBounceCounter
Sub DMDScoreAnim20                                              
	DMDMaskBounce=DMDMaskBounce-2
	If DMDMaskBounce < 0 Then DMDMaskBounce= 0
	DMDMaskBounceCounter=DMDMaskBounceCounter+1
	If DMDMaskBounceCounter>11 THEN DMDMaskBounceCounter=1
	Select Case DMDMaskBounceCounter
		case 1: label.font = FontMaskBounce1
		case 2: label.font = FontMaskBounce2
		case 3: label.font = FontMaskBounce3
		case 4: label.font = FontMaskBounce4
		case 5: label.font = FontMaskBounce5
		case 6: label.font = FontMaskBounce6
		case 7: label.font = FontMaskBounce7
		case 8: label.font = FontMaskBounce8
		case 9: label.font = FontMaskBounce9
		case 10: label.font = FontMaskBounce10
		case 11: label.font = FontMaskBounce11
	End Select
End Sub

Dim DMDMaskBounceBGCounter
Sub DMDScoreAnim20BG                                              
	DMDMaskBounceBG=DMDMaskBounceBG-2
	If DMDMaskBounceBG < 0 Then DMDMaskBounceBG= 0
	DMDMaskBounceBGCounter=DMDMaskBounceBGCounter+1
	If DMDMaskBounceBGCounter>11 THEN DMDMaskBounceBGCounter=1
	Select Case DMDMaskBounceBGCounter
		case 1: label.font = FontMaskBounceBG1
		case 2: label.font = FontMaskBounceBG2
		case 3: label.font = FontMaskBounceBG3
		case 4: label.font = FontMaskBounceBG4
		case 5: label.font = FontMaskBounceBG5
		case 6: label.font = FontMaskBounceBG6
		case 7: label.font = FontMaskBounceBG7
		case 8: label.font = FontMaskBounceBG8
		case 9: label.font = FontMaskBounceBG9
		case 10: label.font = FontMaskBounceBG10
		case 11: label.font = FontMaskBounceBG11
	End Select
End Sub

Dim DMDMaskBounceHCounter
Sub DMDScoreAnim21                                              
	DMDMaskBounceH=DMDMaskBounceH-2
	If DMDMaskBounceH < 0 Then DMDMaskBounceH= 0
	DMDMaskBounceHCounter=DMDMaskBounceHCounter+1
	If DMDMaskBounceHCounter>9 THEN DMDMaskBounceHCounter=1
	Select Case DMDMaskBounceHCounter
		case 1: label.font = FontMaskBounceH1
		case 2: label.font = FontMaskBounceH2
		case 3: label.font = FontMaskBounceH3
		case 4: label.font = FontMaskBounceH4
		case 5: label.font = FontMaskBounceH5
		case 6: label.font = FontMaskBounceH6
		case 7: label.font = FontMaskBounceH7
		case 8: label.font = FontMaskBounceH8
		case 9: label.font = FontMaskBounceH9
	End Select
End Sub

Dim DMDMaskBounceHBGCounter
Sub DMDScoreAnim21BG                                              
	DMDMaskBounceHBG=DMDMaskBounceHBG-2
	If DMDMaskBounceHBG < 0 Then DMDMaskBounceHBG= 0
	DMDMaskBounceHBGCounter=DMDMaskBounceHBGCounter+1
	If DMDMaskBounceHBGCounter>9 THEN DMDMaskBounceHBGCounter=1
	Select Case DMDMaskBounceHBGCounter
		case 1: label.font = FontMaskBounceHBG1
		case 2: label.font = FontMaskBounceHBG2
		case 3: label.font = FontMaskBounceHBG3
		case 4: label.font = FontMaskBounceHBG4
		case 5: label.font = FontMaskBounceHBG5
		case 6: label.font = FontMaskBounceHBG6
		case 7: label.font = FontMaskBounceHBG7
		case 8: label.font = FontMaskBounceHBG8
		case 9: label.font = FontMaskBounceHBG9
	End Select
End Sub

	Dim RandomDMDAnimationCounter
	Sub RandomDMDAnimation
			RandomDMDAnimationCounter = RandomDMDAnimationCounter+1
			If RandomDMDAnimationCounter>17 THEN RandomDMDAnimationCounter=1
			Select Case RandomDMDAnimationCounter
			Case 1: DMDFire=70 : DMDFireBG=70                            'fire
			Case 2: DMDCheckerboard=70 : DMDCheckerboardBG=70            'checkerboard /discoball
			Case 3: DMDExp=70 : DMDExpBG=70                              'explosion
			Case 4: DMDImp=70 :DMDImpBG=70                               'implosion
			Case 5: DMDVerticalThick=70 : DMDVerticalThickBG=70          'vertical stripe - left to right
			Case 6: DMDVerticalThickL=70 : DMDVerticalThickLBG=70        'vertical stripe - right to left
			Case 7: DMDHorizontalThick=70 : DMDHorizontalThickBG=70      'horizontal stripe - top to bottom 
			Case 8: DMDHorizontalThickUp=70 : DMDHorizontalThickUpBG=70  'horizontal stripe - bottom to top
			Case 9: DMDSpiral=70 : DMDSpiralBG=70   					 'spiral - clockwise
			Case 10: DMDSpiralL=70 : DMDSpiralLBG=70    				 'spiral - counterclockwise
			Case 11: DMDStar=70 : DMDStarBG=70          				 'star - clockwise
			Case 12: DMDStarL=70 : DMDStarLBG=70        				 'star - clockwise 
			Case 13: DMDDiag=70 : DMDDiagBG=70                           'diagonal stripe - top left to bottom right
			Case 14: DMDDiagTwo=70 : DMDDiagTwoBG=70					 'diagonal stripe - bottom right to top left
			Case 15: DMDDiagL=70 : DMDDiagLBG=70					     'diagonal stripe - bottom left to top right
			Case 16: DMDDiagLTwo=70 : DMDDiagLTwoBG=70          		 'diagonal stripe - top right to bottom left
			Case 17: DMDBgBlink=1										 'dmd bg blink
		End Select
	End Sub

	Dim DMDCounterBumper
	Sub DMDBumper
			DMDCounterBumper=DMDCounterBumper+1
			If DMDCounterBumper>2 THEN DMDCounterBumper=1
			Select Case DMDCounterBumper
			Case 1: DMDExp=9 : DMDExpBG=9
			Case 2: DMDImp=9 : DMDImpBG=9
		End Select
	End Sub

	Dim DMDCounterOrbit
	Sub DMDOrbit
			DMDCounterOrbit=DMDCounterOrbit+1
			If DMDCounterOrbit>2 THEN DMDCounterOrbit=1
			Select Case DMDCounterOrbit
			Case 1: DMDStar=50 : DMDStarBG=50
			Case 2: DMDSpiral=50 : DMDSpiralBG=50
		End Select
	End Sub

	Dim DMDCounterOrbitCounterclockwise
	Sub DMDOrbitCounterclockwise
			DMDCounterOrbitCounterclockwise=DMDCounterOrbitCounterclockwise+1
			If DMDCounterOrbitCounterclockwise>4 THEN DMDCounterOrbitCounterclockwise=1
			Select Case DMDCounterOrbitCounterclockwise
			Case 1: DMDStarL=50 : DMDStarLBG=50
			Case 2: DMDMaskBounce=50 : DMDMaskBounceBG=50
			Case 3: DMDSpiralL=50 : DMDSpiralLBG=50
			Case 4: DMDMaskBounce=50 : DMDMaskBounceBG=50
		End Select
	End Sub




'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'  VARIABLES
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	Const typefont = "ITC Avant Garde Gothic LT Bold"                
	Const numberfont = "ITC Avant Garde Gothic LT Bold"              
	Const zoomfont = "Whole trains"       
	Const zoombgfont = "Whole trains"     'Outline
	Const cGameName = "MFDOOM"                      
	Const TableName = "MFDOOM"                      
	Const myVersion = "1.0.22"
	Const MaxPlayers = 4   
	Const BallSaverTime = 15 
	Const MaxMultiplier = 6 
	Const MaxMultiballs = 4
	Const bpgcurrent = 3
	Dim toppervideo
	Dim SmokeAnimation
	Dim EnableCallouts
	Dim CalloutVol 
	Dim ballrolleron
	Dim turnoffrules
	Dim PlayersPlayingGame
	Dim CurrentPlayer
	Dim Credits
	Dim BonusPoints(4)
	Dim BonusHeldPoints(4)
	Dim BonusMultiplier(4)
	Dim bBonusHeld
	Dim BallsRemaining(4)
	Dim ExtraBallsAwards(4)
	Dim Score(4)
	Dim HighScore(4)
	Dim HighScoreName(4)
	Dim WaffleScore(4)
	Dim WaffleScoreName(4)
	Dim Jackpot
	Dim SuperJackpot
	Dim Tilt
	Dim TiltSensitivity
	Dim Tilted
	Dim TotalGamesPlayed
	Dim mBalls2Eject
	Dim SkillshotValue(4)
	Dim bAutoPlunger
	Dim bInstantInfo
	Dim bromconfig
	Dim bAttractMode
	Dim LastSwitchHit
	Dim BallsOnPlayfield
	Dim BallsInHole
	Dim bFreePlay
	Dim bGameInPlay
	Dim bOnTheFirstBall
	Dim bBallInPlungerLane
	Dim bBallSaverActive         
	Dim bBallSaverReady
	Dim bMultiBallMode
'	Dim bMusicOn
	Dim bIdleMusicOn
	Dim bSkillshotReady
	Dim bSkillshotRotateLights
	Dim bExtraBallWonThisBall
	Dim bJustStarted
	Dim bDoubleScoringActive       '
	Dim bWarpSpeedMultiballActive  '
	Dim bComboActive               '
	Dim bSuperPopsActive           '
	Dim bSuperRampsActive          '
	Dim bSuperOrbitsActive         '
	Dim bDoubleSpinnerActive       '
	Dim BallsInPlay                '
	Dim BallsLocked                '
	Dim bMBDrainConfirm            '
	Dim LastVideoPlayed            '
	Dim plungerIM 
	Dim bonusbumps(4)
	Dim bonusleftramp(4)
	Dim bonusrightramp(4)
	Dim bonusleftorbit(4)
	Dim bonusspinner(4)
	Dim bonuscentertarget(4)
	Dim bonussmalltarget(4)
	Dim bonusmission(4)

	Dim bSuperRamps(4)
	Dim bSuperOrbits(4)
	Dim bSuperPops(4)
	Dim bDoubleSpinner(4)
	Dim bCombo(4)

	Dim PupAniFolder

	LoadCoreFiles
	Sub LoadCoreFiles
		On Error Resume Next
		ExecuteGlobal GetTextFile("core.vbs")
		If Err Then MsgBox "Can't open core.vbs"
		On Error Goto 0
	End Sub

'*********** Set the default LUT set *********** 
'LUTset Types:
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
'12 = LUT 1on1
'13 = LUT Bassgeige
'14 = LUT Black Light
'15 = B&W Comic Book
'You can change LUT option within game with left and right CTRL keys
Dim LUTset, DisableLUTSelector, LutToggleSound
LutToggleSound = True
LoadLUT
SetLUT

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'  Controller VBS stuff, but with b2s not started
'***Controller.vbs version 1.2***'
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
Const directory = "HKEY_CURRENT_USER\SOFTWARE\Visual Pinball\Controller\"
Dim objShell
Dim PopupMessage
Dim B2SController
Dim Controller
Const DOFContactors = 1
Const DOFKnocker = 2
Const DOFChimes = 3
Const DOFBell = 4
Const DOFGear = 5
Const DOFShaker = 6
Const DOFFlippers = 7
Const DOFTargets = 8
Const DOFDropTargets = 9
Const DOFOff = 0
Const DOFOn = 1
Const DOFPulse = 2

Dim DOFeffects(9)
Dim B2SOn
Dim B2SOnALT

Sub LoadEM
	LoadController("EM")
End Sub
Sub LoadPROC(VPMver, VBSfile, VBSver)
	LoadVBSFiles VPMver, VBSfile, VBSver
	LoadController("PROC")
End Sub
Sub LoadVPM(VPMver, VBSfile, VBSver)
	LoadVBSFiles VPMver, VBSfile, VBSver
	LoadController("VPM")
End Sub
Sub LoadVPMALT(VPMver, VBSfile, VBSver)
	LoadVBSFiles VPMver, VBSfile, VBSver
	LoadController("VPMALT")
End Sub
Sub LoadVBSFiles(VPMver, VBSfile, VBSver)
	On Error Resume Next
	If ScriptEngineMajorVersion < 5 Then MsgBox "VB Script Engine 5.0 or higher required"
	ExecuteGlobal GetTextFile(VBSfile)
	If Err Then MsgBox "Unable to open " & VBSfile & ". Ensure that it is in the same folder as this table. " & vbNewLine & Err.Description	
	InitializeOptions
End Sub
Sub LoadVPinMAME
	Set Controller = CreateObject("VPinMAME.Controller")
	If Err Then MsgBox "Can't load VPinMAME." & vbNewLine & Err.Description
	If VPMver > "" Then If Controller.Version < VPMver Or Err Then MsgBox "VPinMAME ver " & VPMver & " required."
	If VPinMAMEDriverVer < VBSver Or Err Then MsgBox VBSFile & " ver " & VBSver & " or higher required."
	On Error Goto 0
End Sub
Sub LoadController(TableType)
	Dim FileObj
	Dim DOFConfig
	Dim TextStr2
	Dim tempC
	Dim count
	Dim ISDOF
	Dim Answer
	
	B2SOn = False
	B2SOnALT = False
	tempC = 0
	on error resume next
	Set objShell = CreateObject("WScript.Shell")
	objShell.RegRead(directory & "ForceDisableB2S")
	If Err.number <> 0 Then
		PopupMessage = "This latest version of Controller.vbs stores its settings in the registry. To adjust the values, you must use VP 10.2 (or newer) and setup your configuration in the DOF section of the -Keys, Nudge and DOF- dialog of Visual Pinball."
		objShell.RegWrite directory & "ForceDisableB2S",0, "REG_DWORD"
		objShell.RegWrite directory & "DOFContactors",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFKnocker",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFChimes",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFBell",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFGear",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFShaker",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFFlippers",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFTargets",2, "REG_DWORD"
		objShell.RegWrite directory & "DOFDropTargets",2, "REG_DWORD"
		MsgBox PopupMessage
	End If
	tempC = objShell.RegRead(directory & "ForceDisableB2S")
	DOFeffects(1)=objShell.RegRead(directory & "DOFContactors")
	DOFeffects(2)=objShell.RegRead(directory & "DOFKnocker")
	DOFeffects(3)=objShell.RegRead(directory & "DOFChimes")
	DOFeffects(4)=objShell.RegRead(directory & "DOFBell")
	DOFeffects(5)=objShell.RegRead(directory & "DOFGear")
	DOFeffects(6)=objShell.RegRead(directory & "DOFShaker")
	DOFeffects(7)=objShell.RegRead(directory & "DOFFlippers")
	DOFeffects(8)=objShell.RegRead(directory & "DOFTargets")
	DOFeffects(9)=objShell.RegRead(directory & "DOFDropTargets")
	Set objShell = nothing

	If TableType = "PROC" or TableType = "VPMALT" Then
		If TableType = "PROC" Then
			Set Controller = CreateObject("VPROC.Controller")
			If Err Then MsgBox "Can't load PROC"
		Else
			LoadVPinMAME
		End If		
		If tempC = 0 Then
			On Error Resume Next
			If Controller is Nothing Then
				Err.Clear
			Else
				Set B2SController = CreateObject("B2S.Server")
				If B2SController is Nothing Then
					Err.Clear
				Else
					B2SController.B2SName = B2ScGameName
					B2SController.Run()
					On Error Goto 0
					B2SOn = True
					B2SOnALT = True
				End If
			End If
		End If
	Else
		If tempC = 0 Then
			On Error Resume Next
			Set Controller = CreateObject("B2S.Server")
			If Controller is Nothing Then
				Err.Clear
				If TableType = "VPM" Then 
					LoadVPinMAME
				End If
			Else
				Controller.B2SName = cGameName
				If TableType = "EM" Then
					Controller.Run()
				End If
				On Error Goto 0
				B2SOn = True
			End If
		Else
			If TableType = "VPM" Then 
				LoadVPinMAME
			End If
		End If
		Set DOFConfig=Nothing
		Set FileObj=Nothing
	End If
End sub

Function SoundFX (Sound, Effect)
	If ((Effect = 0 And B2SOn) Or DOFeffects(Effect)=1) Then
		SoundFX = ""
	Else
		SoundFX = Sound
	End If
End Function

Function SoundFXDOF (Sound, DOFevent, State, Effect)
	If DOFeffects(Effect)=1 Then
		SoundFXDOF = ""
		DOF DOFevent, State
	ElseIf DOFeffects(Effect)=2 Then
		SoundFXDOF = Sound
		DOF DOFevent, State
	Else
		SoundFXDOF = Sound
	End If
End Function

Sub DOF(DOFevent, State)
	If B2SOn Then
		If State = 2 Then
			Controller.B2SSetData DOFevent, 1:Controller.B2SSetData DOFevent, 0
		Else
			Controller.B2SSetData DOFevent, State
		End If
	End If
End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'   TABLE INITS & MATHS
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	Sub Table1_Init()
		LoadEM
		Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay") 
		if UsePuPEvents Then 
			PuPStart(cPuPPack)
		End If

		LoadOrbital

		flexdmd_Init
		Dim i
		Randomize
		bAttractMode = False
		bOnTheFirstBall = False
		bBallInPlungerLane = False
		bBallSaverActive = False
		bBallSaverReady = False
		bMultiBallMode = False
		bGameInPlay = False
		bOnTheFirstBallScorbit = False
		bDoubleScoringActive = False         '
		bWarpSpeedMultiballActive = False    '
		bComboActive = False                 '
		bSuperPopsActive = False             '
		bSuperRampsActive = False            '
		bSuperOrbitsActive = False           '
		bDoubleSpinnerActive = False         '
		BallsInPlay = 0                      '
		If BallsLocked > 0 Then
			LArrow04.state = 2
		End If
		bAutoPlunger = False
'		bMusicOn = True
		bIdleMusicOn = False
		BallsOnPlayfield = 0
		BallsInHole = 0
		LastSwitchHit = ""
		Tilt = 0
		TiltSensitivity = 6
		Tilted = False
		bBonusHeld = False
		bJustStarted = True
		GiOff

		resetbackglass

		StartAttractMode

		bonusbumps(CurrentPlayer) = 0   '
		bonusleftramp(CurrentPlayer) = 0   '
		bonusrightramp(CurrentPlayer) = 0   '
		bonusleftorbit(CurrentPlayer) = 0   '
		bonusspinner(CurrentPlayer) = 0   '
		bonuscentertarget(CurrentPlayer) = 0   '
		bonussmalltarget(CurrentPlayer) = 0   '
		bonusmission(CurrentPlayer) = 0   '

		bSuperRamps(CurrentPlayer) = 0   '
		bSuperOrbits(CurrentPlayer) = 0   '
		bSuperPops(CurrentPlayer) = 0   '
		bDoubleSpinner(CurrentPlayer) = 0   '
		bCombo(CurrentPlayer) = 0   '

		Const IMPowerSetting = 45 
		Const IMTime = 1.1      
		Set plungerIM = New cvpmImpulseP
		With plungerIM
			.InitImpulseP swplunger, IMPowerSetting, IMTime
			.Random 1.5
			.InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_solenoid", DOFContactors)
			.CreateEvents "plungerIM"
		End With	

		ChangeBall(ChooseBall)
		StartSmokeAnimations

	if Scorbitactive then 
		if Scorbit.DoInit(4303, "PupOverlays", myVersion, " mfdoom-vpin") then 	' Prod
			tmrScorbit.Interval=2000
			tmrScorbit.UserValue = 0
			tmrScorbit.Enabled=True 
			Scorbit.UploadLog = ScorbitUploadLog
		End if 
	End If

End Sub

	StartSmokeAnimations
	Sub StartSmokeAnimations
		If SmokeAnimation=1 Then
			'BluntSmoke.Visible = 0
			BluntSmokeAnimation 0, 24
			'SpliffSmoke.Visible = 0
			SpliffSmokeAnimation 0, 24
			'PhilliesSmoke.Visible = 0
			PhilliesSmokeAnimation 0, 20
			'Bong.Visible = 0
			BongAnimation 0, 20
		End If
	End Sub

'**********************************
' 	ZMAT: General Math Functions
'**********************************
' These get used throughout the script. 

Dim PI
PI = 4 * Atn(1)

Function dSin(degrees)
	dsin = Sin(degrees * Pi / 180)
End Function

Function dCos(degrees)
	dcos = Cos(degrees * Pi / 180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		End If
	ElseIf dx = 0 Then
		If dy = 0 Then
			Atn2 = 0
		Else
			Atn2 = Sgn(dy) * pi / 2
		End If
	End If
End Function

Function ArcCos(x)
	If x = 1 Then
		ArcCos = 0/180*PI
	ElseIf x = -1 Then
		ArcCos = 180/180*PI
	Else
		ArcCos = Atn(-x/Sqr(-x * x + 1)) + 2 * Atn(1)
	End If
End Function

Function max(a,b)
	If a > b Then
		max = a
	Else
		max = b
	End If
End Function

Function min(a,b)
	If a > b Then
		min = b
	Else
		min = a
	End If
End Function

' Used for drop targets
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy) 'Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
	Dim AB, BC, CD, DA
	AB = (bx * py) - (by * px) - (ax * py) + (ay * px) + (ax * by) - (ay * bx)
	BC = (cx * py) - (cy * px) - (bx * py) + (by * px) + (bx * cy) - (by * cx)
	CD = (dx * py) - (dy * px) - (cx * py) + (cy * px) + (cx * dy) - (cy * dx)
	DA = (ax * py) - (ay * px) - (dx * py) + (dy * px) + (dx * ay) - (dy * ax)
	
	If (AB <= 0 And BC <= 0 And CD <= 0 And DA <= 0) Or (AB >= 0 And BC >= 0 And CD >= 0 And DA >= 0) Then
		InRect = True
	Else
		InRect = False
	End If
End Function

Function InRotRect(ballx,bally,px,py,angle,ax,ay,bx,by,cx,cy,dx,dy)
	Dim rax,ray,rbx,rby,rcx,rcy,rdx,rdy
	Dim rotxy
	rotxy = RotPoint(ax,ay,angle)
	rax = rotxy(0) + px
	ray = rotxy(1) + py
	rotxy = RotPoint(bx,by,angle)
	rbx = rotxy(0) + px
	rby = rotxy(1) + py
	rotxy = RotPoint(cx,cy,angle)
	rcx = rotxy(0) + px
	rcy = rotxy(1) + py
	rotxy = RotPoint(dx,dy,angle)
	rdx = rotxy(0) + px
	rdy = rotxy(1) + py
	
	InRotRect = InRect(ballx,bally,rax,ray,rbx,rby,rcx,rcy,rdx,rdy)
End Function

Function RotPoint(x,y,angle)
	Dim rx, ry
	rx = x * dCos(angle) - y * dSin(angle)
	ry = x * dSin(angle) + y * dCos(angle)
	RotPoint = Array(rx,ry)
End Function

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'   KEYS
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

	Sub Table1_KeyDown(ByVal Keycode)

if keycode = "7" Then RTP
		If keycode = LeftFlipperKey Then Pincab_Button_Left.x = Pincab_Button_Left.x + 10
		If keycode = RightFlipperKey Then Pincab_Button_Right.x = Pincab_Button_Right.x - 10
		If keycode = AddCreditKey Then
			Select Case Int(rnd*3)
				Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
						RandomFlasherCenterDropTargets : LaserMask
						FlashForMs LSlimeb, 250, 50, 0
						FlashLevel17 = 1 : Flasherflash17_Timer : FlashLevel18 = 1 : Flasherflash18_Timer : FlashLevel19 = 1 : Flasherflash19_Timer	
				Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
						RandomFlasherCenterDropTargets : LaserMask
						FlashForMs LSlimeb, 250, 50, 0
						FlashLevel17 = 1 : Flasherflash17_Timer : FlashLevel18 = 1 : Flasherflash18_Timer : FlashLevel19 = 1 : Flasherflash19_Timer	
				Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
						RandomFlasherCenterDropTargets : LaserMask
						FlashForMs LSlimeb, 250, 50, 0
						FlashLevel17 = 1 : Flasherflash17_Timer : FlashLevel18 = 1 : Flasherflash18_Timer : FlashLevel19 = 1 : Flasherflash19_Timer	
			End Select
		End If

		If keycode = LeftFlipperKey And bGameInPlay Then FlipperActivate LeftFlipper, LFPress : DOF 101, DOFOn  

		If keycode = RightFlipperKey And bGameInPlay Then FlipperActivate RightFlipper, RFPress : DOF 102, DOFOn      

		If ballrolleron = 1 then
			If keycode = 46 then ' C Key
				If contball = 1 Then
					contball = 0
				Else
					contball = 1
				End If
			End If
		End If
		If keycode = 48 then 'B Key
			If bcboost = 1 Then
				bcboost = bcboostmulti
			Else
				bcboost = 1
			End If
		End If
	If keycode = 203 then bcleft = 1 ' Left Arrow
	If keycode = 200 then bcup = 1 ' Up Arrow
	If keycode = 208 then bcdown = 1 ' Down Arrow
	If keycode = 205 then bcright = 1 ' Right Arrow

		If keycode = PlungerKey Then
			SoundPlungerPull
			Plunger.Pullback
			Playvideo=4+int(rnd(1)*4)
			
		End If

		If hsbModeActive = True Then
			EnterHighScoreKey(keycode)
		elseif bGameInPlay Then
			If keycode = LeftTiltKey Then Nudge 90, 2:SoundNudgeLeft:CheckTilt
			If keycode = RightTiltKey Then Nudge 270, 2:SoundNudgeRight:CheckTilt
			If keycode = CenterTiltKey Then Nudge 0, 3:SoundNudgeCenter:CheckTilt

				If NOT Tilted Then

					If keycode = LeftFlipperKey And bGameInPlay  Then
						SolLFlipper True	'This would be called by the solenoid callbacks if using a ROM

						If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
								RandomSoundReflipUpLeft LeftFlipper
								FlipperLightOnLeft    'test
						Else 
								SoundFlipperUpAttackLeft LeftFlipper
								RandomSoundFlipperUpLeft LeftFlipper
								FlipperLightOnLeft    'test
						End If 
						
						ldown = 1
						checkdown
						If bSkillshotReady = False AND bSkillshotRotateLights Then 
							'RotateLaneLightsLeft
							RotateSkillLightsLeft
						End If
					Else
						'LeftFlipper.RotateToStart
						'If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
						 '       RandomSoundFlipperDownLeft LeftFlipper
						'End If
						'FlipperLeftHitParm = FlipperUpSoundLevel
					End If

					If keycode = RightFlipperKey And bGameInPlay  Then 
						SolRFlipper True	'This would be called by the solenoid callbacks if using a ROM

						If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
								RandomSoundReflipUpRight RightFlipper
								FlipperLightOnRight    'test
						Else 
								SoundFlipperUpAttackRight RightFlipper
								RandomSoundFlipperUpRight RightFlipper
								FlipperLightOnRight    'test
						End If

						rdown = 1
						checkdown
						If bSkillshotReady = False AND bSkillshotRotateLights Then  
							'RotateLaneLightsRight
							RotateSkillLightsRight
						End If

					Else
						'RightFlipper.RotateToStart
						'If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
						'        RandomSoundFlipperDownRight RightFlipper
						'End If        
						'FlipperRightHitParm = FlipperUpSoundLevel
					End If

					If keycode = StartGameKey Then
						If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall = True) ) Then
								PlayersPlayingGame = PlayersPlayingGame + 1
									RandomSoundStartup
									SoundStartButton
									chilloutthemusic
									DrainAllCapsAttract
									FlashForMs LDoubleScoring01, 1500, 50, 0
									FlashForMs LSupreme008, 1500, 50, 0
									DOOMLightsFlash
									FlasherAttract
								If PlayersPlayingGame = 2 Then
									RandomSoundStartup
									SoundStartButton
									chilloutthemusic
									DrainAllCapsAttract
									FlashForMs LDoubleScoring01, 1500, 50, 0
									FlashForMs LSupreme008, 1500, 50, 0
									DOOMLightsFlash
									FlasherAttract
								End If
								If PlayersPlayingGame = 3 Then
									RandomSoundStartup
									SoundStartButton
									chilloutthemusic
									DrainAllCapsAttract
									FlashForMs LDoubleScoring01, 1500, 50, 0
									FlashForMs LSupreme008, 1500, 50, 0
									DOOMLightsFlash
									FlasherAttract
								End If
								If PlayersPlayingGame = 4 Then	
									RandomSoundStartup
									SoundStartButton
									chilloutthemusic
									DrainAllCapsAttract
									FlashForMs LDoubleScoring01, 1500, 50, 0
									FlashForMs LSupreme008, 1500, 50, 0
									DOOMLightsFlash
									FlasherAttract
								End If
								TotalGamesPlayed = TotalGamesPlayed + 1
						End If
					End If
				End If
			Else ' else hsbmodeactive
				If NOT Tilted Then
					If keycode = LeftFlipperKey Then helptime.enabled = true:DMDintroloop:introtime = 0
					If keycode = RightFlipperKey Then helptime.enabled = true:DMDintroloop:introtime = 0
					If keycode = StartGameKey Then
						If(BallsOnPlayfield = 0) Then
							ResetForNewGame()
						End If
					End If
				End If
			End If 

	If keycode = RightMagnaSave Then 
		If DisableLUTSelector = 0 then
            LUTSet = LUTSet  + 1
			If LutSet > 15 then LUTSet = 0
			lutsetsounddir = 1
			If LutToggleSound then
				If lutsetsounddir = 1 And LutSet <> 15 Then
					'todo sounds
					Playsound "fx320", 0, 1, 0, 0.2, 0, 0, 0, 1
				End If
				If lutsetsounddir = -1 And LutSet <> 15 Then
					Playsound "fx320", 0, 1, 0, 0.2, 0, 0, 0, -1
				End If
				If LutSet = 15 Then
					Playsound "fx322", 0, 1, 0, 0.2, 0, 0, 0, -1
				End If
				LutSlctr.enabled = true
			end if
			SetLUT
			ShowLUT
		end if
	end if
	If keycode = LeftMagnaSave Then
		if DisableLUTSelector = 0 then
			LUTSet = LUTSet - 1
			if LutSet < 0 then LUTSet = 15
			lutsetsounddir = -1
			If LutToggleSound then
				If lutsetsounddir = 1 And LutSet <> 15 Then
					Playsound "fx323", 0, 1, 0, 0.2, 0, 0, 0, 1
				End If
				If lutsetsounddir = -1 And LutSet <> 15 Then
					Playsound "fx323", 0, 1, 0, 0.2, 0, 0, 0, -1
				End If
				If LutSet = 15 Then
					Playsound "fx322", 0, 1, 0, 0.2, 0, 0, 0, -1
				End If
				LutSlctr.enabled = true
			end if
			SetLUT
			ShowLUT
		End If
	End If
	
	End Sub

	dim lutsetsounddir
	sub LutSlctr_timer
		If lutsetsounddir = 1 And LutSet <> 15 Then
'			Playsound "", 0, 1, 0, 0.2, 0, 0, 0, -1
		End If
		If lutsetsounddir = -1 And LutSet <> 15 Then
'			Playsound "", 0, 1, 0, 0.2, 0, 0, 0, 1
		End If
		If LutSet = 15 Then
'			Playsound "", 0, 1, 0, 0.2, 0, 0, 0, 1
		End If
		LutSlctr.enabled = False
	end sub

	Sub Table1_KeyUp(ByVal keycode)
		If keycode = LeftFlipperKey Then 
			SolLFlipper False : DOF 101, DOFOff
			Pincab_Button_Left.x = Pincab_Button_Left.x - 10
		End If
		If keycode = RightFlipperKey Then 
			SolRFlipper False : DOF 102, DOFOff
			Pincab_Button_Right.x = Pincab_Button_Right.x + 10
		End If

		'Manual Ball Control
		If keycode = 203 then bcleft = 0 ' Left Arrow
		If keycode = 200 then bcup = 0 ' Up Arrow
		If keycode = 208 then bcdown = 0 ' Down Arrow
		If keycode = 205 then bcright = 0 ' Right Arrow
				
        If KeyCode = PlungerKey Then
			Plunger.Fire
			If LSlime.state = 2 Then     
				SoundPlungerReleaseBall()            
            Else    
        		SoundPlungerReleaseNoBall()             
            End If
        End If
		'Table specific
		If bGameInPLay and hsbModeActive <> True Then
			If keycode = LeftFlipperKey Then
				ldown = 0
				LeftFlipper.RotateToStart

				If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
                        RandomSoundFlipperDownLeft LeftFlipper
						FlipperLightOffLeft
                End If
				FlipperLeftHitParm = FlipperUpSoundLevel
				'RandomSoundFlipperDownLeft LeftFlipper
			End If
			If keycode = RightFlipperKey Then
				rdown = 0
				RightFlipper.RotateToStart
                If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
                        RandomSoundFlipperDownRight RightFlipper
						FlipperLightOffRight
                End If        
                FlipperRightHitParm = FlipperUpSoundLevel
				'RandomSoundFlipperDownRight RightFlipper
			End If
		Else
			If keycode = LeftFlipperKey Then helptime.enabled = false
			If keycode = RightFlipperKey Then helptime.enabled = false
		End If
	End Sub

'*******************************************
'	ZFLP: Flippers
'*******************************************

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled) 'Left flipper solenoid callback
	If Enabled Then
		FlipperActivate LeftFlipper, LFPress
		LF.Fire  'leftflipper.rotatetoend
		
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then
			RandomSoundReflipUpLeft LeftFlipper
		Else
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If
	Else
		FlipperDeActivate LeftFlipper, LFPress
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled) 'Right flipper solenoid callback
	If Enabled Then
		FlipperActivate RightFlipper, RFPress
		RF.Fire 'rightflipper.rotatetoend
		
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		FlipperDeActivate RightFlipper, RFPress
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

' Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch ActiveBall, LeftFlipper, LFCount, parm
	LF.ReProcessBalls ActiveBall
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch ActiveBall, RightFlipper, RFCount, parm
	RF.ReProcessBalls ActiveBall
	RightFlipperCollide parm
End Sub

' This subroutine updates the flipper shadows and visual primitives
Sub FlipperVisualUpdate
	FlipperLSh.RotZ = LeftFlipper.CurrentAngle
	FlipperRSh.RotZ = RightFlipper.CurrentAngle
	LFLogo.RotZ = LeftFlipper.CurrentAngle
	RFlogo.RotZ = RightFlipper.CurrentAngle
End Sub

	'*****************************************
	'	MANUAL BALL CONTROL
	'*****************************************
	ballrolleron = 1  ' set to 0 to turn off the ball roller if you use the "c" key in your cabinet	
	Sub StartControl_Hit()
		Set ControlBall = ActiveBall
		contballinplay = true		
	End Sub
	Sub StopControl_Hit()
		contballinplay = false
	End Sub
	Dim bcup, bcdown, bcleft, bcright, contball, contballinplay, ControlBall, bcboost
	Dim bcvel, bcyveloffset, bcboostmulti
	bcboost = 1 'Do Not Change - default setting
	bcvel = 4 'Controls the speed of the ball movement
	bcyveloffset = -0.01 'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
	bcboostmulti = 3 'Boost multiplier to ball veloctiy (toggled with the B key)
	Sub BallControl_Timer()
		If Contball and ContBallInPlay then
			If bcright = 1 Then
				ControlBall.velx = bcvel*bcboost
			ElseIf bcleft = 1 Then
				ControlBall.velx = - bcvel*bcboost
			Else
				ControlBall.velx=0
			End If
			If bcup = 1 Then
				ControlBall.vely = -bcvel*bcboost
			ElseIf bcdown = 1 Then
				ControlBall.vely = bcvel*bcboost
			Else
				ControlBall.vely= bcyveloffset
			End If
		End If
	End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'   BALL SKINS
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

	Dim CustomBulbIntensity(10)
	Dim CustomBallImage(10), CustomBallLogoMode(10), CustomBallDecal(10)

	' Default Ball
	CustomBallImage(0) = 		"ball_HDR"
	CustomBallLogoMode(0) = 	False
	CustomBallDecal(0) = 		"Scratches"
	CustomBulbIntensity(0) = 	1   

	' Purple Green Swirl Ball
	CustomBallImage(1) = 		"ball_black"
	CustomBallLogoMode(1) = 	False
	CustomBallDecal(1) = 		"ball_purplegreenswirl"
	CustomBulbIntensity(1) = 	1

	' Purple Swirl Ball
	CustomBallImage(2) = 		"ball_black"
	CustomBallLogoMode(2) = 	False
	CustomBallDecal(2) = 		"ball_purpleswirl"
	CustomBulbIntensity(2) = 	1

	' Purple Disco Ball
	CustomBallImage(3) = 		"ball_purpledisco"
	CustomBallLogoMode(3) = 	False
	CustomBallDecal(3) = 		"ball_purpledisco"
	CustomBulbIntensity(3) = 	1

	' Purple Stripes Ball
	CustomBallImage(4) = 		"ball_black"
	CustomBallLogoMode(4) = 	False
	CustomBallDecal(4) = 		"ball_purplestripes"
	CustomBulbIntensity(4) = 	1

	Sub ChangeBall(ballnr)
		Dim BOT, ii, col
		table1.BallDecalMode = CustomBallLogoMode(ballnr)
		table1.BallFrontDecal = CustomBallDecal(ballnr)
		table1.DefaultBulbIntensityScale = CustomBulbIntensity(ballnr)
		table1.BallImage = CustomBallImage(ballnr)
	End Sub
	Dim BallShadowArray
	BallShadowArray = Array (BallShadow1, BallShadow2, BallShadow3)

'Flipper Colors
	Select Case ChooseFlipper
	Case 0
		LFLogo.image = "flipper-left-solid-yellow"
		RFLogo.image = "flipper-right-solid-yellow"
	Case 1
		LFLogo.image = "flipper-left-solid-white"
		RFLogo.image = "flipper-right-solid-white"
	End Select
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'   TILT
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	Sub CheckTilt                                    
		Tilt = Tilt + TiltSensitivity                
		TiltDecreaseTimer.Enabled = True
		'If(Tilt> TiltSensitivity) AND(Tilt <15) Then
		If(Tilt> TiltSensitivity) AND(Tilt <15) Then
			PlaySoundCallOut "tilt1"
			pupevent 700
			DMDBigText "WARNING",100,1
			'chilloutthemusic
			TiltWarningCallout
		End if
		If Tilt> 15 AND LastSwitchHit <> "finaltilt" Then 
			Tilted = True
			LastSwitchHit = "finaltilt"
			PlaySoundCallOut "tiltshutdown"
			pupevent 700
			DMDBigText "SOFA KING",120,0
			BallHandlingQueue.Add "TiltDelayedDMDBigText","TiltDelayedDMDBigText",95,1500,0,0,0,True
			TiltCallout
			DisableTable True
			tilttableclear.enabled = true
			TiltRecoveryTimer.Enabled = True 
		End If
	End Sub

    Sub TiltDelayedDMDBigText
		DMDBigText "WE TODD ID",200,0
	End Sub

	Dim tilttime:tilttime = 0
	Sub tilttableclear_timer
		tilttime = tilttime + 1
		Select Case tilttime
			Case 10
				
		End Select
	End Sub
	Sub TiltDecreaseTimer_Timer
		If Tilt> 0 Then
			Tilt = Tilt - 0.1
		Else
			TiltDecreaseTimer.Enabled = False
		End If
	End Sub
	Sub DisableTable(Enabled)
		If Enabled Then
			GiOff
			LightSeqTilt.Play SeqAllOff
			LeftFlipper.RotateToStart
			RightFlipper.RotateToStart
			LeftSlingshot.Disabled = 1
			RightSlingshot.Disabled = 1
			StopAllMusic
			bWarpSpeedMultiballActive = False
			Bumper1.Threshold = 100    
			Bumper2.Threshold = 100    
			Bumper3.Threshold = 100    
		Else
			GiOn
			LightSeqTilt.StopPlay
			LeftSlingshot.Disabled = 0
			RightSlingshot.Disabled = 0
			Bumper1.Threshold = 1
			Bumper2.Threshold = 1
			Bumper3.Threshold = 1
		End If
	End Sub
	Sub TiltRecoveryTimer_Timer()
		If(BallsOnPlayfield = 0) Then
			EndOfBall()
			TiltRecoveryTimer.Enabled = False
			'ClearMusicCallout
		End If
	End Sub
	
	Sub Spinner_Spin
		RandomSpinnerDOF
		RandomLightsColor
		PlaySound "fx_spinner", 0, 1, AudioPan(Spinner), 0, 0, 0, 1, AudioFade(Spinner)
		AddScore 250
		If (BallsOnPlayfield < 2) AND (Tilted = False) THEN
			LightSeqhit.UpdateInterval = 0
			LightSeqhit.Play SeqBlinking, , 2, 13
			LightSeqGi.UpdateInterval = 0
			LightSeqGi.Play SeqBlinking, , 2, 13
			Randomspinfx
		End If
	End Sub
	Sub RandomSpinnerDOF
		Select Case Int(Rnd * 3) + 1
			Case 1: DOF 934, DOFPulse  'DOF MX
			Case 2: DOF 951, DOFPulse  'DOF MX
			Case 3: DOF 952, DOFPulse  'DOF MX
		End Select
	End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'   START GAME, END GAME
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	Sub ResetForNewGame()
		playvideo=100
		Dim i

		Dbg "RESET FOR NEW GAME"
		if ScorbitActive = 1 And (Scorbit.bNeedsPairing) = False Then 
			Scorbit.StartSession()
		End If

		bGameInPLay = True
		StopAttractMode
		GiOn
		TotalGamesPlayed = TotalGamesPlayed + 1
		CurrentPlayer = 1
		PlayersPlayingGame = 1
		bOnTheFirstBall = True
		bOnTheFirstBallScorbit = True

		For i = 1 To MaxPlayers
			Score(i) = 0
			BonusPoints(i) = 0
			BonusHeldPoints(i) = 0
			BonusMultiplier(i) = 1
			BallsRemaining(i) = 3
			ExtraBallsAwards(i) = 0
		Next
		Tilt = 0
		Game_Init()
		BallHandlingQueue.Add "FirstBall","FirstBall",30,1500,0,0,0,False

	End Sub
	Sub EndOfGame()
		'StopAllMusic
		DelayAttractText
		StopScorbit
		StartAttractMode
		introposition = 0     '0
		bGameInPLay = False
		bJustStarted = False
		Dim i
		GiOff

	End Sub
	Dim hsDelayTextActive:hsDelayTextActive = False
	Sub DelayAttractText
		Dbg "In Delay Attract"
		hsDelayTextActive = TRUE
		BallHandlingQueue.Add "EnableAttractText","EnableAttractText",30,13000,0,0,0,False
		pupevent 701
		pupevent 702
		GameOverCallout
	End Sub
	Sub EnableAttractText
		hsDelayTextActive = FALSE
	End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'   PINUP ACTIVE BACKGLASS
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'*********************************************************
'   PinUp Player Config
'   Change HasPuP = True if using PinUp Player Videos
'*********************************************************
'	Dim HasPup:HasPuP = true
	Dim PuPlayer
	Const pTopper=0
	Const pDMD=1
	Const pBackglass=2
	Const pPlayfield=3
	Const pMusic=4
	Const pAudio=7
	Const pCallouts=8
	


Sub chilloutthemusic
	if PupStatus Then
		If LWarpMultiballCounter.state = 0 Then
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":10 }"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":10 }"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 7, ""FN"":11, ""VL"":10 }"
			BallHandlingQueue.Add "turnitbackup","turnitbackup",20,2200,0,0,0,false
		End If
	End If
End Sub

Sub turnitbackup
	if PupStatus Then
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":99 }"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":99 }"
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 7, ""FN"":11, ""VL"":99 }"
	End If
End Sub


Sub LoadOrbital
	If PuPStatus Then
		PuPlayer.LabelInit pBackglass
		If toppervideo = 1 Then
			RandomTopperVideoLoop
		End If
	End if
End Sub



Sub RandomTopperVideoLoop()
	Select Case Int(Rnd * 2) + 1
		Case 1: pupevent 703
		Case 2: pupevent 704
	End Select
End Sub


Sub resetbackglass
	Loadhs
	PuPlayer.LabelShowPage pBackglass,1,0,""
	if PuPStatus = False Then Exit Sub
	PuPlayer.LabelNew pBackglass,"Smoke",1,		10,RGB(255, 255, 255)			,0,1,0 ,0,0    ,1,1
	dim i
	for i = 0 to 5
		PuPlayer.LabelNew pBackglass,"BumperBG" & i,1 ,		10,RGB(255, 255, 255)	,0,1,0 ,0,0    ,1,1
	Next 

PuPlayer.LabelNew pBackglass,"ScorbitQR1",1 ,		10,RGB(255, 255, 255)	,0,1,0 ,0,0    ,1,1
PuPlayer.LabelNew pBackglass,"ScorbitQR2",1 ,		10,RGB(255, 255, 255)	,0,1,0 ,0,0    ,1,1
PuPlayer.LabelNew pBackglass,"ScorbitQRIcon1",1 ,		10,RGB(255, 255, 255)	,0,1,0 ,0,0    ,1,1
PuPlayer.LabelNew pBackglass,"ScorbitQRIcon2",1 ,		10,RGB(255, 255, 255)	,0,1,0 ,0,0    ,1,1

End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' DO NOT CHANGE ANYTHING IN THIS SECTION
Const     ScorbitClaimSmall		= 1 	' Make Claim QR Code smaller for high res backglass 

	Const cWhite = 	16777215
	Const cRed = 	397512
	Const cGold = 	1604786
	Const cGold2 = 46079
	Const cGreen = 32768
	Const cGrey = 	8421504
	Const cYellow = 65535
	Const cOrange = 33023
	Const cPurple = 16711808
	Const cBlue = 16711680
	Const cLightBlue = 16744448
	Const cBoltYellow = 2148582
	Const cLightGreen = 9747818
	Const cBlack = 0
	Const cPink = 12615935

Dim pGameName       : pGameName=cGameName

 
'Const pFontFixed="Ghost Stories Deluxe"
Const pFontBold="Bronx Bystreets"  'main score font
'Const pFontGodfather = "The Godfather"
Const pFontGodfather = "The Scarface Free Trial"
Const dmddef="Neue Alte Grotesk Bold"


'pages
Const pDMDBlank = 0
Const pScores = 1
Const pAttract = 2
Const pPrevScores = 3
Const pCredits = 4
Const pSlotMachine = 5
Const pBonus = 6
Const pEvent = 7
Const pHighScore = 8

sub pDMDLabelSetPos(labName, xpos, ypos)
   PuPlayer.LabelSet pBackglass,labName,"",1,"{'mt':2,'xpos':"&xpos& ",'ypos':"&ypos&"}"    
end sub

sub pDMDLabelSetSizeImage(labName, lWidth, lHeight)
   PuPlayer.LabelSet pBackglass,labName,"",1,"{'mt':2,'width':"& lWidth & ",'height':"&lHeight&"}" 
end sub

sub pBackglassLabelSetSizeImage(labName, lWidth, lHeight)
   PuPlayer.LabelSet pBackglass,labName,"",1,"{'mt':2,'width':"& lWidth & ",'height':"&lHeight&"}" 
end sub

sub pBackglassLabelSetPos(labName, xpos, ypos)
   PuPlayer.LabelSet pBackglass,labName,"",1,"{'mt':2,'xpos':"&xpos& ",'ypos':"&ypos&"}"    
end sub

Sub pDMDLabelSet(labName,LabText)
	PuPlayer.LabelSet pBackglass, labName, " " & LabText & " ", 1, ""   
end sub

Sub pDMDLabelHide(labName)
	PuPlayer.LabelSet pBackglass,labName," ",0,""
end sub

Sub pDMDLabelShow(labName)
	PuPlayer.LabelSet pBackglass,labName," ",1,""
end sub

Sub pBackglassLabelShow(labName)
PuPlayer.LabelSet pBackglass,labName,"",1,""   
end sub

Sub pBackglassLabelHide(labName)
PuPlayer.LabelSet pBackglass,labName,"",0,""   
end sub


Sub pupCreateLabelImage(lName, lFilename,xpos, ypos, Iwidth, Iheight, pagenum, lvis)
	PuPlayer.LabelNew pBackglass,lName ,"",50,RGB(100,100,100),0,1,1,1,1,pagenum,lvis
	PuPlayer.LabelSet pBackglass,lName,lFilename,lvis,"{'mt':2,'width':"&IWidth&",'height':"&Iheight&",'xpos':"&xpos&",'ypos':"&ypos&"}"
end Sub


sub setPageLayouts
	pupCreateLabelImage "ScorbitQRicon1","PuPOverlays\\QRcodeS.png",50,30,34,60,1,0
	pupCreateLabelImage "ScorbitQR1","PuPOverlays\\QRcode.png",50,30,34,60,1,0

	pupCreateLabelImage "ScorbitQRicon2","PuPOverlays\\QRcodeB.png",50,30,34,60,1,0
	pupCreateLabelImage "ScorbitQR2","PuPOverlays\\QRclaim.png",50,30,34,60,1,0
End Sub

Sub DelayQRClaim_Timer()
	Dbg "Delay QR STARTED"
	if bOnTheFirstBall AND bBallInPlungerLane then ScorbitClaimQR(True)
	DelayQRClaim.Enabled=False
End Sub



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'  HIGH SCORES
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	'Dim hschecker:hschecker = 0
Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function

Sub Loadhs
    Dim x
    x = LoadValue(cGameName, "HighScore1")
    If(x <> "")Then HighScore(0) = CDbl(x)Else HighScore(0) = 2000000 End If
    x = LoadValue(cGameName, "HighScore1Name")
    If(x <> "")Then HighScoreName(0) = x Else HighScoreName(0) = "ILL" End If
    x = LoadValue(cGameName, "HighScore2")
    If(x <> "")then HighScore(1) = CDbl(x)Else HighScore(1) = 1500000 End If
    x = LoadValue(cGameName, "HighScore2Name")
    If(x <> "")then HighScoreName(1) = x Else HighScoreName(1) = "RTP" End If
    x = LoadValue(cGameName, "HighScore3")
    If(x <> "")then HighScore(2) = CDbl(x)Else HighScore(2) = 1000000 End If
    x = LoadValue(cGameName, "HighScore3Name")
    If(x <> "")then HighScoreName(2) = x Else HighScoreName(2) = "IAK" End If
    x = LoadValue(cGameName, "HighScore4")
    If(x <> "")then HighScore(3) = CDbl(x)Else HighScore(3) = 800000 End If
    x = LoadValue(cGameName, "HighScore4Name")
    If(x <> "")then HighScoreName(3) = x Else HighScoreName(3) = "RKP" End If
    x = LoadValue(cGameName, "Credits")
    If(x <> "")then Credits = CInt(x)Else Credits = 0
    x = LoadValue(cGameName, "TotalGamesPlayed")
    If(x <> "")then TotalGamesPlayed = CInt(x)Else TotalGamesPlayed = 0 End If


End Sub

Sub Savehs
    SaveValue cGameName, "HighScore1", HighScore(0)
    SaveValue cGameName, "HighScore1Name", HighScoreName(0)
    SaveValue cGameName, "HighScore2", HighScore(1)
    SaveValue cGameName, "HighScore2Name", HighScoreName(1)
    SaveValue cGameName, "HighScore3", HighScore(2)
    SaveValue cGameName, "HighScore3Name", HighScoreName(2)
    SaveValue cGameName, "HighScore4", HighScore(3)
    SaveValue cGameName, "HighScore4Name", HighScoreName(3)
    SaveValue cGameName, "Credits", Credits
    SaveValue cGameName, "TotalGamesPlayed", TotalGamesPlayed
End Sub

Sub Reseths
    HighScoreName(0) = "ILL"
    HighScoreName(1) = "RTP"
    HighScoreName(2) = "IAK"
    HighScoreName(3) = "RKP"
    HighScore(0) = 2000000
    HighScore(1) = 1500000
    HighScore(2) = 1000000
    HighScore(3) = 800000
    Savehs
End Sub


	' Initials
	Dim hsbModeActive:hsbModeActive = False
	Dim hsEnteredName
	Dim hsEnteredDigits(3)
	Dim hsCurrentDigit
	Dim hsValidLetters
	Dim hsCurrentLetter
	Dim hsLetterFlash

	' Check the scores to see if you got one
	Sub CheckHighscore()
		Dim tmp
		tmp = Score(CurrentPlayer)

		If tmp > HighScore(3) Then
			HighScore(3) = tmp
			'enter player's name
			HighScoreEntryInit()
			DOF 939, DOFPulse   'DOF MX - High Score
		Else
			EndOfBallComplete
		End If
	End Sub

Sub ClearDMDHighScore
	DMDTopSplash "", 1, 0
	DMDBigText "",1,0
End Sub


Sub HighScoreEntryInit()
	DbgTimer.Enabled = 1
	Dbg "Entering High Score Entry"
	UpdateMusicNow
	PlayVideo = 0
	pupevent 798
    hsbModeActive = True
    hsLetterFlash = 0

    hsEnteredDigits(0) = " "
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 0

    hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789(" ' ( is back arrow
    hsCurrentLetter = 1
    HighScoreDisplayNameNow()

    HighScoreFlashTimer.Interval = 250
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreDisplayNameNow()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreDisplayName()
    Dim i
    Dim TempTopStr
    Dim TempBotStr

    TempTopStr = "YOUR NAME:"
	DMDTopSplash TempTopStr, 9999, 1

    TempBotStr = "    > "
    if(hsCurrentDigit > 0)then TempBotStr = TempBotStr & hsEnteredDigits(0)
    if(hsCurrentDigit > 1)then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit > 2)then TempBotStr = TempBotStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3)then
        if(hsLetterFlash <> 0)then
            TempBotStr = TempBotStr & " "
        else
            TempBotStr = TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit < 1)then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit < 2)then TempBotStr = TempBotStr & hsEnteredDigits(2)

    TempBotStr = TempBotStr & " (    "
	DMDBigText TempBotStr,9999,0
End Sub


Sub HighScoreFlashTimer_Timer()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = hsLetterFlash + 1
    if(hsLetterFlash = 2)then hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub EnterHighScoreKey(keycode)
    If keycode = LeftFlipperKey Then
        playsound "fx_Previous"
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0)then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = RightFlipperKey Then
        playsound "fx_Next"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter > len(hsValidLetters))then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = PlungerKey OR keycode = StartGameKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "(")then
            playsound "fx_Enter"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3)then
                HighScoreCommitName()
            else
                HighScoreDisplayNameNow()
            end if
        else
            playsound "fx_Esc"
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit > 0)then
                hsCurrentDigit = hsCurrentDigit - 1
            end if
            HighScoreDisplayNameNow()
        end if
    end if
End Sub


	' post the high score letters
Sub HighScoreCommitName()
    HighScoreFlashTimer.Enabled = False
    hsbModeActive = False': CreateDMD_intro

    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ")then
        hsEnteredName = "RTP"
    end if

    HighScoreName(3) = hsEnteredName
    SortHighscore
	'DMDTextDisplayTime = Frame
    EndOfBallComplete()
End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 0 to 3
        For j = 0 to 2
            If HighScore(j) < HighScore(j + 1)Then
                tmp = HighScore(j + 1)
                tmp2 = HighScoreName(j + 1)
                HighScore(j + 1) = HighScore(j)
                HighScoreName(j + 1) = HighScoreName(j)
                HighScore(j) = tmp
                HighScoreName(j) = tmp2
            End If
        Next
    Next
	Savehs
End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'  ATTRACT MODE
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	Sub StartAttractMode()
		Dim i
		DOF 947, DOFOn   'DOF MX - Attract Mode ON
		DOF 311, DOFOn   'MX-Undercab1
		bAttractMode = True
		ChangeAttractLights2(lightpurple)
		ChangeAttractLights3(purple)
		StartLightSeq
		LightQueue.Add "StartLightSeq2","StartLightSeq2",20,150,0,0,0,false
		LightQueue.Add "StartLightSeq3","StartLightSeq3",20,300,0,0,0,false
		'ShowTableInfo
		StopAllMusic
		i = RndNbr(2)
		PlayMusic "MFDOOM\Attract"&i &".mp3"
		DMDintroloop
		FlasherAttract
		StartRainbow alights
		StartRainbowGI GI
		DMDattract.Enabled = 1
		intromover.enabled = true
		pupevent 705
		'RandomUltraDMDSceneIntro
		'playvideo=37+int(rnd(1)*5)
		StartPLights
		LRuby2.state = 2
	End Sub

	Sub StopAttractMode()
		DOF 947, DOFOff   'DOF MX - Attract Mode Off
		bAttractMode = False
		LightSeqAttract.StopPlay
		LightSeqAttract2.StopPlay
		LightSeqAttract3.StopPlay
		LightSeqALLCAPS.StopPlay
		LightSeqhit2.StopPlay
		LightSeqhit3.StopPlay
		LightSeqGI.StopPlay
		StopRainbow alights
		StopRainbow2 GI
		ResetAllLightsColor
		ResetAllGILightsColor
		DMDattract.Enabled = 0
		intromover.enabled = false
		StopPLights
		LRuby2.state = 0
	End Sub

'*******************************************
'  ZMUS - Music
'*******************************************
Dim fCurrentMusicVol : fCurrentMusicVol = 0
Dim sMusicTrack : sMusicTrack = ""
Dim fSongVolume : fSongVolume = 1


Sub SwitchMusic(sTrack)
if Ubound(Songs) < 1 Then Exit Sub ' make sure there are music files to play
Dbg "sTrack: " &sTrack

	if bIdleMusicOn Then StopIdleSound ' make sure idle music is not playing

	if sTrack<> "" Then
		If sTrack <> sMusicTrack Then
			if sMusicTrack <> "" Then EndMusic
			sMusicTrack = Songs(sTrack)
			Dbg "sMusicTrack: " &sMusicTrack

			PlayMusic "MFDOOM\" & Songs(sTrack), fMusicVolume * fSongVolume
			fCurrentMusicVol = fMusicVolume
		End If
	End If
End Sub

Sub StopAllMusic
	EndMusic
End Sub

Dim newSong
Sub UpdateMusicNow
	StopAllMusic
	newSong = RndNbr(Ubound(Songs))
	newSong = NewSong - 1

	Dbg "NewSong: " &newSong

	SwitchMusic newSong

end sub

Sub CheckNoMusicTimer_Timer()
	if sMusicTrack = "" And bIdleMusicOn = False Then RandomRestartMusicSelection

	'Dbg "Skill:RotateLane" & bSkillshotReady &"-" & bSkillshotRotateLights
End Sub

Sub RandomRestartMusicSelection
	IF (bGameInPLay = True) AND (Tilted = False) AND (BallsOnPlayfield > 0) AND (LWarpMultiballCounter.state = 0) THEN
		UpdateMusicNow		
	END IF
End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

' PLAY MUSIC		
	Sub MusicTrigger_hit()
		If LWarpMultiballCounter.state = 0 Then
			UpdateMusicNow
		End If
		If LKicker.state = 1 then 
			LArrow04.state = 2
			LGAS001.BlinkInterval = 125
			LGAS001.state = 2
			LGAS002.BlinkInterval = 125
			LGAS002.state = 2
			LGAS003.BlinkInterval = 125
			LGAS003.state = 2
			LGAS004.BlinkInterval = 125
			LGAS004.state = 2
			LGAS005.BlinkInterval = 125
			LGAS005.state = 2
			LGAS006.BlinkInterval = 125
			LGAS006.state = 2
			LGAS007.BlinkInterval = 125
			LGAS007.state = 2
			LGAS008.BlinkInterval = 125
			LGAS008.state = 2
			LGAS009.BlinkInterval = 125
			LGAS009.state = 2
		End If
		LSlime.state = 0
		LPressStart.state = 0    
		LDoomsday01.state=0 : LDoomsday02.state=0 : LDoomsday03.state=0 : LDoomsday04.state=0 : LDoomsday05.state=0 : LDoomsday06.state=0 : LDoomsday07.state=0 : LDoomsday08.state=0 : LDoomsday09.state=0

	End Sub

	Sub StopTracks_hit()
		'StopAllMusic
	End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' CALLOUTS
	'MULTIBALL Callouts
	Sub OneMoreHitCallout
		If EnableCallouts=1 Then Playsoundcallout "co_onemorehit"
	End Sub
	Sub TwoMoreHitsCallout
		If EnableCallouts=1 Then Playsoundcallout "co_twomorehits"
	End Sub
	Sub AmericasMostBluntedCallout
		Playsoundcallout "co_americasmostblunted"
	End Sub
	Sub AmericasMostBluntedMultiballCallout
		If EnableCallouts=1 Then Playsoundcallout "co_americasmostbluntedmultiball"
	End Sub
	Sub GasDrawlsCallout
		Playsoundcallout "co_gasdrawls"
	End Sub
	Sub GasDrawlsMultiballCallout
		If EnableCallouts=1 Then Playsoundcallout "co_gasdrawlsmultiball"
	End Sub
	Sub GazzillionEarTenSecondCountdownCallout
		Playsoundcallout "co_gazzillionear-10sec"
	End Sub
	Sub GazzillionEarMultiballCallout
		Playsoundcallout "co_gazzillionear-multiball"
	End Sub

	'JACKPOT Callouts
	Sub GasDrawlsJackpotCallout
		Playsoundcallout "co_lookslikeihitthejackpot"
		'RandomUltraDMDSceneGasDrawlsJackpot
		playvideo=20+int(rnd(1)*3)
	End Sub
	Sub AmericasMostBluntedJackpotCallout
		Playsoundcallout "co_lookslikeihitthejackpot"
		'RandomUltraDMDSceneAmericasMostBluntedJackpot
		playvideo=27+int(rnd(1)*3)
	End Sub
	Sub GazzillionEarJackpotCallout
		Playsoundcallout "co_lookslikeihitthejackpot"
		'RandomUltraDMDSceneGazzillionEarJackpot
		playvideo=34+int(rnd(1)*3)
	End Sub
	Sub GazzillionEarSuperJackpotCallout
		Playsoundcallout "co_gazzillionear-superjackpot"
	End Sub
	Sub GazzillionEarSuperJackpotIsLitCallout
		If EnableCallouts=1 Then Playsoundcallout "co_gazzillionear-superjackpotislit"
	End Sub
	'COMBO Callouts
	Sub TwoWayComboCallout
		If EnableCallouts=1 Then Playsoundcallout "co_twowaycombo"
	End Sub
	Sub ThreeWayComboCallout
		If EnableCallouts=1 Then Playsoundcallout "co_threewaycombo"
	End Sub
	Sub FourWayComboCallout
		If EnableCallouts=1 Then Playsoundcallout "co_fourwaycombo"
	End Sub
	Sub FiveWayComboCallout
		If EnableCallouts=1 Then Playsoundcallout "co_fivewaycombo"
	End Sub
	'MULTIPLIER Callouts
	Sub TwoTimesBonusMultiplierCallout
		If EnableCallouts=1 Then Playsoundcallout "co_twotimesbonusmultiplier"
	End Sub
	Sub ThreeTimesBonusMultiplierCallout
		If EnableCallouts=1 Then Playsoundcallout "co_threetimesbonusmultiplier"
	End Sub
	Sub FiveTimesBonusMultiplierCallout
		If EnableCallouts=1 Then Playsoundcallout "co_fivetimesbonusmultiplier"
	End Sub
	'TILT Callouts
	Sub TiltCallout
		If EnableCallouts=1 Then Playsoundcallout "co_tilt"
	End Sub
	Sub TiltWarningCallout
		If EnableCallouts=1 Then Playsoundcallout "co_tiltwarning"
	End Sub
	'COMPLETED Callouts
	Sub LevelOneCompletedCallout
		If EnableCallouts=1 Then Playsoundcallout "co_levelonecompleted"
	End Sub
	Sub LevelTwoCompletedCallout
		If EnableCallouts=1 Then Playsoundcallout "co_leveltwocompleted"
	End Sub
	Sub OrbitsCompletedCallout
		If EnableCallouts=1 Then Playsoundcallout "co_orbitscompleted"
	End Sub
	Sub RampsCompletedCallout
		If EnableCallouts=1 Then Playsoundcallout "co_rampscompleted"
	End Sub
	Sub SpinnerCompletedCallout
		If EnableCallouts=1 Then Playsoundcallout "co_spinnercompleted"
	End Sub
	Sub StandupsCompleteCallout
		If EnableCallouts=1 Then Playsoundcallout "co_standupscomplete"
	End Sub
	'MISCELLANEOUS Callouts
	Sub BallAddedCallout
		If EnableCallouts=1 Then Playsoundcallout "co_balladded"
	End Sub
	Sub BallLockedCallout
		If EnableCallouts=1 Then Playsoundcallout "co_balllocked"
	End Sub
	Sub BallSavedCallout
		Select Case Int(Rnd * 5) + 1
			Case 1:PlaySoundCallOut "fx376"
			Case 2:PlaySoundCallOut "fx378"
			Case 3:PlaySoundCallOut "fx380"
			Case 4:PlaySoundCallOut "fx382"
			Case 5:PlaySoundCallOut "fx383"
		End Select
	End Sub
	Sub DoubleScoringCallout
		If EnableCallouts=1 Then Playsoundcallout "co_doublescoring"
	End Sub
	Sub MysteryBonusCallout
		If EnableCallouts=1 Then Playsoundcallout "co_mysterybonus"
	End Sub
	Sub SkillshotCallout
		dbg "Calling Skillshot"
		If EnableCallouts=1 Then Playsoundcallout "co_skillshot"
	End Sub
	Sub SuperOrbitsCallout
		If EnableCallouts=1 Then Playsoundcallout "co_superorbits"
	End Sub
	Sub SuperPopsCallout
		If EnableCallouts=1 Then Playsoundcallout "co_superpops"
	End Sub
	Sub SuperRampsCallout
		If EnableCallouts=1 Then Playsoundcallout "co_superramps"
	End Sub
	Sub DoubleSpinnerCallout
		If EnableCallouts=1 Then Playsoundcallout "co_doublespinner"
	End Sub
	Sub Player1Callout
		If EnableCallouts=1 Then Playsoundcallout "player1"
	End Sub
	Sub Player2Callout
		If EnableCallouts=1 Then Playsoundcallout "player2"
	End Sub
	Sub Player3Callout
		If EnableCallouts=1 Then Playsoundcallout "player3"
	End Sub
	Sub Player4Callout
		If EnableCallouts=1 Then Playsoundcallout "player4"
	End Sub
	Sub MysteryAliasMissionsUnlockedCallout
		If EnableCallouts=1 Then Playsoundcallout "co_mysteryaliasmissionsunlocked"
	End Sub
	Sub GameOverCallout
		Playsoundcallout "co_gameover"
	End Sub
	Sub ClearMusicCallout
		'StopAllMusic
		'Playsoundcallout "co_clear"
	End Sub
	'********************************************
	'   FLIPPER LIGHTS ON/OFF
	'********************************************
		Dim FlipLightLeft
		FlipLightLeft = 1
	Sub FlipperLightOnLeft()
		FlipLightLeft = FlipLightLeft + 1
		Select Case FlipLightLeft
			Case 1: LEyes01.state = 1
			Case 2: LEyes001.state = 1
			Case 3: LEyes002.state = 1
					FlipLightLeft = 0
		End Select
	End Sub
		Dim FlipLightRight
		FlipLightRight = 1
	Sub FlipperLightOnRight()
		FlipLightRight = FlipLightRight + 1
		Select Case FlipLightRight 
			Case 1: LSupreme007.state = 1
			Case 2: LSupreme009.state = 1
			Case 3: LSupreme026.state = 1
					FlipLightRight = 0
		End Select
	End Sub
	Sub FlipperLightOffLeft
		LEyes01.state = 0
		LEyes001.state = 0
		LEyes002.state = 0
	End Sub
	Sub FlipperLightOffRight
		LSupreme007.state = 0
		LSupreme009.state = 0
		LSupreme026.state = 0
	End Sub


' CalloutVol

sub PlaySoundCallOut(clip)
	PlaySound clip,0,CalloutVol,0,0,1,1,1
end sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' RANDOM SOUND COLLECTION

	Sub RandomSoundHighScores()
		Select Case Int(Rnd * 8) + 1
			Case 1:PlaySoundCallOut "fx051"
			Case 2:PlaySoundCallOut "fx062"
			Case 3:PlaySoundCallOut "fx075"
			Case 4:PlaySoundCallOut "fx083"
			Case 5:PlaySoundCallOut "fx119"
			Case 6:PlaySoundCallOut "fx154"
			Case 7:PlaySoundCallOut "fx195"
			Case 8:PlaySoundCallOut "fx278"
		End Select
	End Sub

		Dim SoundFairyDust
		SoundFairyDust = 1
	Sub RandomSoundFairyDust()
		SoundFairyDust = SoundFairyDust + 1
		Select Case SoundFairyDust			
			Case 1:PlaySoundCallOut "fx200"
			Case 2:PlaySoundCallOut "fx202"
				   SoundFairyDust = 0
		End Select
	End Sub
		Dim SoundBong
		SoundBong = 1
	Sub RandomSoundBong
		SoundBong = SoundBong + 1
		Select Case SoundBong			
			Case 1:PlaySoundCallOut "fx001"
			Case 2:PlaySoundCallOut "fx002"
			Case 3:PlaySoundCallOut "fx003"
				   SoundBong = 0
		End Select
		SmokeSpinBumper
	End Sub
		
	Sub RandomSoundGas()
		Select Case Int(Rnd * 4) + 1				
			Case 1:PlaySoundCallOut "fx281"
			Case 2:PlaySoundCallOut "fx282"
			Case 3:PlaySoundCallOut "fx283"
			Case 4:PlaySoundCallOut "fx284"
		End Select
	End Sub

	Sub RandomSoundGasToo()
		Select Case Int(Rnd * 4) + 1				
			Case 1:PlaySoundCallOut "fx281"
			Case 2:PlaySoundCallOut "fx282"
			Case 3:PlaySoundCallOut "fx283"
			Case 4:PlaySoundCallOut "fx284"
		End Select
	End Sub

	Sub RandomSoundFood()
		Select Case Int(Rnd * 3) + 1
			Case 1:PlaySoundCallOut "fx034"
			Case 2:PlaySoundCallOut "fx036"
			Case 3:PlaySoundCallOut "fx069"
		End Select
	End Sub

	Sub RandomSoundFoods()
		Select Case Int(Rnd * 3) + 1
			Case 1:PlaySoundCallOut "fx033"
			Case 2:PlaySoundCallOut "fx056"
			Case 3:PlaySoundCallOut "fx068"
		End Select
	End Sub

	Sub RandomSoundWind()
		Select Case Int(Rnd * 1) + 1
			Case 1:PlaySoundCallOut "powerdownn"
		End Select
	End Sub

	Sub RandomSoundStartup()
		PlaySoundCallOut "fx067"
		RotDisc1Step = 14 : 
		Disc1Timer.Enabled = 1
		RotDisc2Step = 14
		Disc2Timer.Enabled = 1
		RotDisc3Step = 14
		Disc3Timer.Enabled = 1
		RotDisc4Step = 14
		Disc4Timer.Enabled = 1		
		RotDisc6Step = 14
		Disc6Timer.Enabled = 1
		RotDisc7Step = 14
		Disc7Timer.Enabled = 1
		RotDisc5Step = 14
		Disc5Timer.Enabled = 1
		RotDisc11Step = 14
		Disc11Timer.Enabled = 1
		RotDisc8Step = 14
		Disc8Timer.Enabled = 1
	End Sub

	Sub RandomKickerRubberSound
		Select Case Int(Rnd * 3) + 1
			Case 1:PlaySoundCallOut "fx_rubber_hit_1"
			Case 2:PlaySoundCallOut "fx_rubber_hit_2"
			Case 3:PlaySoundCallOut "fx_rubber_hit_3"
		End Select
	End Sub

	Sub RandomSoundRampEntry
		Select Case Int(Rnd * 3) + 1
			Case 1:PlaySoundCallOut "ramp_hit1"
			Case 2:PlaySoundCallOut "ramp_hit2"
			Case 3:PlaySoundCallOut "ramp_hit3"
		End Select
	End Sub

	Sub RandomSoundRampHit
		Select Case Int(Rnd * 4) + 1
			Case 1:PlaySoundCallOut "rampbump1"
			Case 2:PlaySoundCallOut "rampbump5"
			Case 3:PlaySoundCallOut "rampbump6"
			Case 4:PlaySoundCallOut "rampbump7"
		End Select
	End Sub

	Sub RandomSoundRampExit
		Select Case Int(Rnd * 6) + 1
			Case 1:PlaySoundCallOut "Ball_Drop_Playfield_1_Delayed"
			Case 2:PlaySoundCallOut "Ball_Drop_Playfield_2_Delayed"
			Case 3:PlaySoundCallOut "Ball_Drop_Playfield_3_Delayed"
			Case 4:PlaySoundCallOut "Ball_Drop_Playfield_4_Delayed"
			Case 5:PlaySoundCallOut "Ball_Drop_Playfield_5_Delayed"
			Case 6:PlaySoundCallOut "fx_ballrampdrop"
		End Select
	End Sub

	Sub RandomSoundWireRampRolling
		Select Case Int(Rnd * 1) + 1
			Case 1:PlaySoundCallOut "fx_metalrolling"		
		End Select
	End Sub

	Sub RandomSoundSwitch()
		Select Case Int(Rnd * 5) + 1
			Case 1:PlaySoundCallOut "fx228"
			Case 2:PlaySoundCallOut "fx240"
			Case 3:PlaySoundCallOut "fx241"
			Case 4:PlaySoundCallOut "fx252"
			Case 5:PlaySoundCallOut "fx253"
		End Select
	End Sub

	Sub RandomSoundBumper()
		Select Case Int(Rnd * 2) + 1			
			Case 1:PlaySoundCallOut "fx236"
			Case 2:PlaySoundCallOut "fx237"
		End Select
	End Sub

		Dim SoundPunch
		SoundPunch = 1
	Sub RandomSoundPunch()
		SoundPunch = SoundPunch + 1
		Select Case SoundPunch			
			Case 1:PlaySoundCallOut "fx232"
			Case 2:PlaySoundCallOut "fx233"
			Case 3:PlaySoundCallOut "fx239"
				   SoundPunch = 0
		End Select
	End Sub

	Sub RandomSoundMetalFingersLeft()
		Select Case Int(Rnd * 15) + 1			
			Case 1: RandomSoundPunch
			Case 2: RandomSoundPunch
			Case 3: RandomSoundPunch
			Case 4: RandomSoundPunch
			Case 5: RandomSoundPunch
			Case 6: RandomSoundPunch
			Case 7: RandomSoundPunch
			Case 8: RandomSoundPunch
			Case 9: PlaySoundCallOut "fx359" : Flash18 : dank6Shaker : dank7Shaker : dank10Shaker : ChangeColorMaskBig
					If DMDFistLeft=0 Then DMDFistLeft=1
			Case 10: PlaySoundCallOut "fx360" : Flash18 : dank6Shaker : dank7Shaker : dank10Shaker : ChangeColorMaskBig
					If DMDFistLeft=0 Then DMDFistLeft=1
			Case 11: PlaySoundCallOut "fx361" : Flash18 : dank6Shaker : dank7Shaker : dank10Shaker : ChangeColorMaskBig
					If DMDFistLeft=0 Then DMDFistLeft=1
			Case 12: PlaySoundCallOut "fx362" : Flash18 : dank6Shaker : dank7Shaker : dank10Shaker : ChangeColorMaskBig
					If DMDFistLeft=0 Then DMDFistLeft=1
			Case 13: PlaySoundCallOut "fx363" : Flash18 : dank6Shaker : dank7Shaker : dank10Shaker : ChangeColorMaskBig
					If DMDFistLeft=0 Then DMDFistLeft=1
			Case 14: PlaySoundCallOut "fx362" : Flash18 : dank6Shaker : dank7Shaker : dank10Shaker : ChangeColorMaskBig
					If DMDFistLeft=0 Then DMDFistLeft=1
			Case 15: PlaySoundCallOut "fx363" : Flash18 : dank6Shaker : dank7Shaker : dank10Shaker : ChangeColorMaskBig
					If DMDFistLeft=0 Then DMDFistLeft=1
		End Select
	End Sub

	Sub RandomSoundMetalFingersRight()
		Select Case Int(Rnd * 15) + 1			
			Case 1: RandomSoundPunch
			Case 2: RandomSoundPunch
			Case 3: RandomSoundPunch
			Case 4: RandomSoundPunch
			Case 5: RandomSoundPunch
			Case 6: RandomSoundPunch
			Case 7: RandomSoundPunch
			Case 8: RandomSoundPunch
			Case 9: PlaySoundCallOut "fx359" : Flash18 : dank6Shaker : dank7Shaker : dank11Shaker : ChangeColorMaskBig
					If DMDFistRight=0 Then DMDFistRight=1
			Case 10: PlaySoundCallOut "fx360" : Flash18 : dank6Shaker : dank7Shaker : dank11Shaker : ChangeColorMaskBig
					If DMDFistRight=0 Then DMDFistRight=1
			Case 11: PlaySoundCallOut "fx361" : Flash18 : dank6Shaker : dank7Shaker : dank11Shaker : ChangeColorMaskBig
					If DMDFistRight=0 Then DMDFistRight=1
			Case 12: PlaySoundCallOut "fx362" : Flash18 : dank6Shaker : dank7Shaker : dank11Shaker : ChangeColorMaskBig
					If DMDFistRight=0 Then DMDFistRight=1
			Case 13: PlaySoundCallOut "fx363" : Flash18 : dank6Shaker : dank7Shaker : dank11Shaker : ChangeColorMaskBig
					If DMDFistRight=0 Then DMDFistRight=1
			Case 14: PlaySoundCallOut "fx362" : Flash18 : dank6Shaker : dank7Shaker : dank11Shaker : ChangeColorMaskBig
					If DMDFistRight=0 Then DMDFistRight=1
			Case 15: PlaySoundCallOut "fx363" : Flash18 : dank6Shaker : dank7Shaker : dank11Shaker : ChangeColorMaskBig
					If DMDFistRight=0 Then DMDFistRight=1
		End Select
	End Sub

	Sub RandomSoundSideTargets()
		Select Case Int(Rnd * 40) + 1			
			Case 1:PlaySoundCallOut "fx026"
			Case 2:PlaySoundCallOut "fx084"
			Case 3:PlaySoundCallOut "fx113"
			Case 4:PlaySoundCallOut "fx120"
			Case 5:PlaySoundCallOut "fx200"
			Case 6:PlaySoundCallOut "fx201"
			Case 7:PlaySoundCallOut "fx202"
			Case 8:PlaySoundCallOut "fx203"
			Case 9:PlaySoundCallOut "fx222"
			Case 10:PlaySoundCallOut "fx223"
			Case 11:PlaySoundCallOut "fx224"
			Case 12:PlaySoundCallOut "fx225"
			Case 13:PlaySoundCallOut "fx226"
			Case 14:PlaySoundCallOut "fx227"
			Case 15:PlaySoundCallOut "fx229"
			Case 16:PlaySoundCallOut "fx230"
			Case 17:PlaySoundCallOut "fx234"
			Case 18:PlaySoundCallOut "fx331"
			Case 19:PlaySoundCallOut "fx242"
			Case 20:PlaySoundCallOut "fx243"
			Case 21:PlaySoundCallOut "fx244"
			Case 22:PlaySoundCallOut "fx245"
			Case 23:PlaySoundCallOut "fx249"
			Case 24:PlaySoundCallOut "fx251"
			Case 25:PlaySoundCallOut "fx254"
			Case 26:PlaySoundCallOut "fx255"
			Case 27:PlaySoundCallOut "fx317"
			Case 28:PlaySoundCallOut "fx318"
			Case 29:PlaySoundCallOut "fx319"
			Case 30:PlaySoundCallOut "fx320"
			Case 31:PlaySoundCallOut "fx321"
			Case 32:PlaySoundCallOut "fx322"
			Case 33:PlaySoundCallOut "fx323"
			Case 34:PlaySoundCallOut "fx325"
			Case 35:PlaySoundCallOut "fx327"
			Case 36:PlaySoundCallOut "fx328"
			Case 37:PlaySoundCallOut "fx340"
			Case 38:PlaySoundCallOut "fx341"
			Case 39:PlaySoundCallOut "fx342"
			Case 40:RandomSoundGas
		End Select
	End Sub

		Dim IdleCalloutIndex
		IdleCalloutIndex = Int(Rnd * 12) + 1
	Sub RandomSoundPlungerIdle
		'chilloutthemusic
		bIdleMusicOn = True
		Dbg "Playing Idle Track: " &IdleCalloutIndex
		StopAllMusic
		IdleCalloutIndex = IdleCalloutIndex + 1
		Select Case IdleCalloutIndex			
			Case 1:PlaySound "fx308-idle", -1
			Case 2:PlaySound "fx309-idle", -1
			Case 3:PlaySound "fx310-idle", -1
			Case 4:PlaySound "fx311-idle", -1
			Case 5:PlaySound "fx312-idle", -1
			Case 6:PlaySound "fx333-idle", -1
			Case 7:PlaySound "fx334-idle", -1
			Case 8:PlaySound "fx335-idle", -1
			Case 9:PlaySound "fx336-idle", -1
			Case 10:PlaySound "fx337-idle", -1
			Case 11:PlaySound "fx338-idle", -1
			Case 12:PlaySound "fx339-idle", -1
				   IdleCalloutIndex = 0
		End Select

	End Sub

'	Sub StopIdleSound
'		Select Case IdleCalloutIndex			
'			Case 1:StopSound "fx308-idle"
'			Case 2:StopSound "fx309-idle"
'			Case 3:StopSound "fx310-idle"
'			Case 4:StopSound "fx311-idle"
'			Case 5:StopSound "fx312-idle"
'			Case 6:StopSound "fx333-idle"
'			Case 7:StopSound "fx334-idle"
'			Case 8:StopSound "fx335-idle"
'			Case 9:StopSound "fx336-idle"
'			Case 10:StopSound "fx337-idle"
'			Case 11:StopSound "fx338-idle"
'			Case 12:StopSound "fx339-idle"
'		End Select
'	End Sub

	Sub StopIdleSound
		Dbg "Stopping Idle Tracks"	
'		Select Case IdleCalloutIndex			
			StopSound "fx308-idle"
			StopSound "fx309-idle"
			StopSound "fx310-idle"
			StopSound "fx311-idle"
			StopSound "fx312-idle"
			StopSound "fx333-idle"
			StopSound "fx334-idle"
			StopSound "fx335-idle"
			StopSound "fx336-idle"
			StopSound "fx337-idle"
			StopSound "fx338-idle"
			StopSound "fx339-idle"
'		End Select
			bIdleMusicOn = False
	End Sub

		Dim SoundPlungerShoot
		SoundPlungerShoot = Int(Rnd * 9) + 1
	Sub RandomSoundPlungerShoot()
		SoundPlungerShoot = SoundPlungerShoot + 1
		'StopIdleSound
		Select Case SoundPlungerShoot			
			Case 1:PlaySoundCallOut "fx061"
			Case 2:PlaySoundCallOut "fx192"
			Case 3:PlaySoundCallOut "fx193"
			Case 4:PlaySoundCallOut "fx231"
			Case 5:PlaySoundCallOut "fx254"
			Case 6:PlaySoundCallOut "fx255"
			Case 7:PlaySoundCallOut "fx329"
			Case 8:PlaySoundCallOut "fx331"
			Case 9:PlaySoundCallOut "fx332"
				   SoundPlungerShoot = 0
		End Select
		'turnitbackup
		RandomRestartMusicSelection
	End Sub
		
		Dim SoundKickout
		SoundKickout = Int(Rnd * 38) + 1	
	Sub RandomSoundKickout()
		SoundKickout = SoundKickout + 1
		Select Case SoundKickout		
			Case 1:PlaySoundCallOut "fx135"
			Case 2:PlaySoundCallOut "fx043"
			Case 3:PlaySoundCallOut "fx047"
			Case 4:PlaySoundCallOut "fx058"
			Case 5:PlaySoundCallOut "fx062"
			Case 6:PlaySoundCallOut "fx063"
			Case 7:PlaySoundCallOut "fx066"
			Case 8:PlaySoundCallOut "fx034"
			Case 9:PlaySoundCallOut "fx080"
			Case 10:PlaySoundCallOut "fx105"
			Case 11:PlaySoundCallOut "fx118"
			Case 12:PlaySoundCallOut "fx129"
			Case 13:PlaySoundCallOut "fx132"
			Case 14:PlaySoundCallOut "fx287"
			Case 15:PlaySoundCallOut "fx145"
			Case 16:PlaySoundCallOut "fx147"
			Case 17:PlaySoundCallOut "fx154"
			Case 18:PlaySoundCallOut "fx157"
			Case 19:PlaySoundCallOut "fx041"
			Case 20:PlaySoundCallOut "fx287"
			Case 21:PlaySoundCallOut "fx195"
			Case 22:PlaySoundCallOut "fx196"
			Case 23:PlaySoundCallOut "fx197"
			Case 24:PlaySoundCallOut "fx201"
			Case 25:PlaySoundCallOut "fx202"
			Case 26:PlaySoundCallOut "fx222"
			Case 27:PlaySoundCallOut "fx225"
			Case 28:PlaySoundCallOut "fx227"
			Case 29:PlaySoundCallOut "fx230"
			Case 30:PlaySoundCallOut "fx226"
			Case 31:PlaySoundCallOut "fx254"
			Case 32:PlaySoundCallOut "fx255"
			Case 33:PlaySoundCallOut "fx319"
			Case 34:PlaySoundCallOut "fx320"
			Case 35:PlaySoundCallOut "fx321"
			Case 36:PlaySoundCallOut "fx322"
			Case 37:PlaySoundCallOut "fx325"
			Case 38:PlaySoundCallOut "fx327"
				    SoundKickout = 0
		End Select
		RandomSmokeGif
	End Sub

		Dim SoundOutlane
		SoundOutlane = Int(Rnd * 39) + 1	
	Sub RandomSoundOutlane()
		SoundOutlane = SoundOutlane + 1
		Select Case SoundOutlane			
			Case 1:PlaySoundCallOut "fx005"
			Case 2:PlaySoundCallOut "fx023"
			Case 3:PlaySoundCallOut "fx026"
			Case 4:PlaySoundCallOut "fx068"
			Case 5:PlaySoundCallOut "fx083"
			Case 6:PlaySoundCallOut "fx085"
			Case 7:PlaySoundCallOut "fx098"
			Case 8:PlaySoundCallOut "fx120"
			Case 9:PlaySoundCallOut "fx125"
			Case 10:PlaySoundCallOut "fx145"
			Case 11:PlaySoundCallOut "fx182"
			Case 12:PlaySoundCallOut "fx199"
			Case 13:PlaySoundCallOut "fx201"
			Case 14:PlaySoundCallOut "fx204"
			Case 15:PlaySoundCallOut "fx206"
			Case 16:PlaySoundCallOut "fx225"
			Case 17:PlaySoundCallOut "fx227"
			Case 18:PlaySoundCallOut "fx243"
			Case 19:PlaySoundCallOut "fx245"
			Case 20:PlaySoundCallOut "fx249"
			Case 21:PlaySoundCallOut "fx259"
			Case 22:PlaySoundCallOut "fx265"
			Case 23:PlaySoundCallOut "fx271"
			Case 24:PlaySoundCallOut "fx272"
			Case 25:PlaySoundCallOut "fx287"
			Case 26:PlaySoundCallOut "fx293"
			Case 27:PlaySoundCallOut "fx301"
			Case 28:PlaySoundCallOut "fx317"
			Case 29:PlaySoundCallOut "fx318"
			Case 30:PlaySoundCallOut "fx319"
			Case 31:PlaySoundCallOut "fx320"
			Case 32:PlaySoundCallOut "fx321"
			Case 33:PlaySoundCallOut "fx323"
			Case 34:PlaySoundCallOut "fx324"
			Case 35:PlaySoundCallOut "fx325"
			Case 36:PlaySoundCallOut "fx326"
			Case 37:PlaySoundCallOut "fx327"
			Case 38:PlaySoundCallOut "fx328"
			Case 39:PlaySoundCallOut "fx343"
				    SoundOutlane = 0
		End Select
	End Sub

	Sub RandomSoundDrainBottom()
		If (BallsOnPlayfield >= 1) THEN
			RandomSoundDrainBottomLight
		Else
			IF bBallSaverActive = True Then
				RandomSoundDrainBottomLight
			Else
				RandomSoundDrainBottomHeavy
			End If
		End If
	End Sub

		Dim SoundDrainBottomLight
		SoundDrainBottomLight = Int(Rnd * 14) + 1
	Sub RandomSoundDrainBottomLight()
		SoundDrainBottomLight = SoundDrainBottomLight + 1
		Select Case SoundDrainBottomLight		
			Case 1:PlaySoundCallOut "fx036"   
			Case 2:PlaySoundCallOut "fx045"
			Case 3:PlaySoundCallOut "fx051"
			Case 4:PlaySoundCallOut "fx053"
			Case 5:PlaySoundCallOut "fx054"
			Case 6:PlaySoundCallOut "fx065"
			Case 7:PlaySoundCallOut "fx068"
			Case 8:PlaySoundCallOut "fx074"
			Case 9:PlaySoundCallOut "fx083"
			Case 10:PlaySoundCallOut "fx098"
			Case 11:PlaySoundCallOut "fx139"
			Case 12:PlaySoundCallOut "fx146"
			Case 13:PlaySoundCallOut "fx160"
			Case 14:PlaySoundCallOut "fx162"			
	                SoundDrainBottomLight = 0
		End Select
	End Sub

		Dim SoundDrainBottomHeavy
		SoundDrainBottomHeavy = Int(Rnd * 46) + 1
	Sub RandomSoundDrainBottomHeavy()
		SoundDrainBottomHeavy = SoundDrainBottomHeavy + 1
		Select Case SoundDrainBottomHeavy			
			Case 1:PlaySoundCallOut "fx008"
			Case 2:PlaySoundCallOut "fx014"
			Case 3:PlaySoundCallOut "fx022"
			Case 4:PlaySoundCallOut "fx033"
			Case 5:PlaySoundCallOut "fx250"
			Case 6:PlaySoundCallOut "fx035"
			Case 7:PlaySoundCallOut "fx040"
			Case 8:PlaySoundCallOut "fx044"
			Case 9:PlaySoundCallOut "fx238"
			Case 10:PlaySoundCallOut "fx289"
			Case 11:PlaySoundCallOut "fx056"
			Case 12:PlaySoundCallOut "fx286"
			Case 13:PlaySoundCallOut "fx071"
			Case 14:PlaySoundCallOut "fx213"
			Case 15:PlaySoundCallOut "fx072"
			Case 16:PlaySoundCallOut "fx082"
			Case 17:PlaySoundCallOut "fx106"
			Case 18:PlaySoundCallOut "fx117"
			Case 19:PlaySoundCallOut "fx196"
			Case 20:PlaySoundCallOut "fx127"
			Case 21:PlaySoundCallOut "fx130"
			Case 22:PlaySoundCallOut "fx163"
			Case 23:PlaySoundCallOut "fx195"
			Case 24:PlaySoundCallOut "fx166"
			Case 25:PlaySoundCallOut "fx167"
			Case 26:PlaySoundCallOut "fx168"
			Case 27:PlaySoundCallOut "fx182"
			Case 28:PlaySoundCallOut "fx216"
			Case 29:PlaySoundCallOut "fx256"
			Case 30:PlaySoundCallOut "fx264"
			Case 31:PlaySoundCallOut "fx267"
			Case 32:PlaySoundCallOut "fx164"
			Case 33:PlaySoundCallOut "fx274"
			Case 34:PlaySoundCallOut "fx276"
			Case 35:PlaySoundCallOut "fx277"
			Case 36:PlaySoundCallOut "fx116"
			Case 37:PlaySoundCallOut "fx278"
			Case 38:PlaySoundCallOut "fx286"
			Case 39:PlaySoundCallOut "fx292"
			Case 40:PlaySoundCallOut "fx316"
			Case 41:PlaySoundCallOut "fx350"
			Case 42:PlaySoundCallOut "fx353"
			Case 43:PlaySoundCallOut "fx355"
			Case 44:PlaySoundCallOut "fx377"
			Case 45:PlaySoundCallOut "fx379"
			Case 46:PlaySoundCallOut "fx384"
	                SoundDrainBottomHeavy = 0
		End Select
	End Sub

	Sub RandomSpinfx()
		Select Case Int(Rnd * 37) + 1
			Case 1:PlaySoundCallOut "fx084"
			Case 2:PlaySoundCallOut "fx113"
			Case 3:PlaySoundCallOut "fx120"
			Case 4:PlaySoundCallOut "fx203"
			Case 5:PlaySoundCallOut "fx204"
			Case 6:PlaySoundCallOut "fx222"
			Case 7:PlaySoundCallOut "fx223"
			Case 8:PlaySoundCallOut "fx225"
			Case 9:PlaySoundCallOut "fx226"
			Case 10:PlaySoundCallOut "fx229"
			Case 11:PlaySoundCallOut "fx230"
			Case 12:PlaySoundCallOut "fx245"
			Case 13:PlaySoundCallOut "fx249"
			Case 14:PlaySoundCallOut "fx251"
			Case 15:PlaySoundCallOut "fx252"
			Case 16:PlaySoundCallOut "fx253"
			Case 17:PlaySoundCallOut "fx254"
			Case 18:PlaySoundCallOut "fx255"
			Case 19:PlaySoundCallOut "fx271"
			Case 20:PlaySoundCallOut "fx294"
			Case 21:PlaySoundCallOut "fx318"
			Case 22:PlaySoundCallOut "fx319"
			Case 23:PlaySoundCallOut "fx320"
			Case 24:PlaySoundCallOut "fx322"
			Case 25:PlaySoundCallOut "fx323"
			Case 26:PlaySoundCallOut "fx325"
			Case 27:PlaySoundCallOut "fx326"
			Case 28:PlaySoundCallOut "fx327"
			Case 29:PlaySoundCallOut "fx343"
			Case 30:PlaySoundCallOut "fx331"
			Case 31:PlaySoundCallOut "fx340"
			Case 32:PlaySoundCallOut "fx341"
			Case 33:PlaySoundCallOut "fx342"
			Case 34:PlaySoundCallOut "Supreme_fx_11"
			Case 35:Randomsoundbong
			Case 36:RandomSoundGasToo
			Case 37:RandomSoundFairyDust
		End Select
	End Sub

	Sub RandomSoundBeat()
		If (BallsOnPlayfield > 1) THEN
			RandomSoundTargetLight
		Else
			Randomdjbeat
		End If
	End Sub

		Dim djbeat
		djbeat = Int(Rnd * 1) + 1
	Sub Randomdjbeat()
		djbeat = djbeat + 1
		Select Case djbeat
			Case 1:RandomSoundTargetLight   
			Case 2:RandomSoundTargetLight   
			Case 3:RandomSoundBeatHeavy
				   djbeat = 0
		End Select
	End Sub

	Sub RandomSoundBeatHeavy()
		Select Case Int(Rnd * 44) + 1
			Case 1:PlaySoundCallOut "fx004"
			Case 2:PlaySoundCallOut "fx009"
			Case 3:PlaySoundCallOut "fx010"
			Case 4:PlaySoundCallOut "fx013"
			Case 5:PlaySoundCallOut "fx020"
			Case 6:PlaySoundCallOut "fx021"
			Case 7:PlaySoundCallOut "fx024"
			Case 8:PlaySoundCallOut "fx025"
			Case 9:PlaySoundCallOut "fx031"
			Case 10:PlaySoundCallOut "fx042"
			Case 11:PlaySoundCallOut "fx081"
			Case 12:PlaySoundCallOut "fx086"
			Case 13:PlaySoundCallOut "fx087"
			Case 14:PlaySoundCallOut "fx089"
			Case 15:PlaySoundCallOut "fx092"
			Case 16:PlaySoundCallOut "fx093"
			Case 17:PlaySoundCallOut "fx094"
			Case 18:PlaySoundCallOut "fx096"
			Case 19:PlaySoundCallOut "fx217"
			Case 20:PlaySoundCallOut "fx211"
			Case 21:PlaySoundCallOut "fx109"
			Case 22:PlaySoundCallOut "fx112"
			Case 23:PlaySoundCallOut "fx302"
			Case 24:PlaySoundCallOut "fx303"
			Case 25:PlaySoundCallOut "fx148"
			Case 26:PlaySoundCallOut "fx149"
			Case 27:PlaySoundCallOut "fx306"
			Case 28:PlaySoundCallOut "fx171"
			Case 29:PlaySoundCallOut "fx174"
			Case 30:PlaySoundCallOut "fx300"
			Case 31:PlaySoundCallOut "fx176"
			Case 32:PlaySoundCallOut "fx210"
			Case 33:PlaySoundCallOut "fx299"
			Case 34:PlaySoundCallOut "fx179"
			Case 35:PlaySoundCallOut "fx180"
			Case 36:PlaySoundCallOut "fx181"
			Case 37:PlaySoundCallOut "fx183"
			Case 38:PlaySoundCallOut "fx184"
			Case 39:PlaySoundCallOut "fx185"
			Case 40:PlaySoundCallOut "fx297"
			Case 41:PlaySoundCallOut "fx212"
			Case 42:PlaySoundCallOut "fx189"
			Case 43:PlaySoundCallOut "fx209"
			Case 44:PlaySoundCallOut "fx198"
		End Select
	End Sub

	Sub RandomSoundTarget()
		If (BallsOnPlayfield > 1) THEN
			RandomSoundTargetLight
		Else
			RandomSoundTargetHeavy
		End If
	End Sub

	Sub RandomSoundTargetLight()
		Select Case Int(Rnd * 151) + 1
			Case 1:RandomSoundBong
			Case 2:RandomSoundBong
			Case 3:RandomSoundBong
			Case 4:PlaySoundCallOut "fx005"
			Case 5:PlaySoundCallOut "fx007"
			Case 6:PlaySoundCallOut "fx008"
			Case 7:PlaySoundCallOut "fx011"
			Case 8:PlaySoundCallOut "fx023"
			Case 9:PlaySoundCallOut "fx026"
			Case 10:PlaySoundCallOut "fx028"
			Case 11:PlaySoundCallOut "fx034"
			Case 12:PlaySoundCallOut "fx036"
			Case 13:PlaySoundCallOut "fx043"
			Case 14:PlaySoundCallOut "fx045"
			Case 15:PlaySoundCallOut "fx051"
			Case 16:PlaySoundCallOut "fx053"
			Case 17:PlaySoundCallOut "fx054"
			Case 18:PlaySoundCallOut "fx056"
			Case 19:PlaySoundCallOut "fx057"
			Case 20:PlaySoundCallOut "fx058"
			Case 21:PlaySoundCallOut "fx287"
			Case 22:PlaySoundCallOut "fx286"
			Case 23:PlaySoundCallOut "fx062"
			Case 24:PlaySoundCallOut "fx063"
			Case 25:PlaySoundCallOut "fx065"
			Case 26:PlaySoundCallOut "fx066"
			Case 27:PlaySoundCallOut "fx067"
			Case 28:PlaySoundCallOut "fx068"
			Case 29:PlaySoundCallOut "fx069"
			Case 30:PlaySoundCallOut "fx074"
			Case 31:PlaySoundCallOut "fx075"
			Case 32:PlaySoundCallOut "fx076"
			Case 33:PlaySoundCallOut "fx077"
			Case 34:PlaySoundCallOut "fx080"
			Case 35:PlaySoundCallOut "fx083"
			Case 36:PlaySoundCallOut "fx084"
			Case 37:PlaySoundCallOut "fx085"
			Case 38:PlaySoundCallOut "fx090"
			Case 39:PlaySoundCallOut "fx097"
			Case 40:PlaySoundCallOut "fx098"
			Case 41:PlaySoundCallOut "fx099"
			Case 42:PlaySoundCallOut "fx102"
			Case 43:PlaySoundCallOut "fx105"
			Case 44:PlaySoundCallOut "fx113"
			Case 45:PlaySoundCallOut "fx115"
			Case 46:PlaySoundCallOut "fx116"
			Case 47:PlaySoundCallOut "fx117"
			Case 48:PlaySoundCallOut "fx128"
			Case 49:PlaySoundCallOut "fx129"
			Case 50:PlaySoundCallOut "fx136"
			Case 51:PlaySoundCallOut "fx137"
			Case 52:PlaySoundCallOut "fx139"
			Case 53:PlaySoundCallOut "fx145"
			Case 54:PlaySoundCallOut "fx146"
			Case 55:PlaySoundCallOut "fx147"
			Case 56:PlaySoundCallOut "fx151"
			Case 57:PlaySoundCallOut "fx154"
			Case 58:PlaySoundCallOut "fx155"
			Case 59:PlaySoundCallOut "fx160"
			Case 60:PlaySoundCallOut "fx162"
			Case 61:PlaySoundCallOut "fx182"
			Case 62:PlaySoundCallOut "fx190"
			Case 63:PlaySoundCallOut "fx192"
			Case 64:PlaySoundCallOut "fx193"
			Case 65:PlaySoundCallOut "fx061"
			Case 66:PlaySoundCallOut "fx195"
			Case 67:PlaySoundCallOut "fx196" 
			Case 68:PlaySoundCallOut "fx197"
			Case 69:PlaySoundCallOut "fx199" 
			Case 70:PlaySoundCallOut "fx200" 
			Case 71:PlaySoundCallOut "fx201" 
			Case 72:PlaySoundCallOut "fx202" 
			Case 73:PlaySoundCallOut "fx203" 
			Case 74:PlaySoundCallOut "fx204" 
			Case 75:PlaySoundCallOut "fx205" 
			Case 76:PlaySoundCallOut "fx206" 
			Case 77:PlaySoundCallOut "fx213" 
			Case 78:PlaySoundCallOut "fx218"  
			Case 79:PlaySoundCallOut "fx222" 
			Case 80:PlaySoundCallOut "fx223" 
			Case 81:PlaySoundCallOut "fx224" 
			Case 82:PlaySoundCallOut "fx225" 
			Case 83:PlaySoundCallOut "fx226" 
			Case 84:PlaySoundCallOut "fx227" 
			Case 85:PlaySoundCallOut "fx229" 
			Case 86:PlaySoundCallOut "fx230" 
			Case 87:PlaySoundCallOut "fx231" 
			Case 88:PlaySoundCallOut "fx234" 
			Case 89:PlaySoundCallOut "fx238" 
			Case 90:PlaySoundCallOut "fx242" 
			Case 91:PlaySoundCallOut "fx243" 
			Case 92:PlaySoundCallOut "fx244" 
			Case 93:PlaySoundCallOut "fx245" 
			Case 94:PlaySoundCallOut "fx246" 
			Case 95:PlaySoundCallOut "fx247"
			Case 96:PlaySoundCallOut "fx248"
			Case 97:PlaySoundCallOut "fx249"
			Case 98:PlaySoundCallOut "fx250"
			Case 99:PlaySoundCallOut "fx251"
			Case 100:PlaySoundCallOut "fx254"
			Case 101:PlaySoundCallOut "fx255"
			Case 102:PlaySoundCallOut "fx257"
			Case 103:PlaySoundCallOut "fx259"
			Case 104:PlaySoundCallOut "fx261"
			Case 105:PlaySoundCallOut "fx263"
			Case 106:PlaySoundCallOut "fx264"
			Case 107:PlaySoundCallOut "fx265"
			Case 108:PlaySoundCallOut "fx268"
			Case 109:PlaySoundCallOut "fx269"
			Case 110:PlaySoundCallOut "fx270"
			Case 111:PlaySoundCallOut "fx271"
			Case 112:PlaySoundCallOut "fx272"
			Case 113:PlaySoundCallOut "fx287"
			Case 114:PlaySoundCallOut "fx290"			
			Case 115:PlaySoundCallOut "fx293"
			Case 116:PlaySoundCallOut "fx294"
			Case 117:PlaySoundCallOut "fx009" 
			Case 118:PlaySoundCallOut "fx031"
			Case 119:PlaySoundCallOut "fx042"
			Case 120:PlaySoundCallOut "fx081"
			Case 121:PlaySoundCallOut "fx089"
			Case 122:PlaySoundCallOut "fx096"
			Case 123:PlaySoundCallOut "fx171"
			Case 124:PlaySoundCallOut "fx198"
			Case 125:PlaySoundCallOut "fx210"
			Case 126:PlaySoundCallOut "fx317"
			Case 127:PlaySoundCallOut "fx318"
			Case 128:PlaySoundCallOut "fx319"
			Case 129:PlaySoundCallOut "fx320"
			Case 130:PlaySoundCallOut "fx321"
			Case 131:PlaySoundCallOut "fx322"
			Case 132:PlaySoundCallOut "fx323"
			Case 133:PlaySoundCallOut "fx325"
			Case 134:PlaySoundCallOut "fx327"
			Case 135:PlaySoundCallOut "fx328"
			Case 136:PlaySoundCallOut "fx340"
			Case 137:PlaySoundCallOut "fx341"
			Case 138:PlaySoundCallOut "fx342"
			Case 139:PlaySoundCallOut "fx343"
			Case 140:PlaySoundCallOut "fx344"
			Case 141:PlaySoundCallOut "fx345"
			Case 142:PlaySoundCallOut "fx346"
			Case 143:PlaySoundCallOut "fx347"
			Case 144:PlaySoundCallOut "fx348"
			Case 145:PlaySoundCallOut "fx349"
			Case 146:PlaySoundCallOut "fx351"
			Case 147:PlaySoundCallOut "fx352"
			Case 148:PlaySoundCallOut "fx353"
			Case 149:PlaySoundCallOut "fx354"
			Case 150:PlaySoundCallOut "fx355"
			Case 151:PlaySoundCallOut "fx357"
		End Select
	End Sub

	Sub RandomSoundTargetHeavy()
		Select Case Int(Rnd * 234) + 1
			Case 1:RandomSoundBong
			Case 2:RandomSoundBong
			Case 3:RandomSoundBong
			Case 4:PlaySoundCallOut "fx005"
			Case 5:PlaySoundCallOut "fx007"
			Case 6:PlaySoundCallOut "fx008"
			Case 7:PlaySoundCallOut "fx011"
			Case 8:PlaySoundCallOut "fx014"
			Case 9:PlaySoundCallOut "fx015"
			Case 10:PlaySoundCallOut "fx016"
			Case 11:PlaySoundCallOut "fx017"
			Case 12:PlaySoundCallOut "fx018"
			Case 13:PlaySoundCallOut "fx019"
			Case 14:PlaySoundCallOut "fx022"
			Case 15:PlaySoundCallOut "fx023"
			Case 16:PlaySoundCallOut "fx026"
			Case 17:PlaySoundCallOut "fx027"
			Case 18:PlaySoundCallOut "fx028"
			Case 19:PlaySoundCallOut "fx029"
			Case 20:PlaySoundCallOut "fx030"
			Case 21:PlaySoundCallOut "fx032"
			Case 22:PlaySoundCallOut "fx033"
			Case 23:PlaySoundCallOut "fx034"
			Case 24:PlaySoundCallOut "fx035"
			Case 25:PlaySoundCallOut "fx036"
			Case 26:PlaySoundCallOut "fx037"
			Case 27:PlaySoundCallOut "fx038"
			Case 28:PlaySoundCallOut "fx039"
			Case 29:PlaySoundCallOut "fx041"
			Case 30:PlaySoundCallOut "fx043"
			Case 31:PlaySoundCallOut "fx044"
			Case 32:PlaySoundCallOut "fx045"
			Case 33:PlaySoundCallOut "fx046"
			Case 34:PlaySoundCallOut "fx047"
			Case 35:PlaySoundCallOut "fx289"
			Case 36:PlaySoundCallOut "fx049"
			Case 37:PlaySoundCallOut "fx050"
			Case 38:PlaySoundCallOut "fx051"
			Case 39:PlaySoundCallOut "fx052"
			Case 40:PlaySoundCallOut "fx053"
			Case 41:PlaySoundCallOut "fx054"
			Case 42:PlaySoundCallOut "fx288"
			Case 43:PlaySoundCallOut "fx056"
			Case 44:PlaySoundCallOut "fx057"
			Case 45:PlaySoundCallOut "fx058"
			Case 46:PlaySoundCallOut "fx287"
			Case 47:PlaySoundCallOut "fx286"
			Case 48:PlaySoundCallOut "fx062"
			Case 49:PlaySoundCallOut "fx063"
			Case 50:PlaySoundCallOut "fx064"
			Case 51:PlaySoundCallOut "fx065"
			Case 52:PlaySoundCallOut "fx066"
			Case 53:PlaySoundCallOut "fx067"
			Case 54:PlaySoundCallOut "fx068"
			Case 55:PlaySoundCallOut "fx069"
			Case 56:PlaySoundCallOut "fx070"
			Case 57:PlaySoundCallOut "fx071"
			Case 58:PlaySoundCallOut "fx072"
			Case 59:PlaySoundCallOut "fx074"
			Case 60:PlaySoundCallOut "fx075"
			Case 61:PlaySoundCallOut "fx076"
			Case 62:PlaySoundCallOut "fx077"
			Case 63:PlaySoundCallOut "fx078"
			Case 64:PlaySoundCallOut "fx079"
			Case 65:PlaySoundCallOut "fx080"
			Case 66:PlaySoundCallOut "fx082"
			Case 67:PlaySoundCallOut "fx083"
			Case 68:PlaySoundCallOut "fx084"
			Case 69:PlaySoundCallOut "fx085"
			Case 70:PlaySoundCallOut "fx088"
			Case 71:PlaySoundCallOut "fx090"
			Case 72:PlaySoundCallOut "fx091"
			Case 73:PlaySoundCallOut "fx097"
			Case 74:PlaySoundCallOut "fx098"
			Case 75:PlaySoundCallOut "fx099"
			Case 76:PlaySoundCallOut "fx102"
			Case 77:PlaySoundCallOut "fx104"
			Case 78:PlaySoundCallOut "fx105"
			Case 79:PlaySoundCallOut "fx106"
			Case 80:PlaySoundCallOut "fx107"
			Case 81:PlaySoundCallOut "fx108"
			Case 82:PlaySoundCallOut "fx110"
			Case 83:PlaySoundCallOut "fx111"
			Case 84:PlaySoundCallOut "fx113"
			Case 85:PlaySoundCallOut "fx114"
			Case 86:PlaySoundCallOut "fx115"
			Case 87:PlaySoundCallOut "fx116"
			Case 88:PlaySoundCallOut "fx117"
			Case 89:PlaySoundCallOut "fx118"
			Case 90:PlaySoundCallOut "fx125"
			Case 91:PlaySoundCallOut "fx126"
			Case 92:PlaySoundCallOut "fx127"
			Case 93:PlaySoundCallOut "fx128"
			Case 94:PlaySoundCallOut "fx129"
			Case 95:PlaySoundCallOut "fx130"
			Case 96:PlaySoundCallOut "fx132"
			Case 97:PlaySoundCallOut "fx133"
			Case 98:PlaySoundCallOut "fx134"
			Case 99:PlaySoundCallOut "fx135"
			Case 100:PlaySoundCallOut "fx301" 'out of order 
			Case 101:PlaySoundCallOut "fx136"
			Case 102:PlaySoundCallOut "fx137"
			Case 103:PlaySoundCallOut "fx139"
			Case 104:PlaySoundCallOut "fx140"
			Case 105:PlaySoundCallOut "fx144"
			Case 106:PlaySoundCallOut "fx145"
			Case 107:PlaySoundCallOut "fx146"
			Case 108:PlaySoundCallOut "fx147"
			Case 109:PlaySoundCallOut "fx151"
			Case 110:PlaySoundCallOut "fx154"
			Case 111:PlaySoundCallOut "fx155"
			Case 112:PlaySoundCallOut "fx157"
			Case 113:PlaySoundCallOut "fx159"
			Case 114:PlaySoundCallOut "fx160"
			Case 115:PlaySoundCallOut "fx162"
			Case 116:PlaySoundCallOut "fx163"
			Case 117:PlaySoundCallOut "fx164"
			Case 118:PlaySoundCallOut "fx166"
			Case 119:PlaySoundCallOut "fx167"
			Case 120:PlaySoundCallOut "fx168"
			Case 121:PlaySoundCallOut "fx173"
			Case 122:PlaySoundCallOut "fx182"
			Case 123:PlaySoundCallOut "fx190"
			Case 124:PlaySoundCallOut "fx192"
			Case 125:PlaySoundCallOut "fx193"
			Case 126:PlaySoundCallOut "fx006"      
			Case 127:PlaySoundCallOut "fx061" 
			Case 128:PlaySoundCallOut "fx120" 
			Case 129:PlaySoundCallOut "fx195" 
			Case 130:PlaySoundCallOut "fx196" 
			Case 131:PlaySoundCallOut "fx197"
			Case 132:PlaySoundCallOut "fx199" 
			Case 133:PlaySoundCallOut "fx200" 
			Case 134:PlaySoundCallOut "fx201" 
			Case 135:PlaySoundCallOut "fx202" 
			Case 136:PlaySoundCallOut "fx203" 
			Case 137:PlaySoundCallOut "fx204" 
			Case 138:PlaySoundCallOut "fx205" 
			Case 139:PlaySoundCallOut "fx206" 
			Case 140:PlaySoundCallOut "fx207" 
			Case 141:PlaySoundCallOut "fx213" 
			Case 142:PlaySoundCallOut "fx214" 
			Case 143:PlaySoundCallOut "fx216" 
			Case 144:PlaySoundCallOut "fx218"  
			Case 145:PlaySoundCallOut "fx222" 
			Case 146:PlaySoundCallOut "fx223" 
			Case 147:PlaySoundCallOut "fx224" 
			Case 148:PlaySoundCallOut "fx225" 
			Case 149:PlaySoundCallOut "fx226" 
			Case 150:PlaySoundCallOut "fx227" 
			Case 151:PlaySoundCallOut "fx229" 
			Case 152:PlaySoundCallOut "fx230" 
			Case 153:PlaySoundCallOut "fx231" 
			Case 154:PlaySoundCallOut "fx234" 
			Case 155:PlaySoundCallOut "fx238" 
			Case 156:PlaySoundCallOut "fx242" 
			Case 157:PlaySoundCallOut "fx243" 
			Case 158:PlaySoundCallOut "fx244" 
			Case 159:PlaySoundCallOut "fx245" 
			Case 160:PlaySoundCallOut "fx246" 
			Case 161:PlaySoundCallOut "fx247"
			Case 162:PlaySoundCallOut "fx248"
			Case 163:PlaySoundCallOut "fx249"
			Case 164:PlaySoundCallOut "fx250"
			Case 165:PlaySoundCallOut "fx251"
			Case 166:PlaySoundCallOut "fx254"
			Case 167:PlaySoundCallOut "fx255"
			Case 168:PlaySoundCallOut "fx256"
			Case 169:PlaySoundCallOut "fx257"
			Case 170:PlaySoundCallOut "fx259"
			Case 171:PlaySoundCallOut "fx260"
			Case 172:PlaySoundCallOut "fx261"
			Case 173:PlaySoundCallOut "fx262"
			Case 174:PlaySoundCallOut "fx263"
			Case 175:PlaySoundCallOut "fx264"
			Case 176:PlaySoundCallOut "fx265"
			Case 177:PlaySoundCallOut "fx266"
			Case 178:PlaySoundCallOut "fx267"
			Case 179:PlaySoundCallOut "fx268"
			Case 180:PlaySoundCallOut "fx269"
			Case 181:PlaySoundCallOut "fx270"
			Case 182:PlaySoundCallOut "fx271"
			Case 183:PlaySoundCallOut "fx272"
			Case 184:PlaySoundCallOut "fx273"
			Case 185:PlaySoundCallOut "fx274"
			Case 186:PlaySoundCallOut "fx275"
			Case 187:PlaySoundCallOut "fx276"
			Case 188:PlaySoundCallOut "fx277"
			Case 189:PlaySoundCallOut "fx278"
			Case 190:PlaySoundCallOut "fx286"
			Case 191:PlaySoundCallOut "fx287"
			Case 192:PlaySoundCallOut "fx288"
			Case 193:PlaySoundCallOut "fx290"
			Case 194:PlaySoundCallOut "fx291"
			Case 195:PlaySoundCallOut "fx292"
			Case 196:PlaySoundCallOut "fx293"
			Case 197:PlaySoundCallOut "fx294"
			Case 198:PlaySoundCallOut "fx295"
			Case 199:PlaySoundCallOut "fx296"
			Case 200:PlaySoundCallOut "fx298"
			Case 201:PlaySoundCallOut "fx313"
			Case 202:PlaySoundCallOut "fx314"
			Case 203:PlaySoundCallOut "fx315"
			Case 204:PlaySoundCallOut "fx317"
			Case 205:PlaySoundCallOut "fx318"
			Case 206:PlaySoundCallOut "fx319"
			Case 207:PlaySoundCallOut "fx320"
			Case 208:PlaySoundCallOut "fx321"
			Case 209:PlaySoundCallOut "fx322"
			Case 210:PlaySoundCallOut "fx323"
			Case 211:PlaySoundCallOut "fx325"
			Case 212:PlaySoundCallOut "fx327"
			Case 213:PlaySoundCallOut "fx328"
			Case 214:PlaySoundCallOut "fx340"
			Case 215:PlaySoundCallOut "fx341"
			Case 216:PlaySoundCallOut "fx342"
			Case 217:PlaySoundCallOut "fx343"
			Case 218:PlaySoundCallOut "fx344"
			Case 219:PlaySoundCallOut "fx345"
			Case 220:PlaySoundCallOut "fx346"
			Case 221:PlaySoundCallOut "fx347"
			Case 222:PlaySoundCallOut "fx348"
			Case 223:PlaySoundCallOut "fx349"
			Case 224:PlaySoundCallOut "fx351"
			Case 225:PlaySoundCallOut "fx352"
			Case 226:PlaySoundCallOut "fx353"
			Case 227:PlaySoundCallOut "fx354"
			Case 228:PlaySoundCallOut "fx355"
			Case 229:PlaySoundCallOut "fx357"
			Case 230:PlaySoundCallOut "fx377"
			Case 231:PlaySoundCallOut "fx380"
			Case 232:PlaySoundCallOut "fx381"
			Case 233:PlaySoundCallOut "fx383"
			Case 234:PlaySoundCallOut "fx384"
		End Select
	End Sub

	Sub LaserMask	
		Select Case Int(Rnd * 14) + 1
			Case 1: PlaySoundCallOut "fx203"
			Case 2: PlaySoundCallOut "fx322"
			Case 3: PlaySoundCallOut "fx364"
			Case 4: PlaySoundCallOut "fx365"
			Case 5: PlaySoundCallOut "fx366"
			Case 6: PlaySoundCallOut "fx367"
			Case 7: PlaySoundCallOut "fx368"
			Case 8: PlaySoundCallOut "fx369"
			Case 9: PlaySoundCallOut "fx370"
			Case 10: PlaySoundCallOut "fx371"
			Case 11: PlaySoundCallOut "fx372"
			Case 12: PlaySoundCallOut "fx373"
			Case 13: PlaySoundCallOut "fx374"
			Case 14: PlaySoundCallOut "fx375"
		End Select
	End Sub

'/// end of sounds


	Dim ColorMaskSmall
		ColorMaskSmall = Int(Rnd * 1) + 1
	Sub ChangeColorMaskSmall()
		ColorMaskSmall = ColorMaskSmall + 1
		Select Case ColorMaskSmall
			Case 1: dank4.material="Metal"   
			Case 2: dank4.material="Metal Gold"  
				    ColorMaskSmall = 0
		End Select
	End Sub
	
	Dim ColorMaskBig
		ColorMaskBig = Int(Rnd * 1) + 1
	Sub ChangeColorMaskBig()
		ColorMaskBig = ColorMaskBig + 1
		Select Case ColorMaskBig
			Case 1: dank6.material="Metal"   
			Case 2: dank6.material="Metal Gold"  
				    ColorMaskBig = 0
		End Select
	End Sub

	Sub Target001_hit()
		Playsound "flip_hit_1"
	End Sub

	'***************************
	'   FLEEP SOUNDS SAMPLE
	'***************************
	Sub RandomSoundBallCollision          
		Select Case Int(Rnd * 7) + 1
			Case 1: Playsound "Ball_Collide_1"
			Case 2: Playsound "Ball_Collide_2"
			Case 3: Playsound "Ball_Collide_3"
			Case 4: Playsound "Ball_Collide_4"
			Case 5: Playsound "Ball_Collide_5"
			Case 6: Playsound "Ball_Collide_6"
			Case 7: Playsound "Ball_Collide_7"
		End Select
	End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' KICKER
	'***************************
	'   KICKER TRIGGER
	'***************************
	Sub KickerFinal_hit()
		dim finalspeed
		finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
		If (BallsOnPlayfield > 1) AND finalspeed > 10 then 
			PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
			RandomKickerEjectFast
		Else
			If (BallsOnPlayfield > 1) AND finalspeed >= 1 AND finalspeed <= 10 then
				RandomKickerRubberSound
				RandomKickerEjectSlow
			Else
				If finalspeed > 11 Then
					RandomKickerRubberSound
					RandomKickerEjectFast
				Else
					If finalspeed >= 1 AND finalspeed <= 10 AND LightShootAgain.state = 2 Then
						RandomKickerRubberSound
						RandomKickerEjectSlow
					Else
						'StopAllMusic
						LightQueue.Add "DelayGasDrawlsMBMX","DelayGasDrawlsMBMX",20,7900,0,0,0,false
						RandomSoundGas
						BallLockedCallout
						AudioQueue.Add "GasDrawlsCallout","GasDrawlsCallout",20,1250,0,0,0,false
						FlasherBallLocked
						LightBonus06    
						LKicker.state = 1
						LArrow04.state = 2
						PlaySound "fx_rubber"
						RANDOMLIGHTSKICKER
						RandomChangeGi
						pupevent 706
						DMDBigText "GAS DRAWLS",410,1   
						DMDTopSplash "FREE TRAPPED BALL!",410,0    
						BallsOnPlayfield = BallsOnplayfield - 1
						BallsLocked = BallsLocked + 1
						AddMultiballForBallLock 1
						bAutoPlunger = True
						RotDisc1Step = 43
						Disc1Timer.Enabled = 1
						RotDisc2Step = 43
						Disc2Timer.Enabled = 1
						FlashForMs Light045, 7350, 50, 0
						RotDisc3Step = 43
						Disc3Timer.Enabled = 1
						FlashForMs Light036, 7350, 50, 0 : FlashForMs Light037, 7350, 50, 0 : FlashForMs Light038, 7350, 50, 0 : FlashForMs Light046, 7350, 50, 0
						RotDisc4Step = 43
						Disc4Timer.Enabled = 1
						FlashForMs Light039, 7350, 50, 0 : FlashForMs Light040, 7350, 50, 0 : FlashForMs Light041, 7350, 50, 0 : FlashForMs Light047, 7350, 50, 0
						RotDisc6Step = 43
						Disc6Timer.Enabled = 1
						RotDisc7Step = 43
						Disc7Timer.Enabled = 1
						RotDisc11Step = 43
						Disc11Timer.Enabled = 1
						FlashForMs Light048, 7350, 50, 0
						FlashForMs Light049, 7350, 50, 0
						RotDisc5Step = 43
						Disc5Timer.Enabled = 1
						FlashForMs Light057, 7350, 50, 0
						FlashForMs Light058, 7350, 50, 0
						RotDisc8Step = 43
						Disc8Timer.Enabled = 1
						DOF 115, DOFPulse     'DOF FAN
						DOF 405, DOFPulse     'DOF Strobe						
					End If
				End If
			End If
		End If
	End Sub

	Sub DelayGasDrawlsMBMX
		DOF 943, DOFOn   'DOF MX
		DOF 972, DOFOn   'DOF MX - BACK	
	End Sub

	Sub TriggerCenter02_hit
		TargetBouncer Activeball, 1
		IF LArrow04.state = 2 then
			KickerFinalEjectBall
		Else
			PlaySound "fx_rubber"
			LArrow04.state = 0
		End If
	End Sub
	'***************************
	'   KICKER EJECT BALL 
	'***************************
	Sub KickerFinalEjectBall
		RandomSoundGasToo
		DOF 943, DOFOff   'DOF MX	
		DOF 972, DOFOff   'DOF MX - BACK	
		LKicker.state = 0
		LArrow04.state = 0
		LGAS001.state = 0
		LGAS002.state = 0
		LGAS003.state = 0
		LGAS004.state = 0
		LGAS005.state = 0
		LGAS006.state = 0
		LGAS007.state = 0
		LGAS008.state = 0
		LGAS009.state = 0
		PlaySound "fx_rubber"
		RandomSoundBallCollision
		RandomSoundGas
		FlasherColorCycle
		RANDOMLIGHTSKICKEREJECT
		RandomKickerEjectUp

		'RandomUltraDMDSceneGasDrawls
		playvideo=16+int(rnd(1)*4)

		ChangeGiForJackpot
		ChangeLights(base)
		SmokeSpin
		pupevent 707
		DOF 944, DOFOn      'DOF MX
		DOF 115, DOFPulse   'DOF FAN
		DOF 405, DOFPulse   'DOF Strobe	
		chilloutthemusic

		LightQueue.Add "StopGasDrawlsMBMX","StopGasDrawlsMBMX",20,8000,0,0,0,false
		AudioQueue.Add "GasDrawlsMultiballCallout","GasDrawlsMultiballCallout",20,1300,0,0,0,false
		AudioQueue.Add "RescueJackpotBonus","RescueJackpotBonus",20,4700,0,0,0,false
		AudioQueue.Add "GasDrawlsJackpotCallout","GasDrawlsJackpotCallout",20,4700,0,0,0,false
		BallsOnPlayfield = BallsOnplayfield + 1
		BallsLocked = Ballslocked - 1
		RotDisc1Step = 36
		Disc1Timer.Enabled = 1
		RotDisc2Step = 36
		Disc2Timer.Enabled = 1
		FlashForMs Light045, 5930, 50, 0
		RotDisc3Step = 36
		Disc3Timer.Enabled = 1
		FlashForMs Light036, 5930, 50, 0 : FlashForMs Light037, 5930, 50, 0 : FlashForMs Light038, 5930, 50, 0 : FlashForMs Light046, 5930, 50, 0		
		RotDisc4Step = 36
		Disc4Timer.Enabled = 1
		FlashForMs Light039, 5930, 50, 0 : FlashForMs Light040, 5930, 50, 0 : FlashForMs Light041, 5930, 50, 0 : FlashForMs Light047, 5930, 50, 0		
		RotDisc6Step = 36
		Disc6Timer.Enabled = 1
		RotDisc7Step = 36
		Disc7Timer.Enabled = 1
		RotDisc11Step = 36
		Disc11Timer.Enabled = 1
		FlashForMs Light048, 5930, 50, 0
		FlashForMs Light049, 5930, 50, 0
		RotDisc5Step = 36
		Disc5Timer.Enabled = 1
		FlashForMs Light057, 5930, 50, 0
		FlashForMs Light058, 5930, 50, 0
		RotDisc8Step = 36
		Disc8Timer.Enabled = 1
	End Sub
	Sub StopGasDrawlsMBMX
		DOF 944, DOFOff   'DOF MX	
	End Sub

	Sub RescueJackpotBonus
		LightBonus07
		LightQueue.Add "LightBonus05","LightBonus05",20,2500,0,0,0,false
	End Sub
	Sub RandomKickerEjectFast()
		Select Case Int(Rnd * 6) + 1
			Case 1:KickerFinal.Kick 25, 8	' Kick the ball (direction, force)
			Case 2:KickerFinal.Kick 26, 8	' Kick the ball (direction, force)
			Case 3:KickerFinal.Kick 27, 8	' Kick the ball (direction, force)
			Case 4:KickerFinal.Kick 28, 8	' Kick the ball (direction, force)
			Case 5:KickerFinal.Kick 29, 8	' Kick the ball (direction, force)
			Case 6:KickerFinal.Kick 30, 8	' Kick the ball (direction, force)
		End Select
	End Sub
	Sub RandomKickerEjectSlow()
		Select Case Int(Rnd * 6) + 1
			Case 1:KickerFinal.Kick 25, 6	' Kick the ball (direction, force)
			Case 2:KickerFinal.Kick 26, 6	' Kick the ball (direction, force)
			Case 3:KickerFinal.Kick 27, 6	' Kick the ball (direction, force)
			Case 4:KickerFinal.Kick 28, 6	' Kick the ball (direction, force)
			Case 5:KickerFinal.Kick 29, 6	' Kick the ball (direction, force)
			Case 6:KickerFinal.Kick 30, 6	' Kick the ball (direction, force)
		End Select
	End Sub
	Sub RandomKickerEjectUp()
		Select Case Int(Rnd * 17) + 1
			Case 1:KickerFinal.Kick 10, 15	' Kick the ball (direction, force)
			Case 2:KickerFinal.Kick 12, 15	' Kick the ball (direction, force)
			Case 3:KickerFinal.Kick 14, 15	' Kick the ball (direction, force)
			Case 4:KickerFinal.Kick 16, 15	' Kick the ball (direction, force)
			Case 5:KickerFinal.Kick 18, 15	' Kick the ball (direction, force)
			Case 6:KickerFinal.Kick 20, 15	' Kick the ball (direction, force)
			Case 7:KickerFinal.Kick 21, 15	' Kick the ball (direction, force)
			Case 8:KickerFinal.Kick 22, 15	' Kick the ball (direction, force)
			Case 9:KickerFinal.Kick 23, 15	' Kick the ball (direction, force)
			Case 10:KickerFinal.Kick 25, 15	' Kick the ball (direction, force)
			Case 11:KickerFinal.Kick 27, 15	' Kick the ball (direction, force)
			Case 12:KickerFinal.Kick 29, 15	' Kick the ball (direction, force)
			Case 13:KickerFinal.Kick 31, 15	' Kick the ball (direction, force)
			Case 14:KickerFinal.Kick 32, 15	' Kick the ball (direction, force)
			Case 15:KickerFinal.Kick 33, 15	' Kick the ball (direction, force)
			Case 16:KickerFinal.Kick 34, 15	' Kick the ball (direction, force)
			Case 17:KickerFinal.Kick 35, 15	' Kick the ball (direction, force)
		End Select
	End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' TRIGGERS & SWITCHES
	'******************************************************
	'   TRIGGER FOR PLAYING PLUNGER BALL LAUNCH VIDEOS
	'******************************************************
	Sub TriggerPlunger_hit()
		DOF 942, DOFOff     'DOF MX
		DOF 971, DOFOff     'DOF MX - BACK
		DOF 941, DOFPulse   'DOF MX
		DOF 968, DOFPulse   'DOF MX - BACK
		RandomSoundPlungerShoot
		RandomSoundFairyDust
		AudioQueue.Add "StopIdleSound","StopIdleSound",20,500,0,0,0,false
		pupevent 708
		GiEffect 1
		FlasherPlunger

		'RandomUltraDMDSceneLogo
		PlayVideo=8+int(rnd(1)*4)
		
		PLightsFlashLaser
	End Sub

	'********************************************
	'   RAMP ENTRY/EXIT SOUND FX
	'********************************************
	Sub TriggerRampEntryLeft_hit()
		RandomSoundRampEntry 
	End Sub
	Sub TriggerRampEntryRight_hit()
		RandomSoundRampEntry 
	End Sub
	Sub TriggerRampHitLeft_hit()
		RandomSoundRampHit
		PLightsFlashRampL
	End Sub
	Sub TriggerRampHitRight_hit()
		RandomSoundRampHit
		PLightsFlashRampR
	End Sub
	Sub TriggerRampExitLeft_hit()
		RandomSoundDelayedBallDropOnPlayfield(PegMetalT047) 
		PlaySound "fx_wireramp_exit"
	End Sub
	Sub TriggerRampExitRight_hit()
		RandomSoundDelayedBallDropOnPlayfield(PegMetalT043)
		PlaySound "fx_wireramp_exit"
	End Sub
	'********************************************
	'   LEFT RAMP: TOP TRIGGER FOR VIDEOS
	'********************************************
	Sub Trigger001_hit()
		If (BallsOnPlayfield < 3) AND (Tilted = False) THEN
			RANDOMLIGHTSRAMPSLEFT
		End If
		RandomSoundWind
		PLightsFlashRampL
	End Sub

	Sub Trigger002_hit()
		PlaySound "fx_wireramp_exit"
		AudioQueue.Add "RandomSoundWireRampRolling","RandomSoundWireRampRolling",20,200,0,0,0,false
		RandomSoundBeat
		DMDStarL=50 : DMDStarLBG=50
		ChangeGiForRampLeft
		FlasherLeftRamp
	End Sub
	Sub Trigger007_hit()
		RotDisc5Step = 14
		Disc5Timer.Enabled = 1
		FlashForMs Light057, 2300, 50, 0 : FlashForMs Light058, 2300, 50, 0
	End Sub
	'***************************************************************************
	'   LEFT RAMP: BOTTOM TRIGGER FOR TRIGGERING GAZZILLION EAR MULTIBALL
	'***************************************************************************
	Sub Trigger003_hit()
		IF LSupShot02.state = 0 and LBolt02.state = 1 and LSupreme02.state = 2 and L10KScore02.State = 2 Then
			LSupShot02.state = 1	'lights Sup Shot #2 completed		
			LSupreme02.State = 1
			LBolt02.State = 1	
			LArrow02.State = 2
			L10KScore02.State = 1
			L25KScore02.State = 2
			If LDoubleScoring01.state = 2 Then
				Addscore 20000
			Else
				Addscore 10000
			End If 
		Else 
			IF LSupShot02.state = 1 and LBolt02.state = 1 and LSupreme02.state = 1 and L10KScore02.State = 1 and L25KScore02.State = 2 Then
				GiEffect 1
				LightBonus08
				DMDTopSplash "RAMP COMPLETED",100,0
				RampsCompletedCallout				
				LSupShot02.state = 1	'lights Sup Shot #2 completed		
				LSupreme02.State = 1
				LBolt02.State = 1	
				LArrow02.State = 1
				L10KScore02.State = 1
				L25KScore02.State = 1
				If LDoubleScoring01.state = 2 Then
					Addscore 50000
				Else
					Addscore 25000
				End If
			ELSE
				IF LSupShot02.state = 1 and LBolt02.state = 1 and LSupreme02.state = 1 and L10KScore02.State = 1 and L25KScore02.State = 1 Then
					LSupShot02.state = 1	'lights Sup Shot #2 completed		
					LSupreme02.State = 1
					LBolt02.State = 1	
					LArrow02.State = 1
					L10KScore02.State = 1
					L25KScore02.State = 1
					If LDoubleScoring01.state = 2 Then
						Addscore 50000
					Else
						Addscore 25000
					End If
				Else
					If LDoubleScoring01.state = 2 Then
						Addscore 2000
					Else
						Addscore 1000
					End If
				End If
			End If
		End If
				
		If LCombo.state = 0 THEN
			EnableComboCountdown 15
		End If

		If LExtraBall01.State = 0 Then
			LightExtraBallLight
		End If

		If LSuperRamps.state = 2 Then
			LightBonus07
		End If

		ComboLightsVideo
		TriggerWarpMultiballLight
		LightQueue.Add "WarpSpeedSuperJackpotNotification","WarpSpeedSuperJackpotNotification",20,1200,0,0,0,false
		LightQueue.Add "CheckBoxLogoLights","CheckBoxLogoLights",20,2000,0,0,0,false
		LightQueue.Add "SuperRampsCheck","SuperRampsCheck",20,2000,0,0,0,false
		LightQueue.Add "LevelCompletedBonus","LevelCompletedBonus",20,2000,0,0,0,false
		BonusLeftRampCount
	End Sub

	'********************************************
	'   RIGHT RAMP: TOP TRIGGER FOR VIDEOS
	'********************************************
	Sub Trigger004_hit()
		If (BallsOnPlayfield < 3) AND (Tilted = False) THEN
			RANDOMLIGHTSRAMPSRIGHT
		End If
		RandomSoundWind
	End Sub

	Sub Trigger005_hit()
		PlaySound "fx_wireramp_exit"
		AudioQueue.Add "RandomSoundWireRampRolling","RandomSoundWireRampRolling",20,200,0,0,0,false
		RandomSoundBeat
		DMDHorizontalThick=50 : DMDHorizontalThickBG=50
		ChangeGiForRampRight
		FlasherRightRamp
	End Sub
	'******************************************************************
	'   RIGHT RAMP: BOTTOM TRIGGER FOR TRIGGERING WARP MULTIBALL
	'******************************************************************
	Sub Trigger006_hit()
		IF LSupShot04.state = 0 and LBolt06.state = 1 and LSupreme04.state = 2 and L10KScore07.State = 2 Then
			LSupShot04.state = 1	'lights Sup Shot #4 completed		
			LSupreme04.State = 1
			LBolt06.State = 1	
			LArrow07.State = 2
			L10KScore07.State = 1
			L25KScore03.State = 2
			If LDoubleScoring01.state = 2 Then
				Addscore 20000
			Else
				Addscore 10000
			End If 
		Else 
			IF LSupShot04.state = 1 and LBolt06.state = 1 and LSupreme04.state = 1 and L10KScore07.State = 1 and L25KScore03.State = 2 Then
				GiEffect 1
				LightBonus08
				DMDTopSplash "RAMP COMPLETED",100,0
				RampsCompletedCallout
				LSupShot04.state = 1	'lights Sup Shot #4 completed		
				LSupreme04.State = 1
				LBolt06.State = 1	
				LArrow07.State = 1
				L10KScore07.State = 1
				L25KScore03.State = 1
				If LDoubleScoring01.state = 2 Then
					Addscore 50000
				Else
					Addscore 25000
				End If
			ELSE
				IF LSupShot04.state = 1 and LBolt06.state = 1 and LSupreme04.state = 1 and L10KScore07.State = 1 and L25KScore03.State = 1 Then
					LSupShot04.state = 1	'lights Supreme Shot #4 completed		
					LSupreme04.State = 1
					LBolt06.State = 1	
					LArrow07.State = 1
					L10KScore07.State = 1
					L25KScore03.State = 1
					If LDoubleScoring01.state = 2 Then
						Addscore 50000
					Else
						Addscore 25000
					End If
				Else
					If LDoubleScoring01.state = 2 Then
						Addscore 2000
					Else
						Addscore 1000
					End If
				End If
			End If
		End If 

		If LCombo.state = 0 THEN
			EnableComboCountdown 15
		End If

		If LExtraBall01.State = 0 Then
			LightExtraBallLight
		End If

		If LSuperRamps.state = 2 Then
			LightBonus07
		End If
		
		ComboLightsVideo
		TriggerWarpMultiballLight
		LightQueue.Add "WarpSpeedSuperJackpotNotification","WarpSpeedSuperJackpotNotification",20,1200,0,0,0,false
		LightQueue.Add "CheckBoxLogoLights","CheckBoxLogoLights",20,2000,0,0,0,false
		LightQueue.Add "SuperRampsCheck","SuperRampsCheck",20,2000,0,0,0,false
		LightQueue.Add "LevelCompletedBonus","LevelCompletedBonus",20,2000,0,0,0,false
		BonusRightRampCount
	End Sub

	'********************************************
	'   SPINNER: TRIGGER BEHIND SPINNER
	'********************************************
	Sub TriggerSpinner_hit()
		BonusSpinnerCount
		If Lastswitchhit = "spinnerbottom" then
			FlasherRightRamp : DMDStar=50 : DMDStarBG=50
		Else
			If Lastswitchhit = "spinnertop" then
				FlasherLeftRamp : DMDStarL=50 : DMDStarLBG=50
			End If
		End If
			'RandomFlasherSpinner
		IF LSupShot03.state = 0 and LBolt03.state = 1 and LSupreme03.state = 2 and L5KScore01.State = 2 Then
			LSupShot03.state = 1	'lights Sup Shot #3 completed		
			LSupreme03.State = 1
			LBolt03.State = 1	
			LArrow06.State = 2
			L5KScore01.State = 1
			L10KScore06.State = 2
			If LDoubleScoring01.state = 2 Then
				Addscore 10000
			Else
				Addscore 5000
			End If 
		Else 
			IF LSupShot03.state = 1 and LBolt03.state = 1 and LSupreme03.state = 1 and L5KScore01.State = 1 and L10KScore06.State = 2 Then
				LightBonus09
				DMDTopSplash "SPINNER COMPLETED",100,0
				SpinnerCompletedCallout
				LSupShot03.state = 1	'lights Sup Shot #3 completed		
				LSupreme03.State = 1
				LBolt03.State = 1	
				LArrow06.State = 1
				L5KScore01.State = 1
				L10KScore06.State = 1
				If LDoubleScoring01.state = 2 Then
					Addscore 20000
				Else
					Addscore 10000
				End If
			ELSE
				IF LSupShot03.state = 1 and LBolt03.state = 1 and LSupreme03.state = 1 and L5KScore01.State = 1 and L10KScore06.State = 1 Then
					LSupShot03.state = 1	'lights Sup Shot #3 completed		
					LSupreme03.State = 1
					LBolt03.State = 1	
					LArrow06.State = 1
					L5KScore01.State = 1
					L10KScore06.State = 1
					If LDoubleScoring01.state = 2 Then
						Addscore 20000
					Else
						Addscore 10000
					End If
				Else
					If LDoubleScoring01.state = 2 Then
						Addscore 1000
					Else
						Addscore 500
					End If
				End If
			End If
		End If 

		DoubleSpinnerCheck 

		If LCombo.state = 0 THEN
			EnableComboCountdown 15
		End If

		If LExtraBall01.State = 0 Then
			LightExtraBallLight
		End If
		
		If LDoubleSpinner.state = 2 Then
			LightBonus05
		End If
		
		ComboLightsVideo
		TriggerWarpMultiballLight
		LightQueue.Add "WarpSpeedSuperJackpotNotification","WarpSpeedSuperJackpotNotification",20,1200,0,0,0,false
		LightQueue.Add "CheckBoxLogoLights","CheckBoxLogoLights",20,2000,0,0,0,false
		LightQueue.Add "LevelCompletedBonus","LevelCompletedBonus",20,2000,0,0,0,false
		DOF 405, DOFPulse	'DOF Strobe	  
	End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' LANE TRIGGERS
	'********************************************
	'   TOP LEFT LANE TRIGGER
	'********************************************
	Sub TriggerTopLane01_hit()  
		If LStar01.state = 2 AND LStar02.state = 0 AND LStar03.state = 0 Then
			AwardSkillshot
			LightBonus05
			RANDOMLIGHTSSKILLSHOTFADE
		Else
		Dbg "Lane 1 not LIT"
		Dbg "" &LStar01.state &":" &LStar02.state & ":" &LStar03.state
			IF LDoubleScoring01.State = 2 THEN
				AddScore 500 
			ELSE
				AddScore 250 
			END IF
		END IF
		RandomSoundSwitch
		LStar01.State = 1	
		BonusMultiplierCheckTopLanes
	End Sub
	'********************************************
	'   TOP CENTER LANE TRIGGER
	'********************************************
	Sub TriggerTopLane02_hit()            
		If LStar01.state = 0 AND LStar02.state = 2 AND LStar03.state = 0 Then
			AwardSkillshot
			LightBonus05
			RANDOMLIGHTSSKILLSHOTFADE
		Else
			IF LDoubleScoring01.State = 2 THEN
				AddScore 500
			Else
				AddScore 250
			End If
		END IF
		RandomSoundSwitch
		LStar02.State = 1	
		BonusMultiplierCheckTopLanes
	End Sub
	'********************************************
	'   TOP RIGHT LANE TRIGGER
	'********************************************
	Sub TriggerTopLane03_hit()  
		If LStar01.state = 0 AND LStar02.state = 0 AND LStar03.state = 2 Then
			AwardSkillshot
			LightBonus05
			RANDOMLIGHTSSKILLSHOTFADE			
		Else
			IF LDoubleScoring01.State = 2 THEN
				AddScore 500 
			ELSE
				AddScore 250 
			END IF
		END IF
		RandomSoundSwitch
		LStar03.State = 1	
		BonusMultiplierCheckTopLanes
	End Sub
	'********************************************
	'   PF TEXT OUTLANE/DRAIN TRIGGERS
	'********************************************
	Sub TriggerOutlaneLeft_hit()  
		Flash8
		'FlashForMs Light050, 250, 50, 0
		FlashForMs Light004, 250, 50, 0
	End Sub
	Sub TriggerOutlaneRight_hit() 
		Flash9
	End Sub
	Sub Triggertext001_hit()
		FlashForMs LALLCAPS01, 250, 50, 0
	End Sub
	Sub Triggertext002_hit() 
		FlashForMs LALLCAPS02, 250, 50, 0
	End Sub
	Sub Triggertext003_hit() 
		FlashForMs LALLCAPS03, 250, 50, 0
	End Sub
	Sub Triggertext004_hit() 
		FlashForMs LALLCAPS04, 250, 50, 0
	End Sub
	Sub Triggertext005_hit() 
		FlashForMs LALLCAPS05, 250, 50, 0
	End Sub
	Sub Triggertext006_hit() 
		FlashForMs LALLCAPS06, 250, 50, 0
	End Sub
	Sub Triggertext007_hit() 
		FlashForMs LALLCAPS07, 250, 50, 0
	End Sub
	Sub Triggertext008_hit() 
		FlashForMs LALLCAPS08, 250, 50, 0
	End Sub
	Sub Triggertext009_hit() 
		FlashForMs LALLCAPS09, 250, 50, 0
	End Sub
	Sub Triggertext010_hit() 
		FlashForMs LALLCAPS10, 250, 50, 0
	End Sub
	Sub Triggertext011_hit() 
		FlashForMs LALLCAPS11, 250, 50, 0
	End Sub
	Sub Triggertext012_hit() 
		FlashForMs LALLCAPS12, 250, 50, 0
	End Sub
	Sub Triggertext013_hit() 
		FlashForMs LALLCAPS13, 250, 50, 0
	End Sub
	Sub Triggertext014_hit() 
		FlashForMs LALLCAPS14, 250, 50, 0
	End Sub
	Sub Triggertext015_hit() 
		FlashForMs LALLCAPS15, 250, 50, 0
	End Sub
	Sub Triggertext016_hit() 
		FlashForMs LALLCAPS16, 250, 50, 0
	End Sub
	Sub Triggertext017_hit() 
		FlashForMs LALLCAPS17, 250, 50, 0
	End Sub
	Sub Triggertext018_hit() 
		FlashForMs LALLCAPS18, 250, 50, 0
	End Sub
	Sub Triggertext019_hit() 
		FlashForMs LALLCAPS19, 250, 50, 0
	End Sub
	Sub Triggertext020_hit() 
		FlashForMs LALLCAPS20, 250, 50, 0
	End Sub
	Sub Triggertext021_hit() 
		FlashForMs LALLCAPS21, 250, 50, 0
	End Sub
	Sub Triggertext022_hit() 
		FlashForMs LALLCAPS22, 250, 50, 0
	End Sub
	Sub Triggertext023_hit() 
		FlashForMs LALLCAPS23, 250, 50, 0
	End Sub
	Sub Triggertext024_hit() 
		FlashForMs LALLCAPS24, 250, 50, 0
	End Sub
	Sub Triggertext025_hit() 
		FlashForMs LALLCAPS25, 250, 50, 0
	End Sub
	Sub Triggertext026_hit() 
		FlashForMs LALLCAPS26, 250, 50, 0
	End Sub
	Sub Triggertext027_hit() 
		FlashForMs LALLCAPS27, 250, 50, 0
	End Sub
	Sub Triggertext028_hit() 
		FlashForMs LALLCAPS28, 250, 50, 0
	End Sub
	Sub Triggertext029_hit() 
		FlashForMs LALLCAPS29, 250, 50, 0
	End Sub
	Sub Triggertext030_hit() 
		FlashForMs LALLCAPS30, 250, 50, 0
	End Sub
	Sub Triggertext031_hit() 
		FlashForMs LALLCAPS31, 250, 50, 0
	End Sub
	Sub Triggertext032_hit() 
		FlashForMs LALLCAPS32, 250, 50, 0
	End Sub
	Sub Triggertext033_hit() 
		FlashForMs LALLCAPS33, 250, 50, 0
	End Sub
	Sub Triggertext034_hit() 
		FlashForMs LALLCAPS34, 250, 50, 0
	End Sub
	Sub Triggertext035_hit() 
		FlashForMs LALLCAPS35, 250, 50, 0
	End Sub
	Sub Triggertext036_hit() 
		FlashForMs LALLCAPS36, 250, 50, 0
	End Sub
	Sub Triggertext037_hit() 
		FlashForMs LALLCAPS37, 250, 50, 0
	End Sub
	Sub Triggertext038_hit() 
		FlashForMs LALLCAPS38, 250, 50, 0
	End Sub
	Sub Triggertext039_hit() 
		FlashForMs LALLCAPS39, 250, 50, 0
	End Sub
	Sub Triggertext040_hit() 
		FlashForMs LALLCAPS40, 250, 50, 0
	End Sub
	Sub Triggertext041_hit() 
		FlashForMs LALLCAPS41, 250, 50, 0
	End Sub
	'********************************************
	'   PF TEXT WRAPAROUND TRIGGERS
	'********************************************
	Sub Triggertexttop001_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText01, 250, 50, 0
	End Sub
	Sub Triggertexttop002_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText02, 250, 50, 0
	End Sub
	Sub Triggertexttop003_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText03, 250, 50, 0
		RandomSoundLeftArch
	End Sub
	Sub Triggertexttop004_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText04, 250, 50, 0
	End Sub
	Sub Triggertexttop005_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText05, 250, 50, 0
	End Sub
	Sub Triggertexttop006_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText06, 250, 50, 0
	End Sub
	Sub Triggertexttop007_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText07, 250, 50, 0
	End Sub
	Sub Triggertexttop008_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText08, 250, 50, 0
	End Sub
	Sub Triggertexttop009_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText09, 250, 50, 0
	End Sub
	Sub Triggertexttop010_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText10, 250, 50, 0
	End Sub
	Sub Triggertexttop011_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText11, 250, 50, 0
	End Sub
	Sub Triggertexttop012_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText12, 250, 50, 0
        LPlungerlane01.state=0:LPlungerlane02.state=0:LPlungerlane03.state=0:LPlungerlane04.state=0:LPlungerlane05.state=0:LPlungerlane06.state=0:LPlungerlane07.state=0:LPlungerlane08.state=0:LPlungerlane09.state=0
	End Sub
	Sub Triggertexttop013_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText13, 250, 50, 0
		RandomSoundLeftArch
	End Sub
	Sub Triggertexttop014_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText14, 250, 50, 0
	End Sub
	Sub Triggertexttop015_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText15, 250, 50, 0
	End Sub
	Sub Triggertexttop016_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText16, 250, 50, 0
	End Sub
	Sub Triggertexttop017_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText17, 250, 50, 0
	End Sub
	Sub Triggertexttop018_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText18, 250, 50, 0
		FlashForMs Light029, 350, 50, 0
		RandomSoundRightArch
	End Sub
	Sub Triggertexttop019_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText19, 250, 50, 0
	End Sub
	Sub Triggertexttop020_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText20, 250, 50, 0
	End Sub
	Sub Triggertexttop021_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText21, 250, 50, 0
		LastSwitchHit = "RightOrbitTop"
	End Sub
	Sub Triggertexttop022_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText22, 250, 50, 0
	End Sub
	Sub Triggertexttop023_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText23, 250, 50, 0
	End Sub
	Sub Triggertexttop024_hit()
		if LText24.state = 2 then
			ltext24.state = 0
		Else
			If bSkillshotReady = TRUE THEN FlashForMs LText24, 250, 50, 0
		End If
		LastSwitchHit = "RightOrbitBottom"
	End Sub
	Sub Triggertexttop025_hit()
		if LText25.state = 2 then
			ltext25.state = 0
		Else
			If bSkillshotReady = TRUE THEN FlashForMs LText25, 250, 50, 0
		End If
	End Sub
	Sub Triggertexttop026_hit()
		if LText26.state = 2 then
			ltext26.state = 0
		Else
			If bSkillshotReady = TRUE THEN FlashForMs LText26, 250, 50, 0
		End If
		RandomSoundRightArch
	End Sub
	Sub Triggertexttop027_hit()
		if LText27.state = 2 then
			ltext27.state = 0
		else
			If bSkillshotReady = TRUE THEN FlashForMs LText27, 250, 50, 0
		End If
	End Sub
	Sub Triggertexttop028_hit()
		if LText28.state = 2 then
			ltext28.state = 0
		Else
			If bSkillshotReady = TRUE THEN FlashForMs LText28, 250, 50, 0
		End If
	End Sub
	Sub Triggertexttop029_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText29, 250, 50, 0
	End Sub
	Sub Triggertexttop030_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText30, 250, 50, 0
	End Sub
	Sub Triggertexttop031_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText31, 250, 50, 0
		Lastswitchhit = "spinnerbottom"
	End Sub
	Sub Triggertexttop032_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText32, 250, 50, 0
	End Sub
	Sub Triggertexttop033_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText33, 250, 50, 0
	End Sub
	Sub Triggertexttop034_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText34, 250, 50, 0
		RandomSoundLeftArch
	End Sub
	Sub Triggertexttop035_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText35, 250, 50, 0
		Lastswitchhit = "spinnertop"
	End Sub
	Sub Triggertexttop036_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText36, 250, 50, 0
	End Sub
	Sub Triggertexttop037_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText37, 250, 50, 0
		RandomSoundRightArch
	End Sub
	Sub Triggertexttop038_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText38, 250, 50, 0
	End Sub
	Sub Triggertexttop039_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText39, 250, 50, 0
	End Sub
	Sub Triggertexttop040_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText40, 250, 50, 0
		RandomSoundRightArch
	End Sub
	Sub Triggertexttop041_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText41, 250, 50, 0
	End Sub
	Sub Triggertexttop042_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText42, 250, 50, 0
	End Sub
	Sub Triggertexttop043_hit()
		If bSkillshotReady = TRUE THEN FlashForMs LText43, 250, 50, 0
	End Sub
	Sub FlashBluntLeft
		FlashForMs Light036, 250, 50, 0 : FlashForMs Light037, 250, 50, 0 : FlashForMs Light038, 250, 50, 0  
	End Sub
	Sub FlashBluntRight
		FlashForMs Light039, 250, 50, 0 : FlashForMs Light040, 250, 50, 0 : FlashForMs Light041, 250, 50, 0 
	End Sub
	Sub FlashBlunts
		FlashForMs Light036, 1700, 50, 0 : FlashForMs Light037, 1700, 50, 0 : FlashForMs Light038, 1700, 50, 0 
		FlashForMs Light039, 1700, 50, 0 : FlashForMs Light040, 1700, 50, 0 : FlashForMs Light041, 1700, 50, 0 
	End Sub
	Sub TriggerPlungerlane01_hit()   'smoke
		FlashForMs LPlungerlane01, 250, 50, 0
	End Sub
	Sub TriggerPlungerlane02_hit()   '
		FlashForMs LPlungerlane02, 250, 50, 0
	End Sub
	Sub TriggerPlungerlane03_hit()   '
		FlashForMs LPlungerlane03, 250, 50, 0
	End Sub
	Sub TriggerPlungerlane04_hit()   '
		FlashForMs LPlungerlane04, 250, 50, 0
	End Sub
	Sub TriggerPlungerlane05_hit()   '
		FlashForMs LPlungerlane05, 250, 50, 0
	End Sub
	Sub TriggerPlungerlane06_hit()   '
		FlashForMs LPlungerlane06, 250, 50, 0
	End Sub
	Sub TriggerPlungerlane07_hit()   '
		FlashForMs LPlungerlane07, 250, 50, 0
	End Sub
	Sub TriggerPlungerlane08_hit()   '
		FlashForMs LPlungerlane08, 250, 50, 0
	End Sub
	Sub TriggerPlungerlane09_hit()   '
		FlashForMs LPlungerlane09, 250, 50, 0
	End Sub

	Sub UnblinkText
		Dim a
			For each a in TextLetters
				a.State = 0
		Next
	End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' ORBIT TRIGGERS
	'******************************************
	'   WRAPAROUND ORBIT: TOP LEFT TRIGGER 
	'******************************************
	Sub TriggerOrbit01_hit()
		IF LSupShot01.state = 0 and LBolt01.state = 1 and LSupreme01.state = 2 and L10KScore01.State = 2 Then
			LSupShot01.state = 1	'lights Supreme Shot #1 completed		
			LSupreme01.State = 1
			LBolt01.State = 1	
			LArrow01.State = 2
			L10KScore01.State = 1
			L25KScore01.State = 2
			If LDoubleScoring01.state = 2 Then
				Addscore 20000
			Else
				Addscore 10000
			End If 
		Else 
			IF LSupShot01.state = 1 and LBolt01.state = 1 and LSupreme01.state = 1 and L10KScore01.State = 1 and L25KScore01.State = 2 Then
				LightBonus08
				LightQueue.Add "LightBonus07","LightBonus07",20,2500,0,0,0,false
				DMDTopSplash "ORBITS COMPLETED",100,0
				'chilloutthemusic
				OrbitsCompletedCallout				
				LSupShot01.state = 1	'lights Supreme Shot #1 completed		
				LSupreme01.State = 1
				LBolt01.State = 1	
				LArrow01.State = 1
				L10KScore01.State = 1
				L25KScore01.State = 1
				If LDoubleScoring01.state = 2 Then
					Addscore 50000
				Else
					Addscore 25000
				End If
			ELSE
				IF LSupShot01.state = 1 and LBolt01.state = 1 and LSupreme01.state = 1 and L10KScore01.State = 1 and L25KScore01.State = 1 Then
					LSupShot01.state = 1	'lights Supreme Shot #1 completed		
					LSupreme01.State = 1
					LBolt01.State = 1	
					LArrow01.State = 1
					L10KScore01.State = 1
					L25KScore01.State = 1
					If LDoubleScoring01.state = 2 Then
						Addscore 50000
					Else
						Addscore 25000
					End If
				Else
					If LDoubleScoring01.state = 2 Then
						Addscore 2000
					Else
						Addscore 1000
					End If
				End If
			End If
		End If 

		If LCombo.state = 0 THEN
			EnableComboCountdown 15
		End If

		If (BallsOnPlayfield < 3) AND (Tilted = False) THEN		
			RANDOMLIGHTSRAMPSRIGHTCASE1
			DMDOrbit
		End If	

		If LExtraBall01.State = 0 Then
			LightExtraBallLight
		End If


		If LSuperOrbits.state = 2 Then
			LightBonus07
		End If

		RandomSoundWind
		RandomSoundBeat
		ComboLightsVideo
		TriggerWarpMultiballLight
		LightQueue.Add "WarpSpeedSuperJackpotNotification","WarpSpeedSuperJackpotNotification",20,1200,0,0,0,false
		LightQueue.Add "CheckBoxLogoLights","CheckBoxLogoLights",20,2000,0,0,0,false
		LightQueue.Add "SuperOrbitsCheck","SuperOrbitsCheck",20,2000,0,0,0,false
		LightQueue.Add "LevelCompletedBonus","LevelCompletedBonus",20,2000,0,0,0,false

		Flash1
		RandomSoundLeftArch
		BonusLeftOrbitCount
	End Sub

	'******************************************
	'   WRAPAROUND ORBIT: TOP RIGHT TRIGGER 
	'******************************************
	Sub TriggerOrbit02_hit()
		If LDoubleScoring01.state = 2 Then
			Addscore 2000
		Else
			Addscore 1000
		End If		
		If (BallsOnPlayfield < 3) AND (Tilted = False) AND bSkillshotReady = FALSE THEN				
				If LastSwitchHit = "RightOrbitTop" THEN
					RANDOMLIGHTSRAMPSRIGHTCASE1 : DMDOrbit
				Else
					IF LastSwitchHit = "RightOrbitBottom" THEN
						RANDOMLIGHTSRAMPSLEFTCASE1 : DMDOrbitCounterclockwise
					END IF
				END IF
		End If
		If LExtraBall01.State = 0 Then
			LightExtraBallLight
		End If

		RandomSoundSwitch
		Flash2
		RandomSoundRightArch
	End Sub
	'***********************************************
	'   WRAPAROUND ORBIT: BOTTOM RIGHT TRIGGER 
	'***********************************************
	Sub TriggerOrbit04_hit()
		IF LSupShot06.state = 0 and LBolt04.state = 1 and LSupreme06.state = 2 and L10KScore09.State = 2 Then
			LSupShot06.state = 1	'lights Supreme Shot #6 completed		
			LSupreme06.State = 1
			LBolt04.State = 1	
			LArrow09.State = 2
			L10KScore09.State = 1
			L25KScore04.State = 2
			If LDoubleScoring01.state = 2 Then
				Addscore 20000
			Else
				Addscore 10000
			End If 
		Else 
			IF LSupShot06.state = 1 and LBolt04.state = 1 and LSupreme06.state = 1 and L10KScore09.State = 1 and L25KScore04.State = 2 Then
				LSupShot06.state = 1	'lights Supreme Shot #6 completed		
				LSupreme06.State = 1
				LBolt04.State = 1	
				LArrow09.State = 1
				L10KScore09.State = 1
				L25KScore04.State = 1
				If LDoubleScoring01.state = 2 Then
					Addscore 50000
				Else
					Addscore 25000
				End If
			ELSE
				IF LSupShot06.state = 1 and LBolt04.state = 1 and LSupreme06.state = 1 and L10KScore09.State = 1 and L25KScore04.State = 1 Then
					LSupShot06.state = 1	'lights Supreme Shot #6 completed		
					LSupreme06.State = 1
					LBolt04.State = 1	
					LArrow09.State = 1
					L10KScore09.State = 1
					L25KScore04.State = 1
					If LDoubleScoring01.state = 2 Then
						Addscore 50000
					Else
						Addscore 25000
					End If
				Else
					If LDoubleScoring01.state = 2 Then
						Addscore 2000
					Else
						Addscore 1000
					End If
				End If
			End If
		End If 

		If LExtraBall01.State = 0 Then
			LightExtraBallLight
		End If

		TriggerWarpMultiballLight
		LightQueue.Add "WarpSpeedSuperJackpotNotification","WarpSpeedSuperJackpotNotification",20,1200,0,0,0,false
		LightQueue.Add "CheckBoxLogoLights","CheckBoxLogoLights",20,2000,0,0,0,false
		LightQueue.Add "LevelCompletedBonus","LevelCompletedBonus",20,2000,0,0,0,false
	End Sub

	Sub TriggerOrbit05_hit()
		Flash11
	End Sub
	Sub TriggerOrbit06_hit()
		Flash10
		dim finalspeed
		finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
        If finalspeed > 20 then                
		RotDisc5Step = 28
		Disc5Timer.Enabled = 1
		FlashForMs Light057, 4600, 50, 0 : FlashForMs Light058, 4600, 50, 0 : FlashForMs Light011, 4600, 50, 0 : FlashForMs Light012, 4600, 50, 0
        End if
        If finalspeed <= 20 then
		RotDisc5Step = 14
		Disc5Timer.Enabled = 1
		FlashForMs Light057, 2300, 50, 0 : FlashForMs Light058, 2300, 50, 0 : FlashForMs Light011, 2300, 50, 0 : FlashForMs Light012, 2300, 50, 0   
        End If
	End Sub
	Sub TriggerOrbit07_hit()
		Flash13
	End Sub
	Sub TriggerOrbit08_hit()
		Flash16 : Flash18 : dank6Shaker : dank7Shaker : LaserMask
	End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' HORSESHOE LOOP TRIGGER
	'**************************************
	'   HORSESHOE LOOP: RIGHT TRIGGER 
	'**************************************
	Sub TriggerOrbit03_hit()
		IF LSupShot05.state = 0 and LBolt05.state = 1 and LSupreme05.state = 2 and L5KScore02.State = 2 Then
			LSupShot05.state = 1	'lights Supreme Shot #5 completed		
			LSupreme05.State = 1
			LBolt05.State = 1	
			LArrow08.State = 2
			L5KScore02.State = 1
			L10KScore08.State = 2
			If LDoubleScoring01.state = 2 Then
				Addscore 10000
			Else
				Addscore 5000
			End If 
		Else 
			IF LSupShot05.state = 1 and LBolt05.state = 1 and LSupreme05.state = 1 and L5KScore02.State = 1 and L10KScore08.State = 2 Then
				LSupShot05.state = 1	'lights Supreme Shot #5 completed		
				LSupreme05.State = 1
				LBolt05.State = 1	
				LArrow08.State = 1
				L5KScore02.State = 1
				L10KScore08.State = 1
				If LDoubleScoring01.state = 2 Then
					Addscore 20000
				Else
					Addscore 10000
				End If
			ELSE
				IF LSupShot05.state = 1 and LBolt05.state = 1 and LSupreme05.state = 1 and L5KScore02.State = 1 and L10KScore08.State = 1 Then
					LSupShot05.state = 1	'lights Supreme Shot #5 completed		
					LSupreme05.State = 1
					LBolt05.State = 1	
					LArrow08.State = 1
					L5KScore02.State = 1
					L10KScore08.State = 1
					If LDoubleScoring01.state = 2 Then
						Addscore 20000
					Else
						Addscore 10000
					End If
				Else
					If LDoubleScoring01.state = 2 Then
						Addscore 1000
					Else
						Addscore 500
					End If
				End If
			End If
		End If 
		
		If LExtraBall01.State = 0 Then
			LightExtraBallLight
		End If

		RandomSoundSwitch
		TriggerWarpMultiballLight
		LightQueue.Add "WarpSpeedSuperJackpotNotification","WarpSpeedSuperJackpotNotification",20,1200,0,0,0,false
		LightQueue.Add "CheckBoxLogoLights","CheckBoxLogoLights",20,2000,0,0,0,false
		LightQueue.Add "LevelCompletedBonus","LevelCompletedBonus",20,2000,0,0,0,false

	End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' HIT TARGETS
	Sub TargetRubberNextToRamp_hit
		DOF 930, DOFPulse    'DOF MX
		DOF 966, DOFPulse   'DOF MX - BACK
		DOF 199, DOFPulse
		STHit 12
		RandomFlasherCenterDropTargets  
		If (BallsOnPlayfield < 3) AND (Tilted = False) THEN				
			RANDOMLIGHTSTARGETSCENTERFADE : DMDBumper
			DMDTopSplash "SUPER WHAT?",100,0
		End If
		If LDoubleScoring01.state = 2 THEN
			AddScore 2000
		Else
			AddScore 1000
		End If
		BonusMissionCount
		BonusSmallTargetCount
	End Sub

Sub TargetRubberNextToRampo_Hit
	TargetBouncer ActiveBall, 1
End Sub

	'********************************************
	'   HIT TARGET: ADD-A-BALL (SINGLE)
	'********************************************
	Sub Target01_hit()
		DOF 929, DOFPulse    'DOF MX
		DOF 966, DOFPulse    'DOF MX - BACK
		DOF 199, DOFPulse
		STHit 1

		If (BallsOnPlayfield < 3) AND (Tilted = False) THEN				
			RANDOMLIGHTSTARGETSCENTERFADE : DMDBumper
		End If

		If LWarpMultiballCounter.state = 2 AND LWarpMultiballSuperJackpotIsLit.state = 1  THEN 
			ChangeGiForJackpot
			ChangeLights(base)	
			RotDisc1Step = 36
			Disc1Timer.Enabled = 1
			RotDisc2Step = 36
			Disc2Timer.Enabled = 1
			FlashForMs Light045, 5930, 50, 0
			RotDisc3Step = 36
			Disc3Timer.Enabled = 1
			FlashForMs Light036, 5930, 50, 0 : FlashForMs Light037, 5930, 50, 0 : FlashForMs Light038, 5930, 50, 0 : FlashForMs Light046, 5930, 50, 0
			RotDisc4Step = 36
			Disc4Timer.Enabled = 1
			FlashForMs Light039, 5930, 50, 0 : FlashForMs Light040, 5930, 50, 0 : FlashForMs Light041, 5930, 50, 0 : FlashForMs Light047, 5930, 50, 0			
			RotDisc6Step = 36
			Disc6Timer.Enabled = 1
			RotDisc7Step = 36
			Disc7Timer.Enabled = 1
			RotDisc11Step = 36
			Disc11Timer.Enabled = 1
			FlashForMs Light048, 5930, 50, 0
			FlashForMs Light049, 5930, 50, 0
			RotDisc8Step = 36
			Disc8Timer.Enabled = 1
			pupevent 709
			'chilloutthemusic
			AudioQueue.Add "GazzillionEarJackpotCallout","GazzillionEarJackpotCallout",20,3200,0,0,0,false
			AddScore 500000
		Else
			IF LKicker.state = 0 and LWarpMultiballCounter.state = 2 and LWarpMultiballSuperJackpotIsLit.state = 0 THEN		
			Else				
				IF LExtraBall01.state = 2 AND BallsOnPlayfield < 2 THEN		
					AddMultiball 1
					bAutoPlunger = True
					DMDBigText "BALL ADDED",100,1  '3.4sec blink
					BallAddedCallout	
				End If
			End If
		End If
		If LDoubleScoring01.state = 2 THEN
			AddScore 50000
		Else
			AddScore 25000
		End If
		BonusCenterTargetCount
		FlasherAllBlinkOnce
	End Sub

Sub Target01o_Hit
	TargetBouncer ActiveBall, 1
End Sub

	Sub WarpSpeedSuperJackpotNotification
		If LWarpMultiballCounter.state = 2 AND LWarpMultiballSuperJackpotIsLit.state = 0 AND LSupreme07.state = 1 AND LSupreme08.state = 1 AND LSupreme09.state = 1 AND LSupreme10.state = 1 THEN
			pupevent 710
			GazzillionEarSuperJackpotIsLitCallout
			AddScore 250000
			LExtraBall01.state = 2
			LWarpMultiballSuperJackpotIsLit.state = 1
		End If
	End Sub

	Sub ResetWarpSpeedSuperJackpotNotification
		LWarpMultiballSuperJackpotIsLit.state = 0
	End Sub
	'********************************************
	'   HIT TARGET: SPECIAL HERBS
	'********************************************
	Sub Target02_hit()
		DOF 928, DOFPulse   'DOF MX
		DOF 966, DOFPulse   'DOF MX - BACK
		DOF 199, DOFPulse
		STHit 2
		If LExtraBall01.State = 0 Then
			LightExtraBallLight
		End If
		If LWarpMultiballCounter.state = 0 THEN
			MultiballIsLitNotifications
		Else 
			If LWarpMultiballCounter.State = 2 Then
				AddScore 50000
			Else
				If LDoubleScoring01.state = 2 THEN
					AddScore 20000
				Else
					AddScore 10000
				End If
			End If
		End If
		If (BallsOnPlayfield < 3) AND (Tilted = False) THEN
			RANDOMLIGHTSTARGETSCENTERFADE
			Select Case Int(Rnd * 1) + 1
				Case 1: DMDMaskBounce=50 : DMDMaskBounceBG=50
				'Case 2: tbd
			End Select
		End If
		Flash17 : LaserMask
		FlashForMs LRuby3, 150, 150, 0
		PLightsFlashC
		dank4Shaker : dank5Shaker : ChangeColorMaskSmall
	End Sub

Sub Target02o_Hit
	TargetBouncer ActiveBall, 1
End Sub

	'***********************************************
	'	HIT TARGETS: MASK (TOP LEFT)
	'***********************************************
	Sub Target06_hit()
		DOF 922, DOFPulse   'DOF MX
		DOF 967, DOFPulse   'DOF MX - BACK
		DOF 199, DOFPulse
		STHit 6
		IF LDoubleScoring01.State = 2 THEN
			AddScore 2000
		ELSE
			AddScore 1000 	
		END IF		
		
		IF LSupShot01.state = 0 and LBolt01.state = 2 and LSupreme01.state = 0 Then
			LSupreme01.State = 2
			LBolt01.State = 1	
			LArrow01.State = 2
			L10KScore01.State = 2
		ELse 
			IF LSupShot01.state = 0 and LBolt01.state = 0 and LSupreme01.state = 0 Then
				LSupreme01.State = 2
				LBolt01.State = 1	
				LArrow01.State = 2
				L10KScore01.State = 2		
			Else
				LBolt01.State = 1	
			End If
		End If

		If (BallsOnPlayfield < 3) AND (Tilted = False) THEN
			RANDOMLIGHTSTARGETSFADE
			
			
			Select Case Int(Rnd * 3) + 1
				Case 1: DMDDiagTwo=50 : DMDDiagTwoBG=50
				Case 2: DMDMaskL=50 : DMDMaskLBG=50
				Case 3: DMDFire=50 : DMDFireBG=50
			End Select
		End If

		If LExtraBall01.State = 0 Then
			LightExtraBallLight
		End If

		AudioQueue.Add "StandupsCompleteNotificationL","StandupsCompleteNotificationL",20,1000,0,0,0,false
		RandomFlasherSideTargetsLeft
		PLightsFlashL
	End Sub

Sub Target06o_Hit
	TargetBouncer ActiveBall, 1
End Sub

	'***********************************************
	'	HIT TARGETS: MASK (CENTER LEFT)
	'***********************************************
	Sub Target07_hit()
		DOF 923, DOFPulse   'DOF MX
		DOF 967, DOFPulse   'DOF MX - BACK
		DOF 199, DOFPulse
		STHit 7
		IF LDoubleScoring01.State = 2 THEN
			AddScore 2000 
		ELSE
			AddScore 1000 	
		END IF	

		IF LSupShot02.state = 0 and LBolt02.state = 2 and LSupreme02.state = 0 Then
			LSupreme02.State = 2
			LBolt02.State = 1	
			LArrow02.State = 2
			L10KScore02.State = 2
		ELse 
			IF LSupShot02.state = 0 and LBolt02.state = 0 and LSupreme02.state = 0 Then
				LSupreme02.State = 2
				LBolt02.State = 1	
				LArrow02.State = 2
				L10KScore02.State = 2		
			Else
				LBolt02.State = 1	
			End If
		End If

		If (BallsOnPlayfield < 3) AND (Tilted = False) THEN
			RANDOMLIGHTSTARGETSFADE
			
			Select Case Int(Rnd * 3) + 1
				Case 1: DMDVerticalThickL=50 : DMDVerticalThickLBG=50
				Case 2: DMDMaskL=50 : DMDMaskLBG=50
				Case 3: DMDFire=50 : DMDFireBG=50
			End Select
		End If

		If LExtraBall01.State = 0 Then
			LightExtraBallLight
		End If

		AudioQueue.Add "StandupsCompleteNotificationL","StandupsCompleteNotificationL",20,1000,0,0,0,false
		RandomFlasherSideTargetsLeft
		PLightsFlashL
	End Sub

Sub Target07o_Hit
	TargetBouncer ActiveBall, 1
End Sub

	'***********************************************
	'	HIT TARGETS: MASK (BOTTOM LEFT)
	'***********************************************
	Sub Target08_hit()
		DOF 924, DOFPulse   'DOF MX
		DOF 967, DOFPulse   'DOF MX - BACK
		DOF 199, DOFPulse
		STHit 8
		IF LDoubleScoring01.State = 2 THEN
			AddScore 2000 
		ELSE
			AddScore 1000 		
		END IF

		IF LSupShot03.state = 0 and LBolt03.state = 2 and LSupreme03.state = 0 Then
			LSupreme03.State = 2
			LBolt03.State = 1	
			LArrow06.State = 2
			L5KScore01.State = 2
		ELse 
			IF LSupShot03.state = 0 and LBolt03.state = 0 and LSupreme03.state = 0 Then
				LSupreme03.State = 2
				LBolt03.State = 1	
				LArrow06.State = 2
				L5KScore01.State = 2		
			Else
				LBolt03.State = 1	
			End If
		End If

		If (BallsOnPlayfield < 3) AND (Tilted = False) THEN		
			RANDOMLIGHTSTARGETSFADE
		
			Select Case Int(Rnd * 3) + 1
				Case 1: DMDDiagLTwo=50 : DMDDiagLTwoBG=50
				Case 2: DMDMaskL=50 : DMDMaskLBG=50
				Case 3: DMDFire=50 : DMDFireBG=50
			End Select
		End If

		If LExtraBall01.State = 0 Then
			LightExtraBallLight
		End If
		
		AudioQueue.Add "StandupsCompleteNotificationL","StandupsCompleteNotificationL",20,1000,0,0,0,false
		RandomFlasherSideTargetsLeft
		PLightsFlashL
	End Sub

Sub Target08o_Hit
	TargetBouncer ActiveBall, 1
End Sub

	'***********************************************
	'	HIT TARGETS: MASK (TOP RIGHT)
	'***********************************************
	Sub Target09_hit()
		DOF 925, DOFPulse   'DOF MX
		DOF 967, DOFPulse   'DOF MX - BACK
		DOF 199, DOFPulse
		STHit 9
		IF LDoubleScoring01.State = 2 THEN
			AddScore 2000 
		ELSE
			AddScore 1000 	
		END IF

		IF LSupShot06.state = 0 and LBolt04.state = 2 and LSupreme06.state = 0 Then
			LSupreme06.State = 2
			LBolt04.State = 1	
			LArrow09.State = 2
			L10KScore09.State = 2
		ELse 
			IF LSupShot06.state = 0 and LBolt04.state = 0 and LSupreme06.state = 0 Then
				LSupreme06.State = 2
				LBolt04.State = 1	
				LArrow09.State = 2
				L10KScore09.State = 2		
			Else
				LBolt04.State = 1	
			End If
		End If

		If (BallsOnPlayfield < 3) AND (Tilted = False) THEN		
			RANDOMLIGHTSTARGETSFADE
			
			Select Case Int(Rnd * 3) + 1
				Case 1: DMDDiagL=50 : DMDDiagLBG=50
				Case 2: DMDMask=50 : DMDMaskBG=50
				Case 3: DMDFire=50 : DMDFireBG=50
			End Select
		End If

		If LExtraBall01.State = 0 Then
			LightExtraBallLight
		End If
		
		AudioQueue.Add "StandupsCompleteNotificationR","StandupsCompleteNotificationR",20,1000,0,0,0,false
		RandomFlasherSideTargetsRight
		PLightsFlashR
	End Sub

Sub Target09o_Hit
	TargetBouncer ActiveBall, 1
End Sub

	'*************************************************
	'	HIT TARGETS: MASK (CENTER RIGHT)
	'*************************************************
	Sub Target10_hit()
		DOF 926, DOFPulse   'DOF MX
		DOF 967, DOFPulse   'DOF MX - BACK
		DOF 199, DOFPulse
		STHit 10
		IF LDoubleScoring01.State = 2 THEN
			AddScore 2000 
		ELSE
			AddScore 1000 
		END IF

		IF LSupShot05.state = 0 and LBolt05.state = 2 and LSupreme05.state = 0 Then
			LSupreme05.State = 2
			LBolt05.State = 1	
			LArrow08.State = 2
			L5KScore02.State = 2
		ELse 
			IF LSupShot05.state = 0 and LBolt05.state = 0 and LSupreme05.state = 0 Then
				LSupreme05.State = 2
				LBolt05.State = 1	
				LArrow08.State = 2
				L5KScore02.State = 2		
			Else
				LBolt05.State = 1	
			End If
		End If

		If (BallsOnPlayfield < 3) AND (Tilted = False) THEN		
			RANDOMLIGHTSTARGETSFADE
			
			Select Case Int(Rnd * 3) + 1
				Case 1: DMDVerticalThick=50 : DMDVerticalThickBG=50
				Case 2: DMDMask=50 : DMDMaskBG=50
				Case 3: DMDFire=50 : DMDFireBG=50
			End Select
		End If

		If LExtraBall01.State = 0 Then
			LightExtraBallLight
		End If	
		
		AudioQueue.Add "StandupsCompleteNotificationR","StandupsCompleteNotificationR",20,1000,0,0,0,false
		RandomFlasherSideTargetsRight
		PLightsFlashR
	End Sub

Sub Target10o_Hit
	TargetBouncer ActiveBall, 1
End Sub

	'*************************************************
	'	HIT TARGETS: MASK (BOTTOM RIGHT)
	'*************************************************
	Sub Target11_hit()
		DOF 927, DOFPulse   'DOF MX
		DOF 967, DOFPulse   'DOF MX - BACK
		DOF 199, DOFPulse
		STHit 11
		IF LDoubleScoring01.State = 2 THEN
			AddScore 2000 
		ELSE
			AddScore 1000 		
		END IF

		IF LSupShot04.state = 0 and LBolt06.state = 2 and LSupreme04.state = 0 Then
			LSupreme04.State = 2
			LBolt06.State = 1	
			LArrow07.State = 2
			L10KScore07.State = 2
		ELse 
			IF LSupShot04.state = 0 and LBolt06.state = 0 and LSupreme04.state = 0 Then
				LSupreme04.State = 2
				LBolt06.State = 1	
				LArrow07.State = 2
				L10KScore07.State = 2		
			Else
				LBolt06.State = 1	
			End If
		End If

		If (BallsOnPlayfield < 3) AND (Tilted = False) THEN	
			RANDOMLIGHTSTARGETSFADE
			
			Select Case Int(Rnd * 3) + 1
				Case 1: DMDDiag=50 : DMDDiagBG=50
				Case 2: DMDMask=50 : DMDMaskBG=50
				Case 3: DMDFire=50 : DMDFireBG=50
			End Select
		End If

		If LExtraBall01.State = 0 Then
			LightExtraBallLight
		End If
		
		AudioQueue.Add "StandupsCompleteNotificationR","StandupsCompleteNotificationR",20,1000,0,0,0,false
		RandomFlasherSideTargetsRight
		PLightsFlashR
	End Sub

Sub Target11o_Hit
	TargetBouncer ActiveBall, 1
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' DROP TARGETS
	'********************************************************
	'   DROP TARGETS: MADVILLAINY (BANK OF 3 SMALLER TARGETS)
	'********************************************************
	Sub Target03_hit() 
		DOF 931, DOFPulse   'DOF MX	
		DOF 966, DOFPulse   'DOF MX - BACK
		DOF 199, DOFPulse
		If (BallsOnPlayfield < 3) AND (Tilted = False) THEN
			RANDOMLIGHTSTARGETSCENTERFADE
			Select Case Int(Rnd * 3) + 1
				Case 1: DMDHorizontalThickUp=50 : DMDHorizontalThickUpBG=50
				Case 2: DMDMaskBounceH=50 : DMDMaskBounceHBG=50
				Case 3: DMDFire=50 : DMDFireBG=50
			End Select
		End If

		DTHit 3 
		IF LDoubleScoring01.State = 2 THEN
			AddScore 20000
		Else
			AddScore 10000
		END IF
		
		L10KScore03.State = 1	
		MultiballIsLit
		RandomFlasherCenterDropTargets
		PLightsFlashC
	End Sub

	Sub Target04_hit() 
		DOF 932, DOFPulse   'DOF MX  
		DOF 966, DOFPulse   'DOF MX - BACK
		DOF 199, DOFPulse          
		If (BallsOnPlayfield < 3) AND (Tilted = False) THEN
			RANDOMLIGHTSTARGETSCENTERFADE
			Select Case Int(Rnd * 3) + 1
				Case 1: DMDHorizontalThickUp=50 : DMDHorizontalThickUpBG=50
				Case 2: DMDMaskBounceH=50 : DMDMaskBounceHBG=50
				Case 3: DMDFire=50 : DMDFireBG=50
			End Select
		End If

		DTHit 4  
		IF LDoubleScoring01.State = 2 THEN
			AddScore 20000
		Else
			AddScore 10000
		END IF
		
		L10KScore04.State = 1
		MultiballIsLit
		RandomFlasherCenterDropTargets
		PLightsFlashC
	End Sub

	Sub Target05_hit()     
		DOF 933, DOFPulse   'DOF MX  
		DOF 966, DOFPulse   'DOF MX - BACK   
		DOF 199, DOFPulse  
		If (BallsOnPlayfield < 3) AND (Tilted = False) THEN
			RANDOMLIGHTSTARGETSCENTERFADE
			Select Case Int(Rnd * 3) + 1
				Case 1: DMDHorizontalThickUp=50 : DMDHorizontalThickUpBG=50
				Case 2: DMDMaskBounceH=50 : DMDMaskBounceHBG=50
				Case 3: DMDFire=50 : DMDFireBG=50
			End Select
		End If

		DTHit 5 
		IF LDoubleScoring01.State = 2 THEN
			AddScore 20000
		Else
			AddScore 10000
		END IF
		
		L10KScore05.State = 1
		MultiballIsLit
		RandomFlasherCenterDropTargets
		PLightsFlashC
	End Sub

	'**************************************************************************
	'	RESET MADVILLAINY MASKS (BANK OF 3 SMALLER TARGETS & LIGHT TRIGGERS)
	'**************************************************************************
	Sub ResetDropTargetsSparks
		DTRaise 3
		DTRaise 4
		DTRaise 5
		RandomSoundDropTargetReset Target03p
		L10KScore03.State = 2   'confirm
		L10KScore04.State = 2   'confirm
		L10KScore05.State = 2   'confirm
		LArrow03.State = 0
		LMultiballIsLit.State = 0
		LMultiball2more.State = 0
		LMultiball1more.State = 0
		LMultiballSupremeMultiball.State = 0
		LMultiballSupremeJackpot.State = 0
	End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' PLAYFIELD LIGHT TRIGGERS
	'*****************************************************
	'	MASKS STANDUPS COMPLETE NOTIFICATION
	'*****************************************************
	Sub StandupsCompleteNotificationL 
		If LStandupsCompleted.state = 0 AND LBolt01.State = 1 AND LBolt02.State = 1 AND LBolt03.State = 1 AND LBolt04.State = 1 AND LBolt05.State = 1 AND LBolt06.State = 1 THEN 
			LStandupsCompleted.state = 1
			RANDOMLIGHTSSTANDUPSCOMPLETEDLEFT
			DMDTopSplash "STANDUPS COMPLETED",100,0
			chilloutthemusic
			StandupsCompleteCallout
			LArrow10.state = 2
			LArrow11.state = 2
		End If	
	End Sub

	Sub StandupsCompleteNotificationR 
		If LStandupsCompleted.state = 0 AND LBolt01.State = 1 AND LBolt02.State = 1 AND LBolt03.State = 1 AND LBolt04.State = 1 AND LBolt05.State = 1 AND LBolt06.State = 1 THEN 
			LStandupsCompleted.state = 1
			RANDOMLIGHTSSTANDUPSCOMPLETEDRIGHT
			DMDTopSplash "STANDUPS COMPLETED",100,0
			chilloutthemusic
			StandupsCompleteCallout
			LArrow10.state = 2
			LArrow11.state = 2
		End If	
	End Sub

	Sub ResetStandupsCompletedLight
		LStandupsCompleted.state = 0
	End Sub
	'*************************************
	'	EXTRA BALL LIGHT NOTIFICATION
	'*************************************
	Sub LightExtraBallLight
		CheckBoxLogoLights
		If LSupreme07.state + LSupreme08.state + LSupreme09.state + LSupreme10.state = 3 Then
			LExtraBall01.State = 2
			LArrow05.state = 2
		Else
			LExtraBall01.State = 0
			LArrow05.state = 0
		End If
	End Sub
	'***************************
	'   DOUBLE SCORING LIGHT
	'***************************
	Sub DoubleScoreCheck
		IF (bGameInPLay = True) AND(Tilted = False) AND LDoubleScoringConfirm.state = 0 AND LStar01.state = 1 AND LStar02.state = 1 AND LStar03.state = 1 THEN 
			LDoubleScoringConfirm.state = 1			
			DMDBigText "2X SCORING",100,1    '3.4sec blink
			AudioQueue.Add "DoubleScoringCallout","DoubleScoringCallout",20,1000,0,0,0,false
			EnableDoubleScoring 30
		ELSE
			IF LDoubleScoring01.State = 2 OR LDoubleScoringConfirm.state = 1 Then
			ELSE
				If NOT LastVideoPlayed="toplane" then
				LastVideoPlayed="toplane"
				GeneralPupQueue.Add "ResetToplaneVideo","ResetToplaneVideo",20,6000,0,0,0,false
				End If
			END IF
		End If
	End Sub

	Sub ResetToplaneVideo
		Lastvideoplayed=""
	End Sub

	Sub EnableDoubleScoring(seconds)
		bDoubleScoringActive = True
		DoubleScoringTimerExpired.Interval = 1000 * seconds
		DoubleScoringTimerExpired.Enabled = True
		LDoubleScoring01.BlinkInterval = 160
		LDoubleScoring01.State = 2
	End Sub

	Sub DoubleScoringTimerExpired_Timer()
		DoubleScoringTimerExpired.Enabled = False
		Dim waittime
		waittime = 0500
		GeneralPupQueue.Add "DoubleScoringGrace","DoubleScoringGrace",20,waittime,0,0,0,false
		LDoubleScoring01.State = 0
		LDoubleScoringConfirm.state = 0
	End Sub

	Sub DoubleScoringGrace
		bDoubleScoringActive = False
	End Sub
	'***********************************************************
	'   BONUS MULTIPLER LIGHTS (2X,3X,5X) FOR BOTTOM LANES
	'***********************************************************
	Sub BonusMultiplierCheck
		IF (bGameInPLay = True) AND(Tilted = False) AND LStar01.state = 1 AND LStar02.state = 1 AND LStar03.state = 1 AND LStar04.state = 1 AND LStar05.state = 1 AND LStar06.state = 1 AND LStar07.state = 1 THEN 
			IF LMultiplier01.state = 0 AND LMultiplier02.state = 0 AND LMultiplier03.state = 0 Then
				LMultiplier01.state = 1
				LMultiplier02.state = 0
				LMultiplier03.state = 0
				ResetAllLaneLights
				LightBonus01 
				DMDBigText "2X BONUS",100,0   '3.4sec
				DMDTopSplash "2X BONUS MULTIPLIER",100,0
				TwoTimesBonusMultiplierCallout
			ELSE 
				IF LMultiplier01.state = 1 AND LMultiplier02.state = 0 AND LMultiplier03.state = 0 Then
					LMultiplier01.state = 0
					LMultiplier02.state = 1
					LMultiplier03.state = 0
					ResetAllLaneLights
					LightBonus01
				    DMDBigText "3X BONUS",100,0   '3.4sec
					DMDTopSplash "3X BONUS MULTIPLIER",100,0
					ThreeTimesBonusMultiplierCallout
				ELSE
					IF LMultiplier01.state = 0 AND LMultiplier02.state = 1 AND LMultiplier03.state = 0 Then
						LMultiplier01.state = 0
						LMultiplier02.state = 0
						LMultiplier03.state = 1
						ResetAllLaneLights 
						LightBonus01
				        DMDBigText "5X BONUS",100,0   '3.4sec
					    DMDTopSplash "5X BONUS MULTIPLIER",100,0
						FiveTimesBonusMultiplierCallout
					Else
						IF LMultiplier01.state = 0 AND LMultiplier02.state = 0 AND LMultiplier03.state = 1 Then
							LMultiplier01.state = 0
							LMultiplier02.state = 0
							LMultiplier03.state = 1
							ResetAllLaneLights 
							LightBonus01
				            DMDBigText "5X BONUS",100,0   '3.4sec
					        DMDTopSplash "3X BONUS MULTIPLIER",100,0
							FiveTimesBonusMultiplierCallout
						End If
					END IF 
				END IF
			END IF
		End IF
	End Sub
	'***********************************************************
	'   BONUS MULTIPLER LIGHTS (2X,3X,5X) FOR TOP LANES
	'***********************************************************
	Sub BonusMultiplierCheckTopLanes
		IF (bGameInPLay = True) AND(Tilted = False) AND LStar01.state = 1 AND LStar02.state = 1 AND LStar03.state = 1 AND LStar04.state = 1 AND LStar05.state = 1 AND LStar06.state = 1 AND LStar07.state = 1 THEN 
			IF LMultiplier01.state = 0 AND LMultiplier02.state = 0 AND LMultiplier03.state = 0 Then
				LMultiplier01.state = 1
				LMultiplier02.state = 0
				LMultiplier03.state = 0
				ResetAllLaneLights
				LightBonus01
				DMDBigText "2X BONUS",100,0   '3.4sec
				DMDTopSplash "2X BONUS MULTIPLIER",100,0
				TwoTimesBonusMultiplierCallout
			ELSE 
				IF LMultiplier01.state = 1 AND LMultiplier02.state = 0 AND LMultiplier03.state = 0 Then
					LMultiplier01.state = 0
					LMultiplier02.state = 1
					LMultiplier03.state = 0
					ResetAllLaneLights
					LightBonus01
				    DMDBigText "3X BONUS",100,0   '3.4sec
					DMDTopSplash "3X BONUS MULTIPLIER",100,0
					ThreeTimesBonusMultiplierCallout
				ELSE
					IF LMultiplier01.state = 0 AND LMultiplier02.state = 1 AND LMultiplier03.state = 0 Then
						LMultiplier01.state = 0
						LMultiplier02.state = 0
						LMultiplier03.state = 1
						ResetAllLaneLights 
						LightBonus01
				        DMDBigText "5X BONUS",100,0   '3.4sec
					    DMDTopSplash "5X BONUS MULTIPLIER",100,0
						FiveTimesBonusMultiplierCallout
					ELSE
						IF LMultiplier01.state = 0 AND LMultiplier02.state = 0 AND LMultiplier03.state = 1 Then
							LMultiplier01.state = 0
							LMultiplier02.state = 0
							LMultiplier03.state = 1
							ResetAllLaneLights 
							LightBonus01
							DMDBigText "5X BONUS",100,0   '3.4sec
							DMDTopSplash "5X BONUS MULTIPLIER",100,0
							FiveTimesBonusMultiplierCallout
						END IF
					END IF 
				END IF
			END IF
		ELSE
			DoubleScoreCheck
		End IF
	End Sub
	'*****************************************
	'   RESETS
	'*****************************************
	Sub ResetAllLaneLights
		LStar01.State = 0	
		LStar02.State = 0 	
		LStar03.State = 0
		LStar04.State = 0
		LStar05.State = 0
		LStar06.State = 0
		LStar07.State = 0
		LDoubleScoringConfirm.state = 0    
	End Sub
	Sub ResetSupShotLights
		LSupShot01.State = 0	
		LSupShot02.State = 0			
		LSupShot03.State = 0	
		LSupShot04.State = 0	
		LSupShot05.State = 0	
		LSupShot06.State = 0	
	End Sub
	Sub ResetBoltLights
		LBolt01.State = 2	
		LBolt02.State = 2			
		LBolt03.State = 2	
		LBolt04.State = 2	
		LBolt05.State = 2	
		LBolt06.State = 2	

		L10KScore03.state = 2 'test
		L10KScore04.state = 2 'test
		L10KScore05.state = 2 'test

		Light036.state = 0
		Light037.state = 0
		Light038.state = 0
		Light039.state = 0
		Light040.state = 0
		Light041.state = 0

		LDoomsday01.state = 2
		LDoomsday02.state = 2
		LDoomsday03.state = 2
		LDoomsday04.state = 2
		LDoomsday05.state = 2
		LDoomsday06.state = 2
		LDoomsday07.state = 2
		LDoomsday08.state = 2
		LDoomsday09.state = 2
	End Sub
	'*******************************************************************
	'   TRIGGER 4 ELEMENTS LIGHTS ON CENTER PLAYFIELD ON 
	'*******************************************************************
	Sub CheckBoxLogoLights
		CheckBoxLogo1
		CheckBoxLogo2
		CheckBoxLogo3
		CheckBoxLogo4
	End Sub

	Sub CheckBoxLogo1
		'Check If 2 Orbit Shots Completed To Light Element #1 On Center Playfield
		IF LSupShot01.State = 1 AND LSupShot06.State = 1 THEN 
			LSupreme07.state = 1
		Else
			LSupreme07.state = 0
		End If
	End Sub

	Sub CheckBoxLogo2
		'Check If MultiBall Triggered/Completed To Light Element #2 On Center Playfield
		IF LMultiballSupremeCompleted.State = 1 THEN 
			LSupreme08.state = 1
			LSupreme08confirm.state = 1
		Else
			'LSupreme08.state = 0
		End If
	End Sub

	Sub ResetLSupreme08confirm
		LSupreme08confirm.state = 0
	End Sub

	Sub CheckBoxLogo3
		'Check If 2 Ramp Shots Completed To Light Element #3 On Center Playfield
		IF LSupShot02.State = 1 AND LSupShot04.State = 1 THEN 
			LSupreme09.state = 1
		Else
			LSupreme09.state = 0
		End If
	End Sub

	Sub CheckBoxLogo4
		'Check If 2 HorseShoe Loop Shots Completed To Light Element #4 On Center Playfield
		IF LSupShot03.State = 1 AND LSupShot05.State = 1 THEN 
			LSupreme10.state = 1
		Else
			LSupreme10.state = 0
		End If
	End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' AMERICA'S MOST BLUNTED MULTIBALL TRIGGERS
	'*******************************************************
	'	INITIAL MULTIBALL IS LIT NOTIFICATION
	'*******************************************************
	Sub MultiballIsLit
		If L10KScore03.State = 1 AND L10KScore04.State = 1 AND L10KScore05.State = 1 THEN 
			LMultiballIsLit.state = 1
			LArrow03.state = 2
			GiEffect 0
			If LWarpMultiballCounter.state = 2 Then
			ELSE
				DMDFire=100
				Flashblunts
				SmokeSpin
				RotDisc5Step = 14
				Disc5Timer.Enabled = 1
				FlashForMs Light057, 2300, 50, 0 : FlashForMs Light058, 2300, 50, 0
				RandomChangeGi				
				Gi001.state = 2	
				FlasherAttract
				PlaySoundCallOut "fx067"
				chilloutthemusic
				AudioQueue.Add "AmericasMostBluntedCallout","AmericasMostBluntedCallout",20,500,0,0,0,false
				pupevent 711
				DMDBigText "MB IS LIT!",100,0   '3.4sec
				DMDTopSplash "GET BLUNTED!",100,0
				
				DOF 405, DOFPulse 'Strobe
				DOF 970, DOFPulse 'DOF - MX BACK
				LRuby2.state = 2
				Dim a
					For each a in herbs
					a.State = 1
				
				Next
			End If
		End If	
	End Sub
	'*******************************************************
	'   REMAINING HIT NOTIFICATIONS
	'*******************************************************
	Sub MultiballIsLitNotifications
			IF (bGameInPLay = True) AND(Tilted = False) AND L10KScore03.state = 1 AND L10KScore04.state = 1 AND L10KScore05.state = 1 THEN
				IF LMultiballIsLit.state = 1 AND LMultiball2more.state = 0 AND LMultiball1more.state = 0 AND LMultiballSupremeMultiball.state = 0 AND LMultiballSupremeJackpot.state = 0 Then
					RandomChangeGi
					PlaySoundCallOut "Thunder7"				
					LMultiballIsLit.state = 1
					LMultiball2more.state = 1
					LMultiball1more.state = 0
					LMultiballSupremeMultiball.state = 0
					LMultiballSupremeJackpot.state = 0
					LRuby2.state = 2
					Dim h
						For each h in herbs
						h.State = 1
					Next
					RandomSoundBong
					pupevent 712
					DMDTopSplash "2 MORE HITS",100,0
					chilloutthemusic
					TwoMoreHitsCallout
						If LDoubleScoring01.state = 2 THEN
							AddScore 20000
						Else
							AddScore 10000
						End If
				ELSE
					IF LMultiballIsLit.state = 1 AND LMultiball2more.state = 1 AND LMultiball1more.state = 0 AND LMultiballSupremeMultiball.state = 0 AND LMultiballSupremeJackpot.state = 0 Then
						RandomChangeGi
						PlaySoundCallOut "Thunder7"						
						LMultiballIsLit.state = 1
						LMultiball2more.state = 1
						LMultiball1more.state = 1
						LMultiballSupremeMultiball.state = 0
						LMultiballSupremeJackpot.state = 0
				        LRuby2.state = 2
						Dim e
							For each e in herbs
							e.State = 1
					
						Next
						RandomSoundBong
						pupevent 713
						DMDBigText "1 MORE HIT",100,0   '3.4sec
						chilloutthemusic
						OneMoreHitCallout
							If LDoubleScoring01.state = 2 THEN
								AddScore 20000
							Else
								AddScore 10000
							End If
					ELSE
						IF LMultiballIsLit.state = 1 AND LMultiball2more.state = 1 AND LMultiball1more.state = 1 AND LMultiballSupremeMultiball.state = 0 AND LMultiballSupremeJackpot.state = 0 Then
							PlaySoundCallOut "Thunder7"
							SmokeSpin
							LMultiballIsLit.state = 1
							LMultiball2more.state = 1
							LMultiball1more.state = 1
							LMultiballSupremeMultiball.state = 1
							LMultiballSupremeJackpot.state = 0
				            LRuby2.state = 2
							Dim r
								For each r in herbs
								r.State = 2
								r.BlinkInterval = 150	
							Next

							'RandomUltraDMDSceneAmericasMostBlunted
							playvideo=23+int(rnd(1)*4)
							bMBDrainConfirm = True
							pupevent 714
							chilloutthemusic
							AudioQueue.Add "AmericasMostBluntedMultiballCallout","AmericasMostBluntedMultiballCallout",20,1100,0,0,0,false
							AddMultiball 2
							bAutoPlunger = True
								If LDoubleScoring01.state = 2 THEN
									AddScore 20000
								Else
									AddScore 10000
								End If
							RandomFlasherTrails
							DOF 970, DOFPulse 'DOF - MX BACK
						ELSE
							IF LMultiballIsLit.state = 1 AND LMultiball2more.state = 1 AND LMultiball1more.state = 1 AND LMultiballSupremeMultiball.state = 1 AND LMultiballSupremeJackpot.state = 0 Then
								
								LMultiballIsLit.state = 1
								LMultiball2more.state = 1
								LMultiball1more.state = 1
								LMultiballSupremeMultiball.state = 1
								LMultiballSupremeJackpot.state = 1
								LMultiballSupremeCompleted.state = 1
								RotDisc1Step = 36
								Disc1Timer.Enabled = 1
								RotDisc2Step = 36
								Disc2Timer.Enabled = 1
								FlashForMs Light045, 5930, 50, 0
								RotDisc3Step = 36
								Disc3Timer.Enabled = 1
								FlashForMs Light036, 5930, 50, 0 : FlashForMs Light037, 5930, 50, 0 : FlashForMs Light038, 5930, 50, 0 : FlashForMs Light046, 5930, 50, 0			
								RotDisc4Step = 36
								Disc4Timer.Enabled = 1
								FlashForMs Light039, 5930, 50, 0 : FlashForMs Light040, 5930, 50, 0 : FlashForMs Light041, 5930, 50, 0 : FlashForMs Light047, 5930, 50, 0		
								RotDisc6Step = 36
								Disc6Timer.Enabled = 1
								RotDisc7Step = 36
								Disc7Timer.Enabled = 1
								RotDisc11Step = 36
								Disc11Timer.Enabled = 1
								RotDisc8Step = 36
								Disc8Timer.Enabled = 1
								FlashForMs Light048, 5930, 50, 0
								FlashForMs Light049, 5930, 50, 0
								ChangeGiForJackpot
								TurnOffGIMultiball
								ChangeLights(base)
								chilloutthemusic
								PlaySoundCallOut "Thunder7"
								pupevent 715
								DOF 945, DOFOn   'DOF MX
								DOF 973, DOFOn   'DOF MX - BACK
								AudioQueue.Add "TurnOffMostBluntedJackpotMX","TurnOffMostBluntedJackpotMX",20,6000,0,0,0,false
								AudioQueue.Add "AmericasMostBluntedJackpotCallout","AmericasMostBluntedJackpotCallout",20,1200,0,0,0,false
								AudioQueue.Add "ResetDropTargetsSparks","ResetDropTargetsSparks",20,1200,0,0,0,false
								LightQueue.Add "LightBonus06","LightBonus06",20,2500,0,0,0,false
								LightBonus09
								If LDoubleScoring01.state = 2 THEN
									AddScore 50000
								Else
									AddScore 25000
								End If
								RandomFlasherTrails
								LRuby2.state = 0
								Dim b
									For each b in herbs
									b.State = 0
								Next
							ELSE
								RandomSoundBong
								If LDoubleScoring01.state = 2 THEN
									AddScore 20000
								Else
									AddScore 10000
								End If	
									ResetDropTargetsSparks
							END IF
						END IF 
					END IF			
				END IF
			ELSE
				RandomSoundPunch
				If LDoubleScoring01.state = 2 THEN
					AddScore 20000
				Else
					AddScore 10000
				End If
			END IF
	End Sub

Sub TurnOffMostBluntedJackpotMX
		DOF 945, DOFOff   'DOF MX
		DOF 973, DOFOff   'DOF MX - BACK
End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' GAZZILLION EAR MULTIBALL TRIGGERS
	'***************************************
	'   TRIGGER GAZZILLION EAR MULTIBALL 
	'***************************************
	Sub TriggerWarpMultiballLight
		IF LSupreme07.State = 1 AND LSupreme08confirm.State = 1 AND LSupreme09.State = 1 AND LSupreme10.State = 1 THEN 
			LWarpMultiball.state = 1
			StartGazzillionEarKeepShooting
		End If
	End Sub

	Sub StartWarpMultiball
		If LWarpMultiballCounter.state = 2 THEN
			'insert
		Else	
			'RandomUltraDMDSceneGazzillionEar
			playvideo=30+int(rnd(1)*4)

			pupevent 716
			GazzillionEarMultiballCallout
			GeneralPupQueue.Add "GazzillionEar10secvideo","GazzillionEar10secvideo",20,80000,0,0,0,false
			GeneralPupQueue.Add "ResetDropTargetsSparks","ResetDropTargetsSparks",20,90000,0,0,0,false
			GeneralPupQueue.Add "ResetLWarpMultiballLight","ResetLWarpMultiballLight",20,90000,0,0,0,false
			GeneralPupQueue.Add "ResetWarpSpeedSuperJackpotNotification","ResetWarpSpeedSuperJackpotNotification",20,91000,0,0,0,false
			GeneralPupQueue.Add "PackItUpKeepBombing","PackItUpKeepBombing",20,91000,0,0,0,false

			EnableWarpSpeedMultiballCountdown 90
			GeneralPupQueue.Add "ResetBoltLights","ResetBoltLights",20,1000,0,0,0,false
			GeneralPupQueue.Add "ResetSupShotLights","ResetSupShotLights",20,1000,0,0,0,false
			GeneralPupQueue.Add "ResetDropTargetsSparks","ResetDropTargetsSparks",20,1000,0,0,0,false
			GeneralPupQueue.Add "CheckBoxLogoLights","CheckBoxLogoLights",20,1000,0,0,0,false
			GeneralPupQueue.Add "ResetLWarpMultiballLight","ResetLWarpMultiballLight",20,1000,0,0,0,false
			GeneralPupQueue.Add "ResetLSupreme08confirm","ResetLSupreme08confirm",20,1000,0,0,0,false
			GeneralPupQueue.Add "DelayWarpSpeedMultiballPlunger","DelayWarpSpeedMultiballPlunger",20,4000,0,0,0,false
			GeneralPupQueue.Add "WarpSpeedLightsSetup","WarpSpeedLightsSetup",20,5000,0,0,0,false

			ChangeGiBlink(yellow)
			ChangeLights(purple)
		End If
	End Sub

		Dim SpecialBlink
		SpecialBlink = 1
	Sub RandomSpecialBlink()
		SpecialBlink = SpecialBlink + 1
		Select Case SpecialBlink	
			Case 1:ChangeGiBlink(yellow)
			       ChangeLights(purple)
			Case 2:ChangeGiBlink(green)
			       ChangeLights(white)
			Case 3:ChangeGiBlink(purple)
			       ChangeLights(yellow)
			Case 4:ChangeGiBlink(white)
			       ChangeLights(green)
	               SpecialBlink = 0
		End Select
	End Sub

	Sub SpecialBlinky
			ChangeGiBlink(yellow)
			ChangeLights(purple)
			LightQueue.Add "ChangeGiBlink(green)","ChangeGiBlink(green)",20,10000,0,0,0,false
			LightQueue.Add "ChangeLights(white)","ChangeLights(white)",20,10000,0,0,0,false
			LightQueue.Add "ChangeGiBlink(purple)","ChangeGiBlink(purple)",20,20000,0,0,0,false
			LightQueue.Add "ChangeLights(yellow)","ChangeLights(yellow)",20,20000,0,0,0,false
			LightQueue.Add "ChangeGiBlink(white)","ChangeGiBlink(white)",20,30000,0,0,0,false
			LightQueue.Add "ChangeLights(green)","ChangeLights(green)",20,30000,0,0,0,false
			LightQueue.Add "ChangeGiBlink(yellow)","ChangeGiBlink(yellow)",20,40000,0,0,0,false
			LightQueue.Add "ChangeLights(purple)","ChangeLights(purple)",20,40000,0,0,0,false
			LightQueue.Add "ChangeGiBlink(green)","ChangeGiBlink(green)",20,50000,0,0,0,false
			LightQueue.Add "ChangeLights(white)","ChangeLights(white)",20,50000,0,0,0,false
			LightQueue.Add "ChangeGiBlink(purple)","ChangeGiBlink(purple)",20,60000,0,0,0,false
			LightQueue.Add "ChangeLights(yellow)","ChangeLights(yellow)",20,60000,0,0,0,false
			LightQueue.Add "ChangeGiBlink(white)","ChangeGiBlink(white)",20,70000,0,0,0,false
			LightQueue.Add "ChangeLights(green)","ChangeLights(green)",20,70000,0,0,0,false
			LightQueue.Add "ChangeGiBlink(yellow)","ChangeGiBlink(yellow)",20,80000,0,0,0,false
			LightQueue.Add "ChangeLights(purple)","ChangeLights(purple)",20,80000,0,0,0,false
	End Sub

	Sub DelayWarpSpeedMultiballPlunger
		AddMultiball 3
		bAutoPlunger = True
	End Sub

	Sub WarpSpeedLightsSetup
		'Simulates Target06 being hit
			LSupreme01.State = 2
			LBolt01.State = 1	
			LArrow01.State = 2
			L10KScore01.State = 2
		'Simulates Target07 being hit
			LSupreme02.State = 2
			LBolt02.State = 1	
			LArrow02.State = 2
			L10KScore02.State = 2
		'Simulates Target08 being hit			
			LSupreme03.State = 2
			LBolt03.State = 1	
			LArrow06.State = 2
			L5KScore01.State = 2
		'Simulates Target09 being hit	
			LSupreme06.State = 2
			LBolt04.State = 1	
			LArrow09.State = 2
			L10KScore09.State = 2
		'Simulates Target10 being hit	
			LSupreme05.State = 2
			LBolt05.State = 1	
			LArrow08.State = 2
			L5KScore02.State = 2
		'Simulates Target10 being hit
			LSupreme04.State = 2
			LBolt06.State = 1	
			LArrow07.State = 2
			L10KScore07.State = 2
		'Simulates Left Ramp being hit
			LSupShot02.state = 1			
			LSupreme02.State = 1
			LBolt02.State = 1	
			LArrow02.State = 2
			L10KScore02.State = 1
			L25KScore02.State = 2
	End Sub

	Sub StartGazzillionEarKeepShooting
			GiOff
			LightSeqTilt.Play SeqAllOff
			LeftSlingshot.Disabled = 1
			RightSlingshot.Disabled = 1
			Tilted = True
			PlaySoundCallOut "tiltshutdown"
			DisableTable True
			tilttableclear.enabled = true
			TiltRecoveryTimer.Enabled = True 
			AwardExtraBall
	End Sub

	Sub PackItUpKeepBombing
			Tilted = True
			PlaySoundCallOut "tiltshutdown"
			DisableTable True
			tilttableclear.enabled = true
			TiltRecoveryTimer.Enabled = True 
			AwardExtraBall
	End Sub

	Sub GazzillionEar10secvideo
		pupevent 717
		GazzillionEarTenSecondCountdownCallout
			'AudioQueue.Add "ClearMusicCallout","ClearMusicCallout",20,16000,0,0,0,false
	End Sub
	'************************************************************************************************
	'   RESETS ALL GAZZILLION EAR MULTIBALL LIGHT TRIGGERS TO OFF AFTER MULTIBALL STARTS
	'************************************************************************************************
	Sub ResetLWarpMultiballLight
		LWarpMultiball.state = 0
		LMultiballIsLit.state = 0
		LMultiball2more.state = 0
		LMultiball1more.state = 0
		LMultiballSupremeMultiball.state = 0
		LMultiballSupremeJackpot.state = 0
		LMultiballSupremeCompleted.state = 0
	End Sub
	'**********************************************
	'   GAZZILLION EAR MULTIBALL TIMER ON/OFF
	'**********************************************
	Sub EnableWarpSpeedMultiballCountdown(seconds)
		bWarpSpeedMultiballActive = True
		WarpSpeedMultiballTimerExpired.Interval = 1000 * seconds
		LWarpMultiballCounter.BlinkInterval = 160
		LWarpMultiballCounter.State = 2
		LightShootAgain.BlinkInterval = 160
		LightShootAgain.State = 2
	End Sub

	Sub WarpSpeedMultiballTimerExpired_Timer()
		WarpSpeedMultiballTimerExpired.Enabled = False
		Dim waittime
		waittime = 1000
		BallHandlingQueue.Add "WarpSpeedMultiballCountdownGrace","WarpSpeedMultiballCountdownGrace",20,waittime,0,0,0,false
		LWarpMultiballCounter.State = 0
		LightShootAgain.State = 0
	End Sub

	Sub WarpSpeedMultiballCountdownGrace
		bWarpSpeedMultiballActive = False
		LWarpMultiballCounter.State = 0
		LightShootAgain.State = 0
	End Sub
	'***************************************
	'   COMBO TIMER ON/OFF
	'***************************************
	Sub EnableComboCountdown(seconds)  
		bComboActive = True
		ComboTimerExpired.Interval = 1000 * seconds
		ComboTimerExpired.Enabled = True
		LCombo.BlinkInterval = 160
		LCombo.State = 2
	End Sub

	Sub ComboTimerExpired_Timer()
		ComboTimerExpired.Enabled = False
		Dim waittime
		waittime = 2000
		BallHandlingQueue.Add "ComboCountdownGrace","ComboCountdownGrace",20,waittime,0,0,0,false
		LCombo.State = 0
		bCombo(CurrentPlayer)=0
	End Sub

	Sub ComboCountdownGrace   
		bComboActive = False
	End Sub
	'**********************************************
	'   TRIGGERING COMBO (2/3/4/5 WAY) VIDEOS 
	'**********************************************
	Sub ComboLightsVideo
		bCombo(CurrentPlayer)=bCombo(CurrentPlayer)+1
		If LCombo.state = 2 AND LDoubleScoring01.state = 0 AND bCombo(CurrentPlayer) = 2 Then
			DMDBigText "2WAY COMBO",100,0
			TwoWayComboCallout
			LightBonus05
			LightQueue.Add "LightBonus06","LightBonus06",20,2500,0,0,0,false
		Else
			If LCombo.state = 2 AND LDoubleScoring01.state = 2 AND bCombo(CurrentPlayer) = 2 Then
				DMDBigText "2WAY COMBO",100,0
				TwoWayComboCallout
				LightBonus08
			Else
				If LCombo.state = 2 AND LDoubleScoring01.state = 0 AND bCombo(CurrentPlayer) = 3 Then
					DMDBigText "3WAY COMBO",100,0
					ThreeWayComboCallout
					LightBonus07
				Else
					If LCombo.state = 2 AND LDoubleScoring01.state = 2 AND bCombo(CurrentPlayer) = 3 Then
						DMDBigText "3WAY COMBO",100,0
						ThreeWayComboCallout
						LightBonus09
					Else
						If LCombo.state = 2 AND LDoubleScoring01.state = 0 AND bCombo(CurrentPlayer) = 4 Then
							DMDBigText "4WAY COMBO",100,0
							FourWayComboCallout
							LightBonus07
							LightQueue.Add "LightBonus05","LightBonus05",20,2500,0,0,0,false
						Else
							If LCombo.state = 2 AND LDoubleScoring01.state = 2 AND bCombo(CurrentPlayer) = 4 Then
								DMDBigText "4WAY COMBO",100,0
								FourWayComboCallout
								LightBonus09
								LightQueue.Add "LightBonus06","LightBonus06",20,2500,0,0,0,false
							Else
								If LCombo.state = 2 AND LDoubleScoring01.state = 0 AND bCombo(CurrentPlayer) = 5 Then
									DMDBigText "5WAY COMBO",100,0
									FiveWayComboCallout	
									LightBonus08
								Else
									If LCombo.state = 2 AND LDoubleScoring01.state = 2 AND bCombo(CurrentPlayer) = 5 Then
										DMDBigText "5WAY COMBO",100,0
										FiveWayComboCallout
										LightBonus09
										LightQueue.Add "LightBonus07","LightBonus07",20,2500,0,0,0,false
									End If
								End If
							End If
						End If
					End If
				End If
			End If
		End If
	End Sub

	'***************************************
	'   SUPER POPS TIMER ON/OFF
	'***************************************
	Sub EnableSuperPopsCountdown(seconds)
		bSuperPopsActive = True
		SuperPopsTimerExpired.Interval = 1000 * seconds
		SuperPopsTimerExpired.Enabled = True
		LSuperPops.BlinkInterval = 160
		LSuperPops.State = 2
	End Sub

	Sub SuperPopsTimerExpired_Timer()
		SuperPopsTimerExpired.Enabled = False
		Dim waittime
		waittime = 2000
		BallHandlingQueue.Add "SuperPopsCountdownGrace","SuperPopsCountdownGrace",20,waittime,0,0,0,false
		LSuperPops.State = 0
		bSuperPops(CurrentPlayer)=0
	End Sub

	Sub SuperPopsCountdownGrace
		bSuperPopsActive = False
	End Sub

	Sub SuperPopsCheck
		bSuperPops(CurrentPlayer)=bSuperPops(CurrentPlayer)+1
		If bSuperPops(CurrentPlayer)=30 AND LSuperPops.State = 0 Then
			DMDBigText "SUPER POPS",100,0   '3.4sec
			SuperPopsCallout
			EnableSuperPopsCountdown 60
			LightQueue.Add "ResetSuperPopsLight","ResetSuperPopsLight",20,60000,0,0,0,false
		End If
	End Sub

	Sub ResetSuperPopsLight
		LSuperPops.State = 0
		bSuperPops(CurrentPlayer) = 0
	End Sub
	'***************************************
	'   SUPER RAMPS TIMER ON/OFF
	'***************************************
	Sub EnableSuperRampsCountdown(seconds)
		bSuperRampsActive = True
		SuperRampsTimerExpired.Interval = 1000 * seconds
		SuperRampsTimerExpired.Enabled = True
		LSuperRamps.BlinkInterval = 160
		LSuperRamps.State = 2
	End Sub

	Sub SuperRampsTimerExpired_Timer()
		SuperRampsTimerExpired.Enabled = False
		Dim waittime
		waittime = 2000
		BallHandlingQueue.Add "SuperRampsCountdownGrace","SuperRampsCountdownGrace",20,waittime,0,0,0,false
		LSuperRamps.State = 0
		bSuperRamps(CurrentPlayer)=0
	End Sub

	Sub SuperRampsCountdownGrace
		bSuperRampsActive = False
	End Sub

	Sub SuperRampsCheck
		bSuperRamps(CurrentPlayer)=bSuperRamps(CurrentPlayer)+1
		If bSuperRamps(CurrentPlayer)=5 AND LSuperRamps.State = 0 Then
            DMDBigText "SUPER RAMP",100,0   '3.4sec 
			SuperRampsCallout
			EnableSuperRampsCountdown 60
			LightQueue.Add "ResetSuperRampsLight","ResetSuperRampsLight",20,60000,0,0,0,false
		End If
	End Sub

	Sub ResetSuperRampsLight
		LSuperRamps.State = 0
		bSuperRamps(CurrentPlayer)=0
	End Sub
	'***************************************
	'   SUPER ORBITS TIMER ON/OFF
	'***************************************
	Sub EnableSuperOrbitsCountdown(seconds)
		bSuperOrbitsActive = True
		SuperOrbitsTimerExpired.Interval = 1000 * seconds
		SuperOrbitsTimerExpired.Enabled = True
		LSuperOrbits.BlinkInterval = 160
		LSuperOrbits.State = 2
	End Sub

	Sub SuperOrbitsTimerExpired_Timer()
		SuperOrbitsTimerExpired.Enabled = False
		Dim waittime
		waittime = 2000
		BallHandlingQueue.Add "SuperOrbitsCountdownGrace","SuperOrbitsCountdownGrace",20,waittime,0,0,0,false
		LSuperOrbits.State = 0
		bSuperOrbits(CurrentPlayer)=0
	End Sub

	Sub SuperOrbitsCountdownGrace
		bSuperOrbitsActive = False
	End Sub

	Sub SuperOrbitsCheck
		bSuperOrbits(CurrentPlayer)=bSuperOrbits(CurrentPlayer)+1
		If bSuperOrbits(CurrentPlayer)=5 AND LSuperOrbits.State = 0 Then
			DMDBigText "SUPERORBIT",100,0   '3.4sec
			SuperOrbitsCallout
			EnableSuperOrbitsCountdown 60
			LightQueue.Add "ResetSuperOrbitsLight","ResetSuperOrbitsLight",20,60000,0,0,0,false
		End If
	End Sub

	Sub ResetSuperOrbitsLight
		LSuperOrbits.State = 0
		bSuperOrbits(CurrentPlayer)=0
	End Sub
	'***************************************
	'   DOUBLE SPINNER TIMER ON/OFF
	'***************************************
	Sub EnableDoubleSpinnerCountdown(seconds)
		bDoubleSpinnerActive = True
		DoubleSpinnerTimerExpired.Interval = 1000 * seconds
		DoubleSpinnerTimerExpired.Enabled = True
		LDoubleSpinner.BlinkInterval = 160
		LDoubleSpinner.State = 2
	End Sub

	Sub DoubleSpinnerTimerExpired_Timer()
		DoubleSpinnerTimerExpired.Enabled = False
		Dim waittime
		waittime = 2000
		BallHandlingQueue.Add "DoubleSpinnerCountdownGrace","DoubleSpinnerCountdownGrace",20,waittime,0,0,0,false
		LDoubleSpinner.State = 0
		bDoubleSpinner(CurrentPlayer)=0
	End Sub

	Sub DoubleSpinnerCountdownGrace
		bDoubleSpinnerActive = False
	End Sub

	Sub DoubleSpinnerCheck
		bDoubleSpinner(CurrentPlayer)=bDoubleSpinner(CurrentPlayer)+1
		If bDoubleSpinner(CurrentPlayer)=5 AND LDoubleSpinner.State = 0 Then
			Dbg "Double Spinner"
			pupevent 718
			DMDBigText "2X SPINNER",100,0    '3.4sec
			DoubleSpinnerCallout
			LightBonus05          
			EnableDoubleSpinnerCountdown 60
			LightQueue.Add "ResetDoubleSpinnerLight","ResetDoubleSpinnerLight",20,60000,0,0,0,false
		End If
	End Sub

	Sub ResetDoubleSpinnerLight
		LDoubleSpinner.State = 0
		bDoubleSpinner(CurrentPlayer)=0
	End Sub
	'***************************************
	'   LEVEL COMPLETED TRIGGERS
	'***************************************
	Sub LevelCompletedBonus
		If LLevelOneCompleted.state = 0 AND L10KScore01.state = 1 AND L10KScore02.state = 1 AND L5KScore01.state = 1 AND L10KScore07.state = 1 AND L5KScore02.state = 1 AND L10KScore09.state = 1 Then
			LLevelOneCompleted.state = 1
			AddScore 75000
			DMDTopSplash "LEVEL 1 COMPLETED",100,0
			LevelOneCompletedCallout
			LightQueue.Add "LightBonus10","LightBonus10",20,6000,0,0,0,false
			AudioQueue.Add "MysteryBonusCallout","MysteryBonusCallout",20,6500,0,0,0,false		
		Else
			If LLevelTwoCompleted.state = 0 AND L25KScore01.state = 1 AND L25KScore02.state = 1 AND L10KScore06.state = 1 AND L25KScore03.state = 1 AND L10KScore08.state = 1 AND L25KScore04.state = 1 Then
				LLevelTwoCompleted.state = 1
				AddScore 150000
				DMDTopSplash "LEVEL 2 COMPLETED",100,0
				LevelTwoCompletedCallout
				LightQueue.Add "LightBonus10","LightBonus10",20,6000,0,0,0,false
				AudioQueue.Add "MysteryBonusCallout","MysteryBonusCallout",20,6500,0,0,0,false	
			End If
		End If
	End Sub
	'***************************************
	'   RESET LEVEL COMPLETED LIGHTS
	'***************************************
	Sub ResetLevelCompletedLights
		LLevelOneCompleted.state = 0
		LLevelTwoCompleted.state = 0
	End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' BONUS IN GAZZILLIONS LIGHTS: BLINK PATTERNS
	'***************************************
	'   BONUS IN GAZZILLIONS LIGHTS ON/OFF
	'***************************************
	Sub LightBonusFlash01
		FlashForMs LBonus01, 140, 10, 0
	End Sub
	Sub LightBonusFlash02
		FlashForMs LBonus02, 140, 10, 0
	End Sub
	Sub LightBonusFlash03
		FlashForMs LBonus03, 140, 10, 0
	End Sub
	Sub LightBonusFlash04
		FlashForMs LBonus04, 140, 10, 0
	End Sub
	Sub LightBonusFlash05
		FlashForMs LBonus05, 140, 10, 0
	End Sub
	Sub LightBonusFlash06
		FlashForMs LBonus06, 140, 10, 0
	End Sub
	Sub LightBonusFlash07
		FlashForMs LBonus07, 140, 10, 0
	End Sub
	Sub LightBonusFlash08
		FlashForMs LBonus08, 140, 10, 0
	End Sub
	Sub LightBonusFlash09
		FlashForMs LBonus09, 140, 10, 0
	End Sub
	Sub LightBonusFlash10
		FlashForMs LBonus10, 140, 10, 0
	End Sub

	Sub LightBonus01
		LightQueue.Add "FlashForMs LBonus01-1","FlashForMs LBonus01, 5000, 10, 0",20,450,0,0,0,false
		LightQueue.Add "LightBonusFlash02","LightBonusFlash02",20,400,0,0,0,false
		LightQueue.Add "LightBonusFlash03","LightBonusFlash03",20,350,0,0,0,false
		LightQueue.Add "LightBonusFlash04","LightBonusFlash04",20,300,0,0,0,false
		LightQueue.Add "LightBonusFlash05","LightBonusFlash05",20,250,0,0,0,false
		LightQueue.Add "LightBonusFlash06","LightBonusFlash06",20,200,0,0,0,false
		LightQueue.Add "LightBonusFlash07","LightBonusFlash07",20,150,0,0,0,false
		LightQueue.Add "LightBonusFlash08","LightBonusFlash08",20,100,0,0,0,false
		LightQueue.Add "LightBonusFlash09","LightBonusFlash09",20,50,0,0,0,false
		LightQueue.Add "LightBonusFlash10","LightBonusFlash10",20,0,0,0,0,false

		LightBonusScore01
	End Sub
	Sub LightBonus02
		LightQueue.Add "FlashForMs LBonus02-1","FlashForMs LBonus02, 5000, 10, 0",20,400,0,0,0,false
		LightQueue.Add "LightBonusFlash01","LightBonusFlash01",20,350,0,0,0,false
		LightQueue.Add "LightBonusFlash03","LightBonusFlash03",20,350,0,0,0,false
		LightQueue.Add "LightBonusFlash04","LightBonusFlash04",20,300,0,0,0,false
		LightQueue.Add "LightBonusFlash05","LightBonusFlash05",20,250,0,0,0,false
		LightQueue.Add "LightBonusFlash06","LightBonusFlash06",20,200,0,0,0,false
		LightQueue.Add "LightBonusFlash07","LightBonusFlash07",20,150,0,0,0,false
		LightQueue.Add "LightBonusFlash08","LightBonusFlash08",20,100,0,0,0,false
		LightQueue.Add "LightBonusFlash09","LightBonusFlash09",20,50,0,0,0,false
		LightQueue.Add "LightBonusFlash10","LightBonusFlash10",20,0,0,0,0,false
		LightBonusScore02
	End Sub
	Sub LightBonus03
		LightQueue.Add "FlashForMs LBonus03-1","FlashForMs LBonus03, 5000, 10, 0",20,350,0,0,0,false
		LightQueue.Add "LightBonusFlash01","LightBonusFlash01",20,250,0,0,0,false
		LightQueue.Add "LightBonusFlash02","LightBonusFlash02",20,300,0,0,0,false
		LightQueue.Add "LightBonusFlash04","LightBonusFlash04",20,300,0,0,0,false
		LightQueue.Add "LightBonusFlash05","LightBonusFlash05",20,250,0,0,0,false
		LightQueue.Add "LightBonusFlash06","LightBonusFlash06",20,200,0,0,0,false
		LightQueue.Add "LightBonusFlash07","LightBonusFlash07",20,150,0,0,0,false
		LightQueue.Add "LightBonusFlash08","LightBonusFlash08",20,100,0,0,0,false
		LightQueue.Add "LightBonusFlash09","LightBonusFlash09",20,50,0,0,0,false
		LightQueue.Add "LightBonusFlash10","LightBonusFlash10",20,0,0,0,0,false
		LightBonusScore03
	End Sub
	Sub LightBonus04
		LightQueue.Add "FlashForMs LBonus04-1","FlashForMs LBonus04, 5000, 10, 0",20,300,0,0,0,false
		LightQueue.Add "LightBonusFlash01","LightBonusFlash01",20,150,0,0,0,false
		LightQueue.Add "LightBonusFlash03","LightBonusFlash03",20,250,0,0,0,false
		LightQueue.Add "LightBonusFlash02","LightBonusFlash02",20,200,0,0,0,false
		LightQueue.Add "LightBonusFlash05","LightBonusFlash05",20,250,0,0,0,false
		LightQueue.Add "LightBonusFlash06","LightBonusFlash06",20,200,0,0,0,false
		LightQueue.Add "LightBonusFlash07","LightBonusFlash07",20,150,0,0,0,false
		LightQueue.Add "LightBonusFlash08","LightBonusFlash08",20,100,0,0,0,false
		LightQueue.Add "LightBonusFlash09","LightBonusFlash09",20,50,0,0,0,false
		LightQueue.Add "LightBonusFlash10","LightBonusFlash10",20,0,0,0,0,false
		LightBonusScore04
	End Sub
	Sub LightBonus05
		LightQueue.Add "FlashForMs LBonus05-1","FlashForMs LBonus05, 5000, 10, 0",20,250,0,0,0,false
		LightQueue.Add "LightBonusFlash01","LightBonusFlash01",20,50,0,0,0,false
		LightQueue.Add "LightBonusFlash03","LightBonusFlash03",20,150,0,0,0,false
		LightQueue.Add "LightBonusFlash04","LightBonusFlash04",20,200,0,0,0,false
		LightQueue.Add "LightBonusFlash02","LightBonusFlash02",20,100,0,0,0,false
		LightQueue.Add "LightBonusFlash06","LightBonusFlash06",20,200,0,0,0,false
		LightQueue.Add "LightBonusFlash07","LightBonusFlash07",20,150,0,0,0,false
		LightQueue.Add "LightBonusFlash08","LightBonusFlash08",20,100,0,0,0,false
		LightQueue.Add "LightBonusFlash09","LightBonusFlash09",20,50,0,0,0,false
		LightQueue.Add "LightBonusFlash10","LightBonusFlash10",20,0,0,0,0,false
		LightBonusScore05
	End Sub
	Sub LightBonus06
		LightQueue.Add "FlashForMs LBonus06-1","FlashForMs LBonus06, 5000, 10, 0",20,250,0,0,0,false
		LightQueue.Add "LightBonusFlash01","LightBonusFlash01",20,0,0,0,0,false
		LightQueue.Add "LightBonusFlash03","LightBonusFlash03",20,100,0,0,0,false
		LightQueue.Add "LightBonusFlash04","LightBonusFlash04",20,150,0,0,0,false
		LightQueue.Add "LightBonusFlash05","LightBonusFlash05",20,200,0,0,0,false
		LightQueue.Add "LightBonusFlash02","LightBonusFlash02",20,50,0,0,0,false
		LightQueue.Add "LightBonusFlash07","LightBonusFlash07",20,200,0,0,0,false
		LightQueue.Add "LightBonusFlash08","LightBonusFlash08",20,150,0,0,0,false
		LightQueue.Add "LightBonusFlash09","LightBonusFlash09",20,100,0,0,0,false
		LightQueue.Add "LightBonusFlash10","LightBonusFlash10",20,50,0,0,0,false
		LightBonusScore06
	End Sub
	Sub LightBonus07
		LightQueue.Add "FlashForMs LBonus07-1","FlashForMs LBonus07, 5000, 10, 0",20,300,0,0,0,false
		LightQueue.Add "LightBonusFlash01","LightBonusFlash01",20,0,0,0,0,false
		LightQueue.Add "LightBonusFlash03","LightBonusFlash03",20,100,0,0,0,false
		LightQueue.Add "LightBonusFlash04","LightBonusFlash04",20,150,0,0,0,false
		LightQueue.Add "LightBonusFlash05","LightBonusFlash05",20,200,0,0,0,false
		LightQueue.Add "LightBonusFlash06","LightBonusFlash06",20,250,0,0,0,false
		LightQueue.Add "LightBonusFlash02","LightBonusFlash02",20,50,0,0,0,false
		LightQueue.Add "LightBonusFlash08","LightBonusFlash08",20,250,0,0,0,false
		LightQueue.Add "LightBonusFlash09","LightBonusFlash09",20,200,0,0,0,false
		LightQueue.Add "LightBonusFlash10","LightBonusFlash10",20,150,0,0,0,false
		LightBonusScore07
	End Sub
	Sub LightBonus08
		LightQueue.Add "FlashForMs LBonus08-1","FlashForMs LBonus08, 5000, 10, 0",20,350,0,0,0,false
		LightQueue.Add "LightBonusFlash01","LightBonusFlash01",20,0,0,0,0,false
		LightQueue.Add "LightBonusFlash03","LightBonusFlash03",20,100,0,0,0,false
		LightQueue.Add "LightBonusFlash04","LightBonusFlash04",20,150,0,0,0,false
		LightQueue.Add "LightBonusFlash05","LightBonusFlash05",20,200,0,0,0,false
		LightQueue.Add "LightBonusFlash06","LightBonusFlash06",20,250,0,0,0,false
		LightQueue.Add "LightBonusFlash07","LightBonusFlash07",20,300,0,0,0,false
		LightQueue.Add "LightBonusFlash02","LightBonusFlash02",20,350,0,0,0,false
		LightQueue.Add "LightBonusFlash09","LightBonusFlash09",20,300,0,0,0,false
		LightQueue.Add "LightBonusFlash10","LightBonusFlash10",20,250,0,0,0,false
		LightBonusScore08
	End Sub
	Sub LightBonus09
		LightQueue.Add "FlashForMs LBonus09-1","FlashForMs LBonus09, 5000, 10, 0",20,400,0,0,0,false
		LightQueue.Add "LightBonusFlash01","LightBonusFlash01",20,0,0,0,0,false
		LightQueue.Add "LightBonusFlash03","LightBonusFlash03",20,100,0,0,0,false
		LightQueue.Add "LightBonusFlash04","LightBonusFlash04",20,150,0,0,0,false
		LightQueue.Add "LightBonusFlash05","LightBonusFlash05",20,200,0,0,0,false
		LightQueue.Add "LightBonusFlash06","LightBonusFlash06",20,250,0,0,0,false
		LightQueue.Add "LightBonusFlash07","LightBonusFlash07",20,300,0,0,0,false
		LightQueue.Add "LightBonusFlash08","LightBonusFlash08",20,350,0,0,0,false
		LightQueue.Add "LightBonusFlash02","LightBonusFlash02",20,400,0,0,0,false
		LightQueue.Add "LightBonusFlash10","LightBonusFlash10",20,350,0,0,0,false
		LightBonusScore09
	End Sub
	Sub LightBonus10
		LightQueue.Add "FlashForMs LBonus010-1","FlashForMs LBonus10, 5000, 10, 0",20,450,0,0,0,false
		LightQueue.Add "LightBonusFlash01","LightBonusFlash01",20,0,0,0,0,false
		LightQueue.Add "LightBonusFlash03","LightBonusFlash03",20,100,0,0,0,false
		LightQueue.Add "LightBonusFlash04","LightBonusFlash04",20,150,0,0,0,false
		LightQueue.Add "LightBonusFlash05","LightBonusFlash05",20,200,0,0,0,false
		LightQueue.Add "LightBonusFlash06","LightBonusFlash06",20,250,0,0,0,false
		LightQueue.Add "LightBonusFlash07","LightBonusFlash07",20,300,0,0,0,false
		LightQueue.Add "LightBonusFlash08","LightBonusFlash08",20,350,0,0,0,false
		LightQueue.Add "LightBonusFlash09","LightBonusFlash09",20,400,0,0,0,false
		LightQueue.Add "LightBonusFlash2","LightBonusFlash02",20,50,0,0,0,false
		RandomLightBonusQuestionMark
	End Sub
	'*******************************************
	'   BONUS IN THOUSANDS SCORE MULTIPLERS
	'*******************************************
	Sub LightBonusScore01
		If LMultiplier01.state = 1 THEN
			AddScore 2000
		Else
			If LMultiplier02.state = 1 THEN
				AddScore 3000	
			Else
				If LMultiplier03.state = 1 THEN
					AddScore 5000
				Else
					AddScore 1000
				End If
			End If
		End If
	End Sub
	Sub LightBonusScore02
		If LMultiplier01.state = 1 THEN
			AddScore 4000
		Else
			If LMultiplier02.state = 1 THEN
				AddScore 6000	
			Else
				If LMultiplier03.state = 1 THEN
					AddScore 10000
				Else
					AddScore 2000
				End If
			End If
		End If
	End Sub
	Sub LightBonusScore03
		If LMultiplier01.state = 1 THEN
			AddScore 6000
		Else
			If LMultiplier02.state = 1 THEN
				AddScore 9000	
			Else
				If LMultiplier03.state = 1 THEN
					AddScore 15000
				Else
					AddScore 3000
				End If
			End If
		End If
	End Sub
	Sub LightBonusScore04
		If LMultiplier01.state = 1 THEN
			AddScore 8000
		Else
			If LMultiplier02.state = 1 THEN
				AddScore 12000	
			Else
				If LMultiplier03.state = 1 THEN
					AddScore 20000
				Else
					AddScore 4000
				End If
			End If
		End If
	End Sub
	Sub LightBonusScore05
		If LMultiplier01.state = 1 THEN
			AddScore 10000
		Else
			If LMultiplier02.state = 1 THEN
				AddScore 15000	
			Else
				If LMultiplier03.state = 1 THEN
					AddScore 25000
				Else
					AddScore 5000
				End If
			End If
		End If
	End Sub
	Sub LightBonusScore06
		If LMultiplier01.state = 1 THEN
			AddScore 20000
		Else
			If LMultiplier02.state = 1 THEN
				AddScore 30000	
			Else
				If LMultiplier03.state = 1 THEN
					AddScore 50000
				Else
					AddScore 10000
				End If
			End If
		End If
	End Sub
	Sub LightBonusScore07
		If LMultiplier01.state = 1 THEN
			AddScore 40000
		Else
			If LMultiplier02.state = 1 THEN
				AddScore 60000	
			Else
				If LMultiplier03.state = 1 THEN
					AddScore 100000
				Else
					AddScore 20000
				End If
			End If
		End If
	End Sub
	Sub LightBonusScore08
		If LMultiplier01.state = 1 THEN
			AddScore 60000
		Else
			If LMultiplier02.state = 1 THEN
				AddScore 90000	
			Else
				If LMultiplier03.state = 1 THEN
					AddScore 150000
				Else
					AddScore 30000
				End If
			End If
		End If
	End Sub
	Sub LightBonusScore09
		If LMultiplier01.state = 1 THEN
			AddScore 80000
		Else
			If LMultiplier02.state = 1 THEN
				AddScore 120000	
			Else
				If LMultiplier03.state = 1 THEN
					AddScore 200000
				Else
					AddScore 40000
				End If
			End If
		End If
	End Sub
	Sub RandomLightBonusQuestionMark()
		Select Case Int(Rnd * 9) + 1
			Case 1:LightBonusScore01
			Case 2:LightBonusScore02
			Case 3:LightBonusScore03
			Case 4:LightBonusScore04
			Case 5:LightBonusScore05
			Case 6:LightBonusScore06
			Case 7:LightBonusScore07
			Case 8:LightBonusScore08
			Case 9:LightBonusScore09
		End Select
	End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'  CINEMATIC SKIPPING
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	Dim ldown:ldown = 0
	Dim rdown:rdown = 0

	Sub checkdown
		If ldown + rdown = 2 Then
			skipscene
		End If
	End Sub
	Sub skipscene
	End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'  LIGHTING / RAINBOW LIGHTS
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'********************************************************************************************
' Only for VPX 10.2 and higher.
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
'********************************************************************************************
Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState) 'thanks gtxjoe for the first version

	If TypeName(MyLight) = "Light" Then

		If FinalState = 2 Then
			FinalState = MyLight.State 'Keep the current light state
		End If
		MyLight.BlinkInterval = BlinkPeriod
		MyLight.Duration 2, TotalPeriod, FinalState
	ElseIf TypeName(MyLight) = "Flasher" Then

		Dim steps

		' Store all blink information
		steps = Int(TotalPeriod / BlinkPeriod + .5) 'Number of ON/OFF steps to perform
		If FinalState = 2 Then                      'Keep the current flasher state
			FinalState = ABS(MyLight.Visible)
		End If
		MyLight.UserValue = steps * 10 + FinalState 'Store # of blinks, and final state

		' Start blink timer and create timer subroutine
		MyLight.TimerInterval = BlinkPeriod
		MyLight.TimerEnabled = 0
		MyLight.TimerEnabled = 1
		ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End if:End Sub"
	End If
End Sub
'******************************************
' Change light color - simulate color leds
' changes the light color and state
'******************************************
Dim amber, lightamber, yellow, lightyellow, green, lightgreen, purple, lightpurple, white, ivory, base, black, red, indigo, lime, magenta, orange, maroon
amber = 1
lightamber = 2
yellow = 3
lightyellow = 4
green = 5
lightgreen = 6
purple = 7
lightpurple = 8
white = 9
ivory = 10
base = 11
black = 12
red = 13
indigo = 14
lime = 15
magenta = 16
orange = 17

' Storing base colors for from insert lights - iaakki
Dim BaseColours()
redim BaseColours(200)

StoreBaseColors
sub StoreBaseColors
	dim k, x : x = 0
	for each k in aLights
		BaseColours(x) = Array(k.name,k.color)
		x = x + 1
	next
	Redim preserve BaseColours(x)
end sub

function returnBaseColor(baseObject)
	dim baseColor, baseName, x
	if IsObject(baseObject) then
		baseName = baseObject.name
		'if baseName = "gi2" then msgbox "hep"
		for x = 0 to UBound(BaseColours) - 1
			'if baseName = "gi2" then debug.print x
			if baseName = BaseColours(x)(0) then
				returnBaseColor = BaseColours(x)(1)
				exit for
			end if
		next
	Else
		'debug.print "not object: " & baseObject
	end if
end function


Sub SetLightColor(n, col, stat)
	dim baseColor
	Select Case col
		Case amber
			n.color = RGB(0, 0, 0)
			n.colorfull = RGB(255, 153, 0)
		Case lightamber
			n.color = RGB(0, 0, 0)
			n.colorfull = RGB(255, 222, 173)
		Case yellow
			n.color = RGB(0, 0, 0)
			n.colorfull = RGB(255, 255, 0)
		Case lightyellow
			n.color = RGB(0, 0, 0)
			n.colorfull = RGB(255, 255, 224)			
		Case green
			n.color = RGB(34, 139, 34)
			n.colorfull = RGB(0, 255, 0)
		Case lightgreen
			n.color = RGB(0, 0, 0)
			n.colorfull = RGB(152, 251, 152)			
		Case purple
			n.color = RGB(148, 0, 211)
			n.colorfull = RGB(128, 0, 128)
		Case lightpurple
			n.color = RGB(0, 0, 0)
			n.colorfull = RGB(186, 85, 211)
		Case white
			n.color = RGB(0, 0, 0)
			n.colorfull = RGB(255, 255, 255)
		Case ivory
			n.color = RGB(255, 252, 224)
			n.colorfull = RGB(193, 91, 0)
		Case base
'			debug.print n.name
			baseColor = returnBaseColor(n)
			n.color = baseColor
			n.colorfull = baseColor
		Case black
			n.color = RGB(0, 0, 0)
			n.colorfull = RGB(0, 0, 0)
		Case red
			n.color = RGB(255, 0, 0)  '255
			n.colorfull = RGB(139, 0, 0)
		Case indigo
			n.color = RGB(128, 0, 128)
			n.colorfull = RGB(75, 0, 130)
		Case lime
			n.color = RGB(173, 255, 47)
			n.colorfull = RGB(0, 255, 0)
		Case magenta
			n.color = RGB(218, 112, 214 )
			n.colorfull = RGB(255, 0, 255)
		Case orange
			n.color = RGB(255, 165, 0 )
			n.colorfull = RGB(255, 165, 0)
		Case maroon
			n.color = RGB(139, 0, 0 )
			n.colorfull = RGB(130, 0, 0)
	End Select
	If stat <> -1 Then
		n.State = 0
		n.State = stat
	End If
End Sub

Sub SetLightColorGI(n, col, stat)
	dim baseColor
	Select Case col
		Case amber
			n.color = RGB(0, 0, 0)
			n.colorfull = RGB(255, 153, 0)
		Case lightamber
			n.color = RGB(0, 0, 0)
			n.colorfull = RGB(255, 222, 173)
		Case yellow
			n.color = RGB(0, 0, 0)
			n.colorfull = RGB(255, 255, 0)
		Case lightyellow
			n.color = RGB(0, 0, 0)
			n.colorfull = RGB(255, 255, 224)			
		Case green
			n.color = RGB(34, 139, 34)
			n.colorfull = RGB(0, 255, 0)
		Case lightgreen
			n.color = RGB(0, 0, 0)
			n.colorfull = RGB(152, 251, 152)			
		Case purple
			n.color = RGB(148, 0, 211)
			n.colorfull = RGB(128, 0, 128)
		Case lightpurple
			n.color = RGB(0, 0, 0)
			n.colorfull = RGB(186, 85, 211)
		Case white
			n.color = RGB(0, 0, 0)
			n.colorfull = RGB(255, 255, 255)
		Case ivory
			n.color = RGB(255, 252, 224)
			n.colorfull = RGB(193, 91, 0)
		Case base
			n.color = RGB(255, 197, 143)
			n.colorfull = RGB(255, 252, 224)
		Case black
			n.color = RGB(0, 0, 0)
			n.colorfull = RGB(0, 0, 0)
		Case red
			n.color = RGB(255, 0, 0)  '255
			n.colorfull = RGB(139, 0, 0)
		Case indigo
			n.color = RGB(128, 0, 128)
			n.colorfull = RGB(75, 0, 130)
		Case lime
			n.color = RGB(173, 255, 47)
			n.colorfull = RGB(0, 255, 0)
		Case magenta
			n.color = RGB(218, 112, 214 )
			n.colorfull = RGB(255, 0, 255)
		Case orange
			n.color = RGB(255, 165, 0 )
			n.colorfull = RGB(255, 165, 0)
		Case maroon
			n.color = RGB(139, 0, 0 )
			n.colorfull = RGB(130, 0, 0)
	End Select
	If stat <> -1 Then
		n.State = 0
		n.State = stat
	End If
End Sub

Sub ResetAllLightsColor 	
	Dim bulb
	For each bulb in alights
		SetLightColor bulb, base, -1
	Next	
End Sub

Sub ResetAllGILightsColor
	Dim bulb
	For each bulb in GI
		SetLightColorGI bulb, base, -1
		'debug.print bulb.name & base
	Next	
	
	TurnOffGIMultiball

End Sub
'*************************
' Rainbow Changing Lights
'*************************
'''''''''''''''''''
Dim RGBStep, RGBFactor, rRed, rGreen, rBlue, RainbowLights

Sub StartRainbow(n)
	set RainbowLights = n
	RGBStep = 0
	RGBFactor = 5
	rRed = 255
	rGreen = 197    '0
	rBlue = 143     '0
	RainbowTimer.Enabled = 1
End Sub

Dim RGBStep2, RGBFactor2, rRed2, rGreen2, rBlue2, RainbowLights2
Sub StartRainbowGI(n)
	set RainbowLights2 = n
	RGBStep2 = 0
	RGBFactor2 = 5
	rRed2 = 255
	rGreen2 = 197   '0
	rBlue2 = 143    '0
	RainbowTimer1.Enabled = 1
End Sub

Sub StopRainbow(n)
	Dim obj
	RainbowTimer.Enabled = 0
	RainbowTimer.Enabled = 0
		For each obj in RainbowLights
			SetLightColor obj, "base", 0
		Next
End Sub

Sub StopRainbow2(n)
	Dim obj
	RainbowTimer1.Enabled = 0
		For each obj in RainbowLights2
			SetLightColorGI obj, "base", 0
			obj.state = 1
			obj.Intensity = 18
		Next
End Sub

Sub RainbowTimer_Timer 'rainbow led light color changing
	Dim obj
	Select Case RGBStep
		Case 0 'Green
			rGreen = rGreen + RGBFactor
			If rGreen > 255 then
				rGreen = 255
				RGBStep = 1
			End If
		Case 1 'Red
			rRed = rRed - RGBFactor
			If rRed < 0 then
				rRed = 0
				RGBStep = 2
			End If
		Case 2 'Blue
			rBlue = rBlue + RGBFactor
			If rBlue > 255 then
				rBlue = 255
				RGBStep = 3
			End If
		Case 3 'Green
			rGreen = rGreen - RGBFactor
			If rGreen < 0 then
				rGreen = 0
				RGBStep = 4
			End If
		Case 4 'Red
			rRed = rRed + RGBFactor
			If rRed > 255 then
				rRed = 255
				RGBStep = 5
			End If
		Case 5 'Blue
			rBlue = rBlue - RGBFactor
			If rBlue < 0 then
				rBlue = 0
				RGBStep = 0
			End If
	End Select
		For each obj in RainbowLights
			obj.color = RGB(rRed \ 10, rGreen \ 10, rBlue \ 10)
			obj.colorfull = RGB(rRed, rGreen, rBlue)
		Next
End Sub

Sub RainbowTimer1_Timer 'rainbow led light color changing
	Dim obj
	Select Case RGBStep2
		Case 0 'Green
			rGreen2 = rGreen2 + RGBFactor2
			If rGreen2 > 255 then
				rGreen2 = 255
				RGBStep2 = 1
			End If
		Case 1 'Red
			rRed2 = rRed2 - RGBFactor2
			If rRed2 < 0 then
				rRed2 = 0
				RGBStep2 = 2
			End If
		Case 2 'Blue
			rBlue2 = rBlue2 + RGBFactor2
			If rBlue2 > 255 then
				rBlue2 = 255
				RGBStep2 = 3
			End If
		Case 3 'Green
			rGreen2 = rGreen2 - RGBFactor2
			If rGreen2 < 0 then
				rGreen2 = 0
				RGBStep2 = 4
			End If
		Case 4 'Red
			rRed2 = rRed2 + RGBFactor2
			If rRed2 > 255 then
				rRed2 = 255
				RGBStep2 = 5
			End If
		Case 5 'Blue
			rBlue2 = rBlue2 - RGBFactor2
			If rBlue2 < 0 then
				rBlue2 = 0
				RGBStep2 = 0
			End If
	End Select
		For each obj in RainbowLights2
			obj.color = RGB(rRed2 \ 10, rGreen2 \ 10, rBlue2 \ 10)
			obj.colorfull = RGB(rRed2, rGreen2, rBlue2)
		Next
End Sub

Sub StartLightSeq()
	Dim a
	For each a in attractlights
		a.State = 1
	Next
LightSeqAttract.stopplay
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqDownOn, 70, 1  
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqCircleOutOn, 70, 1
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqUpOn, 70, 1  
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqDownOn, 70, 1 
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqFanLeftUpOn, 75, 1  
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqFanRightDownOn, 75, 1
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqFanLeftDownOn, 75, 1
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqFanRightUpOn, 75, 1
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqStripe1VertOn, 75, 2  
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqStripe2VertOn, 75, 2
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 75, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 75, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 75, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 75, 1
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqFanRightDownOn, 75, 1  
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqFanLeftUpOn, 75, 1  
		LightSeqAttract.UpdateInterval = 5          
		LightSeqAttract.Play SeqDownOn, 75, 2
		LightSeqAttract.UpdateInterval = 8          
		LightSeqAttract.Play SeqStripe2HorizOn, 75, 1  
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqStripe2VertOn, 75, 1 
		LightSeqAttract.UpdateInterval = 8          
		LightSeqAttract.Play SeqStripe2HorizOn, 75, 1 
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqStripe2VertOn, 75, 1
		LightSeqAttract.UpdateInterval = 8          
		LightSeqAttract.Play SeqStripe2HorizOn, 75, 1 
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqStripe2VertOn, 75, 1
		LightSeqAttract.UpdateInterval = 8          
		LightSeqAttract.Play SeqStripe2HorizOn, 75, 1 
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqDiagDownRightOn, 75, 1  
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqDiagDownLeftOn, 75, 1
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqDiagUpLeftOn, 75, 1
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqDiagUpRightOn, 75, 1
		LightSeqAttract.UpdateInterval = 5          
		LightSeqAttract.Play SeqCircleOutOn, 75, 1  
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqDownOn, 75, 2
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqUpOn, 75, 2
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqDiagDownRightOn, 75, 1
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqDiagDownLeftOn, 75, 1
		LightSeqAttract.UpdateInterval = 7
		LightSeqAttract.Play SeqCircleOutOn, 75, 1
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqFanLeftUpOn, 75, 1  
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqFanRightDownOn, 75, 1
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqFanLeftDownOn, 75, 1
		LightSeqAttract.UpdateInterval = 8          
		LightSeqAttract.Play SeqStripe1HorizOn, 75, 1  
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqStripe1VertOn, 75, 1 
		LightSeqAttract.UpdateInterval = 8          
		LightSeqAttract.Play SeqStripe1HorizOn, 75, 1 
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqStripe1VertOn, 75, 1
		LightSeqAttract.UpdateInterval = 8          
		LightSeqAttract.Play SeqStripe1HorizOn, 75, 1 
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqStripe1VertOn, 75, 1
		LightSeqAttract.UpdateInterval = 8          
		LightSeqAttract.Play SeqStripe1HorizOn, 75, 1 
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqDownOn, 70, 1  
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqCircleOutOn, 70, 1
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqUpOn, 70, 1  
		LightSeqAttract.UpdateInterval = 4         
		LightSeqAttract.Play SeqDownOn, 70, 1 
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqFanLeftUpOn, 75, 1  
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqFanRightDownOn, 75, 1
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqFanLeftDownOn, 75, 1
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqFanRightUpOn, 75, 1
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqStripe1VertOn, 75, 2  
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqStripe2VertOn, 75, 2
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 75, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 75, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqRightOn, 75, 1
		LightSeqAttract.UpdateInterval = 8
		LightSeqAttract.Play SeqLeftOn, 75, 1
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqFanRightDownOn, 75, 1  
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqFanLeftUpOn, 75, 1  
		LightSeqAttract.UpdateInterval = 5          
		LightSeqAttract.Play SeqDownOn, 75, 2
		LightSeqAttract.UpdateInterval = 8          
		LightSeqAttract.Play SeqStripe2HorizOn, 75, 1  
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqStripe2VertOn, 75, 1 
		LightSeqAttract.UpdateInterval = 8          
		LightSeqAttract.Play SeqStripe2HorizOn, 75, 1 
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqStripe2VertOn, 75, 1
		LightSeqAttract.UpdateInterval = 8          
		LightSeqAttract.Play SeqStripe2HorizOn, 75, 1 
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqStripe2VertOn, 75, 1
		LightSeqAttract.UpdateInterval = 8          
		LightSeqAttract.Play SeqStripe2HorizOn, 75, 1 
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqDiagDownRightOn, 75, 1  
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqDiagDownLeftOn, 75, 1
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqDiagUpLeftOn, 75, 1
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqDiagUpRightOn, 75, 1
		LightSeqAttract.UpdateInterval = 5          
		LightSeqAttract.Play SeqCircleOutOn, 75, 1  
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqDownOn, 75, 2
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqUpOn, 75, 2
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqDiagDownRightOn, 75, 1
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqDiagDownLeftOn, 75, 1
		LightSeqAttract.UpdateInterval = 7
		LightSeqAttract.Play SeqCircleOutOn, 75, 1
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqFanLeftUpOn, 75, 1  
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqFanRightDownOn, 75, 1
		LightSeqAttract.UpdateInterval = 4
		LightSeqAttract.Play SeqFanLeftDownOn, 75, 1
		LightSeqAttract.UpdateInterval = 8          
		LightSeqAttract.Play SeqStripe1HorizOn, 75, 1  
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqStripe1VertOn, 75, 1 
		LightSeqAttract.UpdateInterval = 8          
		LightSeqAttract.Play SeqStripe1HorizOn, 75, 1 
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqStripe1VertOn, 75, 1
		LightSeqAttract.UpdateInterval = 8          
		LightSeqAttract.Play SeqStripe1HorizOn, 75, 1 
		LightSeqAttract.UpdateInterval = 4          
		LightSeqAttract.Play SeqStripe1VertOn, 75, 1
		LightSeqAttract.UpdateInterval = 8          
		LightSeqAttract.Play SeqStripe1HorizOn, 75, 1 
	For each a in GI
		a.State = 1
		a.Intensity = 23
	Next
LightSeqGi.stopplay
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqDownOn, 70, 1  
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqCircleOutOn, 70, 1
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqUpOn, 70, 1  
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqDownOn, 70, 1 
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqFanLeftUpOn, 75, 1  
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqFanRightDownOn, 75, 1
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqFanLeftDownOn, 75, 1
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqFanRightUpOn, 75, 1
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqStripe1VertOn, 75, 2  
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqStripe2VertOn, 75, 2
		LightSeqGi.UpdateInterval = 8
		LightSeqGi.Play SeqRightOn, 75, 1
		LightSeqGi.UpdateInterval = 8
		LightSeqGi.Play SeqLeftOn, 75, 1
		LightSeqGi.UpdateInterval = 8
		LightSeqGi.Play SeqRightOn, 75, 1
		LightSeqGi.UpdateInterval = 8
		LightSeqGi.Play SeqLeftOn, 75, 1
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqFanRightDownOn, 75, 1  
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqFanLeftUpOn, 75, 1  
		LightSeqGi.UpdateInterval = 5          
		LightSeqGi.Play SeqDownOn, 75, 2
		LightSeqGi.UpdateInterval = 8          
		LightSeqGi.Play SeqStripe2HorizOn, 75, 1  
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqStripe2VertOn, 75, 1 
		LightSeqGi.UpdateInterval = 8          
		LightSeqGi.Play SeqStripe2HorizOn, 75, 1 
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqStripe2VertOn, 75, 1
		LightSeqGi.UpdateInterval = 8          
		LightSeqGi.Play SeqStripe2HorizOn, 75, 1 
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqStripe2VertOn, 75, 1
		LightSeqGi.UpdateInterval = 8          
		LightSeqGi.Play SeqStripe2HorizOn, 75, 1 
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqDiagDownRightOn, 75, 1  
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqDiagDownLeftOn, 75, 1
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqDiagUpLeftOn, 75, 1
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqDiagUpRightOn, 75, 1
		LightSeqGi.UpdateInterval = 5          
		LightSeqGi.Play SeqCircleOutOn, 75, 1  
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqDownOn, 75, 2
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqUpOn, 75, 2
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqDiagDownRightOn, 75, 1
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqDiagDownLeftOn, 75, 1
		LightSeqGi.UpdateInterval = 7
		LightSeqGi.Play SeqCircleOutOn, 75, 1
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqFanLeftUpOn, 75, 1  
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqFanRightDownOn, 75, 1
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqFanLeftDownOn, 75, 1
		LightSeqGi.UpdateInterval = 8          
		LightSeqGi.Play SeqStripe1HorizOn, 75, 1  
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqStripe1VertOn, 75, 1 
		LightSeqGi.UpdateInterval = 8          
		LightSeqGi.Play SeqStripe1HorizOn, 75, 1 
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqStripe1VertOn, 75, 1
		LightSeqGi.UpdateInterval = 8          
		LightSeqGi.Play SeqStripe1HorizOn, 75, 1 
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqStripe1VertOn, 75, 1
		LightSeqGi.UpdateInterval = 8          
		LightSeqGi.Play SeqStripe1HorizOn, 75, 1 
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqDownOn, 70, 1  
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqCircleOutOn, 70, 1
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqUpOn, 70, 1  
		LightSeqGi.UpdateInterval = 4         
		LightSeqGi.Play SeqDownOn, 70, 1 
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqFanLeftUpOn, 75, 1  
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqFanRightDownOn, 75, 1
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqFanLeftDownOn, 75, 1
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqFanRightUpOn, 75, 1
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqStripe1VertOn, 75, 2  
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqStripe2VertOn, 75, 2
		LightSeqGi.UpdateInterval = 8
		LightSeqGi.Play SeqRightOn, 75, 1
		LightSeqGi.UpdateInterval = 8
		LightSeqGi.Play SeqLeftOn, 75, 1
		LightSeqGi.UpdateInterval = 8
		LightSeqGi.Play SeqRightOn, 75, 1
		LightSeqGi.UpdateInterval = 8
		LightSeqGi.Play SeqLeftOn, 75, 1
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqFanRightDownOn, 75, 1  
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqFanLeftUpOn, 75, 1  
		LightSeqGi.UpdateInterval = 5          
		LightSeqGi.Play SeqDownOn, 75, 2
		LightSeqGi.UpdateInterval = 8          
		LightSeqGi.Play SeqStripe2HorizOn, 75, 1  
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqStripe2VertOn, 75, 1 
		LightSeqGi.UpdateInterval = 8          
		LightSeqGi.Play SeqStripe2HorizOn, 75, 1 
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqStripe2VertOn, 75, 1
		LightSeqGi.UpdateInterval = 8          
		LightSeqGi.Play SeqStripe2HorizOn, 75, 1 
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqStripe2VertOn, 75, 1
		LightSeqGi.UpdateInterval = 8          
		LightSeqGi.Play SeqStripe2HorizOn, 75, 1 
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqDiagDownRightOn, 75, 1  
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqDiagDownLeftOn, 75, 1
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqDiagUpLeftOn, 75, 1
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqDiagUpRightOn, 75, 1
		LightSeqGi.UpdateInterval = 5          
		LightSeqGi.Play SeqCircleOutOn, 75, 1  
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqDownOn, 75, 2
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqUpOn, 75, 2
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqDiagDownRightOn, 75, 1
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqDiagDownLeftOn, 75, 1
		LightSeqGi.UpdateInterval = 7
		LightSeqGi.Play SeqCircleOutOn, 75, 1
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqFanLeftUpOn, 75, 1  
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqFanRightDownOn, 75, 1
		LightSeqGi.UpdateInterval = 4
		LightSeqGi.Play SeqFanLeftDownOn, 75, 1
		LightSeqGi.UpdateInterval = 8          
		LightSeqGi.Play SeqStripe1HorizOn, 75, 1  
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqStripe1VertOn, 75, 1 
		LightSeqGi.UpdateInterval = 8          
		LightSeqGi.Play SeqStripe1HorizOn, 75, 1 
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqStripe1VertOn, 75, 1
		LightSeqGi.UpdateInterval = 8          
		LightSeqGi.Play SeqStripe1HorizOn, 75, 1 
		LightSeqGi.UpdateInterval = 4          
		LightSeqGi.Play SeqStripe1VertOn, 75, 1
		LightSeqGi.UpdateInterval = 8          
		LightSeqGi.Play SeqStripe1HorizOn, 75, 1 
End Sub

Sub StartLightSeq2()
	Dim a
	For each a in attractlights2
		a.State = 1
	Next
LightSeqAttract2.stopplay
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqDownOn, 70, 1  
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqCircleOutOn, 70, 1
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqUpOn, 70, 1  
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqDownOn, 70, 1 
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqFanLeftUpOn, 75, 1  
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqFanRightDownOn, 75, 1
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqFanLeftDownOn, 75, 1
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqFanRightUpOn, 75, 1
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqStripe1VertOn, 75, 2  
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqStripe2VertOn, 75, 2
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 75, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 75, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 75, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 75, 1
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqFanRightDownOn, 75, 1  
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqFanLeftUpOn, 75, 1  
		LightSeqAttract2.UpdateInterval = 5          
		LightSeqAttract2.Play SeqDownOn, 75, 2
		LightSeqAttract2.UpdateInterval = 8          
		LightSeqAttract2.Play SeqStripe2HorizOn, 75, 1  
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqStripe2VertOn, 75, 1 
		LightSeqAttract2.UpdateInterval = 8          
		LightSeqAttract2.Play SeqStripe2HorizOn, 75, 1 
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqStripe2VertOn, 75, 1
		LightSeqAttract2.UpdateInterval = 8          
		LightSeqAttract2.Play SeqStripe2HorizOn, 75, 1 
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqStripe2VertOn, 75, 1
		LightSeqAttract2.UpdateInterval = 8          
		LightSeqAttract2.Play SeqStripe2HorizOn, 75, 1 
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqDiagDownRightOn, 75, 1  
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqDiagDownLeftOn, 75, 1
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqDiagUpLeftOn, 75, 1
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqDiagUpRightOn, 75, 1
		LightSeqAttract2.UpdateInterval = 5          
		LightSeqAttract2.Play SeqCircleOutOn, 75, 1  
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqDownOn, 75, 2
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqUpOn, 75, 2
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqDiagDownRightOn, 75, 1
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqDiagDownLeftOn, 75, 1
		LightSeqAttract2.UpdateInterval = 7
		LightSeqAttract2.Play SeqCircleOutOn, 75, 1
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqFanLeftUpOn, 75, 1  
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqFanRightDownOn, 75, 1
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqFanLeftDownOn, 75, 1
		LightSeqAttract2.UpdateInterval = 8          
		LightSeqAttract2.Play SeqStripe1HorizOn, 75, 1  
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqStripe1VertOn, 75, 1 
		LightSeqAttract2.UpdateInterval = 8          
		LightSeqAttract2.Play SeqStripe1HorizOn, 75, 1 
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqStripe1VertOn, 75, 1
		LightSeqAttract2.UpdateInterval = 8          
		LightSeqAttract2.Play SeqStripe1HorizOn, 75, 1 
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqStripe1VertOn, 75, 1
		LightSeqAttract2.UpdateInterval = 8          
		LightSeqAttract2.Play SeqStripe1HorizOn, 75, 1 
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqDownOn, 70, 1  
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqCircleOutOn, 70, 1
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqUpOn, 70, 1  
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqDownOn, 70, 1 
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqFanLeftUpOn, 75, 1  
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqFanRightDownOn, 75, 1
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqFanLeftDownOn, 75, 1
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqFanRightUpOn, 75, 1
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqStripe1VertOn, 75, 2  
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqStripe2VertOn, 75, 2
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 75, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 75, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqRightOn, 75, 1
		LightSeqAttract2.UpdateInterval = 8
		LightSeqAttract2.Play SeqLeftOn, 75, 1
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqFanRightDownOn, 75, 1  
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqFanLeftUpOn, 75, 1  
		LightSeqAttract2.UpdateInterval = 5          
		LightSeqAttract2.Play SeqDownOn, 75, 2
		LightSeqAttract2.UpdateInterval = 8          
		LightSeqAttract2.Play SeqStripe2HorizOn, 75, 1  
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqStripe2VertOn, 75, 1 
		LightSeqAttract2.UpdateInterval = 8          
		LightSeqAttract2.Play SeqStripe2HorizOn, 75, 1 
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqStripe2VertOn, 75, 1
		LightSeqAttract2.UpdateInterval = 8          
		LightSeqAttract2.Play SeqStripe2HorizOn, 75, 1 
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqStripe2VertOn, 75, 1
		LightSeqAttract2.UpdateInterval = 8          
		LightSeqAttract2.Play SeqStripe2HorizOn, 75, 1 
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqDiagDownRightOn, 75, 1  
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqDiagDownLeftOn, 75, 1
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqDiagUpLeftOn, 75, 1
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqDiagUpRightOn, 75, 1
		LightSeqAttract2.UpdateInterval = 5          
		LightSeqAttract2.Play SeqCircleOutOn, 75, 1  
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqDownOn, 75, 2
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqUpOn, 75, 2
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqDiagDownRightOn, 75, 1
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqDiagDownLeftOn, 75, 1
		LightSeqAttract2.UpdateInterval = 7
		LightSeqAttract2.Play SeqCircleOutOn, 75, 1
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqFanLeftUpOn, 75, 1  
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqFanRightDownOn, 75, 1
		LightSeqAttract2.UpdateInterval = 4
		LightSeqAttract2.Play SeqFanLeftDownOn, 75, 1
		LightSeqAttract2.UpdateInterval = 8          
		LightSeqAttract2.Play SeqStripe1HorizOn, 75, 1  
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqStripe1VertOn, 75, 1 
		LightSeqAttract2.UpdateInterval = 8          
		LightSeqAttract2.Play SeqStripe1HorizOn, 75, 1 
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqStripe1VertOn, 75, 1
		LightSeqAttract2.UpdateInterval = 8          
		LightSeqAttract2.Play SeqStripe1HorizOn, 75, 1 
		LightSeqAttract2.UpdateInterval = 4          
		LightSeqAttract2.Play SeqStripe1VertOn, 75, 1
		LightSeqAttract2.UpdateInterval = 8          
		LightSeqAttract2.Play SeqStripe1HorizOn, 75, 1 
End Sub

Sub StartLightSeq3()
	Dim a
	For each a in attractlights3
		a.State = 1
	Next
LightSeqAttract3.stopplay
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqDownOn, 70, 1  
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqCircleOutOn, 70, 1
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqUpOn, 70, 1  
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqDownOn, 70, 1 
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqFanLeftUpOn, 75, 1  
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqFanRightDownOn, 75, 1
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqFanLeftDownOn, 75, 1
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqFanRightUpOn, 75, 1
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqStripe1VertOn, 75, 2  
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqStripe2VertOn, 75, 2
		LightSeqAttract3.UpdateInterval = 8
		LightSeqAttract3.Play SeqRightOn, 75, 1
		LightSeqAttract3.UpdateInterval = 8
		LightSeqAttract3.Play SeqLeftOn, 75, 1
		LightSeqAttract3.UpdateInterval = 8
		LightSeqAttract3.Play SeqRightOn, 75, 1
		LightSeqAttract3.UpdateInterval = 8
		LightSeqAttract3.Play SeqLeftOn, 75, 1
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqFanRightDownOn, 75, 1  
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqFanLeftUpOn, 75, 1  
		LightSeqAttract3.UpdateInterval = 5          
		LightSeqAttract3.Play SeqDownOn, 75, 2
		LightSeqAttract3.UpdateInterval = 8          
		LightSeqAttract3.Play SeqStripe2HorizOn, 75, 1  
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqStripe2VertOn, 75, 1 
		LightSeqAttract3.UpdateInterval = 8          
		LightSeqAttract3.Play SeqStripe2HorizOn, 75, 1 
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqStripe2VertOn, 75, 1
		LightSeqAttract3.UpdateInterval = 8          
		LightSeqAttract3.Play SeqStripe2HorizOn, 75, 1 
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqStripe2VertOn, 75, 1
		LightSeqAttract3.UpdateInterval = 8          
		LightSeqAttract3.Play SeqStripe2HorizOn, 75, 1 
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqDiagDownRightOn, 75, 1  
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqDiagDownLeftOn, 75, 1
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqDiagUpLeftOn, 75, 1
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqDiagUpRightOn, 75, 1
		LightSeqAttract3.UpdateInterval = 5          
		LightSeqAttract3.Play SeqCircleOutOn, 75, 1  
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqDownOn, 75, 2
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqUpOn, 75, 2
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqDiagDownRightOn, 75, 1
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqDiagDownLeftOn, 75, 1
		LightSeqAttract3.UpdateInterval = 7
		LightSeqAttract3.Play SeqCircleOutOn, 75, 1
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqFanLeftUpOn, 75, 1  
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqFanRightDownOn, 75, 1
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqFanLeftDownOn, 75, 1
		LightSeqAttract3.UpdateInterval = 8          
		LightSeqAttract3.Play SeqStripe1HorizOn, 75, 1  
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqStripe1VertOn, 75, 1 
		LightSeqAttract3.UpdateInterval = 8          
		LightSeqAttract3.Play SeqStripe1HorizOn, 75, 1 
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqStripe1VertOn, 75, 1
		LightSeqAttract3.UpdateInterval = 8          
		LightSeqAttract3.Play SeqStripe1HorizOn, 75, 1 
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqStripe1VertOn, 75, 1
		LightSeqAttract3.UpdateInterval = 8          
		LightSeqAttract3.Play SeqStripe1HorizOn, 75, 1 
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqDownOn, 70, 1  
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqCircleOutOn, 70, 1
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqUpOn, 70, 1  
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqDownOn, 70, 1 
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqFanLeftUpOn, 75, 1  
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqFanRightDownOn, 75, 1
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqFanLeftDownOn, 75, 1
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqFanRightUpOn, 75, 1
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqStripe1VertOn, 75, 2  
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqStripe2VertOn, 75, 2
		LightSeqAttract3.UpdateInterval = 8
		LightSeqAttract3.Play SeqRightOn, 75, 1
		LightSeqAttract3.UpdateInterval = 8
		LightSeqAttract3.Play SeqLeftOn, 75, 1
		LightSeqAttract3.UpdateInterval = 8
		LightSeqAttract3.Play SeqRightOn, 75, 1
		LightSeqAttract3.UpdateInterval = 8
		LightSeqAttract3.Play SeqLeftOn, 75, 1
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqFanRightDownOn, 75, 1  
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqFanLeftUpOn, 75, 1  
		LightSeqAttract3.UpdateInterval = 5          
		LightSeqAttract3.Play SeqDownOn, 75, 2
		LightSeqAttract3.UpdateInterval = 8          
		LightSeqAttract3.Play SeqStripe2HorizOn, 75, 1  
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqStripe2VertOn, 75, 1 
		LightSeqAttract3.UpdateInterval = 8          
		LightSeqAttract3.Play SeqStripe2HorizOn, 75, 1 
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqStripe2VertOn, 75, 1
		LightSeqAttract3.UpdateInterval = 8          
		LightSeqAttract3.Play SeqStripe2HorizOn, 75, 1 
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqStripe2VertOn, 75, 1
		LightSeqAttract3.UpdateInterval = 8          
		LightSeqAttract3.Play SeqStripe2HorizOn, 75, 1 
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqDiagDownRightOn, 75, 1  
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqDiagDownLeftOn, 75, 1
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqDiagUpLeftOn, 75, 1
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqDiagUpRightOn, 75, 1
		LightSeqAttract3.UpdateInterval = 5          
		LightSeqAttract3.Play SeqCircleOutOn, 75, 1  
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqDownOn, 75, 2
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqUpOn, 75, 2
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqDiagDownRightOn, 75, 1
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqDiagDownLeftOn, 75, 1
		LightSeqAttract3.UpdateInterval = 7
		LightSeqAttract3.Play SeqCircleOutOn, 75, 1
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqFanLeftUpOn, 75, 1  
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqFanRightDownOn, 75, 1
		LightSeqAttract3.UpdateInterval = 4
		LightSeqAttract3.Play SeqFanLeftDownOn, 75, 1
		LightSeqAttract3.UpdateInterval = 8          
		LightSeqAttract3.Play SeqStripe1HorizOn, 75, 1  
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqStripe1VertOn, 75, 1 
		LightSeqAttract3.UpdateInterval = 8          
		LightSeqAttract3.Play SeqStripe1HorizOn, 75, 1 
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqStripe1VertOn, 75, 1
		LightSeqAttract3.UpdateInterval = 8          
		LightSeqAttract3.Play SeqStripe1HorizOn, 75, 1 
		LightSeqAttract3.UpdateInterval = 4          
		LightSeqAttract3.Play SeqStripe1VertOn, 75, 1
		LightSeqAttract3.UpdateInterval = 8          
		LightSeqAttract3.Play SeqStripe1HorizOn, 75, 1 
End Sub

Sub StartPLights()
	Dim p
	For each p in pLights
		p.State = 2
		p.BlinkInterval = 150	
	Next
End Sub

Sub StopPLights()
	Dim p
	For each p in pLights
		p.State = 0	
	Next
	Dim a
	For each a in attractlights2
		a.State = 0	
	Next
	For each a in attractlights3
		a.State = 0	
	Next
End Sub

Sub LightSeqTilt_PlayDone()
	LightSeqTilt.Play SeqAllOff
End Sub

Sub LightSeqSkillshot_PlayDone()
	LightSeqSkillshot.Play SeqAllOff
End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'  GI LIGHTING
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	Dim OldGiState
	OldGiState = -1   'start witht the Gi off
	'******************************************
	' Change GI color
	'******************************************
	Sub ChangeGi(col)
		Dim bulb
		For each bulb in GI
			SetLightColor bulb, col, -1
		Next
	End Sub
	Sub ChangeLights(col)
		Dim a
		For each a in alights
			SetLightColor a, col, -1
		Next
	End Sub

	Sub ChangeLights2(col)
		Dim a
		For each a in alights2
			SetLightColor a, col, -1
		Next
	End Sub
	Sub ChangeLights3(col)
		Dim a
		For each a in alights3
			SetLightColor a, col, -1
		Next
	End Sub
	Sub ChangeAttractLights2(col)
		Dim a
		For each a in attractlights2
			SetLightColor a, col, -1
		Next
	End Sub
	Sub ChangeAttractLights3(col)
		Dim a
		For each a in attractlights3
			SetLightColor a, col, -1
		Next
	End Sub
	'******************************************
	' Change GI blinking color
	'******************************************
	Sub ChangeGiBlink(col)
		Dim bulb
		For each bulb in GI
			SetLightColorBlinkGI bulb, col, 2
		Next
	End Sub

	Sub SetLightColorBlinkGI(n, col, stat)
		Select Case col
			Case amber
				n.color = RGB(255, 153, 0)
				n.colorfull = RGB(255, 153, 0)
			Case yellow
				n.color = RGB(255, 255, 0)
				n.colorfull = RGB(255, 255, 0)
			Case green
				n.color = RGB(0, 255, 0)
				n.colorfull = RGB(0, 255, 0)
			Case purple
				n.color = RGB(128, 0, 128)
				n.colorfull = RGB(128, 0, 128)
			Case white
				n.color = RGB(255, 255, 255)
				n.colorfull = RGB(255, 255, 255)
			Case ivory
				n.color = RGB(255, 252, 224)
				n.colorfull = RGB(193, 91, 0)
			Case base
				n.color = RGB(255, 197, 143)
				n.colorfull = RGB(255, 255, 236)
		End Select
		If stat <> -1 THEN
			n.State = 2
			n.State = stat
		End If
	End Sub

'	Sub SetLightColorBlinkMultiball(n, col, stat)
'		dim baseColor
'			Case amber
'				n.color = RGB(255, 153, 0)
'				n.colorfull = RGB(255, 153, 0)
'			Case yellow
'				n.color = RGB(255, 255, 0)
'				n.colorfull = RGB(255, 255, 0)
'			Case green
'				n.color = RGB(0, 255, 0)
'				n.colorfull = RGB(0, 255, 0)
'			Case purple
'				n.color = RGB(128, 0, 128)
'				n.colorfull = RGB(128, 0, 128)
'			Case white
'				n.color = RGB(255, 255, 255)
'				n.colorfull = RGB(255, 255, 255)
'			Case ivory
'				n.color = RGB(255, 252, 224)
'				n.colorfull = RGB(193, 91, 0)
'			Case base
'				n.color = RGB(255, 197, 143)
'				n.colorfull = RGB(255, 255, 236)
'		End Select
'		If stat <> -1 THEN
'			n.State = 0
'			n.State = stat
'		End If
'	End Sub

 	Sub RandomChangeGi()
		RandomGIColor : RandomLightsColor
	End Sub

	Sub RandomGIColor()
		Select Case Int(Rnd * 6) + 1
			Case 1:ChangeGi(amber)
			Case 2:ChangeGi(yellow)
			Case 3:ChangeGi(green)
			Case 4:ChangeGi(purple)
			Case 5:ChangeGi(lightpurple)
			Case 6:ChangeGi(lightgreen)
		End Select
	End Sub
	Sub RandomLightsColor()
		Select Case Int(Rnd * 4) + 1
			Case 1:ChangeLights(amber)
			Case 2:ChangeLights(yellow)
			Case 3:ChangeLights(green)
			Case 4:ChangeLights(purple)
		End Select
	End Sub

		Dim doomchangegiblink
		doomchangegiblink = Int(Rnd * 5) + 1
 	Sub RandomChangeGiBlink()
		doomchangegiblink = doomchangegiblink + 1
		Select Case doomchangegiblink		
			Case 1: ChangeGiBlink(amber)
					ChangeLights(amber)
			Case 2: ChangeGiBlink(yellow)
					ChangeLights(yellow)
			Case 3: ChangeGiBlink(green)
					ChangeLights(green)
			Case 4: ChangeGiBlink(white)
					ChangeLights(base)	
			Case 5: ChangeGiBlink(purple)
					ChangeLights(purple)	
				    doomchangegiblink = 0	
			End Select
	End Sub

	Sub RandomChangeLightsTwo
		Select Case Int(Rnd * 8) + 1
			Case 1: ChangeLights2(purple)
			Case 2: ChangeLights2(green)
			Case 3: ChangeLights2(orange)
			Case 4: ChangeLights2(yellow)
			Case 5: ChangeLights2(indigo)
			Case 6: ChangeLights2(lime)
			Case 7: ChangeLights2(lightpurple)
			Case 8: ChangeLights2(purple)
		End Select
	End Sub
	
	Sub RandomChangeLightsThree
		Select Case Int(Rnd * 8) + 1
			Case 1: ChangeLights3(purple)
			Case 2: ChangeLights3(green)
			Case 3: ChangeLights3(orange)
			Case 4: ChangeLights3(yellow)
			Case 5: ChangeLights3(indigo)
			Case 6: ChangeLights3(lime)
			Case 7: ChangeLights3(lightpurple)
			Case 8: ChangeLights3(purple)
		End Select
	End Sub
	'******************************************
	' Change GI color back to white
	'******************************************
	Sub ChangeGiLit(col)
		Dim bulb
		For each bulb in GI
			SetLightColorLitGI bulb, col, 1
		Next
	End Sub

	Sub SetLightColorLitGI(n, col, stat)
		Select Case col
			Case white
				n.color = RGB(255, 252, 224)
				n.colorfull = RGB(255, 252, 224)
			Case ivory
				n.color = RGB(255, 252, 224)
				n.colorfull = RGB(193, 91, 0)
			Case base
				n.color = RGB(255, 155, 64)
				n.colorfull = RGB(255, 255, 255)
				'n.color = RGB(255, 197, 143)
				'n.colorfull = RGB(255, 255, 236)
		End Select
		If stat <> -1 THEN
			n.State = 1
			n.State = stat
		End If
	End Sub
	'*************************************************
	'	GI STROBES
	'*************************************************
	Sub GIStrobeYellowGreen
		ChangeGi(yellow)
		LightQueue.Add "ChangeGI(green)","ChangeGI(green)",20,50,0,0,0,false
		LightQueue.Add "ChangeGI(yellow)","ChangeGI(yellow)",20,100,0,0,0,false
		LightQueue.Add "ChangeGI(green)","ChangeGI(green)",20,150,0,0,0,false
		LightQueue.Add "ChangeGI(yellow)","ChangeGI(yellow)",20,200,0,0,0,false
		LightQueue.Add "ChangeGI(green)","ChangeGI(green)",20,250,0,0,0,false
		LightQueue.Add "ChangeGI(yellow)","ChangeGI(yellow)",20,300,0,0,0,false
		LightQueue.Add "ChangeGI(green)","ChangeGI(green)",20,350,0,0,0,false
		LightQueue.Add "ChangeGI(yellow)","ChangeGI(yellow)",20,400,0,0,0,false
		LightQueue.Add "ChangeGI(green)","ChangeGI(green)",20,450,0,0,0,false
		LightQueue.Add "ChangeGI(yellow)","ChangeGI(yellow)",20,500,0,0,0,false
		LightQueue.Add "ChangeGI(green)","ChangeGI(green)",20,550,0,0,0,false

	End Sub
	Sub GIStrobeAmberWhite
		ChangeGi(amber)
		LightQueue.Add "ChangeGI(white)","ChangeGI(white)",20,50,0,0,0,false
		LightQueue.Add "ChangeGI(amber)","ChangeGI(amber)",20,100,0,0,0,false
		LightQueue.Add "ChangeGI(white)","ChangeGI(white)",20,150,0,0,0,false
		LightQueue.Add "ChangeGI(amber)","ChangeGI(amber)",20,200,0,0,0,false
		LightQueue.Add "ChangeGI(white)","ChangeGI(white)",20,250,0,0,0,false
		LightQueue.Add "ChangeGI(amber)","ChangeGI(amber)",20,300,0,0,0,false
		LightQueue.Add "ChangeGI(white)","ChangeGI(white)",20,350,0,0,0,false
		LightQueue.Add "ChangeGI(amber)","ChangeGI(amber)",20,400,0,0,0,false
		LightQueue.Add "ChangeGI(white)","ChangeGI(white)",20,450,0,0,0,false
		LightQueue.Add "ChangeGI(amber)","ChangeGI(amber)",20,500,0,0,0,false
		LightQueue.Add "ChangeGI(white)","ChangeGI(white)",20,550,0,0,0,false

	End Sub
	Sub GIStrobeGreenWhite
		ChangeGI(green)
		LightQueue.Add "ChangeGI(white)","ChangeGI(white)",20,50,0,0,0,false
		LightQueue.Add "ChangeGI(green)","ChangeGI(green)",20,100,0,0,0,false
		LightQueue.Add "ChangeGI(white)","ChangeGI(white)",20,150,0,0,0,false
		LightQueue.Add "ChangeGI(green)","ChangeGI(green)",20,200,0,0,0,false
		LightQueue.Add "ChangeGI(white)","ChangeGI(white)",20,250,0,0,0,false
	End Sub
	Sub GIStrobePurpleWhite
		ChangeGI(purple)

		LightQueue.Add "ChangeGI(white)","ChangeGI(white)",20,50,0,0,0,false
		LightQueue.Add "ChangeGI(purple)","ChangeGI(purple)",20,100,0,0,0,false
		LightQueue.Add "ChangeGI(white)","ChangeGI(white)",20,150,0,0,0,false
		LightQueue.Add "ChangeGI(purple)","ChangeGI(purple)",20,200,0,0,0,false
		LightQueue.Add "ChangeGI(white)","ChangeGI(white)",20,250,0,0,0,false
	End Sub
	'**************************************************
	' Change GI blinking colors for specific triggers
	'**************************************************
	Sub ChangeGiBlinkDrainLeft(col)
		SetLightColorBlinkGI gi4, col, 2
		SetLightColorBlinkGI gi6, col, 2
		SetLightColorBlinkGI gi7, col, 2
	End Sub
	Sub ChangeGiBlinkDrainRight(col)
		SetLightColorBlinkGI gi2, col, 2
		SetLightColorBlinkGI gi5, col, 2
		SetLightColorBlinkGI gi8, col, 2
	End Sub
	Sub ChangeGiBlinkRampLeft(col)
		SetLightColorBlinkGI gi4, col, 2
		SetLightColorBlinkGI gi6, col, 2
		SetLightColorBlinkGI gi7, col, 2
		SetLightColorBlinkGI gi12, col, 2
		SetLightColorBlinkGI gi13, col, 2
		SetLightColorBlinkGI gi17, col, 2
	End Sub
	Sub ChangeGiBlinkRampRight(col)
		SetLightColorBlinkGI gi2, col, 2
		SetLightColorBlinkGI gi5, col, 2
		SetLightColorBlinkGI gi8, col, 2
		SetLightColorBlinkGI gi9, col, 2
		SetLightColorBlinkGI gi10, col, 2
	End Sub

	Sub RandomGIForTargets()
		Select Case Int(Rnd * 4) + 1
			Case 1: GIStrobeYellowGreen
			Case 2: GIStrobeAmberWhite
			Case 3: GIStrobeGreenWhite
			Case 4: GIStrobePurpleWhite

		End Select
	End Sub

	Sub ChangeGiForJackpot
		ChangeGiBlink(amber)
		LightQueue.Add "ChangeGiLit(base)","ChangeGiLit(base)",20,6900,0,0,0,false
	End Sub

	Sub TurnOffGIMultiball
		Gi001.state = 0
	End Sub

	Sub ChangeGiForKeepBombing
		ChangeGiBlink(white)
		LightQueue.Add "ChangeGiLit(base)","ChangeGiLit(base)",20,4000,0,0,0,false
	End Sub

	Sub ChangeGiForDrainLeft
		If LMultiballIsLit.state = 1 then
		Else
			If LKicker.state = 1 then
			Else
				If LWarpMultiballCounter.state = 2 then
				Else
					ChangeGiBlinkDrainLeft(purple)
					LightQueue.Add "ChangeGiLit(base)","ChangeGiLit(base)",20,2250,0,0,0,false
				End If
			End If
		End If
	End Sub

	Sub ChangeGiForDrainRight
		If LMultiballIsLit.state = 1 then
		Else
			If LKicker.state = 1 then
			Else
				If LWarpMultiballCounter.state = 2 then
				Else		
					ChangeGiBlinkDrainRight(purple)
					LightQueue.Add "ChangeGiLit(base)","ChangeGiLit(base)",20,2250,0,0,0,false
				End If
			End If
		End If
	End Sub

	Sub ChangeGiForRampLeft
		If LMultiballIsLit.state = 1 then
		Else
			If LKicker.state = 1 then
			Else
				If LWarpMultiballCounter.state = 2 then
				Else
					ChangeGiBlinkDrainLeft(purple)
					LightQueue.Add "ChangeGiLit(base)","ChangeGiLit(base)",20,1600,0,0,0,false
				End If
			End If
		End If
	End Sub

	Sub ChangeGiForRampRight
		If LMultiballIsLit.state = 1 then
		Else
			If LKicker.state = 1 then
			Else
				If LWarpMultiballCounter.state = 2 then
				Else
					ChangeGiBlinkDrainRight(purple)
					LightQueue.Add "ChangeGiLit(base)","ChangeGiLit(base)",20,1600,0,0,0,false
				End If
			End If
		End If
	End Sub
	'******************************************
	' GI Update Timer
	'******************************************
	Sub GIUpdateTimer_Timer
		Dim tmp, obj
		tmp = Getballs
		If UBound(tmp) <> OldGiState Then
			OldGiState = Ubound(tmp)
			If UBound(tmp) = 3 Then 'we have 4 captive balls on the table (-1 means no balls, 0 is the first ball, 1 is the second..)
				'GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
			Else
				'Gion
			End If
		End If
	End Sub

	Sub GiOn
		DOF 126, DOFOn     '?
		Dim bulb
		For each bulb in GI
			SetLightColorGI bulb, base, -1
			bulb.State = 1
		Next
	End Sub

	Sub GiOff
		DOF 126, DOFOff    '?
		Dim bulb
		For each bulb in GI
			bulb.State = 0
		Next
	End Sub

	Sub GiEffect(n)
		Select Case n
			Case 0 
				LightSeqGi.UpdateInterval = 0
				LightSeqGi.Play SeqBlinking, , 10, 10
			Case 1 
				LightSeqGI2.UpdateInterval = 0
				LightSeqGI2.Play SeqBlinking, , 12, 10
			Case 2 'slings
				LightSeqGi.UpdateInterval = 0
				LightSeqGi.Play SeqBlinking, , 2, 10
		End Select
	End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'   SCORING FUNCTIONS
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	Sub AddScore(points)
		If(Tilted = False) Then
			Score(CurrentPlayer) = Score(CurrentPlayer) + points
		End if
	End Sub

	Sub AwardExtraBall()
		If NOT bExtraBallWonThisBall Then
			LightShootAgain.State = 1
			flashflash.Enabled = True
			ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
			bExtraBallWonThisBall = True
			DOF 939, DOFPulse   'DOF MX - Ball Added
			'extraballready(CurrentPlayer) = 0
			If bMultiBallMode = false Then
			'	insert scenario
				'chilloutthemusic
			End If
		Else
		'Insert Scenario Here...
		END If
	End Sub

	Sub AwardSkillshot()
		DOF 125, DOFPulse
		ResetSkillShotTimer_Timer
		AddScore SkillshotValue(CurrentPLayer)
		DOF 939, DOFPulse   'DOF MX - Skillshot
		AudioQueue.Add "SkillshotCallout","SkillshotCallout",20,200,0,0,0,false
		pupevent 719
		DMDBigText "SKILL SHOT",100,0 '3.4sec 
		SkillShotValue(CurrentPLayer) = SkillShotValue(CurrentPLayer) + 10000
	End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'   BALL FUNCTIONS & DRAINS
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	Function Balls
		Dim tmp
		tmp = bpgcurrent - BallsRemaining(CurrentPlayer) + 1
		If tmp> bpgcurrent Then
			Balls = bpgcurrent
		Else
			Balls = tmp
		End If
	End Function

	Sub FirstBall
		ResetForNewPlayerBall()
		CreateNewBall()
	End Sub

	Sub ResetForNewPlayerBall()
		Dbg "Reseting for New Player Ball"
		LRuby2.state = 0
		Dim z
			For each z in herbs
			z.State = 0
		Next
		StopAllMusic
		'hsbModeActive = False ' Reset high score mode active
		If PlayersPlayingGame > 1 Then
			If CurrentPlayer = 1 Then
				Player1Callout
				pupevent 720
				pupevent 721
				RandomSoundKickout
				FlasherAllBlinkOnce
				LightSeqALLCAPS.UpdateInterval = 0
				LightSeqALLCAPS.Play SeqBlinking, , 2, 5
				LightSeqhit.UpdateInterval = 0
				LightSeqhit.Play SeqBlinking, , 2, 5
				'RandomUltraDMDSceneKickout
				playvideo=1+int(rnd(1)*3)
				AudioQueue.Add "RandomSoundPlungerIdle","RandomSoundPlungerIdle",20,500,0,0,0,false
				DOF 942, DOFOn   'DOF MX
				DOF 971, DOFOn   'DOF MX - BACK
				'chilloutthemusic
				'StopAllMusic
				LSlime.state = 2
			Elseif currentplayer = 2 Then
				Player2Callout
				pupevent 720
				pupevent 722
				RandomSoundKickout
				FlasherAllBlinkOnce
				LightSeqALLCAPS.UpdateInterval = 0
				LightSeqALLCAPS.Play SeqBlinking, , 2, 5
				LightSeqhit.UpdateInterval = 0
				LightSeqhit.Play SeqBlinking, , 2, 5
				'RandomUltraDMDSceneKickout
				playvideo=1+int(rnd(1)*3)
				AudioQueue.Add "RandomSoundPlungerIdle","RandomSoundPlungerIdle",20,500,0,0,0,false
				DOF 942, DOFOn   'DOF MX
				DOF 971, DOFOn   'DOF MX - BACK
				'chilloutthemusic
				'StopAllMusic
				LSlime.state = 2
			Elseif currentplayer = 3 Then
				Player3Callout
				pupevent 720
				pupevent 723
				RandomSoundKickout
				FlasherAllBlinkOnce
				LightSeqALLCAPS.UpdateInterval = 0
				LightSeqALLCAPS.Play SeqBlinking, , 2, 5
				LightSeqhit.UpdateInterval = 0
				LightSeqhit.Play SeqBlinking, , 2, 5
				'RandomUltraDMDSceneKickout
				playvideo=1+int(rnd(1)*3)
				AudioQueue.Add "RandomSoundPlungerIdle","RandomSoundPlungerIdle",20,500,0,0,0,false
				DOF 942, DOFOn   'DOF MX	
				DOF 971, DOFOn   'DOF MX - BACK	
				'chilloutthemusic
				StopAllMusic
				LSlime.state = 2
			Elseif currentplayer = 4 Then
				Player4Callout
				pupevent 720
				pupevent 724
				RandomSoundKickout
				FlasherAllBlinkOnce
				LightSeqALLCAPS.UpdateInterval = 0
				LightSeqALLCAPS.Play SeqBlinking, , 2, 5
				LightSeqhit.UpdateInterval = 0
				LightSeqhit.Play SeqBlinking, , 2, 5
				'RandomUltraDMDSceneKickout
				playvideo=1+int(rnd(1)*3)
				AudioQueue.Add "RandomSoundPlungerIdle","RandomSoundPlungerIdle",20,500,0,0,0,false
				DOF 942, DOFOn   'DOF MX	
				DOF 971, DOFOn   'DOF MX - BACK	
				'chilloutthemusic
				'StopAllMusic
				LSlime.state = 2
			End If
		Else
			RandomSoundKickout
			FlasherAllBlinkOnce
			LightSeqALLCAPS.UpdateInterval = 0
			LightSeqALLCAPS.Play SeqBlinking, , 2, 5
			LightSeqhit.UpdateInterval = 0
			LightSeqhit.Play SeqBlinking, , 2, 5			
			'RandomUltraDMDSceneKickout
			playvideo=1+int(rnd(1)*3)
			AudioQueue.Add "RandomSoundPlungerIdle","RandomSoundPlungerIdle",20,500,0,0,0,false
			DOF 942, DOFOn   'DOF MX	
			DOF 971, DOFOn   'DOF MX - BACK
			'chilloutthemusic
			'StopAllMusic
			LSlime.state = 2
		End If
		AddScore 0
		BonusPoints(CurrentPlayer) = 0
		bBonusHeld = False
		bExtraBallWonThisBall = False
		ResetNewBallLights()
		ResetNewBallVariables
		bBallSaverReady = True
		bSkillShotReady = True
		bSkillshotRotateLights = False
		bDoubleScoringActive = False       '
		bWarpSpeedMultiballActive = False  '
		bComboActive = False               '
		bSuperPopsActive = False           '
		bSuperRampsActive = False          '
		bSuperOrbitsActive = False         '
		bDoubleSpinnerActive = False       '
		ResetSupShotLights                 '
		ResetBoltLights                    '
		ResetLevelCompletedLights          '
		ResetStandupsCompletedLight        '
		ResetLSupreme08confirm             '
		ResetDoubleSpinnerLight            '
		ResetSuperOrbitsLight              '
		ResetSuperPopsLight                '
		ResetSuperRampsLight               '
		LCombo.State = 0
		bCombo(CurrentPlayer) = 0
		'StopAllMusic
	End Sub

	Sub CreateNewBall()
		If LPressStart.state = 0 THEN                 'test
		BallRelease.CreateSizedball BallSize / 2
		BallsOnPlayfield = BallsOnPlayfield + 1
		PlaySoundAt SoundFXDOF("", 102, DOFPulse, DOFContactors), BallRelease
		RandomSoundBallRelease BallRelease
		BallRelease.Kick 90, 4
		LightSeqALLCAPS.UpdateInterval = 0
		LightSeqALLCAPS.Play SeqBlinking, , 2, 5
		LightSeqhit.UpdateInterval = 0
		LightSeqhit.Play SeqBlinking, , 2, 5
		LPressStart.state = 1
		FlasherAllBlinkOnce
		If BallsOnPlayfield > 1 Then
			bMultiBallMode = True
			bAutoPlunger = True
		End If
		End If
		'StopAllMusic
	End Sub

	Sub AddMultiball(nballs)
		mBalls2Eject = mBalls2Eject + nballs
		CreateMultiballTimer.Enabled = True
	End Sub

	Sub CreateMultiballTimer_Timer()
		If bBallInPlungerLane Then
			Exit Sub
		Else
			If BallsOnPlayfield <MaxMultiballs Then
				CreateNewBall()
				mBalls2Eject = mBalls2Eject -1
				If mBalls2Eject = 0 Then 
					Me.Enabled = False
				End If
			Else 
				mBalls2Eject = 0
				Me.Enabled = False
			End If
		End If
	End Sub
	'**************************
	'   BALL LOCK ROUTINE
	'**************************
	Sub CreateNewBallForBallLock()
		'EndMusic
		'StopAllMusic
		RandomSoundFood
		BallHandlingQueue.Add "BallForBallLock","BallForBallLock",20,6025,0,0,0,false
		AudioQueue.Add "RandomSoundGas","RandomSoundGas",20,1000,0,0,0,false
		AudioQueue.Add "RandomSoundGas","RandomSoundGas",20,2500,0,0,0,false	
		AudioQueue.Add "RandomSoundFoods","RandomSoundFoods",20,3000,0,0,0,false	
		BallHandlingQueue.Add "FlasherBallLocked","FlasherBallLocked",20,2500,0,0,0,false
		AudioQueue.Add "RandomSoundGas","RandomSoundGas",20,4000,0,0,0,false			
	End Sub

	Sub BallForBallLock()
		BallRelease.CreateSizedball BallSize / 2
		BallsOnPlayfield = BallsOnPlayfield + 1
		PlaySoundAt SoundFXDOF("", 102, DOFPulse, DOFContactors), BallRelease
		RandomSoundBallRelease BallRelease
		BallRelease.Kick 90, 4
		FlasherAllBlinkOnce
		ChangeGiBlink(purple)
		ChangeLights(purple)
		If BallsOnPlayfield > 1 Then
			bMultiBallMode = True
			bAutoPlunger = True
		End If
	End Sub

	Sub AddMultiballForBallLock(nballs)
		mBalls2Eject = mBalls2Eject + nballs
		CreateMultiballBallLockTimer.Enabled = True
	End Sub

	Sub CreateMultiballBallLockTimer_Timer()
		If bBallInPlungerLane Then
			Exit Sub
		Else
			If BallsOnPlayfield <MaxMultiballs Then
				CreateNewBallForBallLock()
				mBalls2Eject = mBalls2Eject -1
				If mBalls2Eject = 0 Then 
					Me.Enabled = False
				End If
			Else 
				mBalls2Eject = 0
				Me.Enabled = False
			End If
		End If
	End Sub
	'**************************
	'   END OF BALL 1
	'**************************
	Sub EndOfBall()
		bMultiBallMode = False
		bOnTheFirstBall = False
		If NOT Tilted Then
			BallHandlingQueue.Add "EndOfBall2","EndOfBall2",20,500,0,0,0,false
		Else 
			BallHandlingQueue.Add "EndOfBall2","EndOfBall2",20,500,0,0,0,false
		End If
	End Sub
	'**************************
	'   END OF BALL 2
	'**************************
	Sub EndOfBall2()
		ResetDropTargetsSparks 
		ResetSupShotLights
		ResetBoltLights
		ResetAllLaneLights
		ResetLevelCompletedLights
		ResetStandupsCompletedLight
		TurnOffGIMultiball
		Tilted = False
		Tilt = 0
		DisableTable False
		tilttableclear.enabled = False
		tilttime = 0
		If(ExtraBallsAwards(CurrentPlayer) <> 0) Then
			ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1
			If(ExtraBallsAwards(CurrentPlayer) = 0) Then
				LightShootAgain.State = 0
			End If
			LightSeqFlasher.UpdateInterval = 150
			LightSeqFlasher.Play SeqRandom, 18, , 3900

			BallHandlingQueue.Add "CreateNewBall()","CreateNewBall()",20,4000,0,0,0,false
			ResetForNewPlayerBall
			If LWarpMultiball.state = 1 THEN
				chilloutthemusic
				StartWarpMultiball
			Else
				pupevent 725
				PlaySoundCallOut "fx078"
				ClearMusicCallout
				LWarpMultiballCounter.state = 0
				LightShootAgain.state = 0
				bBallSaverActive = False
				ChangeGiForKeepBombing
				ChangeLights(base)	
				chilloutthemusic
			End If
		Else
			BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1
			If(BallsRemaining(CurrentPlayer) <= 0) Then
				CheckHighScore()
				'EndOfBallComplete()     
			Else
				EndOfBallComplete()
			End If
		End If
	End Sub

	Sub EndOfBallComplete()
		ClearDMDHighScore
		DMDBigText FormatScore(Score(CurrentPlayer)),100,0
		'CreateDMD_intro
		

		ResetNewBallVariables
		Dim NextPlayer
		If(PlayersPlayingGame> 1) Then
			NextPlayer = CurrentPlayer + 1
			If(NextPlayer> PlayersPlayingGame) Then
				NextPlayer = 1
			End If
		Else
			NextPlayer = CurrentPlayer
		End If
		If((BallsRemaining(CurrentPlayer) <= 0) AND(BallsRemaining(NextPlayer) <= 0) ) Then
			EndOfGame()
		Else
			CurrentPlayer = NextPlayer
			AddScore 0
			ResetForNewPlayerBall()
			CreateNewBall()
			ResetAllLightsColor
			ResetAllGILightsColor
			pupevent 720
			LRuby2.state = 0
			Dim y
				For each y in herbs
				y.State = 0
			Next
			If PlayersPlayingGame> 1 Then
				PlaySoundCallOut "fx067"
			End If
		End If
		ResetLWarpMultiballLight       '
		ResetLSupreme08confirm         '
		bWarpSpeedMultiballActive = False
		StopAllMusic

	End Sub

	Sub Balldrained
		'StopAllMusic
	End Sub
	'**************************
	'   DRAIN HIT
	'**************************
	Sub Drain_Hit()
		Drain.DestroyBall
		BallsOnPlayfield = BallsOnPlayfield - 1
		'EndMusic
		'StopAllMusic
		RandomSoundDrainBottom
		RandomSoundDrain drain
		'AudioQueue.Add "RandomRestartMusicSelection","RandomRestartMusicSelection",20,6000,0,0,0,false

		If Tilted Then
			StopEndOfBallMode
		End If

		If bWarpSpeedMultiballActive = True Then
			AddMultiball 1                         
			bAutoPlunger = True                    
		End If
	
		If(bGameInPLay = True) AND(Tilted = False) Then
			If(bBallSaverActive = True) Then
				IF bWarpSpeedMultiballActive = True OR LWarpMultiballCounter.state = 2 Then
				ELSE
				RandomDrainAllCapsFlashBallSave	
				END IF
				AddMultiball 1
				bAutoPlunger = True
				PuPEvent 203
				DMDBigText "BALL SAVED",100,0   '3.4sec 
				DMDTopSplash "BLUNTED AGAIN",100,0
				If bMultiBallMode = False Then
					chilloutthemusic
					AudioQueue.Add "BallSavedCallout","BallSavedCallout",20,2800,0,0,0,false
				End If
				RANDOMLIGHTSDRAINQUICKFADE
			Else
				If(BallsOnPlayfield = 1) Then
					If(bMultiBallMode = True) then
						bMultiBallMode = False
						ChangeGi "white"
					End If
					bMultiBallMode = False		
					ChangeGi "white"
					IF bWarpSpeedMultiballActive = True OR LWarpMultiballCounter.state = 2 THEN
						'DrainAllCapsFlashBallQuick1
					ELSE
						RandomDrainAllCapsFlashBallQuick
					END IF
					RANDOMLIGHTSDRAINQUICKFADE
			End If
				If(BallsOnPlayfield = 0) AND NOT (bMBDrainConfirm = TRUE) Then
					playvideo=100
					bMultiBallMode = False
					ChangeGi "white"
					BallHandlingQueue.Add "Balldrained","Balldrained",20,1000,0,0,0,false
					BallHandlingQueue.Add "EndOfBall","EndOfBall",20,6000,0,0,0,false
					StopEndOfBallMode
					RandomDrainAllCapsFlashBallComplete
					RANDOMLIGHTSDRAINLONGFADE
					If NOT (bBallSaverActive = True) THEN 
						PuPEvent 202
						Dbg "Event 202"
						DMDBigText "DOOM DRAIN",320,1   'blink
						Select Case Int(Rnd * 15) + 1
							Case 1: DMDTopSplash "EASY ON THE SHROOMS",320,0
							Case 2: DMDTopSplash "BAD LIKE A HOOD RAT",320,0
							Case 3: DMDTopSplash "WELCOME TO VIOLENCE",320,0
							Case 4: DMDTopSplash "UGLY AS RON JEREMY",320,0
							Case 5: DMDTopSplash "TOO MUCH SPARKDALA",320,0
							Case 6: DMDTopSplash "SOFA KING WE TODDID",320,0
							Case 7: DMDTopSplash "ONLY ONE BEER LEFT",320,0
							Case 8: DMDTopSplash "RAPP SNITCH KNISHES",320,0
							Case 9: DMDTopSplash "YOUS A FANCY CLOWN",320,0
							Case 10: DMDTopSplash "HOECAKES NOW SERVED",320,0
							Case 11: DMDTopSplash "SPACE HO BASKETCASE",320,0
							Case 12: DMDTopSplash "BORDERLINE SCHIZO",320,0
							Case 13: DMDTopSplash "HOLY BATTY BOYZ",320,0
							Case 14: DMDTopSplash "PEARL NECKLACES",320,0
							Case 15: DMDTopSplash "DOOM ARE YOU AWAKE",320,0
						End Select
					End IF
				End If
				
				If(BallsOnPlayfield = 2) OR (BallsOnPlayfield = 3) OR (BallsOnPlayfield = 4) Then
					IF bWarpSpeedMultiballActive = True OR LWarpMultiballCounter.state = 2 Then
					ELSE
						RandomDrainAllCapsFlashBallQuick
					END IF
					LightSeqLogo.UpdateInterval = 100          
					LightSeqLogo.Play SeqRandom, 12,, 1100 
				End If
			End If
		End If
		
		DOF 935, DOFOff   'DOF MX
		DOF 936, DOFOff   'DOF MX
		DOF 946, DOFPulse   'DOF MX
		bMBDrainConfirm = FALSE
	End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'  BALL SAVE & LAUNCH
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	Sub ballsavestarttrigger_hit
		If(bBallSaverReady = True) AND(15 <> 0) And(bBallSaverActive = False) Then
			EnableBallSaver 15
		End If
	End Sub

	Sub swPlungerRest_Hit()
		PlaySoundAt "fx_sensor", ActiveBall
		bBallInPlungerLane = True

		if bOnTheFirstBallScorbit And ScorbitActive = 1 And (Scorbit.bNeedsPairing) = false then vpmtimer.addtimer 1000, "ScorbitClaimQR(True) '"
		If bAutoPlunger Then
		    autoplungerdelay.interval = 300
		    autoplungerdelay.enabled = True
'			PlungerIM.Strength = 45
'			'PlungerIM.AutoFire
'			PlungerIM.Strength = Plunger.MechStrength
'			Plunger.AutoPlunger = true
'			Plunger.Pullback
'			Plunger.Fire
'			bAutoPlunger = False
'			Plunger.AutoPlunger = false
		End If
		If bSkillShotReady AND LStar01.State = 0 AND LStar02.State = 0 AND LStar03.State = 0 Then
			swPlungerRest.TimerEnabled = 1
			UpdateSkillshot()
		End If
		LastSwitchHit = "swPlungerRest"
	End Sub

	Sub autoplungerdelay_timer
		PlungerIM.Strength = 45
		PlungerIM.AutoFire
		PlungerIM.Strength = Plunger.MechStrength

		bAutoPlunger = False
		autoplungerdelay.enabled = False
	End Sub


	Sub swPlungerRest_UnHit()
		bBallInPlungerLane = False
		swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
		If bSkillShotReady Then
			ResetSkillShotTimer.Enabled = 1
			ScorbitClaimQR(False)
			hideScorbit 'backup call to make sure all scorbit QR codes are gone
		End If

		bOnTheFirstBallScorbit = False
	End Sub

	Sub swPlungerRest_Timer
		swPlungerRest.TimerEnabled = 0
	End Sub
	'**************************
	'   ENABLE BALL SAVE
	'**************************
	Sub EnableBallSaver(seconds)
		bBallSaverActive = True
		bBallSaverReady = False
		BallSaverTimerExpired.Interval = 1000 * seconds
		BallSaverTimerExpired.Enabled = True
		BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
		BallSaverSpeedUpTimer.Enabled = True
		LightShootAgain.BlinkInterval = 160
		LightShootAgain.State = 2
	End Sub

	Sub BallSaverTimerExpired_Timer()
		BallSaverTimerExpired.Enabled = False
		Dim waittime
		waittime = 1000
		BallHandlingQueue.Add "BallSaveGrace","BallSaveGrace",20,waittime,0,0,0,false
		If bExtraBallWonThisBall = True Then
			LightShootAgain.State = 1
		Else
			LightShootAgain.State = 0
		End If
	End Sub

	Sub BallSaveGrace
		bBallSaverActive = False
	End Sub

	Sub BallSaverSpeedUpTimer_Timer()
		BallSaverSpeedUpTimer.Enabled = False
		LightShootAgain.BlinkInterval = 80
		LightShootAgain.State = 2
	End Sub

sub CheckPairing
		Dbg "***************** In check pairing"
		'pBGSetPage(1)
	if (Scorbit.bNeedsPairing) then 
		pupevent 402
		Dbg "Scorbit Needs Pairing"
		PuPlayer.LabelSet pBackglass, "ScorbitQR1", "PuPOverlays\\QRcode.png",1,"{'mt':2,'width':19.61, 'height':36,'xalign':0,'yalign':0,'ypos':32,'xpos':74.6}"
		DelayQRClaim.Interval=6000
		DelayQRClaim.Enabled=True
	end if
End sub

Sub pBgSetPage(pagenum)    
	PuPlayer.LabelShowPage pBackglass,pagenum,0,""   'set page to blank 0 page if want off
end Sub

Sub hideScorbit
	pupevent 800
	PuPlayer.LabelSet pBackglass, "ScorbitQR1", "PuPOverlays\\clear.png",0,""
	PuPlayer.LabelSet pBackglass, "ScorbitQR2", "PuPOverlays\\clear.png",0,""
End Sub

' Locale independent number formatter
Function FormatScore(nScore)
	Dim i
	Dim sScore, sFormattedScore
	sScore = CStr(nScore)
	sFormattedScore = ""
	Dim nDigitCount
	Dim sDigit
	If "" = sScore or "0" = sScore Then
		FormatScore = "00"
		Exit Function
	End If
	nDigitCount = Len(sScore)
	For i = 1 to nDigitCount
		sDigit = Mid(sScore, (1 + (nDigitCount - i)), 1)
		If (i > 1) and (1 = (i mod 3)) Then
			sFormattedScore = sFormattedScore & "," & sDigit
		Else
			sFormattedScore = sFormattedScore & sDigit
		End If
	Next
	FormatScore = StrReverse(sFormattedScore)
End Function

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'  ATTRACT MODE
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	Dim introposition
	introposition = 0

	Sub DMDintroloop
		Dim i
		introtime = 0	
		introposition = introposition + 1
		Select Case introposition
			Case 1
	Dbg "In the DMD Loop"
				PupEvent 736
				'PupEvent 836
				
				' switchmusic attract bg1.mp3
				DMDTopSplash HighScoreName(0),1000,0
				DMDBigText formatscore(HighScore(0)),1000,0
			Case 2
				DMDTopSplash HighScoreName(1),1000,0
				DMDBigText formatscore(HighScore(1)),1000,0
			Case 3
   				DMDTopSplash HighScoreName(2),1000,0
				DMDBigText formatscore(HighScore(2)),1000,0                   
			Case 4
			'	PupEvent 737
			'	PupEvent 837
   				DMDTopSplash HighScoreName(3),1000,0
				DMDBigText formatscore(HighScore(3)),1000,0  
			Case 5
				FlasherAllBlinkOnce
				introposition = 0 
		End Select
	End Sub

	Dim introtime
	introtime = 0

	Sub intromover_timer
		introtime = introtime + 1
		If introposition = 1 Then
			If introtime = 34 Then  
				DMDintroloop
			End If
		End If
		If introposition = 2 Then
			If introtime = 9 Then 
				DMDintroloop
			End If
		End If
		If introposition = 3 Then
			If introtime = 9 Then 
				DMDintroloop
			End If
		End If
		If introposition = 4 Then
			If introtime = 9 Then 
				DMDintroloop
			End If
		End If
		If introposition = 5 Then
			If introtime = 29 Then
				DMDintroloop
				introposition = 0
			End If
		End If
	End Sub

	Sub AttractModeGrandChampion
			RandomSoundHighScores  
			pupevent 726
			FlasherAllBlinkOnce
	End Sub

	Sub AttractModeHighScores
			RandomSoundHighScores 
			pupevent 727
			FlasherAllBlinkOnce
	End Sub

Sub ShowTableInfo
    Dim i
    ' DMDBlink "black.jpg", " ", "EXTRA BALL IS LIT", 50, 20
    'info goes in a loop only stopped by the credits and the startkey
 '       DMDFlush
  '      DMD "black.jpg", "ACT I", "Trial of Strength!", 8000


    If Score(1) Then
        DMD "black.jpg", "PLAYER1", Score(1), 3000
    End If
    If Score(2) Then
        DMD "black.jpg", "PLAYER2", Score(2), 3000
    End If
    If Score(3) Then
        DMD "black.jpg", "PLAYER3", Score(3), 3000
    End If
    If Score(4) Then
        DMD "black.jpg", "PLAYER4", Score(4), 3000
    End If

    'coins or freeplay
    If bFreePlay Then
        DMD "black.jpg", " ", "FREE PLAY", 2000
       ' DMD "intro-freeplay.wmv", "", "", 63000
    Else
        If Credits> 0 Then
            DMD "black.jpg", "CREDITS " &credits, "PRESS START", 2000
        Else
            DMD "black.jpg", "CREDITS " &credits, "INSERT COIN", 2000
        End If
        'DMD "intro-coins.wmv", "", "", 65000
    End If

    DMD "black.jpg", "HIGHSCORES", "1> " & HighScoreName(0) & " " & FormatNumber(HighScore(0), 0, , , -1), 3000
    DMD "black.jpg", "HIGHSCORES", "2> " & HighScoreName(1) & " " & FormatNumber(HighScore(1), 0, , , -1), 3000
    DMD "black.jpg", "HIGHSCORES", "3> " & HighScoreName(2) & " " & FormatNumber(HighScore(2), 0, , , -1), 3000
    DMD "black.jpg", "HIGHSCORES", "4> " & HighScoreName(3) & " " & FormatNumber(HighScore(3), 0, , , -1), 3000
End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'  TABLE VARIABLES
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	' Game Variables
	Dim LaneBonus
	Dim TargetBonus
	Dim RampBonus
	Dim OrbitBonus
	Dim spinvalue
	Dim finalflips
	Dim Saves
	Dim Drains
	Dim inmode
	Dim OldDMDPrio:OldDMDPrio = 0	
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'  GAME STARTING & RESETS
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	Sub Game_Init() 'called at the start of a new game
		Dbg "=========== Starting Game ==========="
		'PuPEvent 200
		Dim i
		For i = 0 to 4
			SkillshotValue(i) = 10000 ' increases by 10000 each time it is collected
		Next
		bExtraBallWonThisBall = False

		'PuPEvent 201
		PuPEvent 301
		ClearMusicCallout

		resetbackglass
		ResetBoltLights
		ResetAllLaneLights
		RandomSoundStartup
		TurnOffGIMultiball
		FlasherAttract
		DrainAllCapsAttract
		LRuby2.state = 0
		FlashForMs LRuby2b, 1500, 50, 0
		FlashForMs LSupreme008, 1500, 50, 0
		FlashForMs LDoubleScoring01, 1500, 50, 0
		FlashForMs LSlimeb, 1500, 50, 0
		DOOMLightsFlash
		ClearDMDHighScore
		'DMDTopSplash "",1,0
		'DMDBigText "",1,0
		GeneralPupQueue.Add "CheckPairing","CheckPairing",20,2500,0,0,0,false
	End Sub

	Sub StopEndOfBallMode()              'called after the last ball is drained
		ResetSkillShotTimer_Timer
	End Sub

	Sub ResetNewBallVariables()
		'StopAllMusic
			Dim a
				For each a in TotalLights
				a.State = 0
			Next
	End Sub

	Sub TurnOffPlayfieldLights()
		Dim a
		For each a in alights
			a.State = 0
		Next
	End Sub

	Sub ResetNewBallLights() 'turn on or off the needed lights before a new ball is released
		TurnOffPlayfieldLights()
	End Sub

	Sub UpdateSkillShot() 'Updates the skillshot light
		Select Case Int(Rnd * 3) + 1
			Case 1:LStar01.State = 2 
			Case 2:LStar02.State = 2 
			Case 3:LStar03.State = 2 
		End Select	
	End Sub

	Sub SkillshotOff_Hit 'trigger to stop the skillshot due to a weak plunger shot
		If bSkillShotReady Then
			ResetSkillShotTimer_Timer
		End If
	End Sub

	Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
		ResetSkillShotTimer.Enabled = 0
		bSkillShotReady = False
			If LStar01.State = 2 then LStar01.State = 0    
			If LStar02.State = 2 then LStar02.State = 0
			If LStar03.State = 2 then LStar03.State = 0
	End Sub

	Sub SkillShotRotateLights_hit
		BallHandlingQueue.Add "ResetSkillShotTimer_Timer","ResetSkillShotTimer_Timer",90,500,0,0,0,false
		bSkillshotRotateLights = TRUE
	End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'  SECONDARY HIT EVENTS
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	Dim RStep, Lstep
	'*************************
	'   SLINGSHOT RIGHT
	'*************************
	Sub RightSlingShot_Slingshot
		RS.VelocityCorrect(ActiveBall)
		'If DMDFistRight=0 Then DMDFistRight=1
		DOF 921, DOFPulse	'DOF MX
		DOF 969, DOFPulse	'DOF MX - BACK
		DOF 986, DOFPulse	'RGB
		IF (BallsOnPlayfield < 3) AND (Tilted = False) THEN
			GiEffect 2 : DMDBumper
		END IF
		If LDoubleScoring01.state = 2 Then
			Addscore 2000
		Else
			Addscore 1000
		End If
		RandomSoundMetalFingersRight
		RandomSoundSlingshotRight RightInlane'RightSlingShot
		PlaySoundAt SoundFXDOF("", 104, DOFPulse, DOFContactors), RightInlane
		PuPEvent 302

		RSling.Visible = 0
		RSling1.Visible = 1
		sling1.TransZ = -20
		RStep = 0
		RightSlingShot.TimerEnabled = 1
		Flash4
	End Sub

	Sub RightSlingShot_Timer
		Select Case RStep
			Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
			Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
		End Select
		RStep = RStep + 1
	End Sub
	'*************************
	'   SLINGSHOT LEFT
	'*************************
	Sub LeftSlingShot_Slingshot
		LS.VelocityCorrect(ActiveBall)
		'If DMDFistLeft=0 Then DMDFistLeft=1
		DOF 920, DOFPulse	'DOF MX
		DOF 969, DOFPulse	'DOF MX - BACK
		DOF 982, DOFPulse	'RGB
		IF (BallsOnPlayfield < 3) AND (Tilted = False) THEN
			GiEffect 2 : DMDBumper
		END IF
		If LDoubleScoring01.state = 2 Then
			Addscore 2000
		Else
			Addscore 1000
		End If
		RandomSoundMetalFingersLeft
		RandomSoundSlingshotLeft LeftInlane'LeftSlingShot
		PlaySoundAt SoundFXDOF("", 103, DOFPulse, DOFContactors), LeftInlane
		PuPEvent 302

		LSling.Visible = 0
		LSling1.Visible = 1
		sling2.TransZ = -20
		LStep = 0
		LeftSlingShot.TimerEnabled = 1
		Flash3
	End Sub

	Sub LeftSlingShot_Timer
		Select Case LStep
			Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
			Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
		End Select
		LStep = LStep + 1
	End Sub

	Sub RotateSkillLightsRight
		Dim TempState
		TempState = LStar03.State
		LStar03.State = LStar02.State
		LStar02.State = LStar01.State
		LStar01.State = TempState
	End Sub

	Sub RotateSkillLightsLeft
		Dim TempState
		TempState = LStar01.State
		LStar01.State = LStar02.State
		LStar02.State = LStar03.State
		LStar03.State = TempState
	End Sub

	Sub RotateLaneLightsRight
		Dim TempState
		TempState = LStar07.State
		LStar07.State = LStar06.State
		LStar06.State = LStar05.State
		LStar05.State = LStar04.State
		LStar04.State = TempState
	End Sub

	Sub RotateLaneLightsLeft
		Dim TempState
		TempState = LStar04.State
		LStar04.State = LStar05.State
		LStar05.State = LStar06.State
		LStar06.State = LStar07.State
		LStar07.State = TempState
	End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'  BUMPERS
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	'*************************
	'   BUMPER (BOTTOM)
	'*************************
	Sub Bumper1_Hit
		If NOT Tilted Then
			RandomFlasherBumperBottom
			RandomSoundBumper
			RandomSoundBumperBottom bumper1	
			PlaySoundAt SoundFXDOF("", 107, DOFPulse, DOFContactors), ActiveBall
			DOF 917, DOFPulse   'DOF MX - Bumper 1
			DOF 953, DOFPulse   'DOF MX - BACK Bumper 1
			IF LDoubleScoring01.State = 2 THEN
				AddScore 2000 
			ELSE
				AddScore 1000 		
			END IF	
		
			IF (BallsOnPlayfield < 2) AND (Tilted = False) THEN
				BumperScore
				LightSeqbumper.UpdateInterval = 1
				LightSeqbumper.Play SeqBlinking, , 2, 10
				DMDBumper
				If SmokeAnimation = 1 THEN
					BongTimer.enabled = 1
					Bong.Visible = 1
					BongAnimation 0, 20
				End If
			End If	
		Else	
			Exit Sub
		End If
		
		BallHandlingQueue.Add "SuperPopsCheck","SuperPopsCheck",20,2000,0,0,0,false
		If LSuperPops.state = 2 Then
			LightBonus03
		End If
		SmokeSpinBumper
		Flash19
		BonusBumperCount
		dank8Shaker : dank9Shaker
	End Sub
	'*************************
	'   BUMPER (TOP LEFT)
	'*************************
	Sub Bumper2_Hit
		If NOT Tilted Then
			RandomFlasherBumperTopLeft
			RandomSoundBumper
			RandomSoundBumperTop bumper2
			PlaySoundAt SoundFXDOF("", 109, DOFPulse, DOFContactors), ActiveBall
			DOF 918, DOFPulse   'DOF MX - Bumper 2
			DOF 954, DOFPulse   'DOF MX - BACK Bumper 2
			IF LDoubleScoring01.State = 2 THEN
				AddScore 2000 
			ELSE
				AddScore 1000 		
			END IF	

			IF (BallsOnPlayfield < 2) AND (Tilted = False) THEN
				BumperScore
				'LightSeqPlastics.UpdateInterval = 1
				'LightSeqPlastics.Play SeqBlinking, , 2, 10
				FlashForMs Light003, 250, 50, 0
				DMDBumper
			End If
		Else
			Exit Sub
		End If

		BallHandlingQueue.Add "SuperPopsCheck","SuperPopsCheck",20,2000,0,0,0,false
		If LSuperPops.state = 2 Then
			LightBonus03
		End If
		Flash19
		BonusBumperCount

		dim bumper2speed
		bumper2speed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
        If bumper2speed > 20 then                
		RotDisc8Step = 15 :	Disc8Timer.Enabled = 1
        End if
        If bumper2speed <= 20 then
		RotDisc8Step = 7 :	Disc8Timer.Enabled = 1
        End If
	End Sub
	'*************************
	'   BUMPER (TOP RIGHT)
	'*************************
	Sub Bumper3_Hit
		If NOT Tilted Then
			RandomFlasherBumperTopRight
			RandomSoundBumper	
			RandomSoundBumperMiddle bumper3
			PlaySoundAt SoundFXDOF("", 108, DOFPulse, DOFContactors), ActiveBall
			DOF 919, DOFPulse   'DOF MX - Bumper 3
			DOF 955, DOFPulse   'DOF MX - BACK Bumper 3
			IF LDoubleScoring01.State = 2 THEN
				AddScore 2000 
			ELSE
				AddScore 1000 		
			END IF
			
			IF (BallsOnPlayfield < 2) AND (Tilted = False) THEN
				BumperScore
				'LightSeqbumper.UpdateInterval = 1
				'LightSeqbumper.Play SeqBlinking, , 2, 10
				DMDBumper
			End If
		Else
			Exit Sub
		End If

		BallHandlingQueue.Add "SuperPopsCheck","SuperPopsCheck",20,2000,0,0,0,false
		If LSuperPops.state = 2 Then
			LightBonus03
		End If
		Flash19
		BonusBumperCount
		dank1Shaker : dank2Shaker :	dank3Shaker
	End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'  FLIPPER LANES
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	'*****************************************
	'   LEFT INLANE
	'*****************************************
	Sub LeftInlane_Hit
		If bMultiBallMode = False Then
			RandomSoundSwitch
			DOF 937, DOFPulse   'DOF MX - Left Inner Lane
		End If

		If Tilted Then Exit Sub
		LaneBonus = LaneBonus + 1

		IF LStar04.State = 0 AND LStar05.State = 0 Then
			LStar04.State = 1
			LStar05.State = 2
		Else
			IF LStar04.State = 2 AND LStar05.State = 2 Then
				LStar04.State = 1
				LStar05.State = 2
			ELSE 
				IF LStar04.State = 1 AND LStar05.State = 2 Then
					LStar04.State = 1
					LStar05.State = 1
				ELSE 
					IF LStar04.State = 1 AND LStar05.State = 1 Then
						LStar04.State = 1
						LStar05.State = 1
					END IF
				END IF
			END IF
		END IF

		IF LDoubleScoring01.State = 2 THEN
			AddScore 500
		ELSE
			AddScore 250 
		END IF

		LastSwitchHit = "lane1"

		If (BallsOnPlayfield < 2) AND (Tilted = False) THEN
			RANDOMLIGHTSINLANESLEFTFADE
			UnblinkText
		End If
		
		BonusMultiplierCheck

		If LWarpMultiballCounter.state = 2 Then
			RandomSpecialBlink
		End If
		FlashBluntLeft
	End Sub
	'*****************************************
	'   RIGHT INLANE
	'*****************************************
	Sub RightInlane_Hit
		If bMultiBallMode = False Then
			RandomSoundSwitch
			DOF 938, DOFPulse   'DOF MX - Right Inner Lane
		End If

		If Tilted Then Exit Sub
		LaneBonus = LaneBonus + 1

		IF LStar07.State = 0 AND LStar06.State = 0 Then
			LStar07.State = 1
			LStar06.State = 2
		Else
			IF LStar07.State = 2 AND LStar06.State = 2 Then
				LStar07.State = 1
				LStar06.State = 2
			ELSE 
				IF LStar07.State = 1 AND LStar06.State = 2 Then
					LStar07.State = 1
					LStar06.State = 1
				ELSE 
					IF LStar07.State = 1 AND LStar06.State = 1 Then
						LStar07.State = 1
						LStar06.State = 1
					END IF
				END IF
			END IF
		END IF

		IF LDoubleScoring01.State = 2 THEN
			AddScore 500 
		ELSE
			AddScore 250	
		END IF

		LastSwitchHit = "lane1"

		If (BallsOnPlayfield < 2) AND (Tilted = False) THEN
			RANDOMLIGHTSINLANESRIGHTFADE
			UnblinkText
		End If

		BonusMultiplierCheck
		If LWarpMultiballCounter.state = 2 Then
			RandomSpecialBlink
		End If
		FlashBluntRight
	End Sub
	'*****************************************
	'   LEFT OUTLANE
	'*****************************************
	Sub LeftInlane2_Hit
		LastSwitchHit = "lane1"

		If (BallsOnPlayfield < 3) AND (Tilted = False) THEN
			RANDOMLIGHTSOUTLANESLEFTFADE
			ChangeGiForDrainLeft
			FlasherDrainLeft
			UnblinkText
			DMDSpiralL=250 : DMDSpiralLBG=250
		End If

		LightBonus01
		If NOT (bBallSaverActive = True) THEN 
			PuPEvent 202
			Dbg "Event 202"
		End If
		If bMultiBallMode = False Then
			RandomSoundOutlane
			DOF 935, DOFOn   'DOF MX - Left Outer Lane
		End If

		If Tilted Then Exit Sub
		LaneBonus = LaneBonus + 1
		'(insert light name).State = 1

		IF LDoubleScoring01.State = 2 THEN
			AddScore 500 
			LightBonus02
		ELSE
			AddScore 250
			LightBonus01
		END IF
		DOF 996, DOFPulse  'RGB
	End Sub
	'*****************************************
	'   RIGHT OUTLANE
	'*****************************************
	Sub RightInlane2_Hit
		LastSwitchHit = "lane1"

		If (BallsOnPlayfield < 3) AND (Tilted = False) THEN
			RANDOMLIGHTSOUTLANESRIGHTFADE
			ChangeGiForDrainRight
			FlasherDrainRight
			UnblinkText
			DMDSpiral=250 : DMDSpiralBG=250
		End If

		LightBonus01
		If NOT (bBallSaverActive = True) THEN
			PuPEvent 202
		End If
		If bMultiBallMode = False Then
			RandomSoundOutlane
			DOF 936, DOFOn   'DOF MX - Right Outer Lane
		End If

		If Tilted Then Exit Sub
		LaneBonus = LaneBonus + 1
		'(insert light name).State = 1

		IF LDoubleScoring01.State = 2 THEN
			AddScore 500 
			LightBonus02
		ELSE
			AddScore 250
			LightBonus02
		END IF
		DOF 997, DOFPulse  'RGB		
	End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'  FLASHERS
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	'****************************
	' Flashers - Thanks Flupper
	'****************************	
	Dim FlashLevel17,FlashLevel18,FlashLevel19

	'*** flasher 17: mask small ***
	Sub Flasherflash17_Timer()
		dim flashx17, matdim
		If not Flasherflash17.TimerEnabled Then 
			Flasherflash17.TimerEnabled = True
			Flasherflash17.visible = 1
			FlasherLaser.visible = 1  
		End If
		flashx17 = FlashLevel17 * FlashLevel17 * FlashLevel17
		Flasherflash17.opacity = 1500 * flashx17
		FlasherLaser.opacity = 1500 * flashx17  
		matdim = Round(10 * FlashLevel17)
		FlashLevel17 = FlashLevel17 * 0.9 - 0.01
		If FlashLevel17 < 0 Then
			Flasherflash17.TimerEnabled = False
			Flasherflash17.visible = 0
			FlasherLaser.visible = 0  
		End If
	End Sub
	'*** flasher 18: mask big ***
	Sub Flasherflash18_Timer()
		dim flashx18, matdim
		If not Flasherflash18.TimerEnabled Then 
			Flasherflash18.TimerEnabled = True
			Flasherflash18.visible = 1
			FlasherLaser2.visible = 1  
			FlasherLaser3.visible = 1 	
			FlasherLaser4.visible = 1 

			If VRROOM = 1 Then
				Flasherflash20.visible = 1
			End If

		End If
		flashx18 = FlashLevel18 * FlashLevel18 * FlashLevel18
		Flasherflash18.opacity = 1500 * flashx18
		FlasherLaser2.opacity = 1500 * flashx18  
		FlasherLaser3.opacity = 1500 * flashx18  
		FlasherLaser4.opacity = 1500 * flashx18 

 		If VRROOM = 1 Then
			Flasherflash20.opacity = 1500 * flashx18
		End If

		matdim = Round(10 * FlashLevel18)
		FlashLevel18 = FlashLevel18 * 0.9 - 0.01
		If FlashLevel18 < 0 Then
			Flasherflash18.TimerEnabled = False
			Flasherflash18.visible = 0
			FlasherLaser2.visible = 0 
			FlasherLaser3.visible = 0 
			FlasherLaser4.visible = 0  

			If VRROOM = 1 Then
				Flasherflash20.visible = 0
			End If

		End If
	End Sub
	'*** flasher 19: weed ***
	Sub Flasherflash19_Timer()
		dim flashx19, matdim
		If not Flasherflash19.TimerEnabled Then 
			Flasherflash19.TimerEnabled = True
			Flasherflash19.visible = 1
		End If
		flashx19 = FlashLevel19 * FlashLevel19 * FlashLevel19
		Flasherflash19.opacity = 1500 * flashx19
		matdim = Round(10 * FlashLevel19)
		FlashLevel19 = FlashLevel19 * 0.9 - 0.01
		If FlashLevel19 < 0 Then
			Flasherflash19.TimerEnabled = False
			Flasherflash19.visible = 0
		End If
	End Sub
	Sub Flash17
		FlashLevel17 = 1 : Flasherflash17_Timer	
		FlashForMs LRuby2b, 250, 250, 0
	End Sub
	Sub Flash18
		FlashLevel18 = 1 : Flasherflash18_Timer	
	End Sub
	Sub Flash19
		FlashLevel19 = 1 : Flasherflash19_Timer	
		FlashForMs LWeed, 250, 250, 2
	End Sub
	Sub FlashMasks
		Flash17
		Flash18
		FlashForMs LRuby3, 150, 150, 0
	End Sub

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness
								' *********************************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 						***
Set TableRef = Table1   		' *** change this, if your table has another name       						***
FlasherLightIntensity = 0.24	' *** lower this, if the VPX lights are too bright (i.e. 0.1, default 1)		***
FlasherFlareIntensity = 0.24   	' *** lower this, if the flares are too bright (i.e. 0.1, default 1)			***
FlasherBloomIntensity = 0.20	' *** lower this, if the blooms are too bright (i.e. 0.1)						***	
FlasherOffBrightness = 0.45		' *** brightness of the flasher dome when switched off (range 0-2, default 0.5)	***
								' *********************************************************************************
Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objlight(20), objbloom(20)
'Dim tablewidth, tableheight : tablewidth = Table1.width : tableheight = Table1.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
InitFlasher 1, "yellow" : 
InitFlasher 2, "yellow" : 
InitFlasher 3, "purple" : 
InitFlasher 4, "purple" : 
InitFlasher 5, "green" : 
InitFlasher 6, "green" : 
InitFlasher 7, "purple" : 
InitFlasher 8, "yellow" : 
InitFlasher 9, "yellow" : 
InitFlasher 10, "white" : 
InitFlasher 11, "white" : 
InitFlasher 12, "purple" : 
InitFlasher 13, "white" : 
InitFlasher 14, "purple" : 
InitFlasher 15, "green" : 
InitFlasher 16, "white" : 
'InitFlasher 17, "red" : 
'InitFlasher 18, "red" :
'InitFlasher 19, "green" : 

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
	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If Table1.ShowDT and ObjFlasher(nr).RotX = -45 Then 
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
Sub FlasherFlash9_Timer() : FlashFlasher(9) : End Sub
Sub FlasherFlash10_Timer() : FlashFlasher(10) : End Sub
Sub FlasherFlash11_Timer() : FlashFlasher(11) : End Sub
Sub FlasherFlash12_Timer() : FlashFlasher(12) : End Sub
Sub FlasherFlash13_Timer() : FlashFlasher(13) : End Sub
Sub FlasherFlash14_Timer() : FlashFlasher(14) : End Sub
Sub FlasherFlash15_Timer() : FlashFlasher(15) : End Sub
Sub FlasherFlash16_Timer() : FlashFlasher(16) : End Sub
	'*************************************************
	'	FLASHER TIMED/DELAYED RUNS
	'*************************************************
	Sub Flash1
		Objlevel(1) = 1 : FlasherFlash1_Timer
		DOF 901, DOFPulse  'DOF MX
		DOF 980, DOFPulse  'RGB 
	End Sub
	Sub Flash2
		Objlevel(2) = 1 : FlasherFlash2_Timer	
		DOF 902, DOFPulse  
		DOF 981, DOFPulse  'RGB 
	End Sub
	Sub Flash3
		Objlevel(3) = 1 : FlasherFlash3_Timer
		DOF 903, DOFPulse  
		DOF 982, DOFPulse  'RGB 	
	End Sub
	Sub Flash4
		Objlevel(4) = 1 : FlasherFlash4_Timer	
		DOF 904, DOFPulse
		DOF 983, DOFPulse  'RGB 
	End Sub
	Sub Flash5
		Objlevel(5) = 1 : FlasherFlash5_Timer
		DOF 905, DOFPulse	
		DOF 984, DOFPulse  'RGB 
	End Sub
	Sub Flash6
		Objlevel(6) = 1 : FlasherFlash6_Timer	
		DOF 906, DOFPulse
		DOF 985, DOFPulse  'RGB 
	End Sub
	Sub Flash7
		Objlevel(7) = 1 : FlasherFlash7_Timer
		DOF 907, DOFPulse	
		DOF 986, DOFPulse  'RGB 
	End Sub
	Sub Flash8
		Objlevel(8) = 1 : FlasherFlash8_Timer
		DOF 908, DOFPulse	
		DOF 987, DOFPulse  'RGB 
	End Sub
	Sub Flash9
		Objlevel(9) = 1 : FlasherFlash9_Timer	
		DOF 909, DOFPulse
		DOF 988, DOFPulse  'RGB 
	End Sub
	Sub Flash10
		Objlevel(10) = 1 : FlasherFlash10_Timer	
		DOF 910, DOFPulse
		DOF 989, DOFPulse  'RGB 
	End Sub
	Sub Flash11
		Objlevel(11) = 1 : FlasherFlash11_Timer	
		DOF 911, DOFPulse
		DOF 990, DOFPulse  'RGB 
	End Sub
	Sub Flash12
		Objlevel(12) = 1 : FlasherFlash12_Timer	
		DOF 912, DOFPulse
		DOF 991, DOFPulse  'RGB 
	End Sub
	Sub Flash13
		Objlevel(13) = 1 : FlasherFlash13_Timer	
		DOF 913, DOFPulse
		DOF 992, DOFPulse  'RGB 
	End Sub
	Sub Flash14
		Objlevel(14) = 1 : FlasherFlash14_Timer	
		DOF 914, DOFPulse
		DOF 993, DOFPulse  'RGB 
	End Sub
	Sub Flash15
		Objlevel(15) = 1 : FlasherFlash15_Timer	
		DOF 915, DOFPulse
		DOF 994, DOFPulse  'RGB 
	End Sub
	Sub Flash16
		Objlevel(16) = 1 : FlasherFlash16_Timer	
		DOF 916, DOFPulse 
		DOF 995, DOFPulse  'RGB
	End Sub

	Sub FlasherWhiteBlinkOnce
		Flash13
		Flash16
		Flash10
		Flash11
	End Sub
	Sub FlasherPurpleBlinkOnce
		Flash14
		Flash12
		Flash7
		Flash3
		Flash4
	End Sub
	Sub FlasherOnlyGreenBlinkOnce
		Flash5
		Flash6
		Flash15
	End Sub
	Sub FlasherGreenBlinkOnce
		Flash3
		Flash4
		Flash5
		Flash6
		Flash15
	End Sub
	Sub FlasherOnlyYellowBlinkOnce
		Flash8
		Flash9
		Flash1
		Flash2
	End Sub
	Sub FlasherYellowBlinkOnce
		Flash8
		Flash9
		Flash1
		Flash2
		Flash14
		Flash12
		Flash7
	End Sub
	Sub FlasherAllBlinkOnce
		Flash1
		Flash2
		Flash3
		Flash4
		'Flash5
		'Flash6
		Flash7
		Flash8
		Flash9
		Flash10
		Flash11
		Flash12
		Flash13
		Flash14
		'Flash15
		Flash16
		Flash17
		Flash18
		Flash19
		FlashForMs LRuby3, 150, 150, 0
	End Sub

	Sub FlasherRightRamp
		RandomSoundFairyDust
		LightQueue.Add "Flash12","Flash12",10,50,0,0,0,false
		LightQueue.Add "Flash11","Flash11",10,100,0,0,0,false
		LightQueue.Add "Flash9","Flash9",10,150,0,0,0,false
		LightQueue.Add "Flash4","Flash4",10,200,0,0,0,false
		LightQueue.Add "Flash3","Flash3",10,250,0,0,0,false
		LightQueue.Add "Flash8","Flash8",10,300,0,0,0,false
		LightQueue.Add "Flash10","Flash10",10,350,0,0,0,false
		LightQueue.Add "Flash6","Flash6",10,400,0,0,0,false
		LightQueue.Add "Flash1","Flash1",10,450,0,0,0,false
		LightQueue.Add "Flash13","Flash13",10,500,0,0,0,false
		LightQueue.Add "Flash5","Flash5",10,550,0,0,0,false
		LightQueue.Add "Flash14","Flash14",10,600,0,0,0,false
		LightQueue.Add "Flash15","Flash15",10,650,0,0,0,false
		LightQueue.Add "Flash7","Flash7",10,700,0,0,0,false
		LightQueue.Add "Flash16","Flash16",10,750,0,0,0,false
		LightQueue.Add "Flash2","Flash2",10,800,0,0,0,false
		LightQueue.Add "Flash12","Flash12",10,850,0,0,0,false
		LightQueue.Add "Flash11","Flash11",10,900,0,0,0,false
		LightQueue.Add "Flash9","Flash9",10,950,0,0,0,false
		LightQueue.Add "Flash4","Flash4",10,1000,0,0,0,false
		LightQueue.Add "Flash3","Flash3",10,1050,0,0,0,false
		LightQueue.Add "Flash8","Flash8",10,1100,0,0,0,false
		LightQueue.Add "Flash10","Flash10",10,1150,0,0,0,false
		LightQueue.Add "Flash6","Flash6",10,1200,0,0,0,false
		LightQueue.Add "Flash1","Flash1",10,1250,0,0,0,false
		LightQueue.Add "Flash13","Flash13",10,1300,0,0,0,false
		LightQueue.Add "Flash5","Flash5",10,1350,0,0,0,false
		LightQueue.Add "Flash14","Flash14",10,1400,0,0,0,false
		LightQueue.Add "Flash14","Flash14",10,1450,0,0,0,false
		LightQueue.Add "Flash15","Flash15",10,1500,0,0,0,false
		LightQueue.Add "Flash7","Flash7",10,1550,0,0,0,false
		LightQueue.Add "Flash16","Flash16",10,1600,0,0,0,false
		LightQueue.Add "Flash12","Flash12",10,1650,0,0,0,false
		LightQueue.Add "Flash11","Flash11",10,1700,0,0,0,false
		LightQueue.Add "Flash9","Flash9",10,1750,0,0,0,false
		LightQueue.Add "Flash4","Flash4",10,1800,0,0,0,false
		LightQueue.Add "Flash3","Flash3",10,1850,0,0,0,false
		LightQueue.Add "Flash8","Flash8",10,1900,0,0,0,false
		LightQueue.Add "Flash10","Flash10",10,1950,0,0,0,false
		LightQueue.Add "Flash6","Flash6",10,2000,0,0,0,false
		LightQueue.Add "Flash1","Flash1",10,2050,0,0,0,false
		LightQueue.Add "Flash13","Flash13",10,2100,0,0,0,false
		LightQueue.Add "Flash5","Flash5",10,2150,0,0,0,false
		LightQueue.Add "Flash14","Flash14",10,2200,0,0,0,false
		LightQueue.Add "Flash15","Flash15",10,2250,0,0,0,false
		LightQueue.Add "Flash7","Flash7",10,2300,0,0,0,false
		LightQueue.Add "Flash16","Flash16",10,2350,0,0,0,false
		LightQueue.Add "Flash2","Flash2",10,2400,0,0,0,false
	End Sub

	Sub FlasherLeftRamp
		RandomSoundFairyDust
		LightQueue.Add "Flash12","Flash12",10,50,0,0,0,false
		LightQueue.Add "Flash6","Flash6",10,100,0,0,0,false
		LightQueue.Add "Flash10","Flash10",10,150,0,0,0,false
		LightQueue.Add "Flash8","Flash8",10,200,0,0,0,false
		LightQueue.Add "Flash3","Flash3",10,250,0,0,0,false
		LightQueue.Add "Flash4","Flash4",10,300,0,0,0,false

		LightQueue.Add "Flash9","Flash9",10,350,0,0,0,false
		LightQueue.Add "Flash11","Flash11",10,400,0,0,0,false
		LightQueue.Add "Flash2","Flash2",10,450,0,0,0,false
		LightQueue.Add "Flash16","Flash16",10,500,0,0,0,false
		LightQueue.Add "Flash7","Flash7",10,550,0,0,0,false

		LightQueue.Add "Flash15","Flash15",10,600,0,0,0,false
		LightQueue.Add "Flash14","Flash14",10,650,0,0,0,false
		LightQueue.Add "Flash5","Flash5",10,700,0,0,0,false
		LightQueue.Add "Flash13","Flash13",10,750,0,0,0,false
		LightQueue.Add "Flash1","Flash1",10,800,0,0,0,false

		LightQueue.Add "Flash12","Flash12",10,850,0,0,0,false
		LightQueue.Add "Flash6","Flash6",10,900,0,0,0,false
		LightQueue.Add "Flash10","Flash10",10,950,0,0,0,false
		LightQueue.Add "Flash8","Flash8",10,1000,0,0,0,false
		LightQueue.Add "Flash3","Flash3",10,1050,0,0,0,false

		LightQueue.Add "Flash4","Flash4",10,1100,0,0,0,false
		LightQueue.Add "Flash9","Flash9",10,1150,0,0,0,false
		LightQueue.Add "Flash11","Flash11",10,1200,0,0,0,false
		LightQueue.Add "Flash2","Flash2",10,1250,0,0,0,false
		LightQueue.Add "Flash16","Flash16",10,1300,0,0,0,false

		LightQueue.Add "Flash7","Flash7",10,1350,0,0,0,false
		LightQueue.Add "Flash15","Flash15",10,1400,0,0,0,false
		LightQueue.Add "Flash14","Flash14",10,1450,0,0,0,false
		LightQueue.Add "Flash5","Flash5",10,1500,0,0,0,false
		LightQueue.Add "Flash13","Flash13",10,1550,0,0,0,false
		LightQueue.Add "Flash1","Flash1",10,1600,0,0,0,false

		LightQueue.Add "Flash1","Flash1",10,1650,0,0,0,false
		LightQueue.Add "Flash12","Flash12",10,1700,0,0,0,false
		LightQueue.Add "Flash6","Flash6",10,1750,0,0,0,false
		LightQueue.Add "Flash10","Flash10",10,1800,0,0,0,false
		LightQueue.Add "Flash8","Flash8",10,1850,0,0,0,false

		LightQueue.Add "Flash3","Flash3",10,1900,0,0,0,false
		LightQueue.Add "Flash4","Flash4",10,1950,0,0,0,false
		LightQueue.Add "Flash9","Flash9",10,2000,0,0,0,false
		LightQueue.Add "Flash11","Flash11",10,2050,0,0,0,false
		LightQueue.Add "Flash2","Flash2",10,2100,0,0,0,false

		LightQueue.Add "Flash16","Flash16",10,2150,0,0,0,false
		LightQueue.Add "Flash7","Flash7",10,2200,0,0,0,false
		LightQueue.Add "Flash15","Flash15",10,2250,0,0,0,false
		LightQueue.Add "Flash14","Flash14",10,2300,0,0,0,false
		LightQueue.Add "Flash16","Flash16",10,2350,0,0,0,false
		LightQueue.Add "Flash2","Flash2",10,2400,0,0,0,false
	End Sub

	Sub FlasherPlunger
		LightQueue.Add "Flash9","Flash9",10,50,0,0,0,false
		LightQueue.Add "Flash11","Flash11",10,100,0,0,0,false
		LightQueue.Add "Flash2","Flash2",10,150,0,0,0,false
		LightQueue.Add "Flash16","Flash16",10,200,0,0,0,false
		LightQueue.Add "Flash7","Flash7",10,250,0,0,0,false
		LightQueue.Add "Flash15","Flash15",10,300,0,0,0,false

		LightQueue.Add "Flash14","Flash14",10,350,0,0,0,false
		LightQueue.Add "Flash5","Flash5",10,400,0,0,0,false
		LightQueue.Add "Flash13","Flash13",10,450,0,0,0,false
		LightQueue.Add "Flash1","Flash1",10,500,0,0,0,false
		LightQueue.Add "Flash12","Flash12",10,550,0,0,0,false

		LightQueue.Add "Flash6","Flash6",10,600,0,0,0,false
		LightQueue.Add "Flash10","Flash10",10,650,0,0,0,false
		LightQueue.Add "Flash8","Flash8",10,700,0,0,0,false
		LightQueue.Add "Flash3","Flash3",10,750,0,0,0,false
		LightQueue.Add "Flash4","Flash4",10,800,0,0,0,false

		LightQueue.Add "Flash9","Flash9",10,850,0,0,0,false
		LightQueue.Add "Flash11","Flash11",10,900,0,0,0,false
		LightQueue.Add "Flash2","Flash2",10,950,0,0,0,false
		LightQueue.Add "Flash16","Flash16",10,1000,0,0,0,false
		LightQueue.Add "Flash7","Flash7",10,1050,0,0,0,false

		LightQueue.Add "Flash4","Flash4",10,1100,0,0,0,false
		LightQueue.Add "Flash9","Flash9",10,1150,0,0,0,false
		LightQueue.Add "Flash11","Flash11",10,1200,0,0,0,false
		LightQueue.Add "Flash2","Flash2",10,1250,0,0,0,false
		LightQueue.Add "Flash16","Flash16",10,1300,0,0,0,false

		LightQueue.Add "Flash5","Flash15",10,1350,0,0,0,false
		LightQueue.Add "Flash14","Flash14",10,1400,0,0,0,false
		LightQueue.Add "Flash14","Flash5",10,1450,0,0,0,false
		LightQueue.Add "Flash15","Flas13",10,1500,0,0,0,false
		LightQueue.Add "Flash7","Flash1",10,1550,0,0,0,false
		LightQueue.Add "Flash16","Flash4",10,1600,0,0,0,false

		LightQueue.Add "Flash9","Flash9",10,1650,0,0,0,false
		LightQueue.Add "Flash11","Flash11",10,1700,0,0,0,false
		LightQueue.Add "Flash2","Flash2",10,1750,0,0,0,false
		LightQueue.Add "Flash16","Flash16",10,1800,0,0,0,false
		LightQueue.Add "Flash7","Flash7",10,1850,0,0,0,false

		LightQueue.Add "Flash15","Flash15",10,1900,0,0,0,false
		LightQueue.Add "Flash14","Flash14",10,1950,0,0,0,false
		LightQueue.Add "Flash5","Flash5",10,2000,0,0,0,false
		LightQueue.Add "Flash13","Flash13",10,2050,0,0,0,false
		LightQueue.Add "Flash1","Flash1",10,2100,0,0,0,false

		LightQueue.Add "Flash12","Flash12",10,2150,0,0,0,false
		LightQueue.Add "Flash6","Flash6",10,2200,0,0,0,false
		LightQueue.Add "Flash10","Flash10",10,2250,0,0,0,false
		LightQueue.Add "Flash8","Flash8",10,2300,0,0,0,false
		LightQueue.Add "Flash3","Flash3",10,2350,0,0,0,false
		LightQueue.Add "Flash4","Flash4",10,2400,0,0,0,false
	End Sub

	Sub FlasherTopToBottom
		LightQueue.Add "Flash13","Flash13",10,50,0,0,0,false
		LightQueue.Add "Flash16","Flash16",10,50,0,0,0,false
		LightQueue.Add "Flash1","Flash1",10,200,0,0,0,false
		LightQueue.Add "Flash2","Flash2",10,200,0,0,0,false
		LightQueue.Add "Flash10","Flash10",10,350,0,0,0,false
		LightQueue.Add "Flash11","Flash11",10,350,0,0,0,false

		LightQueue.Add "Flash8","Flash8",10,500,0,0,0,false
		LightQueue.Add "Flash9","Flash9",10,500,0,0,0,false
		LightQueue.Add "Flash3","Flash3",10,650,0,0,0,false
		LightQueue.Add "Flash4","Flash4",10,650,0,0,0,false
		LightQueue.Add "Flash13","Flash13",10,800,0,0,0,false

		LightQueue.Add "Flash16","Flash16",10,800,0,0,0,false
		LightQueue.Add "Flash1","Flash1",10,950,0,0,0,false
		LightQueue.Add "Flash2","Flash2",10,950,0,0,0,false
		LightQueue.Add "Flash10","Flash10",10,1100,0,0,0,false
		LightQueue.Add "Flash11","Flash11",10,1100,0,0,0,false

		LightQueue.Add "Flash8","Flash8",10,1250,0,0,0,false
		LightQueue.Add "Flash9","Flash9",10,1250,0,0,0,false
		LightQueue.Add "Flash3","Flash3",10,1400,0,0,0,false
		LightQueue.Add "Flash4","Flash4",10,1400,0,0,0,false
		LightQueue.Add "Flash13","Flash13",10,1550,0,0,0,false

		LightQueue.Add "Flash16","Flash16",10,1550,0,0,0,false
		LightQueue.Add "Flash1","Flash1",10,1700,0,0,0,false
		LightQueue.Add "Flash2","Flash2",10,1700,0,0,0,false
		LightQueue.Add "Flash10","Flash10",10,1850,0,0,0,false
		LightQueue.Add "Flash11","Flash11",10,1850,0,0,0,false

		LightQueue.Add "Flash8","Flash8",10,2000,0,0,0,false
		LightQueue.Add "Flash9","Flash9",10,2000,0,0,0,false
		LightQueue.Add "Flash3","Flash3",10,2150,0,0,0,false
		LightQueue.Add "Flash4","Flash4",10,2150,0,0,0,false
	End Sub

	Sub FlasherTopToBottomShortLeft
		LightQueue.Add "Objlevel(13) = 1 : Flasherflash13_Timer","Objlevel(13) = 1 : Flasherflash13_Timer",10,50,0,0,0,false
		LightQueue.Add "Objlevel(1) = 1 : Flasherflash1_Timer","Objlevel(1) = 1 : Flasherflash1_Timer",10,100,0,0,0,false
		LightQueue.Add "Objlevel(10) = 1 : Flasherflash10_Timer","Objlevel(10) = 1 : Flasherflash10_Timer",10,150,0,0,0,false
		LightQueue.Add "Objlevel(8) = 1 : Flasherflash8_Timer","Objlevel(8) = 1 : Flasherflash8_Timer",10,200,0,0,0,false
		LightQueue.Add "Objlevel(3) = 1 : Flasherflash3_Timer","Objlevel(3) = 1 : Flasherflash3_Timer",10,250,0,0,0,false
		LightQueue.Add "Objlevel(13) = 1 : Flasherflash13_Timer","Objlevel(13) = 1 : Flasherflash13_Timer",10,300,0,0,0,false
		LightQueue.Add "Objlevel(1) = 1 : Flasherflash1_Timer","Objlevel(1) = 1 : Flasherflash1_Timer",10,350,0,0,0,false
		LightQueue.Add "Objlevel(10) = 1 : Flasherflash10_Timer","Objlevel(10) = 1 : Flasherflash10_Timer",10,400,0,0,0,false
		LightQueue.Add "Objlevel(8) = 1 : Flasherflash8_Timer","Objlevel(8) = 1 : Flasherflash8_Timer",10,450,0,0,0,false
		LightQueue.Add "Objlevel(3) = 1 : Flasherflash3_Timer","Objlevel(3) = 1 : Flasherflash3_Timer",10,500,0,0,0,false

	End Sub

	Sub FlasherTopToBottomShortRight
		LightQueue.Add "Objlevel(16) = 1 : Flasherflash16_Timer","Objlevel(16) = 1 : Flasherflash16_Timer",10,50,0,0,0,false
		LightQueue.Add "Objlevel(2) = 1 : Flasherflash2_Timer","Objlevel(2) = 1 : Flasherflash2_Timer",10,100,0,0,0,false
		LightQueue.Add "Objlevel(11) = 1 : Flasherflash11_Timer","Objlevel(11) = 1 : Flasherflash11_Timer",10,150,0,0,0,false
		LightQueue.Add "Objlevel(9) = 1 : Flasherflash9_Timer","Objlevel(9) = 1 : Flasherflash9_Timer",10,200,0,0,0,false
		LightQueue.Add "Objlevel(4) = 1 : Flasherflash4_Timer","Objlevel(4) = 1 : Flasherflash4_Timer",10,250,0,0,0,false
		LightQueue.Add "Objlevel(16) = 1 : Flasherflash16_Timer","Objlevel(16) = 1 : Flasherflash16_Timer",10,300,0,0,0,false
		LightQueue.Add "Objlevel(2) = 1 : Flasherflash2_Timer","Objlevel(2) = 1 : Flasherflash2_Timer",10,350,0,0,0,false
		LightQueue.Add "Objlevel(11) = 1 : Flasherflash11_Timer","Objlevel(11) = 1 : Flasherflash11_Timer",10,400,0,0,0,false
		LightQueue.Add "Objlevel(9) = 1 : Flasherflash9_Timer","Objlevel(9) = 1 : Flasherflash9_Timer",10,450,0,0,0,false
		LightQueue.Add "Objlevel(4) = 1 : Flasherflash4_Timer","Objlevel(4) = 1 : Flasherflash4_Timer",10,500,0,0,0,false
	End Sub

	Sub FlasherBottomToTop
		LightQueue.Add "Flash4","Flash4",10,50,0,0,0,false
		LightQueue.Add "Flash3","Flash3",10,50,0,0,0,false
		LightQueue.Add "Flash9","Flash9",10,200,0,0,0,false
		LightQueue.Add "Flash8","Flash8",10,200,0,0,0,false
		LightQueue.Add "Flash11","Flash11",10,350,0,0,0,false
		LightQueue.Add "Flash10","Flash10",10,350,0,0,0,false

		LightQueue.Add "Flash2","Flash2",10,500,0,0,0,false
		LightQueue.Add "Flash1","Flash1",10,500,0,0,0,false
		LightQueue.Add "Flash16","Flash16",10,650,0,0,0,false
		LightQueue.Add "Flash13","Flash13",10,650,0,0,0,false
		LightQueue.Add "Flash4","Flash4",10,800,0,0,0,false

		LightQueue.Add "Flash3","Flash3",10,800,0,0,0,false
		LightQueue.Add "Flash9","Flash9",10,950,0,0,0,false
		LightQueue.Add "Flash8","Flash8",10,950,0,0,0,false
		LightQueue.Add "Flash11","Flash11",10,1100,0,0,0,false
		LightQueue.Add "Flash10","Flash10",10,1100,0,0,0,false

		LightQueue.Add "Flash2","Flash2",10,1250,0,0,0,false
		LightQueue.Add "Flash1","Flash1",10,1250,0,0,0,false
		LightQueue.Add "Flash16","Flash16",10,1400,0,0,0,false
		LightQueue.Add "Flash13","Flash13",10,1400,0,0,0,false
		LightQueue.Add "Flash4","Flash4",10,1550,0,0,0,false

		LightQueue.Add "Flash3","Flash3",10,1550,0,0,0,false
		LightQueue.Add "Flash9","Flash9",10,1700,0,0,0,false
		LightQueue.Add "Flash8","Flash8",10,1700,0,0,0,false
		LightQueue.Add "Flash11","Flash11",10,1850,0,0,0,false
		LightQueue.Add "Flash10","Flash10",10,1850,0,0,0,false

		LightQueue.Add "Flash2","Flash2",10,2000,0,0,0,false
		LightQueue.Add "Flash1","Flash1",10,2000,0,0,0,false
		LightQueue.Add "Flash16","Flash16",10,2150,0,0,0,false
		LightQueue.Add "Flash13","Flash13",10,2150,0,0,0,false
	End Sub

	Sub FlasherBottomToTopShortRight
		LightQueue.Add "Objlevel(4) = 1 : Flasherflash4_Timer","Objlevel(4) = 1 : Flasherflash4_Timer",10,50,0,0,0,false
		LightQueue.Add "Objlevel(9) = 1 : Flasherflash9_Timer","Objlevel(9) = 1 : Flasherflash9_Timer",10,100,0,0,0,false
		LightQueue.Add "Objlevel(11) = 1 : Flasherflash11_Timer","Objlevel(11) = 1 : Flasherflash11_Timer",10,150,0,0,0,false
		LightQueue.Add "Objlevel(2) = 1 : Flasherflash2_Timer","Objlevel(2) = 1 : Flasherflash2_Timer",10,200,0,0,0,false
		LightQueue.Add "Objlevel(16) = 1 : Flasherflash16_Timer","Objlevel(16) = 1 : Flasherflash16_Timer",10,250,0,0,0,false
		LightQueue.Add "Objlevel(4) = 1 : Flasherflash4_Timer","Objlevel(4) = 1 : Flasherflash4_Timer",10,300,0,0,0,false
		LightQueue.Add "Objlevel(9) = 1 : Flasherflash9_Timer","Objlevel(9) = 1 : Flasherflash9_Timer",10,350,0,0,0,false
		LightQueue.Add "Objlevel(11) = 1 : Flasherflash11_Timer","Objlevel(11) = 1 : Flasherflash11_Timer",10,400,0,0,0,false
		LightQueue.Add "Objlevel(2) = 1 : Flasherflash2_Timer","Objlevel(2) = 1 : Flasherflash2_Timer",10,450,0,0,0,false
		LightQueue.Add "Objlevel(16) = 1 : Flasherflash16_Timer","Objlevel(16) = 1 : Flasherflash16_Timer",10,500,0,0,0,false
	End Sub

	Sub FlasherBottomToTopShortLeft
		LightQueue.Add "Objlevel(3) = 1 : Flasherflash3_Timer","Objlevel(3) = 1 : Flasherflash3_Timer",10,50,0,0,0,false
		LightQueue.Add "Objlevel(8) = 1 : Flasherflash8_Timer","Objlevel(8) = 1 : Flasherflash8_Timer",10,100,0,0,0,false
		LightQueue.Add "Objlevel(10) = 1 : Flasherflash10_Timer","Objlevel(10) = 1 : Flasherflash10_Timer",10,150,0,0,0,false
		LightQueue.Add "Objlevel(1) = 1 : Flasherflash1_Timer","Objlevel(1) = 1 : Flasherflash1_Timer",10,200,0,0,0,false
		LightQueue.Add "Objlevel(13) = 1 : Flasherflash13_Timer","Objlevel(13) = 1 : Flasherflash13_Timer",10,250,0,0,0,false
		LightQueue.Add "Objlevel(3) = 1 : Flasherflash3_Timer","Objlevel(3) = 1 : Flasherflash3_Timer",10,300,0,0,0,false
		LightQueue.Add "Objlevel(8) = 1 : Flasherflash8_Timer","Objlevel(8) = 1 : Flasherflash8_Timer",10,350,0,0,0,false
		LightQueue.Add "Objlevel(10) = 1 : Flasherflash10_Timer","Objlevel(10) = 1 : Flasherflash10_Timer",10,400,0,0,0,false
		LightQueue.Add "Objlevel(1) = 1 : Flasherflash1_Timer","Objlevel(1) = 1 : Flasherflash1_Timer",10,450,0,0,0,false
		LightQueue.Add "Objlevel(13) = 1 : Flasherflash13_Timer","Objlevel(13) = 1 : Flasherflash13_Timer",10,500,0,0,0,false
	End Sub

	Sub FlasherDrainLeft
		LightQueue.Add "Objlevel(1) = 1 : Flasherflash1_Timer","Objlevel(11) = 1 : Flasherflash11_Timer",10,50,0,0,0,false
		LightQueue.Add "Objlevel(10) = 1 : Flasherflash10_Timer","Objlevel(10) = 1 : Flasherflash10_Timer",10,200,0,0,0,false
		LightQueue.Add "Objlevel(8) = 1 : Flasherflash8_Timer","Objlevel(8) = 1 : Flasherflash8_Timer",10,350,0,0,0,false
		LightQueue.Add "Objlevel(3) = 1 : Flasherflash3_Timer","Objlevel(3) = 1 : Flasherflash3_Timer",10,500,0,0,0,false
		LightQueue.Add "Objlevel(1) = 1 : Flasherflash1_Timer","Objlevel(1) = 1 : Flasherflash1_Timer",10,650,0,0,0,false
		LightQueue.Add "Objlevel(10) = 1 : Flasherflash10_Timer","Objlevel(10) = 1 : Flasherflash10_Timer",10,800,0,0,0,false
		LightQueue.Add "Objlevel(8) = 1 : Flasherflash8_Timer","Objlevel(8) = 1 : Flasherflash8_Timer",10,950,0,0,0,false
		LightQueue.Add "Objlevel(3) = 1 : Flasherflash3_Timer","Objlevel(3) = 1 : Flasherflash3_Timer",10,1100,0,0,0,false
		LightQueue.Add "Objlevel(1) = 1 : Flasherflash1_Timer","Objlevel(1) = 1 : Flasherflash1_Timer",10,1250,0,0,0,false
		LightQueue.Add "Objlevel(10) = 1 : Flasherflash10_Timer","Objlevel(10) = 1 : Flasherflash10_Timer",10,1400,0,0,0,false
		LightQueue.Add "Objlevel(8) = 1 : Flasherflash8_Timer","Objlevel(8) = 1 : Flasherflash8_Timer",10,1550,0,0,0,false
		LightQueue.Add "Objlevel(3) = 1 : Flasherflash3_Timer","Objlevel(3) = 1 : Flasherflash3_Timer",10,1700,0,0,0,false
	End Sub

	Sub FlasherDrainRight
		LightQueue.Add "Objlevel(2) = 1 : Flasherflash2_Timer","Objlevel(2) = 1 : Flasherflash2_Timer",10,50,0,0,0,false
		LightQueue.Add "Objlevel(11) = 1 : Flasherflash11_Timer","Objlevel(11) = 1 : Flasherflash11_Timer",10,200,0,0,0,false
		LightQueue.Add "Objlevel(9) = 1 : Flasherflash9_Timer","Objlevel(9) = 1 : Flasherflash9_Timer",10,350,0,0,0,false
		LightQueue.Add "Objlevel(4) = 1 : Flasherflash4_Timer","Objlevel(4) = 1 : Flasherflash4_Timer",10,500,0,0,0,false
		LightQueue.Add "Objlevel(2) = 1 : Flasherflash2_Timer","Objlevel(2) = 1 : Flasherflash2_Timer",10,650,0,0,0,false
		LightQueue.Add "Objlevel(11) = 1 : Flasherflash11_Timer","Objlevel(11) = 1 : Flasherflash11_Timer",10,800,0,0,0,false
		LightQueue.Add "Objlevel(9) = 1 : Flasherflash9_Timer","Objlevel(9) = 1 : Flasherflash9_Timer",10,950,0,0,0,false
		LightQueue.Add "Objlevel(4) = 1 : Flasherflash4_Timer","Objlevel(4) = 1 : Flasherflash4_Timer",10,1100,0,0,0,false
		LightQueue.Add "Objlevel(2) = 1 : Flasherflash2_Timer","Objlevel(2) = 1 : Flasherflash2_Timer",10,1250,0,0,0,false
		LightQueue.Add "Objlevel(11) = 1 : Flasherflash11_Timer","Objlevel(11) = 1 : Flasherflash11_Timer",10,1400,0,0,0,false
		LightQueue.Add "Objlevel(9) = 1 : Flasherflash9_Timer","Objlevel(9) = 1 : Flasherflash9_Timer",10,1550,0,0,0,false
		LightQueue.Add "Objlevel(4) = 1 : Flasherflash4_Timer","Objlevel(4) = 1 : Flasherflash4_Timer",10,1700,0,0,0,false
	End Sub

	Sub FlasherDrainMask
		LightQueue.Add "Objlevel(2) = 1 : Flasherflash2_Timer","Objlevel(2) = 1 : Flasherflash2_Timer",10,50,0,0,0,false
		LightQueue.Add "Objlevel(11) = 1 : Flasherflash11_Timer","Objlevel(11) = 1 : Flasherflash11_Timer",10,200,0,0,0,false
		LightQueue.Add "Objlevel(9) = 1 : Flasherflash9_Timer","Objlevel(9) = 1 : Flasherflash9_Timer",10,350,0,0,0,false
		LightQueue.Add "Objlevel(4) = 1 : Flasherflash4_Timer","Objlevel(4) = 1 : Flasherflash4_Timer",10,500,0,0,0,false
		LightQueue.Add "Objlevel(2) = 1 : Flasherflash2_Timer","Objlevel(2) = 1 : Flasherflash2_Timer",10,650,0,0,0,false
		LightQueue.Add "Objlevel(11) = 1 : Flasherflash11_Timer","Objlevel(11) = 1 : Flasherflash11_Timer",10,800,0,0,0,false
		LightQueue.Add "Objlevel(9) = 1 : Flasherflash9_Timer","Objlevel(9) = 1 : Flasherflash9_Timer",10,950,0,0,0,false
		LightQueue.Add "Objlevel(4) = 1 : Flasherflash4_Timer","Objlevel(4) = 1 : Flasherflash4_Timer",10,1100,0,0,0,false
		LightQueue.Add "Objlevel(2) = 1 : Flasherflash2_Timer","Objlevel(2) = 1 : Flasherflash2_Timer",10,1250,0,0,0,false
		LightQueue.Add "Objlevel(11) = 1 : Flasherflash11_Timer","Objlevel(11) = 1 : Flasherflash11_Timer",10,1400,0,0,0,false
		LightQueue.Add "Objlevel(9) = 1 : Flasherflash9_Timer","Objlevel(9) = 1 : Flasherflash9_Timer",10,1550,0,0,0,false
		LightQueue.Add "Objlevel(4) = 1 : Flasherflash4_Timer","Objlevel(4) = 1 : Flasherflash4_Timer",10,1700,0,0,0,false
	End Sub

	Sub FlasherColorCycle
		LightQueue.Add "FlasherWhiteBlinkOnce","FlasherWhiteBlinkOnce",10,50,0,0,0,false
		LightQueue.Add "FlasherWhiteBlinkOnce","FlasherWhiteBlinkOnce",10,650,0,0,0,false
		LightQueue.Add "FlasherWhiteBlinkOnce","FlasherWhiteBlinkOnce",10,1250,0,0,0,false
		LightQueue.Add "FlasherWhiteBlinkOnce","FlasherWhiteBlinkOnce",10,1850,0,0,0,false

		LightQueue.Add "FlasherYellowBlinkOnce","FlasherYellowBlinkOnce",10,250,0,0,0,false
		LightQueue.Add "FlasherYellowBlinkOnce","FlasherYellowBlinkOnce",10,8500,0,0,0,false
		LightQueue.Add "FlasherYellowBlinkOnce","FlasherYellowBlinkOnce",10,1450,0,0,0,false
		LightQueue.Add "FlasherYellowBlinkOnce","FlasherYellowBlinkOnce",10,2050,0,0,0,false

		LightQueue.Add "FlasherGreenBlinkOnce","FlasherGreenBlinkOnce",10,1450,0,0,0,false
		LightQueue.Add "FlasherGreenBlinkOnce","FlasherGreenBlinkOnce",10,1050,0,0,0,false
		LightQueue.Add "FlasherGreenBlinkOnce","FlasherGreenBlinkOnce",10,1650,0,0,0,false
	End Sub

	Sub FlasherAttract
		LightQueue.Add "FlasherWhiteBlinkOnce","FlasherWhiteBlinkOnce",10,50,0,0,0,false
		LightQueue.Add "FlasherWhiteBlinkOnce","FlasherWhiteBlinkOnce",10,650,0,0,0,false
		LightQueue.Add "FlasherWhiteBlinkOnce","FlasherWhiteBlinkOnce",10,1250,0,0,0,false

		LightQueue.Add "FlasherYellowBlinkOnce","FlasherYellowBlinkOnce",10,250,0,0,0,false
		LightQueue.Add "FlasherYellowBlinkOnce","FlasherYellowBlinkOnce",10,8500,0,0,0,false
		LightQueue.Add "FlasherYellowBlinkOnce","FlasherYellowBlinkOnce",10,1450,0,0,0,false

		LightQueue.Add "FlasherGreenBlinkOnce","FlasherGreenBlinkOnce",10,1450,0,0,0,false
		LightQueue.Add "FlasherGreenBlinkOnce","FlasherGreenBlinkOnce",10,1050,0,0,0,false

	End Sub

	Sub FlasherBallLocked
		LightQueue.Add "FlasherWhiteBlinkOnce","FlasherWhiteBlinkOnce",10,50,0,0,0,false
		LightQueue.Add "FlasherYellowBlinkOnce","FlasherYellowBlinkOnce",10,250,0,0,0,false
		LightQueue.Add "FlasherGreenBlinkOnce","FlasherGreenBlinkOnce",10,450,0,0,0,false
		LightQueue.Add "FlasherWhiteBlinkOnce","FlasherWhiteBlinkOnce",10,650,0,0,0,false
		LightQueue.Add "FlasherYellowBlinkOnce","FlasherYellowBlinkOnce",10,850,0,0,0,false
		LightQueue.Add "FlasherGreenBlinkOnce","FlasherGreenBlinkOnce",10,1050,0,0,0,false
		LightQueue.Add "FlasherWhiteBlinkOnce","FlasherWhiteBlinkOnce",10,1250,0,0,0,false
		LightQueue.Add "FlasherYellowBlinkOnce","FlasherYellowBlinkOnce",10,1450,0,0,0,false
		LightQueue.Add "FlasherGreenBlinkOnce","FlasherGreenBlinkOnce",10,1650,0,0,0,false
		LightQueue.Add "FlasherWhiteBlinkOnce","FlasherWhiteBlinkOnce",10,1850,0,0,0,false
		LightQueue.Add "FlasherYellowBlinkOnce","FlasherYellowBlinkOnce",10,2050,0,0,0,false
		LightQueue.Add "FlasherGreenBlinkOnce","FlasherGreenBlinkOnce",10,2250,0,0,0,false
		LightQueue.Add "FlasherWhiteBlinkOnce","FlasherWhiteBlinkOnce",10,2450,0,0,0,false
		LightQueue.Add "FlasherYellowBlinkOnce","FlasherYellowBlinkOnce",10,2650,0,0,0,false
		LightQueue.Add "FlasherGreenBlinkOnce","FlasherGreenBlinkOnce",10,2850,0,0,0,false
		LightQueue.Add "FlasherWhiteBlinkOnce","FlasherWhiteBlinkOnce",10,3050,0,0,0,false
		LightQueue.Add "FlasherYellowBlinkOnce","FlasherYellowBlinkOnce",10,3250,0,0,0,false
		LightQueue.Add "FlasherGreenBlinkOnce","FlasherGreenBlinkOnce",10,3450,0,0,0,false
		LightQueue.Add "FlasherWhiteBlinkOnce","FlasherWhiteBlinkOnce",10,3650,0,0,0,false
		LightQueue.Add "FlasherYellowBlinkOnce","FlasherYellowBlinkOnce",10,3850,0,0,0,false
		LightQueue.Add "FlasherGreenBlinkOnce","FlasherGreenBlinkOnce",10,4050,0,0,0,false
		LightQueue.Add "FlasherWhiteBlinkOnce","FlasherWhiteBlinkOnce",10,4250,0,0,0,false
		LightQueue.Add "FlasherYellowBlinkOnce","FlasherYellowBlinkOnce",10,4450,0,0,0,false
		LightQueue.Add "FlasherGreenBlinkOnce","FlasherGreenBlinkOnce",10,4650,0,0,0,false
		LightQueue.Add "FlasherWhiteBlinkOnce","FlasherWhiteBlinkOnce",10,4850,0,0,0,false
		LightQueue.Add "FlasherYellowBlinkOnce","FlasherYellowBlinkOnce",10,5050,0,0,0,false
		LightQueue.Add "FlasherGreenBlinkOnce","FlasherGreenBlinkOnce",10,5250,0,0,0,false
		LightQueue.Add "FlasherWhiteBlinkOnce","FlasherWhiteBlinkOnce",10,5450,0,0,0,false
		LightQueue.Add "FlasherYellowBlinkOnce","FlasherYellowBlinkOnce",10,5650,0,0,0,false
		LightQueue.Add "FlasherGreenBlinkOnce","FlasherGreenBlinkOnce",10,5850,0,0,0,false
		LightQueue.Add "FlasherWhiteBlinkOnce","FlasherWhiteBlinkOnce",10,6050,0,0,0,false
		LightQueue.Add "FlasherYellowBlinkOnce","FlasherYellowBlinkOnce",10,6250,0,0,0,false
		LightQueue.Add "FlasherGreenBlinkOnce","FlasherGreenBlinkOnce",10,6450,0,0,0,false
		LightQueue.Add "FlasherWhiteBlinkOnce","FlasherWhiteBlinkOnce",10,6650,0,0,0,false
		LightQueue.Add "FlasherYellowBlinkOnce","FlasherYellowBlinkOnce",10,6850,0,0,0,false
	End Sub

	Sub FlasherMaskQuick
		FlashMasks	 
		LightQueue.Add "FlashMasks","FlashMasks",10,500,0,0,0,false
		LightQueue.Add "Flash19","Flash19",10,750,0,0,0,false
		LightQueue.Add "FlashMasks","FlashMasks",10,1000,0,0,0,false
		LightQueue.Add "Flash19","Flash19",10,1250,0,0,0,false
		LightQueue.Add "FlashMasks","FlashMasks",10,1500,0,0,0,false
	End Sub

	Sub FlasherMaskLong
		FlashMasks	 
		LightQueue.Add "Flash19","Flash19",10,250,0,0,0,false
		LightQueue.Add "FlashMasks","FlashMasks",10,500,0,0,0,false
		LightQueue.Add "Flash19","Flash19",10,750,0,0,0,false
		LightQueue.Add "FlashMasks","FlashMasks",10,1000,0,0,0,false
		LightQueue.Add "Flash19","Flash19",10,1250,0,0,0,false
		LightQueue.Add "FlashMasks","FlashMasks",10,1500,0,0,0,false
		LightQueue.Add "Flash19","Flash19",10,1750,0,0,0,false
		LightQueue.Add "FlashMasks","FlashMasks",10,2000,0,0,0,false
		LightQueue.Add "Flash19","Flash19",10,2250,0,0,0,false
		LightQueue.Add "FlashMasks","FlashMasks",10,2500,0,0,0,false
		LightQueue.Add "Flash19","Flash19",10,2750,0,0,0,false
		LightQueue.Add "FlashMasks","FlashMasks",10,3000,0,0,0,false
		LightQueue.Add "Flash19","Flash19",10,3250,0,0,0,false
		LightQueue.Add "FlashMasks","FlashMasks",10,3500,0,0,0,false
		LightQueue.Add "Flash19","Flash19",10,3750,0,0,0,false
		LightQueue.Add "FlashMasks","FlashMasks",10,4000,0,0,0,false
		LightQueue.Add "Flash19","Flash19",10,4250,0,0,0,false
		LightQueue.Add "FlashMasks","FlashMasks",10,4500,0,0,0,false
		LightQueue.Add "Flash19","Flash19",10,4750,0,0,0,false
		LightQueue.Add "FlashMasks","FlashMasks",10,5000,0,0,0,false
		LightQueue.Add "Flash19","Flash19",10,5250,0,0,0,false
		LightQueue.Add "FlashMasks","FlashMasks",10,5500,0,0,0,false
		LightQueue.Add "Flash19","Flash19",10,5750,0,0,0,false
		LightQueue.Add "FlashMasks","FlashMasks",10,6000,0,0,0,false
		LightQueue.Add "Flash19","Flash19",10,6250,0,0,0,false
		LightQueue.Add "FlashMasks","FlashMasks",10,6500,0,0,0,false
	End Sub
	'*************************************************
	'	RANDOM FLASHER HITS
	'*************************************************
		Dim bumpertopleft
		bumpertopleft = 1		
	Sub RandomFlasherBumperTopLeft()
		bumpertopleft = bumpertopleft + 1
		Select Case bumpertopleft
			Case 1:Flash5
			Case 2:Flash14
	              bumpertopleft = 0
		End Select
	End Sub

		Dim bumpertopright
		bumpertopright = 1	
	Sub RandomFlasherBumperTopRight()
		bumpertopright = bumpertopright + 1
		Select Case bumpertopright		
			Case 1:Flash7
			Case 2:Flash15
	              bumpertopright = 0
		End Select
	End Sub

		Dim bumperbottom
		bumperbottom = 1	
	Sub RandomFlasherBumperBottom()
		bumperbottom = bumperbottom + 1
		Select Case bumperbottom
			Case 1:Flash6
			Case 2:Flash12
	               bumperbottom = 0
		End Select
	End Sub

		Dim FlasherCenterDropTargets
		FlasherCenterDropTargets = 1
	Sub RandomFlasherCenterDropTargets()
		FlasherCenterDropTargets = FlasherCenterDropTargets + 1
		Select Case FlasherCenterDropTargets
			Case 1:FlasherWhiteBlinkOnce
			Case 2:FlasherOnlyGreenBlinkOnce '
			Case 3:FlasherOnlyYellowBlinkOnce '
			Case 4:FlasherPurpleBlinkOnce
	               FlasherCenterDropTargets = 0
		End Select
	End Sub

		Dim FlasherTrails
		FlasherTrails = 1
	Sub RandomFlasherTrails()
		FlasherTrails = FlasherTrails + 1
		Select Case FlasherTrails		
			Case 1:FlasherTopToBottom
			Case 2:FlasherBottomToTop
	               FlasherTrails = 0
		End Select
	End Sub

		Dim FlasherSideTargetsLeft
		FlasherSideTargetsLeft = 1
	Sub RandomFlasherSideTargetsLeft()
		FlasherSideTargetsLeft = FlasherSideTargetsLeft + 1
		Select Case FlasherSideTargetsLeft		
			Case 1:FlasherTopToBottomShortLeft
			Case 2:FlasherBottomToTopShortLeft
	               FlasherSideTargetsLeft = 0
		End Select
	End Sub

		Dim FlasherSideTargetsRight
		FlasherSideTargetsRight = 1
	Sub RandomFlasherSideTargetsRight()
		FlasherSideTargetsRight = FlasherSideTargetsRight + 1
		Select Case FlasherSideTargetsRight		
			Case 1:FlasherTopToBottomShortRight
			Case 2:FlasherBottomToTopShortRight
	               FlasherSideTargetsRight = 0
		End Select
	End Sub

		Dim FlasherSpinner
		FlasherSpinner = 1
	Sub RandomFlasherSpinner()
		FlasherSpinner = FlasherSpinner + 1
		Select Case FlasherSpinner			
			Case 1:FlasherRightRamp
			Case 2:FlasherLeftRamp
	               FlasherSpinner = 0
		End Select
	End Sub
	'*************************************************
	'	RANDOM LIGHT RUNS
	'*************************************************
	Sub DOOMLightsFlash
		FlashForMs LDOOM01, 1500, 50, 0
		FlashForMs LDOOM02, 1500, 50, 0
		FlashForMs LDOOM03, 1500, 50, 0
		FlashForMs LDOOM04, 1500, 50, 0
		FlashForMs LDOOM005, 1500, 50, 0
		FlashForMs LDOOM006, 1500, 50, 0
	End Sub

	Sub PLightsFlashC
		FlashForMs LPlastic001v2, 350, 50, 0
		FlashForMs LPlastic002v2, 350, 50, 0
		FlashForMs LPlastic003v2, 350, 50, 0
		FlashForMs LPlastic004v2, 350, 50, 0
		FlashForMs LPlastic005v2, 350, 50, 0
		FlashForMs LPlastic006v2, 350, 50, 0
		FlashForMs LPlastic007v2, 350, 50, 0
		FlashForMs LPlastic008v2, 350, 50, 0
		FlashForMs LPlastic009v2, 350, 50, 0
		FlashForMs LPlastic010v2, 350, 50, 0
		FlashForMs LPlastic011v2, 350, 50, 0
		FlashForMs LPlastic012v2, 350, 50, 0
		FlashForMs Light048v2, 350, 50, 0
		FlashForMs Light049v2, 350, 50, 0
	End Sub

	Sub PLightsFlashL
		FlashForMs LPlastic013, 350, 50, 0
		FlashForMs LPlastic014, 350, 50, 0
		FlashForMs LPlastic015, 350, 50, 0
		FlashForMs LPlastic016, 350, 50, 0
	End Sub

	Sub PLightsFlashR
		FlashForMs LPlastic017, 350, 50, 0
		FlashForMs LPlastic018, 350, 50, 0
		FlashForMs LPlastic019, 350, 50, 0
		FlashForMs LPlastic020, 350, 50, 0
	End Sub

	Sub PLightsFlashRampL
		FlashForMs Light016, 1000, 50, 0
		FlashForMs Light017, 1000, 50, 0
		FlashForMs Light018, 1000, 50, 0
		FlashForMs Light019, 1000, 50, 0
		FlashForMs Light020, 1000, 50, 0
		FlashForMs Light021, 1000, 50, 0
		FlashForMs Light022, 1000, 50, 0
		FlashForMs Light023, 1000, 50, 0
		FlashForMs Light042, 1000, 50, 0
		FlashForMs Light059, 1000, 50, 0
	End Sub

	Sub PLightsFlashRampR
		FlashForMs Light026, 1000, 50, 0
		FlashForMs Light027, 1000, 50, 0
		FlashForMs Light028, 1000, 50, 0
		'FlashForMs Light043, 1000, 50, 0   
		FlashForMs Light044, 1000, 50, 0
		FlashForMs Light061, 1000, 50, 0
	End Sub
	Sub PLightsFlashLaser
		FlashForMs LPlastic021, 1500, 50, 0
	End Sub

	Sub DrainAllCapsAttract
		Dim bulb
		For each bulb in ALLCAPS
			FlashForMs bulb, 1500, 50, 0
		Next
	End Sub

	Sub DrainAllCapsFlashBallQuick1
		Dim bulb
		For each bulb in ALLCAPS
			FlashForMs bulb, 1000, 150, 0
		Next
	End Sub
	Sub DrainAllCapsFlashBallQuick2
		LightSeqALLCAPS.UpdateInterval = 100
		LightSeqALLCAPS.Play SeqRandom, 15, , 1000
	End Sub

	Sub DrainAllCapsFlashBallSave1
		Dim bulb
		For each bulb in ALLCAPS
			FlashForMs bulb, 1675, 65, 0
		Next
	End Sub
	Sub DrainAllCapsFlashBallSave2
		LightSeqALLCAPS.UpdateInterval = 100
		LightSeqALLCAPS.Play SeqRandom, 15, , 1650
	End Sub
	Sub DrainAllCapsFlashBallComplete1
		Dim bulb
		For each bulb in ALLCAPS
			FlashForMs bulb, 6050, 150, 0
		Next
	End Sub
	Sub DrainAllCapsFlashBallComplete2
		LightSeqALLCAPS.UpdateInterval = 100
		LightSeqALLCAPS.Play SeqRandom, 15, , 6000
	End Sub

		Dim DrainAllCapsFlashBallQuick
		DrainAllCapsFlashBallQuick = 1
	Sub RandomDrainAllCapsFlashBallQuick()
		DrainAllCapsFlashBallQuick = DrainAllCapsFlashBallQuick + 1
		Select Case DrainAllCapsFlashBallQuick		
			Case 1: DrainAllCapsFlashBallQuick1
			Case 2: DrainAllCapsFlashBallQuick2
	                DrainAllCapsFlashBallQuick = 0
		End Select
	End Sub

		Dim DrainAllCapsFlashBallSave
		DrainAllCapsFlashBallSave = 1
	Sub RandomDrainAllCapsFlashBallSave()
		DrainAllCapsFlashBallSave = DrainAllCapsFlashBallSave + 1
		Select Case DrainAllCapsFlashBallSave			
			Case 1: DrainAllCapsFlashBallSave1
			Case 2: DrainAllCapsFlashBallSave2
	                DrainAllCapsFlashBallSave = 0
		End Select
	End Sub

		Dim DrainAllCapsFlashBallComplete
		DrainAllCapsFlashBallComplete = 1
	Sub RandomDrainAllCapsFlashBallComplete()
		DrainAllCapsFlashBallComplete = DrainAllCapsFlashBallComplete + 1
		Select Case DrainAllCapsFlashBallComplete		
			Case 1: DrainAllCapsFlashBallComplete1
			Case 2: DrainAllCapsFlashBallComplete2
	                DrainAllCapsFlashBallComplete = 0
		End Select
	End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'  nFozzy Physics
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'******************************************************
'	ZPHY:  GNEREAL ADVICE ON PHYSICS
'******************************************************
'
' It's advised that flipper corrections, dampeners, and general physics settings should all be updated per these
' examples as all of these improvements work together to provide a realistic physics simulation.
'
' Tutorial videos provided by Bord
' Adding nFozzy roth physics : pt1 rubber dampeners 				https://youtu.be/AXX3aen06FM?si=Xqd-rcaqTlgEd_wx
' Adding nFozzy roth physics : pt2 flipper physics 					https://youtu.be/VSBFuK2RCPE?si=i8ne8Ao2co8rt7fy
' Adding nFozzy roth physics : pt3 other elements 					https://youtu.be/JN8HEJapCvs?si=hvgMOk-ej1BEYjJv
'
' Note: BallMass must be set to 1. BallSize should be set to 50 (in other words the ball radius is 25)
'
' Recommended Table Physics Settings
' | Gravity Constant             | 0.97      |
' | Playfield Friction           | 0.15-0.25 |
' | Playfield Elasticity         | 0.25      |
' | Playfield Elasticity Falloff | 0         |
' | Playfield Scatter            | 0         |
' | Default Element Scatter      | 2         |
'
' Bumpers
' | Force         | 12-15    |
' | Hit Threshold | 1.6-2    |
' | Scatter Angle | 2        |
'
' Slingshots
' | Hit Threshold      | 2    |
' | Slingshot Force    | 3-5  |
' | Slingshot Theshold | 2-3  |
' | Elasticity         | 0.85 |
' | Friction           | 0.8  |
' | Scatter Angle      | 1    |





'******************************************************
'	ZNFF:  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level we'll need the following:
'	1. flippers with specific physics settings
'	2. custom triggers for each flipper (TriggerLF, TriggerRF)
'	3. and, special scripting
'
' TriggerLF and RF should now be 27 vp units from the flippers. In addition, 3 degrees should be added to the end angle
' when creating these triggers.
'
' RF.ReProcessBalls Activeball and LF.ReProcessBalls Activeball must be added the flipper_collide subs.
'
' A common mistake is incorrect flipper length.  A 3-inch flipper with rubbers will be about 3.125 inches long.
' This translates to about 147 vp units.  Therefore, the flipper start radius + the flipper length + the flipper end
' radius should  equal approximately 147 vp units. Another common mistake is is that sometimes the right flipper
' angle was set with a large postive value (like 238 or something). It should be using negative value (like -122).
'
' The following settings are a solid starting point for various eras of pinballs.
' |                    | EM's           | late 70's to mid 80's | mid 80's to early 90's | mid 90's and later |
' | ------------------ | -------------- | --------------------- | ---------------------- | ------------------ |
' | Mass               | 1              | 1                     | 1                      | 1                  |
' | Strength           | 500-1000 (750) | 1400-1600 (1500)      | 2000-2600              | 3200-3300 (3250)   |
' | Elasticity         | 0.88           | 0.88                  | 0.88                   | 0.88               |
' | Elasticity Falloff | 0.15           | 0.15                  | 0.15                   | 0.15               |
' | Fricition          | 0.8-0.9        | 0.9                   | 0.9                    | 0.9                |
' | Return Strength    | 0.11           | 0.09                  | 0.07                   | 0.055              |
' | Coil Ramp Up       | 2.5            | 2.5                   | 2.5                    | 2.5                |
' | Scatter Angle      | 0              | 0                     | 0                      | 0                  |
' | EOS Torque         | 0.4            | 0.4                   | 0.375                  | 0.375              |
' | EOS Torque Angle   | 4              | 4                     | 6                      | 6                  |
'

'******************************************************
' Flippers Polarity (Select appropriate sub based on era)
'******************************************************

Dim LF : Set LF = New FlipperPolarity
Dim RF : Set RF = New FlipperPolarity

InitPolarity

'
''*******************************************
'' Late 70's to early 80's
'
'Sub InitPolarity()
'   dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 80
'		x.DebugOn=False ' prints some info in debugger
'
'
'        x.AddPt "Polarity", 0, 0, 0
'        x.AddPt "Polarity", 1, 0.05, - 2.7
'        x.AddPt "Polarity", 2, 0.16, - 2.7
'        x.AddPt "Polarity", 3, 0.22, - 0
'        x.AddPt "Polarity", 4, 0.25, - 0
'        x.AddPt "Polarity", 5, 0.3, - 1
'        x.AddPt "Polarity", 6, 0.4, - 2
'        x.AddPt "Polarity", 7, 0.5, - 2.7
'        x.AddPt "Polarity", 8, 0.65, - 1.8
'        x.AddPt "Polarity", 9, 0.75, - 0.5
'        x.AddPt "Polarity", 10, 0.81, - 0.5
'        x.AddPt "Polarity", 11, 0.88, 0
'        x.AddPt "Polarity", 12, 1.3, 0
'
'		x.AddPt "Velocity", 0, 0, 0.85
'		x.AddPt "Velocity", 1, 0.15, 0.85
'		x.AddPt "Velocity", 2, 0.2, 0.9
'		x.AddPt "Velocity", 3, 0.23, 0.95
'		x.AddPt "Velocity", 4, 0.41, 0.95
'		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
'		x.AddPt "Velocity", 6, 0.62, 1.0
'		x.AddPt "Velocity", 7, 0.702, 0.968
'		x.AddPt "Velocity", 8, 0.95,  0.968
'		x.AddPt "Velocity", 9, 1.03,  0.945
'		x.AddPt "Velocity", 10, 1.5,  0.945
'
'	Next
'
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'    LF.SetObjects "LF", LeftFlipper, TriggerLF
'    RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub
'
'
'
''*******************************************
'' Mid 80's
'
'Sub InitPolarity()
'   dim x, a : a = Array(LF, RF)
'	for each x in a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 80
'		x.DebugOn=False ' prints some info in debugger
'
'		x.AddPt "Polarity", 0, 0, 0
'		x.AddPt "Polarity", 1, 0.05, - 3.7
'		x.AddPt "Polarity", 2, 0.16, - 3.7
'		x.AddPt "Polarity", 3, 0.22, - 0
'		x.AddPt "Polarity", 4, 0.25, - 0
'		x.AddPt "Polarity", 5, 0.3, - 2
'		x.AddPt "Polarity", 6, 0.4, - 3
'		x.AddPt "Polarity", 7, 0.5, - 3.7
'		x.AddPt "Polarity", 8, 0.65, - 2.3
'		x.AddPt "Polarity", 9, 0.75, - 1.5
'		x.AddPt "Polarity", 10, 0.81, - 1
'		x.AddPt "Polarity", 11, 0.88, 0
'		x.AddPt "Polarity", 12, 1.3, 0
'
'		x.AddPt "Velocity", 0, 0, 0.85
'		x.AddPt "Velocity", 1, 0.15, 0.85
'		x.AddPt "Velocity", 2, 0.2, 0.9
'		x.AddPt "Velocity", 3, 0.23, 0.95
'		x.AddPt "Velocity", 4, 0.41, 0.95
'		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
'		x.AddPt "Velocity", 6, 0.62, 1.0
'		x.AddPt "Velocity", 7, 0.702, 0.968
'		x.AddPt "Velocity", 8, 0.95,  0.968
'		x.AddPt "Velocity", 9, 1.03,  0.945
'		x.AddPt "Velocity", 10, 1.5,  0.945
'
'	Next
'
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'    LF.SetObjects "LF", LeftFlipper, TriggerLF
'    RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub
'
''*******************************************
''  Late 80's early 90's

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
		x.DebugOn=False ' prints some info in debugger

		x.AddPt "Polarity", 0, 0, 0
		x.AddPt "Polarity", 1, 0.05, - 5
		x.AddPt "Polarity", 2, 0.16, - 5
		x.AddPt "Polarity", 3, 0.22, - 0
		x.AddPt "Polarity", 4, 0.25, - 0
		x.AddPt "Polarity", 5, 0.3, - 2
		x.AddPt "Polarity", 6, 0.4, - 3
		x.AddPt "Polarity", 7, 0.5, - 4.0
		x.AddPt "Polarity", 8, 0.7, - 3.5
		x.AddPt "Polarity", 9, 0.75, - 3.0
		x.AddPt "Polarity", 10, 0.8, - 2.5
		x.AddPt "Polarity", 11, 0.85, - 2.0
		x.AddPt "Polarity", 12, 0.9, - 1.5
		x.AddPt "Polarity", 13, 0.95, - 1.0
		x.AddPt "Polarity", 14, 1, - 0.5
		x.AddPt "Polarity", 15, 1.1, 0
		x.AddPt "Polarity", 16, 1.3, 0

		x.AddPt "Velocity", 0, 0, 0.85
		x.AddPt "Velocity", 1, 0.15, 0.85
		x.AddPt "Velocity", 2, 0.2, 0.9
		x.AddPt "Velocity", 3, 0.23, 0.95
		x.AddPt "Velocity", 4, 0.41, 0.95
		x.AddPt "Velocity", 5, 0.53, 0.95 '0.982
		x.AddPt "Velocity", 6, 0.62, 1.0
		x.AddPt "Velocity", 7, 0.702, 0.968
		x.AddPt "Velocity", 8, 0.95,  0.968
		x.AddPt "Velocity", 9, 1.03,  0.945
		x.AddPt "Velocity", 10, 1.5,  0.945

	Next

	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
	LF.SetObjects "LF", LeftFlipper, TriggerLF
	RF.SetObjects "RF", RightFlipper, TriggerRF
End Sub

'*******************************************
' Early 90's and after
'
'Sub InitPolarity()
'	Dim x, a
'	a = Array(LF, RF)
'	For Each x In a
'		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
'		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
'		x.enabled = True
'		x.TimeDelay = 60
'		x.DebugOn=False ' prints some info in debugger
'
'		x.AddPt "Polarity", 0, 0, 0
'		x.AddPt "Polarity", 1, 0.05, - 5.5
'		x.AddPt "Polarity", 2, 0.16, - 5.5
'		x.AddPt "Polarity", 3, 0.20, - 0.75
'		x.AddPt "Polarity", 4, 0.25, - 1.25
'		x.AddPt "Polarity", 5, 0.3, - 1.75
'		x.AddPt "Polarity", 6, 0.4, - 3.5
'		x.AddPt "Polarity", 7, 0.5, - 5.25
'		x.AddPt "Polarity", 8, 0.7, - 4.0
'		x.AddPt "Polarity", 9, 0.75, - 3.5
'		x.AddPt "Polarity", 10, 0.8, - 3.0
'		x.AddPt "Polarity", 11, 0.85, - 2.5
'		x.AddPt "Polarity", 12, 0.9, - 2.0
'		x.AddPt "Polarity", 13, 0.95, - 1.5
'		x.AddPt "Polarity", 14, 1, - 1.0
'		x.AddPt "Polarity", 15, 1.05, -0.5
'		x.AddPt "Polarity", 16, 1.1, 0
'		x.AddPt "Polarity", 17, 1.3, 0
'
'		x.AddPt "Velocity", 0, 0, 0.85
'		x.AddPt "Velocity", 1, 0.23, 0.85
'		x.AddPt "Velocity", 2, 0.27, 1
'		x.AddPt "Velocity", 3, 0.3, 1
'		x.AddPt "Velocity", 4, 0.35, 1
'		x.AddPt "Velocity", 5, 0.6, 1 '0.982
'		x.AddPt "Velocity", 6, 0.62, 1.0
'		x.AddPt "Velocity", 7, 0.702, 0.968
'		x.AddPt "Velocity", 8, 0.95,  0.968
'		x.AddPt "Velocity", 9, 1.03,  0.945
'		x.AddPt "Velocity", 10, 1.5,  0.945
'
'	Next
'	
'	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
'	LF.SetObjects "LF", LeftFlipper, TriggerLF
'	RF.SetObjects "RF", RightFlipper, TriggerRF
'End Sub

'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

' modified 2023 by nFozzy
' Removed need for 'endpoint' objects
' Added 'createvents' type thing for TriggerLF / TriggerRF triggers.
' Removed AddPt function which complicated setup imo
' made DebugOn do something (prints some stuff in debugger)
'   Otherwise it should function exactly the same as before\
' modified 2024 by rothbauerw
' Added Reprocessballs for flipper collisions (LF.Reprocessballs Activeball and RF.Reprocessballs Activeball must be added to the flipper collide subs
' Improved handling to remove correction for backhand shots when the flipper is raised

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt		'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay		'delay before trigger turns off and polarity is disabled
	Private Flipper, FlipperStart, FlipperEnd, FlipperEndY, LR, PartialFlipCoef, FlipStartAngle
	Private Balls(20), balldata(20)
	Private Name
	
	Dim PolarityIn, PolarityOut
	Dim VelocityIn, VelocityOut
	Dim YcoefIn, YcoefOut
	Public Sub Class_Initialize
		ReDim PolarityIn(0)
		ReDim PolarityOut(0)
		ReDim VelocityIn(0)
		ReDim VelocityOut(0)
		ReDim YcoefIn(0)
		ReDim YcoefOut(0)
		Enabled = True
		TimeDelay = 50
		LR = 1
		Dim x
		For x = 0 To UBound(balls)
			balls(x) = Empty
			Set Balldata(x) = new SpoofBall
		Next
	End Sub
	
	Public Sub SetObjects(aName, aFlipper, aTrigger)
		
		If TypeName(aName) <> "String" Then MsgBox "FlipperPolarity: .SetObjects error: first argument must be a String (And name of Object). Found:" & TypeName(aName) End If
		If TypeName(aFlipper) <> "Flipper" Then MsgBox "FlipperPolarity: .SetObjects error: Second argument must be a flipper. Found:" & TypeName(aFlipper) End If
		If TypeName(aTrigger) <> "Trigger" Then MsgBox "FlipperPolarity: .SetObjects error: third argument must be a trigger. Found:" & TypeName(aTrigger) End If
		If aFlipper.EndAngle > aFlipper.StartAngle Then LR = -1 Else LR = 1 End If
		Name = aName
		Set Flipper = aFlipper
		FlipperStart = aFlipper.x
		FlipperEnd = Flipper.Length * Sin((Flipper.StartAngle / 57.295779513082320876798154814105)) + Flipper.X ' big floats for degree to rad conversion
		FlipperEndY = Flipper.Length * Cos(Flipper.StartAngle / 57.295779513082320876798154814105)*-1 + Flipper.Y
		
		Dim str
		str = "Sub " & aTrigger.name & "_Hit() : " & aName & ".AddBall ActiveBall : End Sub'"
		ExecuteGlobal(str)
		str = "Sub " & aTrigger.name & "_UnHit() : " & aName & ".PolarityCorrect ActiveBall : End Sub'"
		ExecuteGlobal(str)
		
	End Sub
	
	' Legacy: just no op
	Public Property Let EndPoint(aInput)
		
	End Property
	
	Public Sub AddPt(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out)
		Select Case aChooseArray
			Case "Polarity"
				ShuffleArrays PolarityIn, PolarityOut, 1
				PolarityIn(aIDX) = aX
				PolarityOut(aIDX) = aY
				ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity"
				ShuffleArrays VelocityIn, VelocityOut, 1
				VelocityIn(aIDX) = aX
				VelocityOut(aIDX) = aY
				ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef"
				ShuffleArrays YcoefIn, YcoefOut, 1
				YcoefIn(aIDX) = aX
				YcoefOut(aIDX) = aY
				ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
	End Sub
	
	Public Sub AddBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If IsEmpty(balls(x)) Then
				Set balls(x) = aBall
				Exit Sub
			End If
		Next
	End Sub
	
	Private Sub RemoveBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If TypeName(balls(x) ) = "IBall" Then
				If aBall.ID = Balls(x).ID Then
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
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next
	End Property
	
	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				balldata(x).Data = balls(x)
			End If
		Next
		FlipStartAngle = Flipper.currentangle
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub

	Public Sub ReProcessBalls(aBall) 'save data of balls in flipper range
		If FlipperOn() Then
			Dim x
			For x = 0 To UBound(balls)
				If Not IsEmpty(balls(x)) Then
					if balls(x).ID = aBall.ID Then
						If isempty(balldata(x).ID) Then
							balldata(x).Data = balls(x)
						End If
					End If
				End If
			Next
		End If
	End Sub

	'Timer shutoff for polaritycorrect
	Private Function FlipperOn()
		If GameTime < FlipAt+TimeDelay Then
			FlipperOn = True
		End If
	End Function
	
	Public Sub PolarityCorrect(aBall)
		If FlipperOn() Then
			Dim tmp, BallPos, x, IDX, Ycoef, BalltoFlip, BalltoBase, NoCorrection, checkHit
			Ycoef = 1
			
			'y safety Exit
			If aBall.VelY > -8 Then 'ball going down
				RemoveBall aBall
				Exit Sub
			End If
			
			'Find balldata. BallPos = % on Flipper
			For x = 0 To UBound(Balls)
				If aBall.id = BallData(x).id And Not IsEmpty(BallData(x).id) Then
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					BalltoFlip = DistanceFromFlipperAngle(BallData(x).x, BallData(x).y, Flipper, FlipStartAngle)
					If ballpos > 0.65 Then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)								'find safety coefficient 'ycoef' data
				End If
			Next
			
			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				If ballpos > 0.65 Then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)												'find safety coefficient 'ycoef' data
				NoCorrection = 1
			Else
				checkHit = 50 + (20 * BallPos) 

				If BalltoFlip > checkHit or (PartialFlipCoef < 0.5 and BallPos > 0.22) Then
					NoCorrection = 1
				Else
					NoCorrection = 0
				End If
			End If
			
			'Velocity correction
			If Not IsEmpty(VelocityIn(0) ) Then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
				
				'If partialflipcoef < 1 Then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
				
				If Enabled Then aBall.Velx = aBall.Velx*VelCoef
				If Enabled Then aBall.Vely = aBall.Vely*VelCoef
			End If
			
			'Polarity Correction (optional now)
			If Not IsEmpty(PolarityIn(0) ) Then
				Dim AddX
				AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				
				If Enabled and NoCorrection = 0 Then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef*VelCoef)
			End If
			If DebugOn Then debug.print "PolarityCorrect" & " " & Name & " @ " & GameTime & " " & Round(BallPos*100) & "%" & " AddX:" & Round(AddX,2) & " Vel%:" & Round(VelCoef*100)
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	Dim x, aCount
	aCount = 0
	ReDim a(UBound(aArray) )
	For x = 0 To UBound(aArray)		'Shuffle objects in a temp array
		If Not IsEmpty(aArray(x) ) Then
			If IsObject(aArray(x)) Then
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	If offset < 0 Then offset = 0
	ReDim aArray(aCount-1+offset)		'Resize original array
	For x = 0 To aCount-1				'set objects back into original array
		If IsObject(a(x)) Then
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
	BallSpeed = Sqr(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)		'Set up line via two points, no clamping. Input X, output Y
	Dim x, y, b, m
	x = input
	m = (Y2 - Y1) / (X2 - X1)
	b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius
	Public Property Let Data(aBall)
		With aBall
			x = .x
			y = .y
			z = .z
			velx = .velx
			vely = .vely
			velz = .velz
			id = .ID
			mass = .mass
			radius = .radius
		End With
	End Property
	Public Sub Reset()
		x = Empty
		y = Empty
		z = Empty
		velx = Empty
		vely = Empty
		velz = Empty
		id = Empty
		mass = Empty
		radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	Dim y 'Y output
	Dim L 'Line
	'find active line
	Dim ii
	For ii = 1 To UBound(xKeyFrame)
		If xInput <= xKeyFrame(ii) Then
			L = ii
			Exit For
		End If
	Next
	If xInput > xKeyFrame(UBound(xKeyFrame) ) Then L = UBound(xKeyFrame)		'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )
	
	If xInput <= xKeyFrame(LBound(xKeyFrame) ) Then Y = yLvl(LBound(xKeyFrame) )		 'Clamp lower
	If xInput >= xKeyFrame(UBound(xKeyFrame) ) Then Y = yLvl(UBound(xKeyFrame) )		'Clamp upper
	
	LinearEnvelope = Y
End Function

'******************************************************
'  FLIPPER TRICKS
'******************************************************
' To add the flipper tricks you must
'	 - Include a call to FlipperCradleCollision from within OnBallBallCollision subroutine
'	 - Include a call the CheckLiveCatch from the LeftFlipper_Collide and RightFlipper_Collide subroutines
'	 - Include FlipperActivate and FlipperDeactivate in the Flipper solenoid subs

RightFlipper.timerinterval = 1
Rightflipper.timerenabled = True

Sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
End Sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b
	Dim BOT
	BOT = GetBalls
	
	If Flipper1.currentangle = Endangle1 And EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'   debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					Exit Sub
				End If
			Next
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				End If
			Next
		End If
	Else
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 Then EOSNudge1 = 0
	End If
End Sub


Dim FCCDamping: FCCDamping = 0.4

Sub FlipperCradleCollision(ball1, ball2, velocity)
	if velocity < 0.7 then exit sub		'filter out gentle collisions
    Dim DoDamping, coef
    DoDamping = false
    'Check left flipper
    If LeftFlipper.currentangle = LFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, LeftFlipper) OR FlipperTrigger(ball2.x, ball2.y, LeftFlipper) Then DoDamping = true
    End If
    'Check right flipper
    If RightFlipper.currentangle = RFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, RightFlipper) OR FlipperTrigger(ball2.x, ball2.y, RightFlipper) Then DoDamping = true
    End If
    If DoDamping Then
		coef = FCCDamping
        ball1.velx = ball1.velx * coef: ball1.vely = ball1.vely * coef: ball1.velz = ball1.velz * coef
        ball2.velx = ball2.velx * coef: ball2.vely = ball2.vely * coef: ball2.velz = ball2.velz * coef
    End If
End Sub
	




'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = Sqr((ax - bx) ^ 2 + (ay - by) ^ 2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) 'Distance between a point and a line where point Is px,py
	DistancePL = Abs((by - ay) * px - (bx - ax) * py + bx * ay - by * ax) / Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI / 180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax)) * 180 / PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle + 90)) + Flipper.x, Sin(Radians(Flipper.currentangle + 90)) + Flipper.y)
End Function

Function DistanceFromFlipperAngle(ballx, bally, Flipper, Angle)
	DistanceFromFlipperAngle = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Angle + 90)) + Flipper.x, Sin(Radians(angle + 90)) + Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle = Abs(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360
	
	If DistanceFromFlipper(ballx,bally,Flipper) < 48 And DiffAngle <= 90 And Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If
End Function

'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

Dim LFPress, RFPress, LFCount, RFCount
Dim LFState, RFState
Dim EOST, EOSA,Frampup, FElasticity,FReturn
Dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0 '0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1.5 'EM's to late 80's - new recommendation by rothbauerw (previously 1)
Const EOSTnew = 1.2 '90's and later - new recommendation by rothbauerw (previously 0.8)
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode
	Case 0
		SOSRampup = 2.5
	Case 1
		SOSRampup = 6
	Case 2
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'   Const EOSReturn = 0.055  'EM's
'   Const EOSReturn = 0.045  'late 70's to mid 80's
Const EOSReturn = 0.035  'mid 80's to early 90's
'   Const EOSReturn = 0.025  'mid 90's and later

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
	Flipper.eostorque = EOST * EOSReturn / FReturn
	
	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, BOT
		BOT = GetBalls
		
		For b = 0 To UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= - 0.4 Then BOT(b).vely =  - 0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState)
	Dim Dir
	Dir = Flipper.startangle / Abs(Flipper.startangle) '-1 for Right Flipper
	
	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup
			Flipper.endangle = FEndAngle - 3 * Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) And FlipperPress = 1 Then
		If FCount = 0 Then FCount = GameTime
		
		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	ElseIf Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 And FlipperPress = 1 Then
		If FState <> 3 Then
			Flipper.eostorque = EOST
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If
	End If
End Sub

Const LiveDistanceMin = 5  'minimum distance In vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114 'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)
Const BaseDampen = 0.55

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
    Dim Dir, LiveDist
    Dir = Flipper.startangle / Abs(Flipper.startangle)    '-1 for Right Flipper
    Dim LiveCatchBounce   'If live catch is not perfect, it won't freeze ball totally
    Dim CatchTime
    CatchTime = GameTime - FCount
    LiveDist = Abs(Flipper.x - ball.x)

    If CatchTime <= LiveCatch And parm > 3 And LiveDist > LiveDistanceMin And LiveDist < LiveDistanceMax Then
        If CatchTime <= LiveCatch * 0.5 Then   'Perfect catch only when catch time happens in the beginning of the window
            LiveCatchBounce = 0
        Else
            LiveCatchBounce = Abs((LiveCatch / 2) - CatchTime)  'Partial catch when catch happens a bit late
        End If
        
        If LiveCatchBounce = 0 And ball.velx * Dir > 0 And LiveDist > 30 Then ball.velx = 0

        If ball.velx * Dir > 0 And LiveDist < 30 Then
            ball.velx = BaseDampen * ball.velx
            ball.vely = BaseDampen * ball.vely
            ball.angmomx = BaseDampen * ball.angmomx
            ball.angmomy = BaseDampen * ball.angmomy
            ball.angmomz = BaseDampen * ball.angmomz
        Elseif LiveDist > 30 Then
            ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
            ball.angmomx = 0
            ball.angmomy = 0
            ball.angmomz = 0
        End If
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf ActiveBall, parm
    End If
End Sub

'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************





'******************************************************
' 	ZDMP:  RUBBER  DAMPENERS
'******************************************************
' These are data mined bounce curves,
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx)
	RubbersD.dampen ActiveBall
	TargetBouncer ActiveBall, 1
End Sub

Sub dSleeves_Hit(idx)
	SleevesD.Dampen ActiveBall
	TargetBouncer ActiveBall, 0.7
End Sub

Dim RubbersD				'frubber
Set RubbersD = New Dampener
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	  'debug, reports In debugger (In vel, out cor); cor bounce curve (linear)

'for best results, try to match in-game velocity as closely as possible to the desired curve
'   RubbersD.addpoint 0, 0, 0.935   'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1		 'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	   'there's clamping so interpolate up to 56 at least

Dim SleevesD	'this is just rubber but cut down to 85%...
Set SleevesD = New Dampener
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	  'debug, reports In debugger (In vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'######################### Adjust these values to increase or lessen the elasticity

Dim FlippersD
Set FlippersD = New Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False
FlippersD.addpoint 0, 0, 1.1
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn   'tbpOut.text
	Public name, Threshold  'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
	End Sub
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Dampen(aBall)
		If threshold Then
			If BallSpeed(aBall) < threshold Then Exit Sub
		End If
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If debugOn Then str = name & " In vel:" & Round(cor.ballvel(aBall.id),2 ) & vbNewLine & "desired cor: " & Round(desiredcor,4) & vbNewLine & _
		"actual cor: " & Round(realCOR,4) & vbNewLine & "ballspeed coef: " & Round(coef, 3) & vbNewLine
		If Print Then Debug.print Round(cor.ballvel(aBall.id),2) & ", " & Round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef
		aBall.vely = aBall.vely * coef
		aBall.velz = aBall.velz * coef
		If debugOn Then TBPout.text = str
	End Sub
	
	Public Sub Dampenf(aBall, parm) 'Rubberizer is handle here
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If Abs(aball.velx) < 2 And aball.vely < 0 And aball.vely >  - 3.75 Then
			aBall.velx = aBall.velx * coef
			aBall.vely = aBall.vely * coef
			aBall.velz = aBall.velz * coef
		End If
	End Sub
	
	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		Dim x
		For x = 0 To UBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x) * aCoef
		Next
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
End Class

'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

Dim cor
Set cor = New CoRTracker

Class CoRTracker
	Public ballvel, ballvelx, ballvely
	
	Private Sub Class_Initialize
		ReDim ballvel(0)
		ReDim ballvelx(0)
		ReDim ballvely(0)
	End Sub
	
	Public Sub Update()	'tracks in-ball-velocity
		Dim str, b, AllBalls, highestID
		allBalls = GetBalls
		
		For Each b In allballs
			If b.id >= HighestID Then highestID = b.id
		Next
		
		If UBound(ballvel) < highestID Then ReDim ballvel(highestID)	'set bounds
		If UBound(ballvelx) < highestID Then ReDim ballvelx(highestID)	'set bounds
		If UBound(ballvely) < highestID Then ReDim ballvely(highestID)	'set bounds
		
		For Each b In allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

' Note, cor.update must be called in a 10 ms timer. The example table uses the GameTimer for this purpose, but sometimes a dedicated timer call RDampen is used.
'
'Sub RDampen_Timer
'	Cor.Update
'End Sub

'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************



'******************************************************
' 	ZBOU: VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1	  '0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.9	 'Level of bounces. Recommmended value of 0.7-1

Sub TargetBouncer(aBall,defvalue)
	Dim zMultiplier, vel, vratio
	If TargetBouncerEnabled = 1 And aball.z < 30 Then
		'   debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		vel = BallSpeed(aBall)
		If aBall.velx = 0 Then vratio = 1 Else vratio = aBall.vely / aBall.velx
		Select Case Int(Rnd * 6) + 1
			Case 1
				zMultiplier = 0.2 * defvalue
			Case 2
				zMultiplier = 0.25 * defvalue
			Case 3
				zMultiplier = 0.3 * defvalue
			Case 4
				zMultiplier = 0.4 * defvalue
			Case 5
				zMultiplier = 0.45 * defvalue
			Case 6
				zMultiplier = 0.5 * defvalue
		End Select
		aBall.velz = Abs(vel * zMultiplier * TargetBouncerFactor)
		aBall.velx = Sgn(aBall.velx) * Sqr(Abs((vel ^ 2 - aBall.velz ^ 2) / (1 + vratio ^ 2)))
		aBall.vely = aBall.velx * vratio
		'   debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		'   debug.print "conservation check: " & BallSpeed(aBall)/vel
	End If
End Sub

'Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit(idx)
	TargetBouncer ActiveBall, 1
End Sub



'******************************************************
'	ZSSC: SLINGSHOT CORRECTION FUNCTIONS by apophis
'******************************************************
' To add these slingshot corrections:
'	 - On the table, add the endpoint primitives that define the two ends of the Slingshot
'	 - Initialize the SlingshotCorrection objects in InitSlingCorrection
'	 - Call the .VelocityCorrect methods from the respective _Slingshot event sub

Dim LS
Set LS = New SlingshotCorrection
Dim RS
Set RS = New SlingshotCorrection

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
	AddSlingsPt 0, 0.00, - 4
	AddSlingsPt 1, 0.45, - 7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4
End Sub

Sub AddSlingsPt(idx, aX, aY)		'debugger wrapper for adjusting flipper script In-game
	Dim a
	a = Array(LS, RS)
	Dim x
	For Each x In a
		x.addpoint idx, aX, aY
	Next
End Sub

'' The following sub are needed, however they may exist somewhere else in the script. Uncomment below if needed
'Dim PI: PI = 4*Atn(1)
'Function dSin(degrees)
'	dsin = sin(degrees * Pi/180)
'End Function
'Function dCos(degrees)
'	dcos = cos(degrees * Pi/180)
'End Function
'
'Function RotPoint(x,y,angle)
'	dim rx, ry
'	rx = x*dCos(angle) - y*dSin(angle)
'	ry = x*dSin(angle) + y*dCos(angle)
'	RotPoint = Array(rx,ry)
'End Function

Class SlingshotCorrection
	Public DebugOn, Enabled
	Private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2
	
	Public ModIn, ModOut
	
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
		Enabled = True
	End Sub
	
	Public Property Let Object(aInput)
		Set Slingshot = aInput
	End Property
	
	Public Property Let EndPoint1(aInput)
		SlingX1 = aInput.x
		SlingY1 = aInput.y
	End Property
	
	Public Property Let EndPoint2(aInput)
		SlingX2 = aInput.x
		SlingY2 = aInput.y
	End Property
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
	
	
	Public Sub VelocityCorrect(aBall)
		Dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then
			XL = SlingX1
			YL = SlingY1
			XR = SlingX2
			YR = SlingY2
		Else
			XL = SlingX2
			YL = SlingY2
			XR = SlingX1
			YR = SlingY1
		End If
		
		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then
			If Abs(XR - XL) > Abs(YR - YL) Then
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If
		
		'Velocity angle correction
		If Not IsEmpty(ModIn(0) ) Then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'   debug.print " BallPos=" & BallPos &" Angle=" & Angle
			'   debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled Then aBall.Velx = RotVxVy(0)
			If Enabled Then aBall.Vely = RotVxVy(1)
			'   debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			'   debug.print " "
		End If
	End Sub
End Class

'******************************************************
' 	ZRDT:  DROP TARGETS by Rothbauerw
'******************************************************
' The Stand Up and Drop Target solutions improve the physics for targets to create more realistic behavior. It allows the ball
' to move through the target enabling the ability to score more than one target with a well placed shot.
' It also handles full target animation, switch handling and deflection on hit. For drop targets there is also a slight lift when
' the drop targets raise, bricking, and popping the ball up if it's over the drop target when it raises.
'
' Add a Timers named DTAnim and STAnim to editor to handle drop & standup target animations, or run them off an always-on 10ms timer (GameTimer)
' DTAnim.interval = 10
' DTAnim.enabled = True

' Sub DTAnim_Timer
' 	DoDTAnim
'	DoSTAnim
' End Sub

' For each drop target, we'll use two wall objects for physics calculations and one primitive for visuals and
' animation. We will not use target objects.  Place your drop target primitive the same as you would a VP drop target.
' The primitive should have it's pivot point centered on the x and y axis and at or just below the playfield
' level on the z axis. Orientation needs to be set using Rotz and bending deflection using Rotx. You'll find a hooded
' target mesh in this table's example. It uses the same texture map as the VP drop targets.
'
' For each stand up target we'll use a vp target, a laid back collidable primitive, and one primitive for visuals and animation.
' The visual primitive should should have it's pivot point centered on the x and y axis and the z should be at or just below the playfield.
' The target should animate backwards using transy.
'
' To create visual target primitives that work with the stand up and drop target code, follow the below instructions:
' (Other methods will work as well, but this is easy for even non-blender users to do)
' 1) Open a new blank table. Delete everything off the table in editor.
' 2) Copy and paste the VP target from your table into this blank table.
' 3) Place the target at x = 0, y = 0  (upper left hand corner) with an orientation of 0 (target facing the front of the table)
' 4) Under the file menu, select Export "OBJ Mesh"
' 5) Go to "https://threejs.org/editor/". Here you can modify the exported obj file. When you export, it exports your target and also 
'    the playfield mesh. You need to delete the playfield mesh here. Under the file menu, chose import, and select the obj you exported
'    from VPX. In the right hand panel, find the Playfield object and click on it and delete. Then use the file menu to Export OBJ.
' 6) In VPX, you can add a primitive and use "Import Mesh" to import the exported obj from the previous step. X,Y,Z scale should be 1.
'    The primitive will use the same target texture as the VP target object. 
'
' * Note, each target must have a unique switch number. If they share a same number, add 100 to additional target with that number.
' For example, three targets with switch 32 would use 32, 132, 232 for their switch numbers.
' The 100 and 200 will be removed when setting the switch value for the target.

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
Dim DT3, DT4, DT5

'Set array with drop target objects
'
'DropTargetvar = Array(primary, secondary, prim, swtich, animate)
'   primary:	primary target wall to determine drop
'   secondary:  wall used to simulate the ball striking a bent or offset target after the initial Hit
'   prim:	   primitive target used for visuals and animation
'				   IMPORTANT!!!
'				   rotz must be used for orientation
'				   rotx to bend the target back
'				   transz to move it up and down
'				   the pivot point should be in the center of the target on the x, y and at or below the playfield (0) on z
'   switch:	 ROM switch number
'   animate:	Array slot for handling the animation instrucitons, set to 0
'				   Values for animate: 1 - bend target (hit to primary), 2 - drop target (hit to secondary), 3 - brick target (high velocity hit to secondary), -1 - raise target
'   isDropped:  Boolean which determines whether a drop target is dropped. Set to false if they are initially raised, true if initially dropped.
'					Use the function DTDropped(switchid) to check a target's drop status.

Set DT3 = (new DropTarget)(Target03, Target03a, Target03p, 3, 0, False)
Set DT4 = (new DropTarget)(Target04, Target04a, Target04p, 4, 0, False)
Set DT5 = (new DropTarget)(Target05, Target05a, Target05p, 5, 0, False)

Dim DTArray
DTArray = Array(DT3, DT4, DT5)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 20 'in milliseconds
Const DTDropUpSpeed = 20 'in milliseconds
Const DTDropUnits = 60 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 100 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30 'velocity at which the target will brick, set to '0' to disable brick
Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const DTMass = 0.2 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance

'******************************************************
'  DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)
	
	PlayTargetSound
	DTArray(i).animate = DTCheckBrick(ActiveBall,DTArray(i).prim)
	If DTArray(i).animate = 1 Or DTArray(i).animate = 3 Or DTArray(i).animate = 4 Then
		DTBallPhysics ActiveBall, DTArray(i).prim.rotz, DTMass
	End If
	DoDTAnim
End Sub

Sub DTRaise(switch)
	Dim i
	i = DTArrayID(switch)
	
	DTArray(i).animate =  - 1
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
	For i = 0 To UBound(DTArray)
		If DTArray(i).sw = switch Then
			DTArrayID = i
			Exit Function
		End If
	Next
End Function

Sub DTBallPhysics(aBall, angle, mass)
	Dim rangle,bangle,calc1, calc2, calc3
	rangle = (angle - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	
	calc1 = cor.BallVel(aball.id) * Cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
	calc2 = cor.BallVel(aball.id) * Sin(bangle - rangle) * Cos(rangle + 4 * Atn(1) / 2)
	calc3 = cor.BallVel(aball.id) * Sin(bangle - rangle) * Sin(rangle + 4 * Atn(1) / 2)
	
	aBall.velx = calc1 * Cos(rangle) + calc2
	aBall.vely = calc1 * Sin(rangle) + calc3
End Sub

'Check if target is hit on it's face or sides and whether a 'brick' occurred
Function DTCheckBrick(aBall, dtprim)
	Dim bangle, bangleafter, rangle, rangle2, Xintersect, Yintersect, cdist, perpvel, perpvelafter, paravel, paravelafter
	rangle = (dtprim.rotz - 90) * 3.1416 / 180
	rangle2 = dtprim.rotz * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)
	
	Xintersect = (aBall.y - dtprim.y - Tan(bangle) * aball.x + Tan(rangle2) * dtprim.x) / (Tan(rangle2) - Tan(bangle))
	Yintersect = Tan(rangle2) * Xintersect + (dtprim.y - Tan(rangle2) * dtprim.x)
	
	cdist = Distance(dtprim.x, dtprim.y, Xintersect, Yintersect)
	
	perpvel = cor.BallVel(aball.id) * Cos(bangle - rangle)
	paravel = cor.BallVel(aball.id) * Sin(bangle - rangle)
	
	perpvelafter = BallSpeed(aBall) * Cos(bangleafter - rangle)
	paravelafter = BallSpeed(aBall) * Sin(bangleafter - rangle)
	
	If perpvel > 0 And  perpvelafter <= 0 Then
		If DTEnableBrick = 1 And  perpvel > DTBrickVel And DTBrickVel <> 0 And cdist < 8 Then
			DTCheckBrick = 3
		Else
			DTCheckBrick = 1
		End If
	ElseIf perpvel > 0 And ((paravel > 0 And paravelafter > 0) Or (paravel < 0 And paravelafter < 0)) Then
		DTCheckBrick = 4
	Else
		DTCheckBrick = 0
	End If
End Function

Sub DoDTAnim()
	Dim i
	For i = 0 To UBound(DTArray)
		DTArray(i).animate = DTAnimate(DTArray(i).primary,DTArray(i).secondary,DTArray(i).prim,DTArray(i).sw,DTArray(i).animate)
	Next
End Sub

Function DTAnimate(primary, secondary, prim, switch, animate)
	Dim transz, switchid
	Dim animtime, rangle
	
	switchid = switch
	
	Dim ind
	ind = DTArrayID(switchid)
	
	rangle = prim.rotz * PI / 180
	
	DTAnimate = animate
	
	If animate = 0 Then
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	ElseIf primary.uservalue = 0 Then
		primary.uservalue = GameTime
	End If
	
	animtime = GameTime - primary.uservalue
	
	If (animate = 1 Or animate = 4) And animtime < DTDropDelay Then
		primary.collidable = 0
		If animate = 1 Then secondary.collidable = 1 Else secondary.collidable = 0
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
		DTAnimate = animate
		Exit Function
	ElseIf (animate = 1 Or animate = 4) And animtime > DTDropDelay Then
		primary.collidable = 0
		If animate = 1 Then secondary.collidable = 1 Else secondary.collidable = 1 'If animate = 1 Then secondary.collidable = 1 Else secondary.collidable = 0 'updated by rothbauerw to account for edge case
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
		animate = 2
		SoundDropTargetDrop prim
	End If
	
	If animate = 2 Then
		transz = (animtime - DTDropDelay) / DTDropSpeed * DTDropUnits *  - 1
		If prim.transz >  - DTDropUnits  Then
			prim.transz = transz
		End If
		
		prim.rotx = DTMaxBend * Cos(rangle) / 2
		prim.roty = DTMaxBend * Sin(rangle) / 2
		
		If prim.transz <= - DTDropUnits Then
			prim.transz =  - DTDropUnits
			secondary.collidable = 0
			DTArray(ind).isDropped = True 'Mark target as dropped
			primary.uservalue = 0
			DTAnimate = 0
			Exit Function
		Else
			DTAnimate = 2
			Exit Function
		End If
	End If
	
	If animate = 3 And animtime < DTDropDelay Then
		primary.collidable = 0
		secondary.collidable = 1
		prim.rotx = DTMaxBend * Cos(rangle)
		prim.roty = DTMaxBend * Sin(rangle)
	ElseIf animate = 3 And animtime > DTDropDelay Then
		primary.collidable = 1
		secondary.collidable = 0
		prim.rotx = 0
		prim.roty = 0
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	End If
	
	If animate =  - 1 Then
		transz = (1 - (animtime) / DTDropUpSpeed) * DTDropUnits *  - 1
		
		If prim.transz =  - DTDropUnits Then
			Dim b
			Dim BOT
			BOT = GetBalls
			
			For b = 0 To UBound(BOT)
				If InRotRect(BOT(b).x,BOT(b).y,prim.x, prim.y, prim.rotz, - 25, - 10,25, - 10,25,25, - 25,25) And BOT(b).z < prim.z + DTDropUnits + 25 Then
					BOT(b).velz = 20
				End If
			Next
		End If
		
		If prim.transz < 0 Then
			prim.transz = transz
		ElseIf transz > 0 Then
			prim.transz = transz
		End If
		
		If prim.transz > DTDropUpUnits Then
			DTAnimate =  - 2
			prim.transz = DTDropUpUnits
			prim.rotx = 0
			prim.roty = 0
			primary.uservalue = GameTime
		End If
		primary.collidable = 0
		secondary.collidable = 1
		DTArray(ind).isDropped = False 'Mark target as not dropped
	End If
	
	If animate =  - 2 And animtime > DTRaiseDelay Then
		prim.transz = (animtime - DTRaiseDelay) / DTDropSpeed * DTDropUnits *  - 1 + DTDropUpUnits
		If prim.transz < 0 Then
			prim.transz = 0
			primary.uservalue = 0
			DTAnimate = 0
			
			primary.collidable = 1
			secondary.collidable = 0
		End If
	End If
End Function

Function DTDropped(switchid)
	Dim ind
	ind = DTArrayID(switchid)
	
	DTDropped = DTArray(ind).isDropped
End Function

'******************************************************
'****  END DROP TARGETS
'******************************************************

'******************************************************
'	ZRST: STAND-UP TARGETS by Rothbauerw
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
Dim ST1, ST2, ST6, ST7, ST8, ST9, ST10, ST11, ST12

'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, swtich)
'   primary:	vp target to determine target hit
'   prim:	   primitive target used for visuals and animation
'				   IMPORTANT!!!
'				   transy must be used to offset the target animation
'   switch:	 ROM switch number
'   animate:	Arrary slot for handling the animation instrucitons, set to 0
'
'You will also need to add a secondary hit object for each stand up (name sw11o, sw12o, and sw13o on the example Table1)
'these are inclined primitives to simulate hitting a bent target and should provide so z velocity on high speed impacts


Set ST1 = (new StandupTarget)(Target01, Target01p, 1, 0)
Set ST2 = (new StandupTarget)(Target02, Target02p, 2, 0)
Set ST6 = (new StandupTarget)(Target06, Target06p, 6, 0)
Set ST7 = (new StandupTarget)(Target07, Target07p, 7, 0)
Set ST8 = (new StandupTarget)(Target08, Target08p, 8, 0)
Set ST9 = (new StandupTarget)(Target09, Target09p, 9, 0)
Set ST10 = (new StandupTarget)(Target10, Target10p, 10, 0)
Set ST11 = (new StandupTarget)(Target11, Target11p, 11, 0)
Set ST12 = (new StandupTarget)(TargetRubberNextToRamp, TargetRubberNextToRampp, 12, 0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
'   STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST1, ST2, ST6, ST7, ST8, ST9, ST10, ST11, ST12)

'Configure the behavior of Stand-up Targets
Const STAnimStep = 1.5  'vpunits per animation step (control return to Start)
Const STMaxOffset = 9   'max vp units target moves when hit

Const STMass = 0.2	  'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				STAND-UP TARGETS FUNCTIONS
'******************************************************

Sub STHit(switch)
	Dim i
	i = STArrayID(switch)
	
	PlayTargetSound
	STArray(i).animate = STCheckHit(ActiveBall,STArray(i).primary)
	
	If STArray(i).animate <> 0 Then
		DTBallPhysics ActiveBall, STArray(i).primary.orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 To UBound(STArray)
		If STArray(i).sw = switch Then
			STArrayID = i
			Exit Function
		End If
	Next
End Function

Function STCheckHit(aBall, target) 'Check if target is hit on it's face
	Dim bangle, bangleafter, rangle, rangle2, perpvel, perpvelafter, paravel, paravelafter
	rangle = (target.orientation - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)
	
	perpvel = cor.BallVel(aball.id) * Cos(bangle - rangle)
	paravel = cor.BallVel(aball.id) * Sin(bangle - rangle)
	
	perpvelafter = BallSpeed(aBall) * Cos(bangleafter - rangle)
	paravelafter = BallSpeed(aBall) * Sin(bangleafter - rangle)
	
	If perpvel > 0 And  perpvelafter <= 0 Then
		STCheckHit = 1
	ElseIf perpvel > 0 And ((paravel > 0 And paravelafter > 0) Or (paravel < 0 And paravelafter < 0)) Then
		STCheckHit = 1
	Else
		STCheckHit = 0
	End If
End Function

Sub DoSTAnim()
	Dim i
	For i = 0 To UBound(STArray)
		STArray(i).animate = STAnimate(STArray(i).primary,STArray(i).prim,STArray(i).sw,STArray(i).animate)
	Next
End Sub

Function STAnimate(primary, prim, switch,  animate)
	Dim animtime
	
	STAnimate = animate
	
	If animate = 0  Then
		primary.uservalue = 0
		STAnimate = 0
		Exit Function
	ElseIf primary.uservalue = 0 Then
		primary.uservalue = GameTime
	End If
	
	animtime = GameTime - primary.uservalue
	
	If animate = 1 Then
		primary.collidable = 0
		prim.transy =  - STMaxOffset
		STAnimate = 2
		Exit Function
	ElseIf animate = 2 Then
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
'****   END STAND-UP TARGETS
'******************************************************

'******************************************************
'	ZBRL:  BALL ROLLING AND DROP SOUNDS
'******************************************************

' Be sure to call RollingUpdate in a timer with a 10ms interval see the GameTimer_Timer() sub

ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
	Dim i
	For i = 0 To tnob
		rolling(i) = False
	Next
End Sub

Sub RollingUpdate()
	Dim b
	Dim BOT
	BOT = GetBalls
	
	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 To tnob - 1
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next
	
	' exit the sub if no balls on the table
	If UBound(BOT) =  - 1 Then Exit Sub
	
	' play the rolling sound for each ball
	For b = 0 To UBound(BOT)
		If BallVel(BOT(b)) > 1 And BOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), - 1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))







		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If
		
		' Ball Drop Sounds
		If BOT(b).VelZ <  - 1 And BOT(b).z < 55 And BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If BOT(b).velz >  - 7 Then
					RandomSoundBallBouncePlayfieldSoft BOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If
			End If
		End If
		
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If
	Next
End Sub

'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************




'******************************************************
' 	ZRRL: RAMP ROLLING SFX
'******************************************************

'Ball tracking ramp SFX 1.0
'   Reqirements:
'		  * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'		  * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'		  * Create a Timer called RampRoll, that is enabled, with a interval of 100
'		  * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'		  * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'		  * To stop tracking ball
'				 * call WireRampOff
'				 * Otherwise, the ball will auto remove if it's below 30 vp units
'

Dim RampMinLoops
RampMinLoops = 4

' RampBalls
' Setup:  Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RampBalls(6,2)
Dim RampBalls(6,2)
'x,0 = ball x,1 = ID, 2 = Protection against ending early (minimum amount of updates)

'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
' Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
' Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
Dim RampType(6)

Sub WireRampOn(input)
	Waddball ActiveBall, input
	RampRollUpdate
End Sub

Sub WireRampOff()
	WRemoveBall ActiveBall.ID
End Sub

' WaddBall (Active Ball, Boolean)
Sub Waddball(input, RampInput) 'This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	Dim x
	For x = 1 To UBound(RampBalls)	'Check, don't add balls twice
		If RampBalls(x, 1) = input.id Then
			If Not IsEmpty(RampBalls(x,1) ) Then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next
	
	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
	For x = 1 To UBound(RampBalls)
		If IsEmpty(RampBalls(x, 1)) Then
			Set RampBalls(x, 0) = input
			RampBalls(x, 1) = input.ID
			RampType(x) = RampInput
			RampBalls(x, 2) = 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			Exit Sub
		End If
	Next
End Sub

' WRemoveBall (BallId)
Sub WRemoveBall(ID) 'This subroutine is called from the RampRollUpdate subroutine and is used to remove and stop the ball rolling sounds
	'   Debug.Print "In WRemoveBall() + Remove ball from loop array"
	Dim ballcount
	ballcount = 0
	Dim x
	For x = 1 To UBound(RampBalls)
		If ID = RampBalls(x, 1) Then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		End If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		If Not IsEmpty(Rampballs(x,1)) Then ballcount = ballcount + 1
	Next
	If BallCount = 0 Then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer()
	RampRollUpdate
End Sub

Sub RampRollUpdate()	'Timer update
	Dim x
	For x = 1 To UBound(RampBalls)
		If Not IsEmpty(RampBalls(x,1) ) Then
			If BallVel(RampBalls(x,0) ) > 1 Then ' if ball is moving, play rolling sound
				If RampType(x) Then
					PlaySound("RampLoop" & x), - 1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), - 1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2) = RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
			End If
			If RampBalls(x,0).Z < 30 And RampBalls(x, 2) > RampMinLoops Then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		End If
	Next
	If Not RampBalls(0,0) Then RampRoll.enabled = 0
End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
Sub tbWR_Timer()	'debug textbox
	Me.text = "on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbNewLine & _
	"1 " & TypeName(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbNewLine & _
	"2 " & TypeName(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbNewLine & _
	"3 " & TypeName(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbNewLine & _
	"4 " & TypeName(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbNewLine & _
	"5 " & TypeName(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbNewLine & _
	"6 " & TypeName(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbNewLine & _
	" "
End Sub

Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
	BallPitch = pSlope(BallVel(ball), 1, - 1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, - 4000, 60, 7000)
End Function

Sub RandomSoundRampStop(obj)
	Select Case Int(rnd*3)
		Case 0: PlaySoundAtVol "wireramp_stop1", obj, 0.2*VolumeDial:PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 1: PlaySoundAtVol "wireramp_stop2", obj, 0.2*VolumeDial:PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 2: PlaySoundAtVol "wireramp_stop3", obj, 0.2*VolumeDial:PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
	End Select
End Sub

'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************





'******************************************************
' 	ZFLE:  FLEEP MECHANICAL SOUNDS
'******************************************************

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.
' Create the following new collections:
'	 Metals (all metal objects, metal walls, metal posts, metal wire guides)
'	 Apron (the apron walls and plunger wall)
'	 Walls (all wood or plastic walls)
'	 Rollovers (wire rollover triggers, star triggers, or button triggers)
'	 Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
'	 Gates (plate gates)
'	 GatesWire (wire gates)
'	 Rubbers (all rubbers including posts, sleeves, pegs, and bands)
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
' Tutorial videos by Apophis
' Audio : Adding Fleep Part 1					https://youtu.be/rG35JVHxtx4?si=zdN9W4cZWEyXbOz_
' Audio : Adding Fleep Part 2					https://youtu.be/dk110pWMxGo?si=2iGMImXXZ0SFKVCh
' Audio : Adding Fleep Part 3					https://youtu.be/ESXWGJZY_EI?si=6D20E2nUM-xAw7xy


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1					  'volume level; range [0, 1]
NudgeLeftSoundLevel = 1				 'volume level; range [0, 1]
NudgeRightSoundLevel = 1				'volume level; range [0, 1]
NudgeCenterSoundLevel = 1			   'volume level; range [0, 1]
StartButtonSoundLevel = 0.1			 'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr   'volume level; range [0, 1]
PlungerPullSoundLevel = 1			   'volume level; range [0, 1]
RollingSoundFactor = .8

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010		'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635		'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0					   'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45					'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel		'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel	   'sound helper; not configurable
SlingshotSoundLevel = 0.95					  'volume level; range [0, 1]
BumperSoundFactor = 4.25						'volume multiplier; must not be zero
KnockerSoundLevel = 1						   'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2		  'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055 / 5			 'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075 / 5			   'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075 / 5			'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025		   'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025		   'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8	  'volume level; range [0, 1]
WallImpactSoundFactor = 0.075				   'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075 / 3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5 / 5			'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10	 'volume multiplier; must not be zero
DTSoundLevel = 0.25				 'volume multiplier; must not be zero
RolloverSoundLevel = 0.25		   'volume level; range [0, 1]
SpinnerSoundLevel = 0.5			 'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor

DrainSoundLevel = 0.8				   'volume level; range [0, 1]
BallReleaseSoundLevel = 1			   'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2	'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015	 'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025 / 5			 'volume multiplier; must not be zero

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
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, - 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
	PlaySound playsoundparams, - 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
	PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	PlaySound soundname, 1,min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	PlaySound soundname, 1,min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.y * 2 / tableheight - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioFade = CSng(tmp ^ 10)
	Else
		AudioFade = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.x * 2 / tablewidth - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioPan = CSng(tmp ^ 10)
	Else
		AudioPan = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = CSng(BallVel(ball) ^ 2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = CSng((ball.velz) ^ 2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = Int(Sqr((ball.VelX ^ 2) + (ball.VelY ^ 2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * CSng(BallVel(ball) ^ 3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^ 2 * 15
End Function

Function RndInt(min, max) ' Sets a random number integer between min and max
	RndInt = Int(Rnd() * (max - min + 1) + min)
End Function

Function RndNum(min, max) ' Sets a random number between min and max
	RndNum = Rnd() * (max - min) + min
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////

Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeLeftSoundLevel * VolumeDial, - 0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
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
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd * 11) + 1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd * 7) + 1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd * 10) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd * 8) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
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
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd * 7) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd * 8) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm / 10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm / 10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd * 7) + 1), parm * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////

Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd * 4) + 1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////

Sub Rubbers_Hit(idx)
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 5 Then
		RandomSoundRubberStrong 1
	End If
	If finalspeed <= 5 Then
		RandomSoundRubberWeak()
	End If
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////

Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd * 10) + 1
		Case 1
			PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 2
			PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 3
			PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 4
			PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 5
			PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 6
			PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 7
			PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 8
			PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 9
			PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 10
			PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6 * voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////

Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd * 9) + 1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////

Sub Walls_Hit(idx)
	RandomSoundWall()
End Sub

Sub RandomSoundWall()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 5) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 4) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 3) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////

Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd * 13) + 1), Vol(ActiveBall) * MetalImpactSoundFactor
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
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Bounce_" & Int(Rnd * 2) + 1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////

Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd * 3) + 1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(ActiveBall.id) < 4) And cor.ballvely(ActiveBall.id) > 7 Then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////

Sub RandomSoundFlipperBallGuide()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd * 3) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd * 7) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////

Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 10 Then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft ActiveBall
	Else
		RandomSoundTargetHitWeak()
	End If
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////

Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd * 9) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd * 7) + 1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////

Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd * 5) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd * 2) + 1), GateSoundLevel, ActiveBall
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, ActiveBall
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)
	SoundPlayfieldGate
End Sub

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub Arch1_hit()
	If ActiveBall.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If ActiveBall.velx <  - 8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If ActiveBall.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If ActiveBall.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd * 2) + 1), SaucerLockSoundLevel, ActiveBall
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0
			PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1
			PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////

Sub OnBallBallCollision(ball1, ball2, velocity)

	FlipperCradleCollision ball1, ball2, velocity

	Dim snd
	Select Case Int(Rnd * 7) + 1
		Case 1
			snd = "Ball_Collide_1"
		Case 2
			snd = "Ball_Collide_2"
		Case 3
			snd = "Ball_Collide_3"
		Case 4
			snd = "Ball_Collide_4"
		Case 5
			snd = "Ball_Collide_5"
		Case 6
			snd = "Ball_Collide_6"
		Case 7
			snd = "Ball_Collide_7"
	End Select
	
	PlaySound (snd), 0, CSng(velocity) ^ 2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd * 6) + 1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd * 6) + 1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315  'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05	  'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025 * RelayGISoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025 * RelayGISoundLevel, obj
	End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025 * RelayFlashSoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025 * RelayFlashSoundLevel, obj
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************

'******************************************************
' 					LUT
'******************************************************
Sub SetLUT  
	Table1.ColorGradeImage = "LUT" & LUTset
End sub 

Sub LUTBox_Timer
	LUTBox.TimerEnabled = 0 
	LUTBox.Visible = 0
End Sub

Sub ShowLUT
	LUTBox.visible = 1
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
  		Case 11: LUTBox.text = "Tomate Washed Out"
        Case 12: LUTBox.text = "VPW Original 1on1"
        Case 13: LUTBox.text = "Bassgeige"
        Case 14: LUTBox.text = "Black Light"   
        Case 15: LUTBox.text = "B&W Comic Book"        
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

	if LUTset = "" then LUTset = 0 'failsafe

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "TFTCLUT.txt",True)
	ScoreFile.WriteLine LUTset
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub
Sub LoadLUT
	Dim FileObj, ScoreFile, TextStr
	dim rLine

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		LUTset=12
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "TFTCLUT.txt") then
		LUTset=12
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "TFTCLUT.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		rLine = TextStr.ReadLine
		If rLine = "" then
			LUTset=12
			Exit Sub
		End if
		LUTset = int (rLine) 
		Set ScoreFile = Nothing
	    Set FileObj = Nothing
End Sub

Sub Table1_exit()
	SaveLUT
'	Controller.Stop
    If Not FlexDMD is Nothing Then
		FlexDMD.Show = False
		FlexDMD.Run = False
		FlexDMD = NULL
    End If
End sub

'******************************************************
' 				   PRIM ANIMATIONS			
'******************************************************
	Sub TriggerSmoke1_hit()
		dim finalspeed
		finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
        If finalspeed > 20 then                
        RotDisc1Step = 28 : Disc1Timer.Enabled = 1 : RotDisc2Step = 28 : Disc2Timer.Enabled = 1
		FlashForMs Light045, 4600, 50, 0
        End if
        If finalspeed <= 20 then
        RotDisc1Step = 14 : Disc1Timer.Enabled = 1 : RotDisc2Step = 14 : Disc2Timer.Enabled = 1
		FlashForMs Light045, 2300, 50, 0    
        End If
		
		If SmokeAnimation=1 Then
			PhilliesSmokeTimer.enabled = 1
			PhilliesSmoke.Visible = 1
			PhilliesSmokeAnimation 0, 20
		End If
	End Sub

	Sub TriggerSmoke3_hit()
		dim finalspeed
		finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
        If finalspeed > 20 then                
		RotDisc3Step = 28 :	Disc3Timer.Enabled = 1
		FlashForMs Light036, 4600, 50, 0 : FlashForMs Light037, 4600, 50, 0 : FlashForMs Light038, 4600, 50, 0 : FlashForMs Light046, 4600, 50, 0
        End if
        If finalspeed <= 20 then
		RotDisc3Step = 14 :	Disc3Timer.Enabled = 1
		FlashForMs Light036, 2300, 50, 0 : FlashForMs Light037, 2300, 50, 0 : FlashForMs Light038, 2300, 50, 0 : FlashForMs Light046, 2300, 50, 0
        End If
		FlashForMs Light005, 250, 50, 0
		
		If SmokeAnimation=1 Then
			BluntSmokeTimer.enabled = 1
			BluntSmoke.Visible = 1
			BluntSmokeAnimation 0, 24
		End If
	End Sub

	Sub TriggerSmoke4_hit()
		dim finalspeed
		finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
        If finalspeed > 20 then                
		RotDisc4Step = 28 :	Disc4Timer.Enabled = 1
		FlashForMs Light039, 4600, 50, 0 : FlashForMs Light040, 4600, 50, 0 : FlashForMs Light041, 4600, 50, 0 : FlashForMs Light047, 4600, 50, 0        
        End if
        If finalspeed <= 20 then
		RotDisc4Step = 14 :	Disc4Timer.Enabled = 1
		FlashForMs Light039, 2300, 50, 0 : FlashForMs Light040, 2300, 50, 0 : FlashForMs Light041, 2300, 50, 0 : FlashForMs Light047, 2300, 50, 0           
        End If
		
		If SmokeAnimation=1 Then
			SpliffSmokeTimer.enabled = 1
			SpliffSmoke.Visible = 1
			SpliffSmokeAnimation 0, 24
		End If
	End Sub

	Dim RotDisc1, RotDisc1Step   'big blunt smoke
	RotDisc1 = 0
	Sub Disc1Timer_Timer
		RotDisc1 = (RotDisc1 - RotDisc1Step)MOD 360
		disc1.Rotx = RotDisc1
		RotDisc1Step = RotDisc1Step - 0.05
		If RotDisc1Step <1 Then Disc1Timer.Enabled = 0
	End Sub
	
	Dim RotDisc2, RotDisc2Step    'big blunt cherry
	RotDisc2 = 0
	Sub Disc2Timer_Timer
		RotDisc2 = (RotDisc2 + RotDisc2Step)MOD 360
		disc2.Roty = RotDisc2
		RotDisc2Step = RotDisc2Step - 0.05
		If RotDisc2Step <1 Then Disc2Timer.Enabled = 0
	End Sub
	
	Dim RotDisc3, RotDisc3Step
	RotDisc3 = 0
	Sub Disc3Timer_Timer
		RotDisc3 = (RotDisc3 + RotDisc3Step)MOD 360
		disc3.Rotx = RotDisc3
		RotDisc3Step = RotDisc3Step - 0.05
		If RotDisc3Step <1 Then Disc3Timer.Enabled = 0
	End Sub
	
	Dim RotDisc4, RotDisc4Step
	RotDisc4 = 0
	Sub Disc4Timer_Timer
		RotDisc4 = (RotDisc4 - RotDisc4Step)MOD 360
		disc4.Rotx = RotDisc4
		RotDisc4Step = RotDisc4Step - 0.05
		If RotDisc4Step <1 Then Disc4Timer.Enabled = 0
	End Sub
	
	Dim RotDisc5, RotDisc5Step
	RotDisc5 = 0
	Sub Disc5Timer_Timer
		RotDisc5 = (RotDisc5 + RotDisc5Step)MOD 360
		disc5.RotY = RotDisc5
		RotDisc5Step = RotDisc5Step - 0.05
		If RotDisc5Step <1 Then Disc5Timer.Enabled = 0
	End Sub

	Dim RotDisc6, RotDisc6Step
	RotDisc6 = 0
	Sub Disc6Timer_Timer
		RotDisc6 = (RotDisc6 + RotDisc6Step)MOD 360
		disc6.Rotx = RotDisc6
		RotDisc6Step = RotDisc6Step - 0.05
		If RotDisc6Step <1 Then Disc6Timer.Enabled = 0
	End Sub

	Dim RotDisc7, RotDisc7Step
	RotDisc7 = 0
	Sub Disc7Timer_Timer
		RotDisc7 = (RotDisc7 - RotDisc7Step)MOD 360
		disc7.Rotx = RotDisc7
		RotDisc7Step = RotDisc7Step - 0.05
		If RotDisc7Step <1 Then Disc7Timer.Enabled = 0
	End Sub

	Dim RotDisc8, RotDisc8Step
	RotDisc8 = 0
	Sub Disc8Timer_Timer
		RotDisc8 = (RotDisc8 + RotDisc8Step)MOD 360
		disc8.Roty = RotDisc8
		RotDisc8Step = RotDisc8Step - 0.05
		If RotDisc8Step <1 Then Disc8Timer.Enabled = 0
	End Sub

	Dim RotDisc11, RotDisc11Step
	RotDisc11 = 0
	Sub Disc11Timer_Timer
		RotDisc11 = (RotDisc11 - RotDisc11Step)MOD 360
		disc11.Rotx = RotDisc11
		RotDisc11Step = RotDisc11Step - 0.05
		If RotDisc11Step <1 Then Disc11Timer.Enabled = 0
	End Sub

	Sub SmokeSpin
		RotDisc1Step = 14
		Disc1Timer.Enabled = 1
		RotDisc2Step = 14
		Disc2Timer.Enabled = 1
		FlashForMs Light045, 2300, 50, 0
		RotDisc3Step = 14
		Disc3Timer.Enabled = 1
		FlashForMs Light036, 2300, 50, 0 : FlashForMs Light037, 2300, 50, 0 : FlashForMs Light038, 2300, 50, 0 : FlashForMs Light046, 2300, 50, 0
		RotDisc4Step = 14
		Disc4Timer.Enabled = 1
		FlashForMs Light039, 2300, 50, 0 : FlashForMs Light040, 2300, 50, 0 : FlashForMs Light041, 2300, 50, 0 : FlashForMs Light047, 2300, 50, 0		
		RotDisc6Step = 14
		Disc6Timer.Enabled = 1
		RotDisc7Step = 14
		Disc7Timer.Enabled = 1
		RotDisc11Step = 14
		Disc11Timer.Enabled = 1
		FlashForMs Light048, 2300, 50, 0 : FlashForMs Light049, 2300, 50, 0
	End Sub

	Sub SmokeSpinBumper
		RotDisc6Step = 10
		Disc6Timer.Enabled = 1
		RotDisc7Step = 10
		Disc7Timer.Enabled = 1
		RotDisc11Step = 10
		Disc11Timer.Enabled = 1
	End Sub

Dim dank1Shake,dank2Shake,dank3Shake,dank4Shake,dank5Shake,dank6Shake,dank7Shake,dank8Shake,dank9Shake,dank10Shake,dank11Shake  

	Sub dank1Shaker()  'top left bud
		 dank1Shake = 6
		dank1Timer.Enabled = True
	End Sub
	'A number between 1 and 360 to calculate Sin on
	Dim dankShake1:dankShake1 = 1    '1
	'How far to move the buds; bigger numbers will increase how far away from the center it moves, smaller numbers means smaller movement
	'We could get the velocity of the ball when it hits the bumper to dynamically change this.  Harder hits = bigger shakes (and longer since it has to move farther)
	Dim dankShakeDist1:dankShakeDist1 = 6   '12
	'How fast the bud shakes; bigger numbers will make it vibrate, smaller numbers will make it move in slow motion
	Dim dankShakeSpeed1:dankShakeSpeed1 = 4   '4
	'Bigger numbers will make it wobble less as if it were connect to stiff rod, smaller numbers make it wobble longer like it was on a loose spring
	Dim dankStiff1:dankStiff1 = 0.5    '0.5
	Sub dank1Timer_Timer()
		me.Interval = 1
		dank1.TransZ = dSin(dankShake1)*dankShakeDist1
		dankShake1 = dankShake1 + dankShakeSpeed1
		If dankShake1 > 360 Then
			dankShake1 = 1
			dankShakeDist1 = dankShakeDist1 - dankStiff1
		End If
		If dankShakeDist1 <= 0 Then
			me.Enabled = 0
			dankShakeDist1 = 6   '12
		End If
	End Sub

	Sub dank2Shaker()  'top right bud
		dank2Shake = 6
		dank2Timer.Enabled = True
	End Sub
	Dim dankShake2:dankShake2 = 1    '1
	Dim dankShakeDist2:dankShakeDist2 = 6    '12
	Dim dankShakeSpeed2:dankShakeSpeed2 = 4   '4
	Dim dankStiff2:dankStiff2 = 0.5   '0.5
	Sub dank2Timer_Timer()
		me.Interval = 1
		dank2.TransZ = dSin(dankShake2)*dankShakeDist2
		dankShake2 = dankShake2 + dankShakeSpeed2
		If dankShake2 > 360 Then
			dankShake2 = 1
			dankShakeDist2 = dankShakeDist2 - dankStiff2
		End If
		If dankShakeDist2 <= 0 Then
			me.Enabled = 0
			dankShakeDist2 = 6   '12
		End If
	End Sub

	Sub dank3Shaker()    'center bud
		dank3Shake = 6
		dank3Timer.Enabled = True
	End Sub
	Dim dankShake3:dankShake3 = 1    '1
	Dim dankShakeDist3:dankShakeDist3 = 6    '12
	Dim dankShakeSpeed3:dankShakeSpeed3 = 4   '4
	Dim dankStiff3:dankStiff3 = 0.5   '0.5
	Sub dank3Timer_Timer()
		me.Interval = 1
		dank3.TransZ = dSin(dankShake3)*dankShakeDist3
		dankShake3 = dankShake3 + dankShakeSpeed3
		If dankShake3 > 360 Then
			dankShake3 = 1
			dankShakeDist3 = dankShakeDist3 - dankStiff3
		End If
		If dankShakeDist3 <= 0 Then
			me.Enabled = 0
			dankShakeDist3 = 6   '12
		End If
	End Sub

	Sub dank4Shaker()    'small mask
		dank4Shake = 6
		dank4Timer.Enabled = True
	End Sub
	Dim dankShake4:dankShake4 = 1    '1
	Dim dankShakeDist4:dankShakeDist4 = 5    '12
	Dim dankShakeSpeed4:dankShakeSpeed4 = 4   '4
	Dim dankStiff4:dankStiff4 = 0.5   '0.5
	Sub dank4Timer_Timer()
		me.Interval = 1
		dank4.TransZ = dSin(dankShake4)*dankShakeDist4
		dankShake4 = dankShake4 + dankShakeSpeed4
		If dankShake4 > 360 Then
			dankShake4 = 1
			dankShakeDist4 = dankShakeDist4 - dankStiff4
		End If
		If dankShakeDist4 <= 0 Then
			me.Enabled = 0
			dankShakeDist4 = 5   '12
		End If
	End Sub

	Sub dank5Shaker()  'small ruby
		dank5Shake = 6
		dank5Timer.Enabled = True
	End Sub
	Dim dankShake5:dankShake5 = 1    '1
	Dim dankShakeDist5:dankShakeDist5 = 5    '12
	Dim dankShakeSpeed5:dankShakeSpeed5 = 4   '4
	Dim dankStiff5:dankStiff5 = 0.5   '0.5
	Sub dank5Timer_Timer()
		me.Interval = 1
		dank5.TransZ = dSin(dankShake5)*dankShakeDist5
		dankShake5 = dankShake5 + dankShakeSpeed5
		If dankShake5 > 360 Then
			dankShake5 = 1
			dankShakeDist5 = dankShakeDist5 - dankStiff5
		End If
		If dankShakeDist5 <= 0 Then
			me.Enabled = 0
			dankShakeDist5 = 5   '12
		End If
	End Sub

	Sub dank6Shaker()    'big mask
		dank6Shake = 6
		dank6Timer.Enabled = True
		FlashForMs LPlastic021, 250, 50, 0
	End Sub
	Dim dankShake6:dankShake6 = 1    '1
	Dim dankShakeDist6:dankShakeDist6 = 7    '12
	Dim dankShakeSpeed6:dankShakeSpeed6 = 4   '4
	Dim dankStiff6:dankStiff6 = 0.5   '0.5
	Sub dank6Timer_Timer()
		me.Interval = 1
		dank6.TransZ = dSin(dankShake6)*dankShakeDist6
		dankShake6 = dankShake6 + dankShakeSpeed6
		If dankShake6 > 360 Then
			dankShake6 = 1
			dankShakeDist6 = dankShakeDist6 - dankStiff6
		End If
		If dankShakeDist6 <= 0 Then
			me.Enabled = 0
			dankShakeDist6 = 7   '12
		End If
	End Sub

	Sub dank7Shaker()  'big ruby
		dank7Shake = 6
		dank7Timer.Enabled = True
	End Sub
	Dim dankShake7:dankShake7 = 1    '1
	Dim dankShakeDist7:dankShakeDist7 = 7    '12
	Dim dankShakeSpeed7:dankShakeSpeed7 = 4   '4
	Dim dankStiff7:dankStiff7 = 0.5   '0.5
	Sub dank7Timer_Timer()
		me.Interval = 1
		dank7.TransZ = dSin(dankShake7)*dankShakeDist7
		dankShake7 = dankShake7 + dankShakeSpeed7
		If dankShake7 > 360 Then
			dankShake7 = 1
			dankShakeDist7 = dankShakeDist7 - dankStiff7
		End If
		If dankShakeDist7 <= 0 Then
			me.Enabled = 0
			dankShakeDist7 = 7   '12
		End If
	End Sub

	Sub dank8Shaker()   'bong
		dank8Shake = 6
		dank8Timer.Enabled = True
	End Sub
	Dim dankShake8:dankShake8 = 1    '1
	Dim dankShakeDist8:dankShakeDist8 = 5    '12
	Dim dankShakeSpeed8:dankShakeSpeed8 = 4   '4
	Dim dankStiff8:dankStiff8 = 0.5   '0.5
	Sub dank8Timer_Timer()
		me.Interval = 1
		dank8.TransZ = dSin(dankShake8)*dankShakeDist8
		dankShake8 = dankShake8 + dankShakeSpeed8
		If dankShake8 > 360 Then
			dankShake8 = 1
			dankShakeDist8 = dankShakeDist8 - dankStiff8
		End If
		If dankShakeDist8 <= 0 Then
			me.Enabled = 0
			dankShakeDist8 = 5   '12
		End If
	End Sub

	Sub dank9Shaker()   'cherry
		dank9Shake = 6
		dank9Timer.Enabled = True
	End Sub
	Dim dankShake9:dankShake9 = 1    '1
	Dim dankShakeDist9:dankShakeDist9 = 5    '12
	Dim dankShakeSpeed9:dankShakeSpeed9 = 4   '4
	Dim dankStiff9:dankStiff9 = 0.5   '0.5
	Sub dank9Timer_Timer()
		me.Interval = 1
		dank9.TransZ = dSin(dankShake9)*dankShakeDist9
		dankShake9 = dankShake9 + dankShakeSpeed9
		If dankShake9 > 360 Then
			dankShake9 = 1
			dankShakeDist9 = dankShakeDist9 - dankStiff9
		End If
		If dankShakeDist9 <= 0 Then
			me.Enabled = 0
			dankShakeDist9 = 5   '12
		End If
	End Sub

	Sub dank10Shaker()    'left metal fist
		dank10Shake = 6
		dank10Timer.Enabled = True
	End Sub
	Dim dankShake10:dankShake10 = 1    '1
	Dim dankShakeDist10:dankShakeDist10 = 7    '12
	Dim dankShakeSpeed10:dankShakeSpeed10 = 4   '4
	Dim dankStiff10:dankStiff10 = 0.5   '0.5
	Sub dank10Timer_Timer()
		me.Interval = 1
		dank10.TransZ = dSin(dankShake10)*dankShakeDist10
		dankShake10 = dankShake10 + dankShakeSpeed10
		If dankShake10 > 360 Then
			dankShake10 = 1
			dankShakeDist10 = dankShakeDist10 - dankStiff10
		End If
		If dankShakeDist10 <= 0 Then
			me.Enabled = 0
			dankShakeDist10 = 7   '12
		End If
	End Sub

	Sub dank11Shaker()    'right metal fist
		dank11Shake = 6
		dank11Timer.Enabled = True
	End Sub
	Dim dankShake11:dankShake11 = 1    '1
	Dim dankShakeDist11:dankShakeDist11 = 7    '12
	Dim dankShakeSpeed11:dankShakeSpeed11 = 4   '4
	Dim dankStiff11:dankStiff11 = 0.5   '0.5
	Sub dank11Timer_Timer()
		me.Interval = 1
		dank11.TransZ = dSin(dankShake11)*dankShakeDist11
		dankShake11 = dankShake11 + dankShakeSpeed11
		If dankShake11 > 360 Then
			dankShake11 = 1
			dankShakeDist11 = dankShakeDist11 - dankStiff11
		End If
		If dankShakeDist11 <= 0 Then
			me.Enabled = 0
			dankShakeDist11 = 7   '12
		End If
	End Sub

'*******************************************
'	ZTIM: Timers
'*******************************************

'The FrameTimer interval should be -1, so executes at the display frame rate
'The frame timer should be used to update anything visual, like some animations, shadows, etc.
'However, a lot of animations will be handled in their respective _animate subroutines.

Dim FrameTime, InitFrameTime
InitFrameTime = 0

FrameTimer.Interval = -1
Sub FrameTimer_Timer() 'The frame timer interval should be -1, so executes at the display frame rate
	FrameTime = GameTime - InitFrameTime
	InitFrameTime = GameTime	'Count frametime
	'Add animation stuff here
	RollingUpdate   		'update rolling sounds
	DoDTAnim				'handle drop target animations
	DoSTAnim				'handle stand up target animations
	FlipperVisualUpdate				'update flipper shadows and primitives
	If DynamicBallShadowsOn=1 Then DynamicBSUpdate 'update ball shadows
End Sub

'The CorTimer interval should be 10. It's sole purpose is to update the Cor calculations
CorTimer.Interval = 10
Sub CorTimer_Timer(): Cor.Update: End Sub

'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

' The "DynamicBSUpdate" sub should be called with an interval of -1 (framerate)
' Place a toggleable variable (DynamicBallShadowsOn) in user options at the top of the script
' Import the "bsrtx7" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#, with at least as many objects each as there can be balls
'
' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
'***These must be organized in order, so that lights that intersect on the table are adjacent in the collection***
' This is because the code will only project two shadows if they are coming from lights that are consecutive in the collection
' The easiest way to keep track of this is to start with the group on the left slingshot and move clockwise around the table
'	For example, if you use 6 lights: A & B on the left slingshot and C & D on the right, with E near A&B and F next to C&D, your collection would look like EBACDF
'
'																E
'	A		 C													B
'	 B		D			your collection should look like		A		because E&B, B&A, etc. intersect; but B&D or E&F do not
'  E		  F													C
'																D
'																F
'
'Update shadow options in the code to fit your table and preference

'****** End Instructions ******

' *** Example timer sub

' The frame timer interval is -1, so executes at the display frame rate
'Sub FrameTimer_Timer()
'	If DynamicBallShadowsOn=1 Then DynamicBSUpdate 'update ball shadows
'End Sub

' *** These are usually defined elsewhere (ballrolling), but activate here if necessary

'Const tnob = 10 ' total number of balls
'Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done

' *** Example "Top of Script" User Option
'Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow, 1 = enable dynamic ball shadow

' *** Shadow Options ***
Const fovY					= -2	'Offset y position under ball to account for layback or inclination (more pronounced need further back, -2 seems best for alignment at slings)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker, 1 will always be maxed even with 2 sources
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const Wideness				= 20	'Sets how wide the shadows can get (20 +5 thinness should be most realistic)
Const Thinness				= 5		'Sets minimum as ball moves away from source
' ***				 ***

Dim sourcenames, currentShadowCount

sourcenames = Array ("","","","","","","","","","","","")
currentShadowCount = Array (0,0,0,0,0,0,0,0,0,0,0,0)


dim objrtx1(20), objrtx2(20)
dim objBallShadow(20)
DynamicBSInit

sub DynamicBSInit()
	Dim iii

	for iii = 0 to tnob									'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = iii/1000 + 0.01
		objrtx1(iii).visible = 0
		'objrtx1(iii).uservalue=0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = (iii)/1000 + 0.02
		objrtx2(iii).visible = 0
		'objrtx2(iii).uservalue=0
		currentShadowCount(iii) = 0
		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		objBallShadow(iii).Z = iii/1000 + 0.04
	Next
end sub

Sub DynamicBSUpdate
	Dim falloff:	falloff = 150			'Max distance to light sources, can be changed if you have a reason
	Const AmbientShadowOn = 1				'Toggle for just the moving shadow primitive (ninuzzu's)
	Dim ShadowOpacity, ShadowOpacity2 
	Dim s, Source, LSd, b, currentMat, AnotherSource, BOT
	BOT = GetBalls

	'Hide shadow of deleted balls
	For s = UBound(BOT) + 1 to tnob
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
	Next

	If UBound(BOT) = lob - 1 Then Exit Sub		'No balls in play, exit

'The Magic happens here
	For s = lob to UBound(BOT)

' *** Normal "ambient light" ball shadow
		If AmbientShadowOn = 1 Then
			If BOT(s).X < tablewidth/2 Then
				objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/13)) + 5
			Else
				objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/13)) - 5
			End If
			objBallShadow(s).Y = BOT(s).Y + fovY

			If BOT(s).Z < 30 Then 'or BOT(s).Z > 105 Then		'Defining when (height-wise) you want ambient shadows
				objBallShadow(s).visible = 1
	'			objBallShadow(s).Z = BOT(s).Z - 25 + s/1000 + 0.04		'Uncomment if you want to add shadows to an upper/lower pf
			Else
				objBallShadow(s).visible = 0
			end if
		End If
' *** Dynamic shadows
		For Each Source in DynamicSources
			LSd=DistanceFast((BOT(s).x-Source.x),(BOT(s).y-Source.y))	'Calculating the Linear distance to the Source
			If BOT(s).Z < 30 Then 'Or BOT(s).Z > 105 Then				'Defining when (height-wise) you want dynamic shadows
				If LSd < falloff and Source.state=1 Then	    		'If the ball is within the falloff range of a light and light is on
					currentShadowCount(s) = currentShadowCount(s) + 1	'Within range of 1 or 2
					if currentShadowCount(s) = 1 Then					'1 dynamic shadow source
						sourcenames(s) = source.name
						currentMat = objrtx1(s).material
						objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01							'Uncomment if you want to add shadows to an upper/lower pf
						objrtx1(s).rotz = AnglePP(Source.x, Source.y, BOT(s).X, BOT(s).Y) + 90
						ShadowOpacity = (falloff-LSd)/falloff									'Sets opacity/darkness of shadow by distance to light
						objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness						'Scales shape of shadow with distance/opacity
						UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^2,RGB(0,0,0),0,0,False,True,0,0,0,0
						'debug.print "update1" & source.name & " at:" & ShadowOpacity

						currentMat = objBallShadow(s).material
						UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-ShadowOpacity),RGB(0,0,0),0,0,False,True,0,0,0,0

					Elseif currentShadowCount(s) = 2 Then
																'Same logic as 1 shadow, but twice
						currentMat = objrtx1(s).material
						set AnotherSource = Eval(sourcenames(s))
						objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01							'Uncomment if you want to add shadows to an upper/lower pf
						objrtx1(s).rotz = AnglePP(AnotherSource.x, AnotherSource.y, BOT(s).X, BOT(s).Y) + 90
						ShadowOpacity = (falloff-(((BOT(s).x-AnotherSource.x)^2+(BOT(s).y-AnotherSource.y)^2)^0.5))/falloff
						objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness
						UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

						currentMat = objrtx2(s).material
						objrtx2(s).visible = 1 : objrtx2(s).X = BOT(s).X : objrtx2(s).Y = BOT(s).Y + fovY
'						objrtx2(s).Z = BOT(s).Z - 25 + s/1000 + 0.02							'Uncomment if you want to add shadows to an upper/lower pf
						objrtx2(s).rotz = AnglePP(Source.x, Source.y, BOT(s).X, BOT(s).Y) + 90
						ShadowOpacity2 = (falloff-LSd)/falloff
						objrtx2(s).size_y = Wideness*ShadowOpacity2+Thinness
						UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
						'debug.print "update2: " & source.name & " at:" & ShadowOpacity & " and "  & Eval(sourcenames(s)).name & " at:" & ShadowOpacity2

						currentMat = objBallShadow(s).material
						UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
					end if
				Else
					currentShadowCount(s) = 0
				End If
			Else									'Hide dynamic shadows everywhere else
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If
		Next
	Next
End Sub


Function DistanceFast(x, y)
	dim ratio, ax, ay
	'Get absolute value of each vector
	ax = abs(x)
	ay = abs(y)
	'Create a ratio
	ratio = 1 / max(ax, ay)
	ratio = ratio * (1.29289 - (ax + ay) * ratio * 0.29289)
	if ratio > 0 then
		DistanceFast = 1/ratio
	Else
		DistanceFast = 0
	End if
end Function

'Function max(a,b)
'	if a > b then 
'		max = a
'	Else
'		max = b
'	end if
'end Function

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' ADDITIONAL LIGHT SEQUENCERS
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'RAMPS
		Sub RANDOMLIGHTSRAMPSLEFT()
			Select Case Int(Rnd * 2) + 1
				Case 1: RANDOMLIGHTSRAMPSLEFTCASE1	' + 200 = ms     
				Case 2: RANDOMLIGHTSRAMPSLEFTCASE2	' + 200 =
			End Select
		End Sub
	'CASE1
		Sub RANDOMLIGHTSRAMPSLEFTCASE1()
				RandomChangeLightsTwo
				RandomChangeLightsThree
						LightSeqSwitch.UpdateInterval = 2     
						LightSeqSwitch.Play SeqScrewLeftOn, 95, 1
						LightSeqGI2.UpdateInterval = 2     
						LightSeqGI2.Play SeqScrewLeftOn, 95, 1
				LightQueue.Add "RANDOMLIGHTSRAMPSLEFTCASE1b","RANDOMLIGHTSRAMPSLEFTCASE1b",20,100,0,0,0,false
				LightQueue.Add "RANDOMLIGHTSRAMPSLEFTCASE1c","RANDOMLIGHTSRAMPSLEFTCASE1c",20,200,0,0,0,false	
		End Sub
		Sub RANDOMLIGHTSRAMPSLEFTCASE1b()
						LightSeqSwitch2.UpdateInterval = 2     
						LightSeqSwitch2.Play SeqScrewLeftOn, 95, 1
		End Sub
		Sub RANDOMLIGHTSRAMPSLEFTCASE1c()
						LightSeqSwitch3.UpdateInterval = 2     
						LightSeqSwitch3.Play SeqScrewLeftOn, 95, 1
		End Sub	
	'CASE2
		Sub RANDOMLIGHTSRAMPSLEFTCASE2()
				RandomChangeLightsTwo
				RandomChangeLightsThree
						LightSeqSwitch.UpdateInterval = 3    
						LightSeqSwitch.Play SeqStripe2VertOn, 100, 1
						LightSeqGI2.UpdateInterval = 3    
						LightSeqGI2.Play SeqStripe2VertOn, 100, 1
				LightQueue.Add "RANDOMLIGHTSRAMPSLEFTCASE2b","RANDOMLIGHTSRAMPSLEFTCASE2b",20,100,0,0,0,false
				LightQueue.Add "RANDOMLIGHTSRAMPSLEFTCASE2c","RANDOMLIGHTSRAMPSLEFTCASE2c",20,200,0,0,0,false			
		End Sub
		Sub RANDOMLIGHTSRAMPSLEFTCASE2b()
						LightSeqSwitch2.UpdateInterval = 3    
						LightSeqSwitch2.Play SeqStripe2VertOn, 100, 1
		End Sub
		Sub RANDOMLIGHTSRAMPSLEFTCASE2c()
						LightSeqSwitch3.UpdateInterval = 3    
						LightSeqSwitch3.Play SeqStripe2VertOn, 100, 1
		End Sub
'**************************************************************
		Sub RANDOMLIGHTSRAMPSRIGHT()
			Select Case Int(Rnd * 2) + 1
				Case 1: RANDOMLIGHTSRAMPSRIGHTCASE1	' + 200 = ms     
				Case 2: RANDOMLIGHTSRAMPSRIGHTCASE2	' + 200 =
			End Select
		End Sub
	'CASE1
		Sub RANDOMLIGHTSRAMPSRIGHTCASE1()
				RandomChangeLightsTwo
				RandomChangeLightsThree
						LightSeqSwitch.UpdateInterval = 2 
						LightSeqSwitch.Play SeqScrewRightOn, 95, 1
						LightSeqGI2.UpdateInterval = 2 
						LightSeqGI2.Play SeqScrewRightOn, 95, 1
				LightQueue.Add "RANDOMLIGHTSRAMPsRIGHTCASE1b","RANDOMLIGHTSRAMPSRIGHTCASE1b",20,100,0,0,0,false
				LightQueue.Add "RANDOMLIGHTSRAMPSRIGHTCASE1c","RANDOMLIGHTSRAMPSRIGHTCASE1c",20,200,0,0,0,false		
		End Sub
		Sub RANDOMLIGHTSRAMPSRIGHTCASE1b()
						LightSeqSwitch2.UpdateInterval = 2 
						LightSeqSwitch2.Play SeqScrewRightOn, 95, 1
		End Sub
		Sub RANDOMLIGHTSRAMPSRIGHTCASE1c()
						LightSeqSwitch3.UpdateInterval = 2 
						LightSeqSwitch3.Play SeqScrewRightOn, 95, 1
		End Sub
	'CASE2
		Sub RANDOMLIGHTSRAMPSRIGHTCASE2()
				RandomChangeLightsTwo
				RandomChangeLightsThree
						LightSeqSwitch.UpdateInterval = 3    
						LightSeqSwitch.Play SeqStripe1VertOn, 100, 1
						LightSeqGI2.UpdateInterval = 3    
						LightSeqGI2.Play SeqStripe1VertOn, 100, 1
				LightQueue.Add "RANDOMLIGHTSRAMPSRIGHTCASE2b","RANDOMLIGHTSRAMPSRIGHTCASE2b",20,100,0,0,0,false
				LightQueue.Add "RANDOMLIGHTSRAMPSRIGHTCASE2c","RANDOMLIGHTSRAMPSRIGHTCASE2c",20,200,0,0,0,false		
		End Sub
		Sub RANDOMLIGHTSRAMPSRIGHTCASE2b()
						LightSeqSwitch2.UpdateInterval = 3    
						LightSeqSwitch2.Play SeqStripe1VertOn, 100, 1
		End Sub
		Sub RANDOMLIGHTSRAMPSRIGHTCASE2c()
						LightSeqSwitch3.UpdateInterval = 3    
						LightSeqSwitch3.Play SeqStripe1VertOn, 100, 1
		End Sub
'**************************************************************
'DRAIN QUICK
		Sub RANDOMLIGHTSDRAINQUICKFADE()
			Select Case Int(Rnd * 8) + 1
				Case 1: RANDOMLIGHTSDRAINQUICKCASE1	'1125 + 200 = 1325ms     
				Case 2: RANDOMLIGHTSDRAINQUICKCASE2	'1800 + 200 = 2000
				Case 3: RANDOMLIGHTSDRAINQUICKCASE3	'1620 + 200 = 1820
				Case 4: RANDOMLIGHTSDRAINQUICKCASE4	'1680 + 200 = 1880 
				Case 5: RANDOMLIGHTSDRAINQUICKCASE5	'1720 + 200 = 1920
				Case 6: RANDOMLIGHTSDRAINQUICKCASE6	'1675 + 200 = 1875
				Case 7: RANDOMLIGHTSDRAINQUICKCASE7	'1720 + 200 = 1920
				Case 8: RANDOMLIGHTSDRAINQUICKCASE8	'1605 + 200 = 1805 
			End Select
		End Sub

	'CASE1
		Sub RANDOMLIGHTSDRAINQUICKCASE1()
				RandomChangeLightsTwo
				ChangeLights3(red)
					LightSeqLogo.UpdateInterval = 9          
					LightSeqLogo.Play SeqDownOn, 125, 0 
					LightSeqGI2.UpdateInterval = 9          
					LightSeqGI2.Play SeqDownOn, 125, 0
				LightQueue.Add "RANDOMLIGHTSDRAINQUICKCASE1b","RANDOMLIGHTSDRAINQUICKCASE1b",20,100,0,0,0,false
				LightQueue.Add "RANDOMLIGHTSDRAINQUICKCASE1c","RANDOMLIGHTSDRAINQUICKCASE1c",20,200,0,0,0,false	
					FlasherMaskQuick		
		End Sub
		Sub RANDOMLIGHTSDRAINQUICKCASE1b()
					LightSeqLogo2.UpdateInterval = 9          
					LightSeqLogo2.Play SeqDownOn, 125, 0 
		End Sub
		Sub RANDOMLIGHTSDRAINQUICKCASE1c()
					LightSeqLogo3.UpdateInterval = 9          
					LightSeqLogo3.Play SeqDownOn, 125, 0 		
		End Sub
	'CASE2
		Sub RANDOMLIGHTSDRAINQUICKCASE2()
				RandomChangeLightsTwo
				ChangeLights3(red)
					LightSeqLogo.UpdateInterval = 3         
					LightSeqLogo.Play SeqDiagDownRightOn, 50, 1 
					LightSeqLogo.UpdateInterval = 3 
					LightSeqLogo.Play SeqDiagDownLeftOn, 50, 1 
					LightSeqGI2.UpdateInterval = 3         
					LightSeqGI2.Play SeqDiagDownRightOn, 50, 1 
					LightSeqGI2.UpdateInterval = 3 
					LightSeqGI2.Play SeqDiagDownLeftOn, 50, 1 
				LightQueue.Add "RANDOMLIGHTSDRAINQUICKCASE2b","RANDOMLIGHTSDRAINQUICKCASE2b",20,100,0,0,0,false
				LightQueue.Add "RANDOMLIGHTSDRAINQUICKCASE2c","RANDOMLIGHTSDRAINQUICKCASE2c",20,200,0,0,0,false		
					FlasherMaskQuick	
		End Sub
		Sub RANDOMLIGHTSDRAINQUICKCASE2b() 
					LightSeqLogo2.UpdateInterval = 3         
					LightSeqLogo2.Play SeqDiagDownRightOn, 50, 1 
					LightSeqLogo2.UpdateInterval = 3 
					LightSeqLogo2.Play SeqDiagDownLeftOn, 50, 1 
		End Sub
		Sub RANDOMLIGHTSDRAINQUICKCASE2c()
					LightSeqLogo3.UpdateInterval = 3         
					LightSeqLogo3.Play SeqDiagDownRightOn, 50, 1 
					LightSeqLogo3.UpdateInterval = 3 
					LightSeqLogo3.Play SeqDiagDownLeftOn, 50, 1 
		End Sub
	'CASE3
		Sub RANDOMLIGHTSDRAINQUICKCASE3()
				RandomChangeLightsTwo
				ChangeLights3(red)
					LightSeqLogo.UpdateInterval = 3         
					LightSeqLogo.Play SeqFanRightDownOn, 90, 1 
					LightSeqLogo.UpdateInterval = 3         
					LightSeqLogo.Play SeqFanLeftUpOn, 90, 1 

					LightSeqGI2.UpdateInterval = 3         
					LightSeqGI2.Play SeqFanRightDownOn, 90, 1 
					LightSeqGI2.UpdateInterval = 3         
					LightSeqGI2.Play SeqFanLeftUpOn, 90, 1 
					LightQueue.Add "RANDOMLIGHTSDRAINQUICKCASE3b","RANDOMLIGHTSDRAINQUICKCASE3b",20,100,0,0,0,false
					LightQueue.Add "RANDOMLIGHTSDRAINQUICKCASE3c","RANDOMLIGHTSDRAINQUICKCASE3c",20,200,0,0,0,false	
					FlasherMaskQuick			
		End Sub
		Sub RANDOMLIGHTSDRAINQUICKCASE3b()
					LightSeqLogo2.UpdateInterval = 3         
					LightSeqLogo2.Play SeqFanRightDownOn, 90, 1 
					LightSeqLogo2.UpdateInterval = 3         
					LightSeqLogo2.Play SeqFanLeftUpOn, 90, 1  
		End Sub
		Sub RANDOMLIGHTSDRAINQUICKCASE3c()
					LightSeqLogo3.UpdateInterval = 3         
					LightSeqLogo3.Play SeqFanRightDownOn, 90, 1 
					LightSeqLogo3.UpdateInterval = 3         
					LightSeqLogo3.Play SeqFanLeftUpOn, 90, 1 
		End Sub
	'CASE4
		Sub RANDOMLIGHTSDRAINQUICKCASE4()
				RandomChangeLightsTwo
				ChangeLights3(red)
					LightSeqLogo.UpdateInterval = 4
					LightSeqLogo.Play SeqCircleOutOn, 110, 1 
					LightSeqLogo.UpdateInterval = 4
					LightSeqLogo.Play SeqCircleInOn, 110, 1
					LightSeqGI2.UpdateInterval = 4
					LightSeqGI2.Play SeqCircleOutOn, 110, 1 
					LightSeqGI2.UpdateInterval = 4
					LightSeqGI2.Play SeqCircleInOn, 110, 1
					LightQueue.Add "RANDOMLIGHTSDRAINQUICKCASE4b","RANDOMLIGHTSDRAINQUICKCASE4b",20,100,0,0,0,false
					LightQueue.Add "RANDOMLIGHTSDRAINQUICKCASE4c","RANDOMLIGHTSDRAINQUICKCASE4c",20,200,0,0,0,false	
					FlasherMaskQuick		
		End Sub
		Sub RANDOMLIGHTSDRAINQUICKCASE4b()
					LightSeqLogo2.UpdateInterval = 4
					LightSeqLogo2.Play SeqCircleOutOn, 110, 1
					LightSeqLogo2.UpdateInterval = 4
					LightSeqLogo2.Play SeqCircleInOn, 110, 1
		End Sub
		Sub RANDOMLIGHTSDRAINQUICKCASE4c()
					LightSeqLogo3.UpdateInterval = 4
					LightSeqLogo3.Play SeqCircleOutOn, 110, 1 
					LightSeqLogo3.UpdateInterval = 4
					LightSeqLogo3.Play SeqCircleInOn, 110, 1	
		End Sub
	'CASE5
		Sub RANDOMLIGHTSDRAINQUICKCASE5()
				RandomChangeLightsTwo
				ChangeLights3(red)
					LightSeqLogo.UpdateInterval = 4
					LightSeqLogo.Play SeqClockRightOn, 70, 1
					LightSeqGI2.UpdateInterval = 4
					LightSeqGI2.Play SeqClockRightOn, 70, 1
					LightQueue.Add "RANDOMLIGHTSDRAINQUICKCASE5b","RANDOMLIGHTSDRAINQUICKCASE5b",20,100,0,0,0,false
					LightQueue.Add "RANDOMLIGHTSDRAINQUICKCASE5c","RANDOMLIGHTSDRAINQUICKCASE5c",20,200,0,0,0,false		
					FlasherMaskQuick	
		End Sub
		Sub RANDOMLIGHTSDRAINQUICKCASE5b()
					LightSeqLogo2.UpdateInterval = 4
					LightSeqLogo2.Play SeqClockRightOn, 70, 1
		End Sub
		Sub RANDOMLIGHTSDRAINQUICKCASE5c()
					LightSeqLogo3.UpdateInterval = 4
					LightSeqLogo3.Play SeqClockRightOn, 70, 1
		End Sub
	'CASE6
		Sub RANDOMLIGHTSDRAINQUICKCASE6()
				RandomChangeLightsTwo
				ChangeLights3(red)
					LightSeqLogo.UpdateInterval = 100          
					LightSeqLogo.Play SeqRandom, 5,, 1650 
					LightSeqGI2.UpdateInterval = 100          
					LightSeqGI2.Play SeqRandom, 5,, 1650 
					LightQueue.Add "RANDOMLIGHTSDRAINQUICKCASE6b","RANDOMLIGHTSDRAINQUICKCASE6b",20,100,0,0,0,false
					LightQueue.Add "RANDOMLIGHTSDRAINQUICKCASE6c","RANDOMLIGHTSDRAINQUICKCASE6c",20,200,0,0,0,false	
					FlasherMaskQuick 		
		End Sub
		Sub RANDOMLIGHTSDRAINQUICKCASE6b()
					LightSeqLogo2.UpdateInterval = 100          
					LightSeqLogo2.Play SeqRandom, 5,, 1650
		End Sub
		Sub RANDOMLIGHTSDRAINQUICKCASE6c()
					LightSeqLogo3.UpdateInterval = 100          
					LightSeqLogo3.Play SeqRandom, 5,, 1650
		End Sub
	'CASE7
		Sub RANDOMLIGHTSDRAINQUICKCASE7()
				RandomChangeLightsTwo
				ChangeLights3(red)
					LightSeqLogo.UpdateInterval = 4
					LightSeqLogo.Play SeqClockLeftOn, 70, 1
					LightSeqGI2.UpdateInterval = 4
					LightSeqGI2.Play SeqClockLeftOn, 70, 1
					LightQueue.Add "RANDOMLIGHTSDRAINQUICKCASE7b","RANDOMLIGHTSDRAINQUICKCASE7b",20,100,0,0,0,false
					LightQueue.Add "RANDOMLIGHTSDRAINQUICKCASE7c","RANDOMLIGHTSDRAINQUICKCASE7c",20,200,0,0,0,false	
					FlasherMaskQuick	
		End Sub
		Sub RANDOMLIGHTSDRAINQUICKCASE7b()
					LightSeqLogo2.UpdateInterval = 4
					LightSeqLogo2.Play SeqClockLeftOn, 70, 1
		End Sub
		Sub RANDOMLIGHTSDRAINQUICKCASE7c()
					LightSeqLogo3.UpdateInterval = 4
					LightSeqLogo3.Play SeqClockLeftOn, 70, 1
		End Sub
	'CASE8
		Sub RANDOMLIGHTSDRAINQUICKCASE8()
				RandomChangeLightsTwo
				ChangeLights3(red)
					LightSeqLogo.UpdateInterval = 3         
					LightSeqLogo.Play SeqArcTopRightUpOn, 85, 1 
					LightSeqLogo.UpdateInterval = 3 
					LightSeqLogo.Play SeqArcTopLeftUpOn, 85, 1 
					LightSeqLogo.UpdateInterval = 3 
					LightSeqLogo.Play SeqDownOn, 85, 1 

					LightSeqGI2.UpdateInterval = 3         
					LightSeqGI2.Play SeqArcTopRightUpOn, 85, 1 
					LightSeqGI2.UpdateInterval = 3 
					LightSeqGI2.Play SeqArcTopLeftUpOn, 85, 1 
					LightSeqGI2.UpdateInterval = 3 
					LightSeqGI2.Play SeqDownOn, 85, 1 
					LightQueue.Add "RANDOMLIGHTSDRAINQUICKCASE8b","RANDOMLIGHTSDRAINQUICKCASE8b",20,100,0,0,0,false
					LightQueue.Add "RANDOMLIGHTSDRAINQUICKCASE8c","RANDOMLIGHTSDRAINQUICKCASE8c",20,200,0,0,0,false	
					FlasherMaskQuick			
		End Sub
		Sub RANDOMLIGHTSDRAINQUICKCASE8b() 
					LightSeqLogo2.UpdateInterval = 3         
					LightSeqLogo2.Play SeqArcTopRightUpOn, 85, 1 
					LightSeqLogo2.UpdateInterval = 3 
					LightSeqLogo2.Play SeqArcTopLeftUpOn, 85, 1 
					LightSeqLogo2.UpdateInterval = 3 
					LightSeqLogo2.Play SeqDownOn, 85, 1  
		End Sub
		Sub RANDOMLIGHTSDRAINQUICKCASE8c()
					LightSeqLogo3.UpdateInterval = 3         
					LightSeqLogo3.Play SeqArcTopRightUpOn, 85, 1 
					LightSeqLogo3.UpdateInterval = 3 
					LightSeqLogo3.Play SeqArcTopLeftUpOn, 85, 1 
					LightSeqLogo3.UpdateInterval = 3 
					LightSeqLogo3.Play SeqDownOn, 85, 1 
		End Sub
'**************************************************************
'DRAIN LONG
		Sub RANDOMLIGHTSDRAINLONGFADE()
			RandomSmokeGif
			Select Case Int(Rnd * 8) + 1
				Case 1: RANDOMLIGHTSDRAINLONGCASE1	'6225 + 200 = 6425 ok   
				Case 2: RANDOMLIGHTSDRAINLONGCASE2	'6215 + 200 = 6415 ok
				Case 3: RANDOMLIGHTSDRAINLONGCASE3	'6250 + 200 = 6450 ok
				Case 4: RANDOMLIGHTSDRAINLONGCASE4	'6290 + 200 = 6490 ok
				Case 5: RANDOMLIGHTSDRAINLONGCASE5	'6300 + 200 = 6500 ok
				Case 6: RANDOMLIGHTSDRAINLONGCASE6	'6280 + 200 = 6480 ok
				Case 7: RANDOMLIGHTSDRAINLONGCASE7	'6300 + 200 = 6500 ok
				Case 8: RANDOMLIGHTSDRAINLONGCASE8	'6300 + 200 = 6500 ok
			End Select
		End Sub

	'CASE1
		Sub RANDOMLIGHTSDRAINLONGCASE1()
				RandomChangeLightsTwo
				ChangeLights3(maroon)
						LightSeqLogo.UpdateInterval = 4          
						LightSeqLogo.Play SeqDownOn, 75, 2
						LightSeqLogo.UpdateInterval = 4          
						LightSeqLogo.Play SeqUpOn, 75, 2
						LightSeqLogo.UpdateInterval = 4
						LightSeqLogo.Play SeqDiagDownRightOn, 75, 1
						LightSeqLogo.UpdateInterval = 4
						LightSeqLogo.Play SeqDiagDownLeftOn, 75, 1
						LightSeqLogo.UpdateInterval = 7
						LightSeqLogo.Play SeqCircleOutOn, 75, 1

						LightSeqGI2.UpdateInterval = 4          
						LightSeqGI2.Play SeqDownOn, 75, 2
						LightSeqGI2.UpdateInterval = 4          
						LightSeqGI2.Play SeqUpOn, 75, 2
						LightSeqGI2.UpdateInterval = 4
						LightSeqGI2.Play SeqDiagDownRightOn, 75, 1
						LightSeqGI2.UpdateInterval = 4
						LightSeqGI2.Play SeqDiagDownLeftOn, 75, 1
						LightSeqGI2.UpdateInterval = 7
						LightSeqGI2.Play SeqCircleOutOn, 75, 1
						LightQueue.Add "RANDOMLIGHTSDRAINLONGCASE1b","RANDOMLIGHTSDRAINLONGCASE1b",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSDRAINLONGCASE1c","RANDOMLIGHTSDRAINLONGCASE1c",20,200,0,0,0,false	
						FlasherMaskLong			
		End Sub
		Sub RANDOMLIGHTSDRAINLONGCASE1b()
						LightSeqLogo2.UpdateInterval = 4          
						LightSeqLogo2.Play SeqDownOn, 75, 2
						LightSeqLogo2.UpdateInterval = 4          
						LightSeqLogo2.Play SeqUpOn, 75, 2
						LightSeqLogo2.UpdateInterval = 4
						LightSeqLogo2.Play SeqDiagDownRightOn, 75, 1
						LightSeqLogo2.UpdateInterval = 4
						LightSeqLogo2.Play SeqDiagDownLeftOn, 75, 1
						LightSeqLogo2.UpdateInterval = 7
						LightSeqLogo2.Play SeqCircleOutOn, 75, 1
		End Sub
		Sub RANDOMLIGHTSDRAINLONGCASE1c()
						LightSeqLogo3.UpdateInterval = 4          
						LightSeqLogo3.Play SeqDownOn, 75, 2
						LightSeqLogo3.UpdateInterval = 4          
						LightSeqLogo3.Play SeqUpOn, 75, 2
						LightSeqLogo3.UpdateInterval = 4
						LightSeqLogo3.Play SeqDiagDownRightOn, 75, 1
						LightSeqLogo3.UpdateInterval = 4
						LightSeqLogo3.Play SeqDiagDownLeftOn, 75, 1
						LightSeqLogo3.UpdateInterval = 7
						LightSeqLogo3.Play SeqCircleOutOn, 75, 1
		End Sub
	'CASE 2
		Sub RANDOMLIGHTSDRAINLONGCASE2()
				RandomChangeLightsTwo
				ChangeLights3(maroon)
						LightSeqLogo.UpdateInterval = 8
						LightSeqLogo.Play SeqRightOn, 75, 1
						LightSeqLogo.UpdateInterval = 8
						LightSeqLogo.Play SeqLeftOn, 75, 1
						LightSeqLogo.UpdateInterval = 8
						LightSeqLogo.Play SeqRightOn, 75, 1
						LightSeqLogo.UpdateInterval = 8
						LightSeqLogo.Play SeqLeftOn, 75, 1
						LightSeqLogo.UpdateInterval = 4          
						LightSeqLogo.Play SeqFanRightDownOn, 75, 1  
						LightSeqLogo.UpdateInterval = 4          
						LightSeqLogo.Play SeqFanLeftUpOn, 75, 1  
						LightSeqLogo.UpdateInterval = 5          
						LightSeqLogo.Play SeqDownOn, 75, 2

						LightSeqGI2.UpdateInterval = 8
						LightSeqGI2.Play SeqRightOn, 75, 1
						LightSeqGI2.UpdateInterval = 8
						LightSeqGI2.Play SeqLeftOn, 75, 1
						LightSeqGI2.UpdateInterval = 8
						LightSeqGI2.Play SeqRightOn, 75, 1
						LightSeqGI2.UpdateInterval = 8
						LightSeqGI2.Play SeqLeftOn, 75, 1
						LightSeqGI2.UpdateInterval = 4          
						LightSeqGI2.Play SeqFanRightDownOn, 75, 1  
						LightSeqGI2.UpdateInterval = 4          
						LightSeqGI2.Play SeqFanLeftUpOn, 75, 1  
						LightSeqGI2.UpdateInterval = 5          
						LightSeqGI2.Play SeqDownOn, 75, 2
						LightQueue.Add "RANDOMLIGHTSDRAINLONGCASE2b","RANDOMLIGHTSDRAINLONGCASE2b",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSDRAINLONGCASE2c","RANDOMLIGHTSDRAINLONGCASE2c",20,200,0,0,0,false	
						FlasherMaskLong			
		End Sub
		Sub RANDOMLIGHTSDRAINLONGCASE2b()
						LightSeqLogo2.UpdateInterval = 8
						LightSeqLogo2.Play SeqRightOn, 75, 1
						LightSeqLogo2.UpdateInterval = 8
						LightSeqLogo2.Play SeqLeftOn, 75, 1
						LightSeqLogo2.UpdateInterval = 8
						LightSeqLogo2.Play SeqRightOn, 75, 1
						LightSeqLogo2.UpdateInterval = 8
						LightSeqLogo2.Play SeqLeftOn, 75, 1
						LightSeqLogo2.UpdateInterval = 4          
						LightSeqLogo2.Play SeqFanRightDownOn, 75, 1  
						LightSeqLogo2.UpdateInterval = 4          
						LightSeqLogo2.Play SeqFanLeftUpOn, 75, 1  
						LightSeqLogo2.UpdateInterval = 5          
						LightSeqLogo2.Play SeqDownOn, 75, 2
		End Sub
		Sub RANDOMLIGHTSDRAINLONGCASE2c()
						LightSeqLogo3.UpdateInterval = 8
						LightSeqLogo3.Play SeqRightOn, 75, 1
						LightSeqLogo3.UpdateInterval = 8
						LightSeqLogo3.Play SeqLeftOn, 75, 1
						LightSeqLogo3.UpdateInterval = 8
						LightSeqLogo3.Play SeqRightOn, 75, 1
						LightSeqLogo3.UpdateInterval = 8
						LightSeqLogo3.Play SeqLeftOn, 75, 1
						LightSeqLogo3.UpdateInterval = 4          
						LightSeqLogo3.Play SeqFanRightDownOn, 75, 1  
						LightSeqLogo3.UpdateInterval = 4          
						LightSeqLogo3.Play SeqFanLeftUpOn, 75, 1  
						LightSeqLogo3.UpdateInterval = 5          
						LightSeqLogo3.Play SeqDownOn, 75, 2
		End Sub
	'CASE 3
		Sub RANDOMLIGHTSDRAINLONGCASE3()
				RandomChangeLightsTwo
				ChangeLights3(red)
						LightSeqLogo.UpdateInterval = 100          
						LightSeqLogo.Play SeqRandom, 7,, 6200  
						LightSeqGI2.UpdateInterval = 100          
						LightSeqGI2.Play SeqRandom, 15,, 6200
						LightQueue.Add "RANDOMLIGHTSDRAINLONGCASE3b","RANDOMLIGHTSDRAINLONGCASE3b",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSDRAINLONGCASE3c","RANDOMLIGHTSDRAINLONGCASE3c",20,200,0,0,0,false	
						FlasherMaskLong				
		End Sub
		Sub RANDOMLIGHTSDRAINLONGCASE3b()
						LightSeqLogo2.UpdateInterval = 100          
						LightSeqLogo2.Play SeqRandom, 7,, 6200  
		End Sub
		Sub RANDOMLIGHTSDRAINLONGCASE3c()
						LightSeqLogo3.UpdateInterval = 100          
						LightSeqLogo3.Play SeqRandom, 7,, 6200 
		End Sub
	'CASE 4
		Sub RANDOMLIGHTSDRAINLONGCASE4()
				RandomChangeLightsTwo
				ChangeLights3(maroon)
						LightSeqLogo.UpdateInterval = 4          
						LightSeqLogo.Play SeqDownOn, 70, 1  
						LightSeqLogo.UpdateInterval = 4
						LightSeqLogo.Play SeqCircleOutOn, 70, 1
						LightSeqLogo.UpdateInterval = 4          
						LightSeqLogo.Play SeqUpOn, 70, 1  
						LightSeqLogo.UpdateInterval = 25          
						LightSeqLogo.Play SeqDownOn, 70, 1 
 
						LightSeqGI2.UpdateInterval = 4          
						LightSeqGI2.Play SeqDownOn, 70, 1  
						LightSeqGI2.UpdateInterval = 4
						LightSeqGI2.Play SeqCircleOutOn, 70, 1
						LightSeqGI2.UpdateInterval = 4          
						LightSeqGI2.Play SeqUpOn, 70, 1  
						LightSeqGI2.UpdateInterval = 25          
						LightSeqGI2.Play SeqDownOn, 70, 1  
						LightQueue.Add "RANDOMLIGHTSDRAINLONGCASE4b","RANDOMLIGHTSDRAINLONGCASE4b",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSDRAINLONGCASE4c","RANDOMLIGHTSDRAINLONGCASE4c",20,200,0,0,0,false	
						FlasherMaskLong			
		End Sub
		Sub RANDOMLIGHTSDRAINLONGCASE4b()
						LightSeqLogo2.UpdateInterval = 4          
						LightSeqLogo2.Play SeqDownOn, 70, 1  
						LightSeqLogo2.UpdateInterval = 4
						LightSeqLogo2.Play SeqCircleOutOn, 70, 1
						LightSeqLogo2.UpdateInterval = 4          
						LightSeqLogo2.Play SeqUpOn, 70, 1  
						LightSeqLogo2.UpdateInterval = 25          
						LightSeqLogo2.Play SeqDownOn, 70, 1 
		End Sub
		Sub RANDOMLIGHTSDRAINLONGCASE4c()
						LightSeqLogo3.UpdateInterval = 4          
						LightSeqLogo3.Play SeqDownOn, 70, 1  
						LightSeqLogo3.UpdateInterval = 4
						LightSeqLogo3.Play SeqCircleOutOn, 70, 1
						LightSeqLogo3.UpdateInterval = 4          
						LightSeqLogo3.Play SeqUpOn, 70, 1  
						LightSeqLogo3.UpdateInterval = 25          
						LightSeqLogo3.Play SeqDownOn, 70, 1 
		End Sub
	'CASE 5
		Sub RANDOMLIGHTSDRAINLONGCASE5()
				RandomChangeLightsTwo
				ChangeLights3(maroon)
						LightSeqLogo.UpdateInterval = 4          
						LightSeqLogo.Play SeqDiagDownRightOn, 75, 1  
						LightSeqLogo.UpdateInterval = 4
						LightSeqLogo.Play SeqDiagDownLeftOn, 75, 1
						LightSeqLogo.UpdateInterval = 4
						LightSeqLogo.Play SeqDiagUpLeftOn, 75, 1
						LightSeqLogo.UpdateInterval = 4
						LightSeqLogo.Play SeqDiagUpRightOn, 75, 1
						LightSeqLogo.UpdateInterval = 5          
						LightSeqLogo.Play SeqCircleOutOn, 75, 1  
						
						LightSeqGI2.UpdateInterval = 4          
						LightSeqGI2.Play SeqDiagDownRightOn, 75, 1  
						LightSeqGI2.UpdateInterval = 4
						LightSeqGI2.Play SeqDiagDownLeftOn, 75, 1
						LightSeqGI2.UpdateInterval = 4
						LightSeqGI2.Play SeqDiagUpLeftOn, 75, 1
						LightSeqGI2.UpdateInterval = 4
						LightSeqGI2.Play SeqDiagUpRightOn, 75, 1
						LightSeqGI2.UpdateInterval = 5          
						LightSeqGI2.Play SeqCircleOutOn, 75, 1  
						LightQueue.Add "RANDOMLIGHTSDRAINLONGCASE5b","RANDOMLIGHTSDRAINLONGCASE5b",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSDRAINLONGCASE5c","RANDOMLIGHTSDRAINLONGCASE5c",20,200,0,0,0,false		
						FlasherMaskLong			
		End Sub
		Sub RANDOMLIGHTSDRAINLONGCASE5b()
						LightSeqLogo2.UpdateInterval = 4          
						LightSeqLogo2.Play SeqDiagDownRightOn, 75, 1  
						LightSeqLogo2.UpdateInterval = 4
						LightSeqLogo2.Play SeqDiagDownLeftOn, 75, 1
						LightSeqLogo2.UpdateInterval = 4
						LightSeqLogo2.Play SeqDiagUpLeftOn, 75, 1
						LightSeqLogo2.UpdateInterval = 4
						LightSeqLogo2.Play SeqDiagUpRightOn, 75, 1
						LightSeqLogo2.UpdateInterval = 5          
						LightSeqLogo2.Play SeqCircleOutOn, 75, 1    
		End Sub
		Sub RANDOMLIGHTSDRAINLONGCASE5c()
						LightSeqLogo3.UpdateInterval = 4          
						LightSeqLogo3.Play SeqDiagDownRightOn, 75, 1  
						LightSeqLogo3.UpdateInterval = 4
						LightSeqLogo3.Play SeqDiagDownLeftOn, 75, 1
						LightSeqLogo3.UpdateInterval = 4
						LightSeqLogo3.Play SeqDiagUpLeftOn, 75, 1
						LightSeqLogo3.UpdateInterval = 4
						LightSeqLogo3.Play SeqDiagUpRightOn, 75, 1
						LightSeqLogo3.UpdateInterval = 5          
						LightSeqLogo3.Play SeqCircleOutOn, 75, 1  
		End Sub
	'CASE 6
		Sub RANDOMLIGHTSDRAINLONGCASE6()
				RandomChangeLightsTwo
				ChangeLights3(maroon)
						LightSeqLogo.UpdateInterval = 4          
						LightSeqLogo.Play SeqFanLeftUpOn, 75, 1  
						LightSeqLogo.UpdateInterval = 4
						LightSeqLogo.Play SeqFanRightDownOn, 75, 1
						LightSeqLogo.UpdateInterval = 4
						LightSeqLogo.Play SeqFanLeftDownOn, 75, 1
						LightSeqLogo.UpdateInterval = 4
						LightSeqLogo.Play SeqFanRightUpOn, 75, 1
						LightSeqLogo.UpdateInterval = 4          
						LightSeqLogo.Play SeqStripe1VertOn, 75, 1  
						LightSeqLogo.UpdateInterval = 4
						LightSeqLogo.Play SeqStripe2VertOn, 75, 1
						LightSeqLogo.UpdateInterval = 4          
						LightSeqLogo.Play SeqCircleOutOn, 70, 1 
					
						LightSeqGI2.UpdateInterval = 4          
						LightSeqGI2.Play SeqFanLeftUpOn, 75, 1  
						LightSeqGI2.UpdateInterval = 4
						LightSeqGI2.Play SeqFanRightDownOn, 75, 1
						LightSeqGI2.UpdateInterval = 4
						LightSeqGI2.Play SeqFanLeftDownOn, 75, 1
						LightSeqGI2.UpdateInterval = 4
						LightSeqGI2.Play SeqFanRightUpOn, 75, 1
						LightSeqGI2.UpdateInterval = 4          
						LightSeqGI2.Play SeqStripe1VertOn, 75, 1  
						LightSeqGI2.UpdateInterval = 4
						LightSeqGI2.Play SeqStripe2VertOn, 75, 1 	
						LightSeqGI2.UpdateInterval = 4          
						LightSeqGI2.Play SeqCircleOutOn, 70, 1 		
						LightQueue.Add "RANDOMLIGHTSDRAINLONGCASE6b","RANDOMLIGHTSDRAINLONGCASE6b",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSDRAINLONGCASE6c","RANDOMLIGHTSDRAINLONGCASE6c",20,200,0,0,0,false			
						FlasherMaskLong				
		End Sub
		Sub RANDOMLIGHTSDRAINLONGCASE6b()
						LightSeqLogo2.UpdateInterval = 4          
						LightSeqLogo2.Play SeqFanLeftUpOn, 75, 1  
						LightSeqLogo2.UpdateInterval = 4
						LightSeqLogo2.Play SeqFanRightDownOn, 75, 1
						LightSeqLogo2.UpdateInterval = 4
						LightSeqLogo2.Play SeqFanLeftDownOn, 75, 1
						LightSeqLogo2.UpdateInterval = 4
						LightSeqLogo2.Play SeqFanRightUpOn, 75, 1
						LightSeqLogo2.UpdateInterval = 4          
						LightSeqLogo2.Play SeqStripe1VertOn, 75, 1  
						LightSeqLogo2.UpdateInterval = 4
						LightSeqLogo2.Play SeqStripe2VertOn, 75, 1
						LightSeqLogo2.UpdateInterval = 4          
						LightSeqLogo2.Play SeqCircleOutOn, 70, 1 
		End Sub
		Sub RANDOMLIGHTSDRAINLONGCASE6c()
						LightSeqLogo3.UpdateInterval = 4          
						LightSeqLogo3.Play SeqFanLeftUpOn, 75, 1  
						LightSeqLogo3.UpdateInterval = 4
						LightSeqLogo3.Play SeqFanRightDownOn, 75, 1
						LightSeqLogo3.UpdateInterval = 4
						LightSeqLogo3.Play SeqFanLeftDownOn, 75, 1
						LightSeqLogo3.UpdateInterval = 4
						LightSeqLogo3.Play SeqFanRightUpOn, 75, 1
						LightSeqLogo3.UpdateInterval = 4          
						LightSeqLogo3.Play SeqStripe1VertOn, 75, 1  
						LightSeqLogo3.UpdateInterval = 4
						LightSeqLogo3.Play SeqStripe2VertOn, 75, 1 
						LightSeqLogo3.UpdateInterval = 4          
						LightSeqLogo3.Play SeqCircleOutOn, 70, 1 
		End Sub
	'CASE 7
		Sub RANDOMLIGHTSDRAINLONGCASE7()
				RandomChangeLightsTwo
				ChangeLights3(maroon)
						LightSeqLogo.UpdateInterval = 8          
						LightSeqLogo.Play SeqStripe2HorizOn, 75, 1  
						LightSeqLogo.UpdateInterval = 4          
						LightSeqLogo.Play SeqStripe2VertOn, 75, 1 
						LightSeqLogo.UpdateInterval = 8          
						LightSeqLogo.Play SeqStripe2HorizOn, 75, 1 
						LightSeqLogo.UpdateInterval = 4          
						LightSeqLogo.Play SeqStripe2VertOn, 75, 1
						LightSeqLogo.UpdateInterval = 8          
						LightSeqLogo.Play SeqStripe2HorizOn, 75, 1 
						LightSeqLogo.UpdateInterval = 4          
						LightSeqLogo.Play SeqStripe2VertOn, 75, 1
						LightSeqLogo.UpdateInterval = 8          
						LightSeqLogo.Play SeqStripe2HorizOn, 75, 1 
 
						LightSeqGI2.UpdateInterval = 8          
						LightSeqGI2.Play SeqStripe2HorizOn, 75, 1  
						LightSeqGI2.UpdateInterval = 4          
						LightSeqGI2.Play SeqStripe2VertOn, 75, 1 
						LightSeqGI2.UpdateInterval = 8          
						LightSeqGI2.Play SeqStripe2HorizOn, 75, 1 
						LightSeqGI2.UpdateInterval = 4          
						LightSeqGI2.Play SeqStripe2VertOn, 75, 1
						LightSeqGI2.UpdateInterval = 8          
						LightSeqGI2.Play SeqStripe2HorizOn, 75, 1 
						LightSeqGI2.UpdateInterval = 4          
						LightSeqGI2.Play SeqStripe2VertOn, 75, 1
						LightSeqGI2.UpdateInterval = 8          
						LightSeqGI2.Play SeqStripe2HorizOn, 75, 1 
			
						LightQueue.Add "RANDOMLIGHTSDRAINLONGCASE7b","RANDOMLIGHTSDRAINLONGCASE7b",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSDRAINLONGCASE7c","RANDOMLIGHTSDRAINLONGCASE7c",20,200,0,0,0,false	
						FlasherMaskLong			
		End Sub
		Sub RANDOMLIGHTSDRAINLONGCASE7b()
						LightSeqLogo2.UpdateInterval = 8          
						LightSeqLogo2.Play SeqStripe2HorizOn, 75, 1  
						LightSeqLogo2.UpdateInterval = 4          
						LightSeqLogo2.Play SeqStripe2VertOn, 75, 1 
						LightSeqLogo2.UpdateInterval = 8          
						LightSeqLogo2.Play SeqStripe2HorizOn, 75, 1 
						LightSeqLogo2.UpdateInterval = 4          
						LightSeqLogo2.Play SeqStripe2VertOn, 75, 1
						LightSeqLogo2.UpdateInterval = 8          
						LightSeqLogo2.Play SeqStripe2HorizOn, 75, 1 
						LightSeqLogo2.UpdateInterval = 4          
						LightSeqLogo2.Play SeqStripe2VertOn, 75, 1
						LightSeqLogo2.UpdateInterval = 8          
						LightSeqLogo2.Play SeqStripe2HorizOn, 75, 1 				
		End Sub
		Sub RANDOMLIGHTSDRAINLONGCASE7c()
						LightSeqLogo3.UpdateInterval = 8          
						LightSeqLogo3.Play SeqStripe2HorizOn, 75, 1  
						LightSeqLogo3.UpdateInterval = 4          
						LightSeqLogo3.Play SeqStripe2VertOn, 75, 1 
						LightSeqLogo3.UpdateInterval = 8          
						LightSeqLogo3.Play SeqStripe2HorizOn, 75, 1 
						LightSeqLogo3.UpdateInterval = 4          
						LightSeqLogo3.Play SeqStripe2VertOn, 75, 1
						LightSeqLogo3.UpdateInterval = 8          
						LightSeqLogo3.Play SeqStripe2HorizOn, 75, 1 
						LightSeqLogo3.UpdateInterval = 4          
						LightSeqLogo3.Play SeqStripe2VertOn, 75, 1
						LightSeqLogo3.UpdateInterval = 8          
						LightSeqLogo3.Play SeqStripe2HorizOn, 75, 1 					  
		End Sub
	'CASE 8
		Sub RANDOMLIGHTSDRAINLONGCASE8()
				RandomChangeLightsTwo
				ChangeLights3(maroon)
						LightSeqLogo.UpdateInterval = 8          
						LightSeqLogo.Play SeqStripe1HorizOn, 75, 1  
						LightSeqLogo.UpdateInterval = 4          
						LightSeqLogo.Play SeqStripe1VertOn, 75, 1 
						LightSeqLogo.UpdateInterval = 8          
						LightSeqLogo.Play SeqStripe1HorizOn, 75, 1 
						LightSeqLogo.UpdateInterval = 4          
						LightSeqLogo.Play SeqStripe1VertOn, 75, 1
						LightSeqLogo.UpdateInterval = 8          
						LightSeqLogo.Play SeqStripe1HorizOn, 75, 1 
						LightSeqLogo.UpdateInterval = 4          
						LightSeqLogo.Play SeqStripe1VertOn, 75, 1
						LightSeqLogo.UpdateInterval = 8          
						LightSeqLogo.Play SeqStripe1HorizOn, 75, 1 
 
						LightSeqGI2.UpdateInterval = 8          
						LightSeqGI2.Play SeqStripe1HorizOn, 75, 1  
						LightSeqGI2.UpdateInterval = 4          
						LightSeqGI2.Play SeqStripe1VertOn, 75, 1 
						LightSeqGI2.UpdateInterval = 8          
						LightSeqGI2.Play SeqStripe1HorizOn, 75, 1 
						LightSeqGI2.UpdateInterval = 4          
						LightSeqGI2.Play SeqStripe1VertOn, 75, 1
						LightSeqGI2.UpdateInterval = 8          
						LightSeqGI2.Play SeqStripe1HorizOn, 75, 1 
						LightSeqGI2.UpdateInterval = 4          
						LightSeqGI2.Play SeqStripe1VertOn, 75, 1
						LightSeqGI2.UpdateInterval = 8          
						LightSeqGI2.Play SeqStripe1HorizOn, 75, 1 
			
						LightQueue.Add "RANDOMLIGHTSDRAINLONGCASE8b","RANDOMLIGHTSDRAINLONGCASE8b",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSDRAINLONGCASE8c","RANDOMLIGHTSDRAINLONGCASE8c",20,200,0,0,0,false	
						FlasherMaskLong			
		End Sub
		Sub RANDOMLIGHTSDRAINLONGCASE8b()
						LightSeqLogo2.UpdateInterval = 8          
						LightSeqLogo2.Play SeqStripe1HorizOn, 75, 1  
						LightSeqLogo2.UpdateInterval = 4          
						LightSeqLogo2.Play SeqStripe1VertOn, 75, 1 
						LightSeqLogo2.UpdateInterval = 8          
						LightSeqLogo2.Play SeqStripe1HorizOn, 75, 1 
						LightSeqLogo2.UpdateInterval = 4          
						LightSeqLogo2.Play SeqStripe1VertOn, 75, 1
						LightSeqLogo2.UpdateInterval = 8          
						LightSeqLogo2.Play SeqStripe1HorizOn, 75, 1 
						LightSeqLogo2.UpdateInterval = 4          
						LightSeqLogo2.Play SeqStripe1VertOn, 75, 1
						LightSeqLogo2.UpdateInterval = 8          
						LightSeqLogo2.Play SeqStripe1HorizOn, 75, 1 				
		End Sub
		Sub RANDOMLIGHTSDRAINLONGCASE8c()
						LightSeqLogo3.UpdateInterval = 8          
						LightSeqLogo3.Play SeqStripe1HorizOn, 75, 1  
						LightSeqLogo3.UpdateInterval = 4          
						LightSeqLogo3.Play SeqStripe1VertOn, 75, 1 
						LightSeqLogo3.UpdateInterval = 8          
						LightSeqLogo3.Play SeqStripe1HorizOn, 75, 1 
						LightSeqLogo3.UpdateInterval = 4          
						LightSeqLogo3.Play SeqStripe1VertOn, 75, 1
						LightSeqLogo3.UpdateInterval = 8          
						LightSeqLogo3.Play SeqStripe1HorizOn, 75, 1 
						LightSeqLogo3.UpdateInterval = 4          
						LightSeqLogo3.Play SeqStripe1VertOn, 75, 1
						LightSeqLogo3.UpdateInterval = 8          
						LightSeqLogo3.Play SeqStripe1HorizOn, 75, 1 					  
		End Sub
'**************************************************************
'LEFT INLANE
		Dim InlaneLightsLeft
		InlaneLightsLeft = 1
		Sub RANDOMLIGHTSINLANESLEFTFADE()
				InlaneLightsLeft = InlaneLightsLeft + 1
				Select Case InlaneLightsLeft	
				Case 1: RANDOMLIGHTSINLANESLEFTCASE1	'400 + 200 = 600ms
				Case 2: RANDOMLIGHTSINLANESLEFTCASE2	'400 + 200 = 600
				Case 3: RANDOMLIGHTSINLANESLEFTCASE3	' + 200 =
						InlaneLightsLeft = 0
			End Select
		End Sub

	'CASE1
		Sub RANDOMLIGHTSINLANESLEFTCASE1()
				RandomChangeLightsTwo
				RandomChangeLightsThree
						LightSeqCustom.UpdateInterval = 4
						LightSeqCustom.Play SeqDiagDownLeftOn, 50, 0
						LightSeqGI2.UpdateInterval = 4
						LightSeqGI2.Play SeqDiagDownLeftOn, 50, 0	
						LightQueue.Add "RANDOMLIGHTSINLANESLEFTCASE1b","RANDOMLIGHTSINLANESLEFTCASE1b",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSINLANESLEFTCASE1c","RANDOMLIGHTSINLANESLEFTCASE1c",20,200,0,0,0,false						
		End Sub
		Sub RANDOMLIGHTSINLANESLEFTCASE1b()
						LightSeqCustom2.UpdateInterval = 4
						LightSeqCustom2.Play SeqDiagDownLeftOn, 50, 0				
		End Sub
		Sub RANDOMLIGHTSINLANESLEFTCASE1c()
						LightSeqCustom3.UpdateInterval = 4
						LightSeqCustom3.Play SeqDiagDownLeftOn, 50, 0			
		End Sub
	'CASE2
		Sub RANDOMLIGHTSINLANESLEFTCASE2()
				RandomChangeLightsTwo
				RandomChangeLightsThree
						LightSeqCustom.UpdateInterval = 4
						LightSeqCustom.Play SeqDiagUpRightOn, 50, 0	
						LightSeqGI2.UpdateInterval = 4
						LightSeqGI2.Play SeqDiagUpRightOn, 50, 0			
						LightQueue.Add "RANDOMLIGHTSINLANESLEFTCASE2b","RANDOMLIGHTSINLANESLEFTCASE2b",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSINLANESLEFTCASE2c","RANDOMLIGHTSINLANESLEFTCASE2c",20,200,0,0,0,false			
		End Sub
		Sub RANDOMLIGHTSINLANESLEFTCASE2b()
						LightSeqCustom2.UpdateInterval = 4
						LightSeqCustom2.Play SeqDiagUpRightOn, 50, 0										
		End Sub
		Sub RANDOMLIGHTSINLANESLEFTCASE2c()
						LightSeqCustom3.UpdateInterval = 4
						LightSeqCustom3.Play SeqDiagUpRightOn, 50, 0						
		End Sub
	'CASE3
		Sub RANDOMLIGHTSINLANESLEFTCASE3()
				RandomChangeLightsTwo
				RandomChangeLightsThree
						LightSeqCustom.UpdateInterval = 4
						LightSeqCustom.Play SeqFanRightUpOn, 50, 1	
						LightSeqGI2.UpdateInterval = 4
						LightSeqGI2.Play SeqFanRightUpOn, 50, 1				
						LightQueue.Add "RANDOMLIGHTSINLANESLEFTCASE3b","RANDOMLIGHTSINLANESLEFTCASE3b",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSINLANESLEFTCASE3c","RANDOMLIGHTSINLANESLEFTCASE3c",20,200,0,0,0,false			
		End Sub
		Sub RANDOMLIGHTSINLANESLEFTCASE3b()
						LightSeqCustom2.UpdateInterval = 4
						LightSeqCustom2.Play SeqFanRightUpOn, 50, 1											
		End Sub
		Sub RANDOMLIGHTSINLANESLEFTCASE3c()
						LightSeqCustom3.UpdateInterval = 4
						LightSeqCustom3.Play SeqFanRightUpOn, 50, 1					
		End Sub
'**************************************************************
'RIGHT INLANE
		Dim InlaneLightsRight
		InlaneLightsRight = 1
		Sub RANDOMLIGHTSINLANESRIGHTFADE()
				InlaneLightsRight = InlaneLightsRight + 1
				Select Case InlaneLightsRight	
				Case 1: RANDOMLIGHTSINLANESRIGHTCASE1	'400 + 200 = 600ms
				Case 2: RANDOMLIGHTSINLANESRIGHTCASE2	'400 + 200 = 600
				Case 3: RANDOMLIGHTSINLANESRIGHTCASE3	'600 + 200 = 800
						InlaneLightsRight = 0
			End Select
		End Sub
	'CASE 1
		Sub RANDOMLIGHTSINLANESRIGHTCASE1()
				RandomChangeLightsTwo
				RandomChangeLightsThree
						LightSeqCustom.UpdateInterval = 4
						LightSeqCustom.Play SeqDiagDownRightOn, 50, 0
						LightSeqGI2.UpdateInterval = 4
						LightSeqGI2.Play SeqDiagDownRightOn, 50, 0
						LightQueue.Add "RANDOMLIGHTSINLANESRIGHTCASE1b","RANDOMLIGHTSINLANESRIGHTCASE1b",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSINLANESRIGHTCASE1c","RANDOMLIGHTSINLANESRIGHTCASE1c",20,200,0,0,0,false			
		End Sub
		Sub RANDOMLIGHTSINLANESRIGHTCASE1b()
						LightSeqCustom2.UpdateInterval = 4
						LightSeqCustom2.Play SeqDiagDownRightOn, 50, 0									
		End Sub
		Sub RANDOMLIGHTSINLANESRIGHTCASE1c()
						LightSeqCustom3.UpdateInterval = 4
						LightSeqCustom3.Play SeqDiagDownRightOn, 50, 0							
		End Sub	
	'CASE 2
		Sub RANDOMLIGHTSINLANESRIGHTCASE2()
				RandomChangeLightsTwo
				RandomChangeLightsThree
						LightSeqCustom.UpdateInterval = 4
						LightSeqCustom.Play SeqDiagUpLeftOn, 50, 0
						LightSeqGI2.UpdateInterval = 4
						LightSeqGI2.Play SeqDiagUpLeftOn, 50, 0				
						LightQueue.Add "RANDOMLIGHTSINLANESRIGHTCASE2b","RANDOMLIGHTSINLANESRIGHTCASE2b",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSINLANESRIGHTCASE2c","RANDOMLIGHTSINLANESRIGHTCASE2c",20,200,0,0,0,false			
		End Sub

		Sub RANDOMLIGHTSINLANESRIGHTCASE2b()
						LightSeqCustom2.UpdateInterval = 4
						LightSeqCustom2.Play SeqDiagUpLeftOn, 50, 0									
		End Sub
		Sub RANDOMLIGHTSINLANESRIGHTCASE2c()
						LightSeqCustom3.UpdateInterval = 4
						LightSeqCustom3.Play SeqDiagUpLeftOn, 50, 0							
		End Sub	
	'CASE 3
		Sub RANDOMLIGHTSINLANESRIGHTCASE3()
				RandomChangeLightsTwo
				RandomChangeLightsThree
						LightSeqCustom.UpdateInterval = 4
						LightSeqCustom.Play SeqFanLeftDownOn, 50, 1	
						LightSeqGI2.UpdateInterval = 4
						LightSeqGI2.Play SeqFanLeftDownOn, 50, 1				
						LightQueue.Add "RANDOMLIGHTSINLANESRIGHTCASE3b","RANDOMLIGHTSINLANESRIGHTCASE3b",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSINLANESRIGHTCASE3c","RANDOMLIGHTSINLANESRIGHTCASE3c",20,200,0,0,0,false				
		End Sub

		Sub RANDOMLIGHTSINLANESRIGHTCASE3b()
						LightSeqCustom2.UpdateInterval = 4
						LightSeqCustom2.Play SeqFanLeftDownOn, 50, 1										
		End Sub
		Sub RANDOMLIGHTSINLANESRIGHTCASE3c()
						LightSeqCustom3.UpdateInterval = 4
						LightSeqCustom3.Play SeqFanLeftDownOn, 50, 1								
		End Sub
'**************************************************************
'LEFT OUTLANE
		Sub RANDOMLIGHTSOUTLANESLEFTFADE()
			Select Case Int(Rnd * 3) + 1
				Case 1: RANDOMLIGHTSOUTLANESLEFTCASE1	' + 200 =ms     
				Case 2: RANDOMLIGHTSOUTLANESLEFTCASE2	'1120 + 200 = 1320
				Case 3: RANDOMLIGHTSOUTLANESLEFTCASE3	'1120 + 200 = 1320
			End Select
		End Sub
	'CASE 1
		Sub RANDOMLIGHTSOUTLANESLEFTCASE1()   '1200ms
				RandomChangeLightsTwo
				RandomChangeLightsThree
						LightSeqCustom.UpdateInterval = 5
						LightSeqCustom.Play SeqStripe2VertOn, 124, 1
						LightSeqGI2.UpdateInterval = 5
						LightSeqGI2.Play SeqStripe2VertOn, 124, 1
						LightQueue.Add "RANDOMLIGHTSOUTLANESLEFTCASE1b","RANDOMLIGHTSOUTLANESLEFTCASE1b",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSOUTLANESLEFTCASE1c","RANDOMLIGHTSOUTLANESLEFTCASE1c",20,200,0,0,0,false				
		End Sub
		Sub RANDOMLIGHTSOUTLANESLEFTCASE1b()
						LightSeqCustom2.UpdateInterval = 5
						LightSeqCustom2.Play SeqStripe2VertOn, 124, 1										
		End Sub
		Sub RANDOMLIGHTSOUTLANESLEFTCASE1c()
						LightSeqCustom3.UpdateInterval = 5
						LightSeqCustom3.Play SeqStripe2VertOn, 124, 1												
		End Sub	
	'CASE 2
		Sub RANDOMLIGHTSOUTLANESLEFTCASE2()   '1120ms
				RandomChangeLightsTwo
				RandomChangeLightsThree
						LightSeqCustom.UpdateInterval = 4
						LightSeqCustom.Play SeqFanRightUpOn, 100, 1
						LightSeqGI2.UpdateInterval = 4
						LightSeqGI2.Play SeqFanRightUpOn, 100, 1
						LightQueue.Add "RANDOMLIGHTSOUTLANESLEFTCASE2b","RANDOMLIGHTSOUTLANESLEFTCASE2b",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSOUTLANESLEFTCASE2c","RANDOMLIGHTSOUTLANESLEFTCASE2c",20,200,0,0,0,false					
		End Sub
		Sub RANDOMLIGHTSOUTLANESLEFTCASE2b()
						LightSeqCustom2.UpdateInterval = 4
						LightSeqCustom2.Play SeqFanRightUpOn, 100, 1										
		End Sub
		Sub RANDOMLIGHTSOUTLANESLEFTCASE2c()
						LightSeqCustom3.UpdateInterval = 4
						LightSeqCustom3.Play SeqFanRightUpOn, 100, 1												
		End Sub	
	'CASE 3
		Sub RANDOMLIGHTSOUTLANESLEFTCASE3()   '1120ms
				RandomChangeLightsTwo
				RandomChangeLightsThree
						LightSeqCustom.UpdateInterval = 4
						LightSeqCustom.Play SeqScrewLeftOn, 100, 1
						LightSeqGI2.UpdateInterval = 4
						LightSeqGI2.Play SeqScrewLeftOn, 100, 1
						LightQueue.Add "RANDOMLIGHTSOUTLANESLEFTCASE3b","RANDOMLIGHTSOUTLANESLEFTCASE3b",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSOUTLANESLEFTCASE3c","RANDOMLIGHTSOUTLANESLEFTCASE3c",20,200,0,0,0,false					
		End Sub
		Sub RANDOMLIGHTSOUTLANESLEFTCASE3b()
						LightSeqCustom2.UpdateInterval = 4
						LightSeqCustom2.Play SeqScrewLeftOn, 100, 1										
		End Sub
		Sub RANDOMLIGHTSOUTLANESLEFTCASE3c()
						LightSeqCustom3.UpdateInterval = 4
						LightSeqCustom3.Play SeqScrewLeftOn, 100, 1												
		End Sub	
'**************************************************************
'RIGHT OUTLANE
		Sub RANDOMLIGHTSOUTLANESRIGHTFADE()
			Select Case Int(Rnd * 3) + 1
				Case 1: RANDOMLIGHTSOUTLANESRIGHTCASE1	' + 200 =  ms     
				Case 2: RANDOMLIGHTSOUTLANESRIGHTCASE2	'1120 + 200 = 1320
				Case 3: RANDOMLIGHTSOUTLANESRIGHTCASE3	'1120 + 200 = 1320
			End Select
		End Sub
	'CASE 1
		Sub RANDOMLIGHTSOUTLANESRIGHTCASE1()
				RandomChangeLightsTwo
				RandomChangeLightsThree
						LightSeqCustom.UpdateInterval = 5
						LightSeqCustom.Play SeqStripe1VertOn, 124, 1
						LightSeqGI2.UpdateInterval = 5
						LightSeqGI2.Play SeqStripe1VertOn, 124, 1
						LightQueue.Add "RANDOMLIGHTSOUTLANESRIGHTCASE1b","RANDOMLIGHTSOUTLANESRIGHTCASE1b",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSOUTLANESRIGHTCASE1c","RANDOMLIGHTSOUTLANESRIGHTCASE1c",20,200,0,0,0,false				
		End Sub
		Sub RANDOMLIGHTSOUTLANESRIGHTCASE1b()
						LightSeqCustom2.UpdateInterval = 5
						LightSeqCustom2.Play SeqStripe1VertOn, 124, 1										
		End Sub
		Sub RANDOMLIGHTSOUTLANESRIGHTCASE1c()
						LightSeqCustom3.UpdateInterval = 5
						LightSeqCustom3.Play SeqStripe1VertOn, 124, 1												
		End Sub	
	'CASE 2
		Sub RANDOMLIGHTSOUTLANESRIGHTCASE2()
				RandomChangeLightsTwo
				RandomChangeLightsThree
						LightSeqCustom.UpdateInterval = 4
						LightSeqCustom.Play SeqFanLeftDownOn, 100, 1
						LightSeqGI2.UpdateInterval = 4
						LightSeqGI2.Play SeqFanLeftDownOn, 100, 1
						LightQueue.Add "RANDOMLIGHTSOUTLANESRIGHTCASE2b","RANDOMLIGHTSOUTLANESRIGHTCASE2b",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSOUTLANESRIGHTCASE2c","RANDOMLIGHTSOUTLANESRIGHTCASE2c",20,200,0,0,0,false		
		End Sub
		Sub RANDOMLIGHTSOUTLANESRIGHTCASE2b()
						LightSeqCustom2.UpdateInterval = 4
						LightSeqCustom2.Play SeqFanLeftDownOn, 100, 1										
		End Sub
		Sub RANDOMLIGHTSOUTLANESRIGHTCASE2c()
						LightSeqCustom3.UpdateInterval = 4
						LightSeqCustom3.Play SeqFanLeftDownOn, 100, 1					
		End Sub	
	'CASE 3
		Sub RANDOMLIGHTSOUTLANESRIGHTCASE3()
				RandomChangeLightsTwo
				RandomChangeLightsThree
						LightSeqCustom.UpdateInterval = 4
						LightSeqCustom.Play SeqScrewRightOn, 100, 1
						LightSeqGI2.UpdateInterval = 4
						LightSeqGI2.Play SeqScrewRightOn, 100, 1
						LightQueue.Add "RANDOMLIGHTSOUTLANESRIGHTCASE3b","RANDOMLIGHTSOUTLANESRIGHTCASE3b",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSOUTLANESRIGHTCASE3c","RANDOMLIGHTSOUTLANESRIGHTCASE3c",20,200,0,0,0,false			
		End Sub
		Sub RANDOMLIGHTSOUTLANESRIGHTCASE3b()
						LightSeqCustom2.UpdateInterval = 4
						LightSeqCustom2.Play SeqScrewRightOn, 100, 1										
		End Sub
		Sub RANDOMLIGHTSOUTLANESRIGHTCASE3c()
						LightSeqCustom3.UpdateInterval = 4
						LightSeqCustom3.Play SeqScrewRightOn, 100, 1					
		End Sub
'**************************************************************
'SKILL SHOT
		Sub RANDOMLIGHTSSKILLSHOTFADE()
			Select Case Int(Rnd * 6) + 1
				Case 1: RANDOMLIGHTSSKILLSHOTFADECASE1	'1600 + 300 = 1900ms   ok
				Case 2: RANDOMLIGHTSSKILLSHOTFADECASE2	'1600 + 300 = 1900     ok
				Case 3: RANDOMLIGHTSSKILLSHOTFADECASE3	'1600 + 300 = 1900     ok
				Case 4: RANDOMLIGHTSSKILLSHOTFADECASE4	'1600 + 300 = 1900     ok
				Case 5: RANDOMLIGHTSSKILLSHOTFADECASE5	'1720 + 300 = 2020     ok
				Case 6: RANDOMLIGHTSSKILLSHOTFADECASE6	'1720 + 300 = 2020     ok
			End Select
		End Sub

	'CASE 1
		Sub RANDOMLIGHTSSKILLSHOTFADECASE1()   '1001ms
				RandomChangeLightsTwo
				RandomChangeLightsThree
				LightSeqCustom.UpdateInterval = 4
				LightSeqCustom.Play SeqStripe1VertOn, 100, 1
				LightSeqCustom.UpdateInterval = 4
				LightSeqCustom.Play SeqStripe2VertOn, 100, 1
				LightSeqGI.UpdateInterval = 4
				LightSeqGI.Play SeqStripe1VertOn, 100, 1
				LightSeqGI.UpdateInterval = 4
				LightSeqGI.Play SeqStripe2VertOn, 100, 1
				LightQueue.Add "RANDOMLIGHTSSKILLSHOTFADECASE1b","RANDOMLIGHTSSKILLSHOTFADECASE1b",20,150,0,0,0,false
				LightQueue.Add "RANDOMLIGHTSSKILLSHOTFADECASE1c","RANDOMLIGHTSSKILLSHOTFADECASE1c",20,300,0,0,0,false			
		End Sub
		Sub RANDOMLIGHTSSKILLSHOTFADECASE1b()
				LightSeqCustom2.UpdateInterval = 4
				LightSeqCustom2.Play SeqStripe1VertOn, 100, 1	
				LightSeqCustom2.UpdateInterval = 4
				LightSeqCustom2.Play SeqStripe2VertOn, 100, 1				
		End Sub
		Sub RANDOMLIGHTSSKILLSHOTFADECASE1c()
				LightSeqCustom3.UpdateInterval = 4
				LightSeqCustom3.Play SeqStripe1VertOn, 100, 1
				LightSeqCustom3.UpdateInterval = 4
				LightSeqCustom3.Play SeqStripe2VertOn, 100, 1											
		End Sub	
	'CASE 2
		Sub RANDOMLIGHTSSKILLSHOTFADECASE2()   '
				RandomChangeLightsTwo
				RandomChangeLightsThree
					LightSeqCustom.UpdateInterval = 4         
					LightSeqCustom.Play SeqDownOn, 100, 1 
					LightSeqCustom.UpdateInterval = 4         
					LightSeqCustom.Play SeqUpOn, 100, 1 
					LightSeqGi.UpdateInterval = 4         
					LightSeqGi.Play SeqDownOn, 100, 1 
					LightSeqGi.UpdateInterval = 4         
					LightSeqGi.Play SeqUpOn, 100, 1 
					LightQueue.Add "RANDOMLIGHTSSKILLSHOTFADECASE2b","RANDOMLIGHTSSKILLSHOTFADECASE2b",20,150,0,0,0,false
					LightQueue.Add "RANDOMLIGHTSSKILLSHOTFADECASE2c","RANDOMLIGHTSSKILLSHOTFADECASE2c",20,300,0,0,0,false				
		End Sub
		Sub RANDOMLIGHTSSKILLSHOTFADECASE2b()
					LightSeqCustom2.UpdateInterval = 4         
					LightSeqCustom2.Play SeqDownOn, 100, 1 
					LightSeqCustom2.UpdateInterval = 4         
					LightSeqCustom2.Play SeqUpOn, 100, 1 			
		End Sub
		Sub RANDOMLIGHTSSKILLSHOTFADECASE2c()
					LightSeqCustom3.UpdateInterval = 4         
					LightSeqCustom3.Play SeqDownOn, 100, 1 
					LightSeqCustom3.UpdateInterval = 4         
					LightSeqCustom3.Play SeqUpOn, 100, 1 					
		End Sub	
	'CASE 3
		Sub RANDOMLIGHTSSKILLSHOTFADECASE3()
				RandomChangeLightsTwo
				RandomChangeLightsThree
					LightSeqCustom.UpdateInterval = 4         
					LightSeqCustom.Play SeqUpOn, 100, 1 
					LightSeqCustom.UpdateInterval = 4         
					LightSeqCustom.Play SeqDownOn, 100, 1 
					LightSeqGi.UpdateInterval = 4         
					LightSeqGi.Play SeqUpOn, 100, 1 
					LightSeqGi.UpdateInterval = 4         
					LightSeqGi.Play SeqDownOn, 100, 1 
					LightQueue.Add "RANDOMLIGHTSSKILLSHOTFADECASE3b","RANDOMLIGHTSSKILLSHOTFADECASE3b",20,150,0,0,0,false
					LightQueue.Add "RANDOMLIGHTSSKILLSHOTFADECASE3c","RANDOMLIGHTSSKILLSHOTFADECASE3c",20,300,0,0,0,false					
		End Sub
		Sub RANDOMLIGHTSSKILLSHOTFADECASE3b()
					LightSeqCustom2.UpdateInterval = 4         
					LightSeqCustom2.Play SeqUpOn, 100, 1 
					LightSeqCustom2.UpdateInterval = 4         
					LightSeqCustom2.Play SeqDownOn, 100, 1 			
		End Sub
		Sub RANDOMLIGHTSSKILLSHOTFADECASE3c()
					LightSeqCustom3.UpdateInterval = 4         
					LightSeqCustom3.Play SeqUpOn, 100, 1 
					LightSeqCustom3.UpdateInterval = 4         
					LightSeqCustom3.Play SeqDownOn, 100, 1 				
		End Sub
	'CASE 4
		Sub RANDOMLIGHTSSKILLSHOTFADECASE4()   '1001ms
				RandomChangeLightsTwo
				RandomChangeLightsThree
				LightSeqCustom.UpdateInterval = 4
				LightSeqCustom.Play SeqStripe2VertOn, 100, 1
				LightSeqCustom.UpdateInterval = 4
				LightSeqCustom.Play SeqStripe1VertOn, 100, 1
				LightSeqGI.UpdateInterval = 4
				LightSeqGI.Play SeqStripe2VertOn, 100, 1
				LightSeqGI.UpdateInterval = 4
				LightSeqGI.Play SeqStripe1VertOn, 100, 1
				LightQueue.Add "RANDOMLIGHTSSKILLSHOTFADECASE4b","RANDOMLIGHTSSKILLSHOTFADECASE4b",20,150,0,0,0,false
				LightQueue.Add "RANDOMLIGHTSSKILLSHOTFADECASE4c","RANDOMLIGHTSSKILLSHOTFADECASE4c",20,300,0,0,0,false				
		End Sub
		Sub RANDOMLIGHTSSKILLSHOTFADECASE4b()
				LightSeqCustom2.UpdateInterval = 4
				LightSeqCustom2.Play SeqStripe2VertOn, 100, 1	
				LightSeqCustom2.UpdateInterval = 4
				LightSeqCustom2.Play SeqStripe1VertOn, 100, 1				
		End Sub
		Sub RANDOMLIGHTSSKILLSHOTFADECASE4c()
				LightSeqCustom3.UpdateInterval = 4
				LightSeqCustom3.Play SeqStripe2VertOn, 100, 1
				LightSeqCustom3.UpdateInterval = 4
				LightSeqCustom3.Play SeqStripe1VertOn, 100, 1											
		End Sub	
	'CASE 5
		Sub RANDOMLIGHTSSKILLSHOTFADECASE5()   '1001ms
				RandomChangeLightsTwo
				RandomChangeLightsThree
				LightSeqCustom.UpdateInterval = 4
				LightSeqCustom.Play SeqClockRightOn, 70, 1
				LightSeqGI.UpdateInterval = 4
				LightSeqGI.Play SeqClockRightOn, 70, 1
				LightQueue.Add "RANDOMLIGHTSSKILLSHOTFADECASE5b","RANDOMLIGHTSSKILLSHOTFADECASE5b",20,150,0,0,0,false
				LightQueue.Add "RANDOMLIGHTSSKILLSHOTFADECASE5c","RANDOMLIGHTSSKILLSHOTFADECASE5c",20,300,0,0,0,false			
		End Sub
		Sub RANDOMLIGHTSSKILLSHOTFADECASE5b()
				LightSeqCustom2.UpdateInterval = 4
				LightSeqCustom2.Play SeqClockRightOn, 70, 1									
		End Sub
		Sub RANDOMLIGHTSSKILLSHOTFADECASE5c()
				LightSeqCustom3.UpdateInterval = 4
				LightSeqCustom3.Play SeqClockRightOn, 70, 1											
		End Sub	
	'CASE 6
		Sub RANDOMLIGHTSSKILLSHOTFADECASE6()   '1001ms
				RandomChangeLightsTwo
				RandomChangeLightsThree
				LightSeqCustom.UpdateInterval = 4
				LightSeqCustom.Play SeqClockLeftOn, 70, 1
				LightSeqGI.UpdateInterval = 4
				LightSeqGI.Play SeqClockLeftOn, 70, 1
				LightQueue.Add "RANDOMLIGHTSSKILLSHOTFADECASE6b","RANDOMLIGHTSSKILLSHOTFADECASE6b",20,150,0,0,0,false
				LightQueue.Add "RANDOMLIGHTSSKILLSHOTFADECASE6c","RANDOMLIGHTSSKILLSHOTFADECASE6c",20,300,0,0,0,false				
		End Sub
		Sub RANDOMLIGHTSSKILLSHOTFADECASE6b()
				LightSeqCustom2.UpdateInterval = 4
				LightSeqCustom2.Play SeqClockLeftOn, 70, 1									
		End Sub
		Sub RANDOMLIGHTSSKILLSHOTFADECASE6c()
				LightSeqCustom3.UpdateInterval = 4
				LightSeqCustom3.Play SeqClockLeftOn, 70, 1											
		End Sub	
'**************************************************************
'TARGETS CENTER
		Sub RANDOMLIGHTSTARGETSCENTERFADE()	'ms
			LightSeqGI2.stopplay
			Select Case Int(Rnd * 4) + 1
				Case 1: LightSeqGI2.UpdateInterval = 3       
						LightSeqGI2.Play SeqMiddleOutVertOn, 10, 2	
				Case 2: LightSeqGI2.UpdateInterval = 3      
						LightSeqGI2.Play SeqMiddleInVertOn, 10, 2
				Case 3: LightSeqGI2.UpdateInterval = 2   
						LightSeqGI2.Play SeqCircleOutOn, 10, 2	
				Case 4: LightSeqGI2.UpdateInterval = 2   
						LightSeqGI2.Play SeqCircleInOn, 10, 2
			End Select
		End Sub
'**************************************************************
'TARGETS
		Sub RANDOMLIGHTSTARGETSFADE()	'ms
			LightSeqGI2.stopplay
			Select Case Int(Rnd * 3) + 1
				Case 1: LightSeqGI2.UpdateInterval = 1
						LightSeqGI2.Play SeqBlinking, , 2, 10
				Case 2: LightSeqGI2.UpdateInterval = 2
						LightSeqGI2.Play SeqRightOn, 75, 1	
				Case 3: LightSeqGI2.UpdateInterval = 2
						LightSeqGI2.Play SeqLeftOn, 75, 1	
			End Select
		End Sub
'STANDUPS COMPLETED
		Sub RANDOMLIGHTSSTANDUPSCOMPLETEDLEFT()
				RandomChangeLightsTwo
				RandomChangeLightsThree
						LightSeqLogo.UpdateInterval = 8
						LightSeqLogo.Play SeqLeftOn, 75, 1
						LightSeqGI2.UpdateInterval = 8
						LightSeqGI2.Play SeqLeftOn, 75, 1
						LightQueue.Add "RANDOMLIGHTSSTANDUPSCOMPLETEDLEFTCASEb","RANDOMLIGHTSSTANDUPSCOMPLETEDLEFTCASEb",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSSTANDUPSCOMPLETEDLEFTCASEc","RANDOMLIGHTSSTANDUPSCOMPLETEDLEFTCASEc",20,200,0,0,0,false					
		End Sub
		Sub RANDOMLIGHTSSTANDUPSCOMPLETEDLEFTCASEb()
						LightSeqLogo2.UpdateInterval = 8
						LightSeqLogo2.Play SeqLeftOn, 75, 1
		End Sub
		Sub RANDOMLIGHTSSTANDUPSCOMPLETEDLEFTCASEc()
						LightSeqLogo3.UpdateInterval = 8
						LightSeqLogo3.Play SeqLeftOn, 75, 1
		End Sub

		Sub RANDOMLIGHTSSTANDUPSCOMPLETEDRIGHT()
				RandomChangeLightsTwo
				RandomChangeLightsThree
						LightSeqLogo.UpdateInterval = 8
						LightSeqLogo.Play SeqRightOn, 75, 1
						LightSeqGI2.UpdateInterval = 8
						LightSeqGI2.Play SeqRightOn, 75, 1
						LightQueue.Add "RANDOMLIGHTSSTANDUPSCOMPLETEDRIGHTCASEb","RANDOMLIGHTSSTANDUPSCOMPLETEDRIGHTCASEb",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSSTANDUPSCOMPLETEDRIGHTCASEc","RANDOMLIGHTSSTANDUPSCOMPLETEDRIGHTCASEc",20,200,0,0,0,false			
		End Sub
		Sub RANDOMLIGHTSSTANDUPSCOMPLETEDRIGHTCASEb()
						LightSeqLogo2.UpdateInterval = 8
						LightSeqLogo2.Play SeqRightOn, 75, 1
		End Sub
		Sub RANDOMLIGHTSSTANDUPSCOMPLETEDRIGHTCASEc()
						LightSeqLogo3.UpdateInterval = 8
						LightSeqLogo3.Play SeqRightOn, 75, 1
		End Sub
'KICKER
		Sub RANDOMLIGHTSKICKER()
				RandomChangeLightsTwo
				ChangeLights3(purple)
						LightSeqFlasher.UpdateInterval = 75
						LightSeqFlasher.Play SeqRandom, 2, , 7000
						LightSeqGI2.UpdateInterval = 75
						LightSeqGI2.Play SeqRandom, 2, , 7000
						LightSeqLogo2.UpdateInterval = 75
						LightSeqLogo2.Play SeqRandom, 2, , 7000
						LightSeqLogo3.UpdateInterval = 75
						LightSeqLogo3.Play SeqRandom, 2, , 7000
		End Sub

		Sub RANDOMLIGHTSKICKEREJECT()
				RandomChangeLightsTwo
				RandomChangeLightsThree
						LightSeqLogo.UpdateInterval = 7          
						LightSeqLogo.Play SeqCircleOutOn, 75, 1  
						LightSeqGI2.UpdateInterval = 7          
						LightSeqGI2.Play SeqCircleOutOn, 75, 1
						LightQueue.Add "RANDOMLIGHTSKICKEREJECTCASEb","RANDOMLIGHTSKICKEREJECTCASEb",20,100,0,0,0,false
						LightQueue.Add "RANDOMLIGHTSKICKEREJECTCASEc","RANDOMLIGHTSKICKEREJECTCASEc",20,200,0,0,0,false				
		End Sub
		Sub RANDOMLIGHTSKICKEREJECTCASEb()
						LightSeqLogo2.UpdateInterval = 7          
						LightSeqLogo2.Play SeqCircleOutOn, 75, 1  
		End Sub
		Sub RANDOMLIGHTSKICKEREJECTCASEc()
						LightSeqLogo3.UpdateInterval = 7          
						LightSeqLogo3.Play SeqCircleOutOn, 75, 1  
		End Sub
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' BONUS MISSIONS TRIGGERS/COUNTS
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	Sub BonusBumperCount
		If LSupreme003.state = 2 THEN
			bonusbumps(CurrentPlayer)=bonusbumps(CurrentPlayer)+1
			If bonusbumps(CurrentPlayer)=60 Then
				pupevent 728
				DMDBigText "VIK VAUGHN",175,0
				LSupreme003.state = 1   
				bonusbumps(CurrentPlayer)=0
				RandomBonusMissionLight
				RandomBonusMissionScore
				ScorbitBuildGameModes "VIK VAUGHN"
			End If
		End If
	End Sub
	Sub BonusLeftRampCount
		If LSupreme002.state = 2 THEN
			bonusleftramp(CurrentPlayer)=bonusleftramp(CurrentPlayer)+1
			If bonusleftramp(CurrentPlayer)=4 Then   
				pupevent 729
				DMDBigText "GEEDORAH",175,0    
				LSupreme002.state = 1   
				bonusleftramp(CurrentPlayer)=0
				RandomBonusMissionLight
				RandomBonusMissionScore
				ScorbitBuildGameModes "GEEDORAH"
			End If
		End If
	End Sub
	Sub BonusRightRampCount
		If LSupreme004.state = 2 THEN
			bonusrightramp(CurrentPlayer)=bonusrightramp(CurrentPlayer)+1
			If bonusrightramp(CurrentPlayer)=4 Then
				pupevent 730
				DMDBigText "MADVILLAIN",175,0    
				LSupreme004.state = 1 
				bonusrightramp(CurrentPlayer)=0
				RandomBonusMissionLight
				RandomBonusMissionScore
				ScorbitBuildGameModes "MAD VILLAIN"
			End If
		End If
	End Sub
	Sub BonusLeftOrbitCount
		If LSupreme001.state = 2 THEN
			bonusleftorbit(CurrentPlayer)=bonusleftorbit(CurrentPlayer)+1
			If bonusleftorbit(CurrentPlayer)=4 Then 
				pupevent 731
				DMDBigText "ZEV LOVE X",175,0   
				LSupreme001.state = 1  
				bonusleftorbit(CurrentPlayer)=0
				RandomBonusMissionLight
				RandomBonusMissionScore
				ScorbitBuildGameModes "ZEV LOVE X"
			End If
		End If
	End Sub
	Sub BonusSpinnerCount
		If LSupreme005.state = 2 THEN
			bonusspinner(CurrentPlayer)=bonusspinner(CurrentPlayer)+1
			If bonusspinner(CurrentPlayer)=4 Then
				pupevent 732
				DMDBigText "DANGERDOOM",175,0  
				LSupreme005.state = 1  
				bonusspinner(CurrentPlayer)=0
				RandomBonusMissionLight
				RandomBonusMissionScore
				ScorbitBuildGameModes "DANGER DOOM"
			End If
		End If
	End Sub
	Sub BonusCenterTargetCount
		If LStar08.state = 2 THEN
			bonuscentertarget(CurrentPlayer)=bonuscentertarget(CurrentPlayer)+1
			If bonuscentertarget(CurrentPlayer)=3 Then
				pupevent 733
				DMDBigText "JJ DOOM",175,0   
				LStar08.state = 1   
				bonuscentertarget(CurrentPlayer)=0
				RandomBonusMissionLight
				RandomBonusMissionScore
				ScorbitBuildGameModes "JJ DOOM"
			End If
		End If
	End Sub
	Sub BonusSmallTargetCount
		If LStar09.state = 2 THEN
			bonussmalltarget(CurrentPlayer)=bonussmalltarget(CurrentPlayer)+1
			If bonussmalltarget(CurrentPlayer)=4 Then 
				pupevent 734
				DMDBigText "NEHRUVIAN",175,0   
				LStar09.state = 1   
				bonussmalltarget(CurrentPlayer)=0
				RandomBonusMissionLight
				RandomBonusMissionScore
				ScorbitBuildGameModes "NEHRUVIAN"
			End If
		End If
	End Sub
	Sub BonusMissionCount
		If LArrow10.state = 2 THEN
			BonusMission(CurrentPlayer)=BonusMission(CurrentPlayer)+1
			If BonusMission(CurrentPlayer)=2 Then
				pupevent 735
				Dbg "Event 935"
				DMDBigText "MYSTERY?",100,0   '3.4sec
				MysteryAliasMissionsUnlockedCallout
				BonusMission(CurrentPlayer)=0
				LArrow10.state = 1
				LArrow11.state = 1
				LSupreme001.state = 2
				LSupreme002.state = 2
				LSupreme003.state = 2
				LSupreme004.state = 2
				LSupreme005.state = 2
				LStar08.state = 2
				LStar09.state = 2
			End If
		End If
	End Sub
	Sub RandomBonusMissionLight
		MysteryBonusCallout
		LightQueue.Add "LightBonusFlash01","LightBonusFlash01",10,0,0,0,0,false
		LightQueue.Add "LightBonusFlash02","LightBonusFlash02",10,50,0,0,0,false
		LightQueue.Add "LightBonusFlash03","LightBonusFlash03",10,100,0,0,0,false
		LightQueue.Add "LightBonusFlash04","LightBonusFlash04",10,150,0,0,0,false
		LightQueue.Add "LightBonusFlash05","LightBonusFlash05",10,200,0,0,0,false
		LightQueue.Add "LightBonusFlash06","LightBonusFlash06",10,250,0,0,0,false
		LightQueue.Add "LightBonusFlash07","LightBonusFlash07",10,300,0,0,0,false
		LightQueue.Add "LightBonusFlash08","LightBonusFlash08",10,350,0,0,0,false
		LightQueue.Add "LightBonusFlash09","LightBonusFlash09",10,400,0,0,0,false
		LightQueue.Add "FlashForMs LBonus10, 5000, 10, 0","FlashForMs LBonus10, 5000, 10, 0",10,450,0,0,0,false
	End Sub
	Sub RandomBonusMissionScore
		Select Case Int(Rnd * 5) + 1
			Case 1: AddScore 100000
			Case 2: AddScore 150000
			Case 3: AddScore 200000
			Case 4: AddScore 250000
			Case 5: AddScore 300000
		End Select
	End Sub
'*****************************************************************
'	PUP POPUPS, GIFS & ANIMATIONS 
'*****************************************************************
	Dim BumperScoreFree(6)
	Sub BumperScore()		' Display a popup Animation on the backglass 
		Dim idx
		Dim idx1
		Dim idx2
		Dim size
		Dim x
		Dim y
		Dim i
		Dim Score
		Score = ""
		For i = 0 to 5
			if BumperScoreFree(i)=False then 
				BumperScoreFree(i)=True
				idx=i
				idx1=INT(RND*1)+1    'idx1=INT(RND*3)+1
				idx2=INT(RND*11)      'idx2=INT(RND*11)
				size=INT(RND*12)      'size=INT(RND*12)
				x=INT(RND*75)+12.5     'x=INT(RND*75)+12.5
				y=INT(RND*75)+8       'y=INT(RND*75)+8
	''debug.print "BumperBG" & i &" PuPOverlays\\BumperBurst"&idx1&"-"&idx2&".png {'mt':2, 'zback':1, 'width':"& 15 + size&", 'height':"& 25 + size&",'yalign':1,'xalign':1,'ypos':"&y&",'xpos':"&x&"}"
		if PupStatus = True Then
				puPlayer.LabelSet pBackglass,"BumperBG" & idx, "PuPOverlays2\\BumperBurst"&idx1&"-"&idx2&".png" ,1,"{'mt':2, 'zback':1, 'width':"& 15 + size&", 'height':"& 25 + size&",'yalign':1,'xalign':1,'ypos':"&y&",'xpos':"&x&"}"
		End If
' Merlin RTP  may need some pupDMD to get these BumperBG images to show up
	''debug.print  "BumperScore2 " & Score & "," & x & "," & y & "," & size & "," & idx & "," & idx2 & " '"
BallHandlingQueue.Add "BumperScore2","BumperScore2 """ & Score & """," & x & "," & y & "," & size & "," & idx & "," & idx2 & "",20,900,0,0,0,false
				Exit sub 
			End if 
		Next 
	End Sub
	Sub BumperScore2(Score, x, y, size, idx, idx2)		' Display a popup Animation on the backglass 
		if PupStatus = True Then
			puPlayer.LabelSet pBackglass,"BumperBG"  & idx, "PuPOverlays2\\BumperBurst0-"&idx2&".png" ,1,"{'mt':2, 'zback':1, 'width':"& 15 + size&", 'height':"& 25 + size&",'yalign':1,'xalign':1,'ypos':"&y&",'xpos':"&x&"}"
		End If
' Merlin RTP  may need some pupDMD to get these BumperBG images to show up
BallHandlingQueue.Add "BumperScore3","BumperScore3 " & idx & "",20,900,0,0,0,false
	End Sub 
	Sub BumperScore3(idx)		' Display a popup Animation on the backglass 
		if PupStatus = True Then
			puPlayer.LabelSet pBackglass,"BumperBG"  & idx, "PuPOverlays2\\Clear.png" ,1,""
		End If
' Merlin RTP  may need some pupDMD to get these BumperBG images to show up
		BumperScoreFree(idx)=False
	End Sub 

	Sub ClearSmoke
		if PuPStatus = False Then Exit Sub
' Merlin RTP  may need some pupDMD to get these BumperBG images to show up
		puPlayer.LabelSet pBackglass,"BumperBG"  & 0, "PuPOverlays2\\Clear.png" ,1,""
		puPlayer.LabelSet pBackglass,"BumperBG"  & 1, "PuPOverlays2\\Clear.png" ,1,""
		puPlayer.LabelSet pBackglass,"BumperBG"  & 2, "PuPOverlays2\\Clear.png" ,1,""
		puPlayer.LabelSet pBackglass,"BumperBG"  & 3, "PuPOverlays2\\Clear.png" ,1,""
		puPlayer.LabelSet pBackglass,"BumperBG"  & 4, "PuPOverlays2\\Clear.png" ,1,""
		puPlayer.LabelSet pBackglass,"BumperBG"  & 5, "PuPOverlays2\\Clear.png" ,1,""
		BumperScoreFree(0)=False
		BumperScoreFree(1)=False
		BumperScoreFree(2)=False
		BumperScoreFree(3)=False
		BumperScoreFree(4)=False
		BumperScoreFree(5)=False
		puPlayer.LabelSet pBackglass,"Smoke", "PuPOverlays2\\clear.png" ,1,""
	End Sub
	Sub ClearSmoke_Hit(idx)
		if PuPStatus = False Then Exit Sub
		ClearSmoke
	End Sub

	Sub RandomSmokeGif
		if PuPStatus = False Then Exit Sub
		Dim image
		PupAniFolder="gifs"
		image=PupAniFolder&"\\"
		Select Case Int(Rnd * 1) + 1
			Case 1:	image=image&"smoke6.gif"
					GeneralPupQueue.Add "ClearSmoke","ClearSmoke",20,1600,0,0,0,false
		End Select
' Merlin RTP  may need some pupDMD to get these BumperBG images to show up
		puPlayer.LabelSet pBackglass,"Smoke", image ,1,"{'mt':2,'color':111111, 'anigif':100, 'width':100, 'height':100,'yalign':0,'xalign':0,'ypos':0,'xpos':0}"
	End Sub

	Dim HPos, HPosEnd
'Bong Smoke
	Sub Bongtimer_timer() 
		Bong.imageA = "bongs"& Hpos
		If Hpos < HposEnd Then
			HPos = HPos + 1
		Else
			BongTimer.enabled = 0
			Bong.Visible = 0
		End If
	End Sub 
	Sub BongAnimation(FrameStart, FrameEnd)
		HPos = FrameStart         
		HPosEnd = FrameEnd
		BongTimer.enabled = 1
		Bong.Visible = 1
	End Sub
'Left Outlane Blunt
	Sub BluntSmokeTimer_timer() 
		BluntSmoke.imageA = "bluntsmoke"& Hpos
		If Hpos < HposEnd Then
			HPos = HPos + 1
		Else
			BluntSmokeTimer.enabled = 0
			BluntSmoke.Visible = 0
		End If
	End Sub 
	Sub BluntSmokeAnimation(FrameStart, FrameEnd)
		HPos = FrameStart         
		HPosEnd = FrameEnd
		BluntSmokeTimer.enabled = 1
		BluntSmoke.Visible = 1
	End Sub
'Right Outlane Blunt
	Sub SpliffSmokeTimer_timer() 
		SpliffSmoke.imageA = "spliffsmoke"& Hpos
		If Hpos < HposEnd Then
			HPos = HPos + 1
		Else
			SpliffSmokeTimer.enabled = 0
			SpliffSmoke.Visible = 0
		End If
	End Sub 
	Sub SpliffSmokeAnimation(FrameStart, FrameEnd)
		HPos = FrameStart         
		HPosEnd = FrameEnd
		SpliffSmokeTimer.enabled = 1
		SpliffSmoke.Visible = 1
	End Sub
'Right Orbit Blunt
	Sub PhilliesSmokeTimer_timer() 
		PhilliesSmoke.imageA = "bongs"& Hpos
		If Hpos < HposEnd Then
			HPos = HPos + 1
		Else
			PhilliesSmokeTimer.enabled = 0
			PhilliesSmoke.Visible = 0
		End If
	End Sub 
	Sub PhilliesSmokeAnimation(FrameStart, FrameEnd)
		HPos = FrameStart         
		HPosEnd = FrameEnd
		PhilliesSmokeTimer.enabled = 1
		PhilliesSmoke.Visible = 1
	End Sub

'************** VR Plunger *****************
Sub TimerVRPlunger_Timer
  If PinCab_Shooter.Y < -99 then
       PinCab_Shooter.Y = PinCab_Shooter.Y + 5
  End If
End Sub

Sub TimerVRPlunger2_Timer
	PinCab_Shooter.Y = -215 + (5* Plunger.Position) -20
End Sub


'*******************************************
' Insert primitive fading and color assignment
' iaakki
'*******************************************
sub L10KScore03_animate
	pL10KScore03.blenddisablelighting = 1000 * (L10KScore03.GetInPlayIntensity / L10KScore03.Intensity)
	pL10KScore03.color = GrayedRGB(L10KScore03.colorfull)
end sub

sub L10KScore04_animate
	pL10KScore04.blenddisablelighting = 1000 * (L10KScore04.GetInPlayIntensity / L10KScore04.Intensity)
	pL10KScore04.color = GrayedRGB(L10KScore04.colorfull)
end sub

sub L10KScore05_animate
	pL10KScore05.blenddisablelighting = 1000 * (L10KScore05.GetInPlayIntensity / L10KScore05.Intensity)
	pL10KScore05.color = GrayedRGB(L10KScore05.colorfull)
end sub
'
'LStar08
'LMultiplier01
'LMultiplier02
'LMultiplier03
'LStar09



sub LStar04_animate
	pLStar04.blenddisablelighting = 1000 * (LStar04.GetInPlayIntensity / LStar04.Intensity)
	pLStar04.color = GrayedRGB(LStar04.colorfull)
end sub

sub LStar05_animate
	pLStar05.blenddisablelighting = 1000 * (LStar05.GetInPlayIntensity / LStar05.Intensity)
	pLStar05.color = GrayedRGB(LStar05.colorfull)
end sub

sub LStar06_animate
	pLStar06.blenddisablelighting = 1000 * (LStar06.GetInPlayIntensity / LStar06.Intensity)
	pLStar06.color = GrayedRGB(LStar06.colorfull)
end sub

sub LStar07_animate
	pLStar07.blenddisablelighting = 1000 * (LStar07.GetInPlayIntensity / LStar07.Intensity)
	pLStar07.color = GrayedRGB(LStar07.colorfull)
end sub


sub LStar08_animate
	pLStar08.blenddisablelighting = 1000 * (LStar08.GetInPlayIntensity / LStar08.Intensity)
	pLStar08.color = GrayedRGB(LStar08.colorfull)
end sub

sub LMultiplier01_animate
	pLMultiplier01.blenddisablelighting = 1300 * (LMultiplier01.GetInPlayIntensity / LMultiplier01.Intensity)
	pLMultiplier01.color = GrayedRGB(LMultiplier01.colorfull)
end sub

sub LMultiplier02_animate
	pLMultiplier02.blenddisablelighting = 1300 * (LMultiplier02.GetInPlayIntensity / LMultiplier02.Intensity)
	pLMultiplier02.color = GrayedRGB(LMultiplier02.colorfull)
end sub

sub LMultiplier03_animate
	pLMultiplier03.blenddisablelighting = 1300 * (LMultiplier03.GetInPlayIntensity / LMultiplier03.Intensity)
	pLMultiplier03.color = GrayedRGB(LMultiplier03.colorfull)
end sub

sub LStar09_animate
	pLStar09.blenddisablelighting = 1000 * (LStar09.GetInPlayIntensity / LStar09.Intensity)
	pLStar09.color = GrayedRGB(LStar09.colorfull)
end sub

'LBonus01
'LBonus02
'LBonus03
'LBonus04
'LBonus05
'LBonus06
'LBonus07
'LBonus08
'LBonus09
'LBonus10

sub LBonus01_animate
	pLBonus01.blenddisablelighting = 1000 * (LBonus01.GetInPlayIntensity / LBonus01.Intensity)
	pLBonus01.color = GrayedRGB(LBonus01.colorfull)
end sub

sub LBonus02_animate
	pLBonus02.blenddisablelighting = 1000 * (LBonus02.GetInPlayIntensity / LBonus02.Intensity)
	pLBonus02.color = GrayedRGB(LBonus02.colorfull)
end sub

sub LBonus03_animate
	pLBonus03.blenddisablelighting = 1000 * (LBonus03.GetInPlayIntensity / LBonus03.Intensity)
	pLBonus03.color = GrayedRGB(LBonus03.colorfull)
end sub

sub LBonus04_animate
	pLBonus04.blenddisablelighting = 1000 * (LBonus04.GetInPlayIntensity / LBonus04.Intensity)
	pLBonus04.color = GrayedRGB(LBonus04.colorfull)
end sub

sub LBonus05_animate
	pLBonus05.blenddisablelighting = 1000 * (LBonus05.GetInPlayIntensity / LBonus05.Intensity)
	pLBonus05.color = GrayedRGB(LBonus05.colorfull)
end sub

sub LBonus06_animate
	pLBonus06.blenddisablelighting = 1000 * (LBonus06.GetInPlayIntensity / LBonus06.Intensity)
	pLBonus06.color = GrayedRGB(LBonus06.colorfull)
end sub

sub LBonus07_animate
	pLBonus07.blenddisablelighting = 1000 * (LBonus07.GetInPlayIntensity / LBonus07.Intensity)
	pLBonus07.color = GrayedRGB(LBonus07.colorfull)
end sub

sub LBonus08_animate
	pLBonus08.blenddisablelighting = 1000 * (LBonus08.GetInPlayIntensity / LBonus08.Intensity)
	pLBonus08.color = GrayedRGB(LBonus08.colorfull)
end sub

sub LBonus09_animate
	pLBonus09.blenddisablelighting = 1000 * (LBonus09.GetInPlayIntensity / LBonus09.Intensity)
	pLBonus09.color = GrayedRGB(LBonus09.colorfull)
end sub

sub LBonus10_animate
	pLBonus10.blenddisablelighting = 1000 * (LBonus10.GetInPlayIntensity / LBonus10.Intensity)
	pLBonus10.color = GrayedRGB(LBonus10.colorfull)
end sub

'LightShootAgain
sub LightShootAgain_animate
	pLightShootAgain.blenddisablelighting = 1500 * (LightShootAgain.GetInPlayIntensity / LightShootAgain.Intensity)
	pLightShootAgain.color = GrayedRGB(LightShootAgain.colorfull)
end sub

'LBonusInThousands01
sub LBonusInThousands01_animate
	pLBonusInThousands01.blenddisablelighting = 1000 * (LBonusInThousands01.GetInPlayIntensity / LBonusInThousands01.Intensity)
	pLBonusInThousands01.color = GrayedRGB(LBonusInThousands01.colorfull)
end sub

sub LSupreme001_animate
	pLSupreme001.blenddisablelighting = 1000 * (LSupreme001.GetInPlayIntensity / LSupreme001.Intensity)
	pLSupreme001.color = GrayedRGB(LSupreme001.colorfull)
end sub

sub LSupreme002_animate
	pLSupreme002.blenddisablelighting = 1000 * (LSupreme002.GetInPlayIntensity / LSupreme002.Intensity)
	pLSupreme002.color = GrayedRGB(LSupreme002.colorfull)
end sub

sub LSupreme003_animate
	pLSupreme003.blenddisablelighting = 1000 * (LSupreme003.GetInPlayIntensity / LSupreme003.Intensity)
	pLSupreme003.color = GrayedRGB(LSupreme003.colorfull)
end sub


sub LSupreme004_animate
	pLSupreme004.blenddisablelighting = 1000 * (LSupreme004.GetInPlayIntensity / LSupreme004.Intensity)
	pLSupreme004.color = GrayedRGB(LSupreme004.colorfull)
end sub

sub LSupreme005_animate
	pLSupreme005.blenddisablelighting = 1000 * (LSupreme005.GetInPlayIntensity / LSupreme005.Intensity)
	pLSupreme005.color = GrayedRGB(LSupreme005.colorfull)
end sub

sub LSupreme07_animate
	pLSupreme07.blenddisablelighting = 1000 * (LSupreme07.GetInPlayIntensity / LSupreme07.Intensity)
	pLSupreme07.color = GrayedRGB(LSupreme07.colorfull)
end sub

sub LSupreme08_animate
	pLSupreme08.blenddisablelighting = 1000 * (LSupreme08.GetInPlayIntensity / LSupreme08.Intensity)
	pLSupreme08.color = GrayedRGB(LSupreme08.colorfull)
end sub

sub LSupreme09_animate
	pLSupreme09.blenddisablelighting = 1000 * (LSupreme09.GetInPlayIntensity / LSupreme09.Intensity)
	pLSupreme09.color = GrayedRGB(LSupreme09.colorfull)
end sub

sub LSupreme10_animate
	pLSupreme10.blenddisablelighting = 1000 * (LSupreme10.GetInPlayIntensity / LSupreme10.Intensity)
	pLSupreme10.color = GrayedRGB(LSupreme10.colorfull)
end sub

' Side stand up targets
sub LBolt01_animate
	pLBolt01.blenddisablelighting = 1000 * (LBolt01.GetInPlayIntensity / LBolt01.Intensity)
	pLBolt01.color = GrayedRGB(LBolt01.colorfull)
end sub

sub LBolt02_animate
	pLBolt02.blenddisablelighting = 1000 * (LBolt02.GetInPlayIntensity / LBolt02.Intensity)
	pLBolt02.color = GrayedRGB(LBolt02.colorfull)
end sub

sub LBolt03_animate
	pLBolt03.blenddisablelighting = 1000 * (LBolt03.GetInPlayIntensity / LBolt03.Intensity)
	pLBolt03.color = GrayedRGB(LBolt03.colorfull)
end sub


sub LBolt04_animate
	pLBolt04.blenddisablelighting = 1000 * (LBolt04.GetInPlayIntensity / LBolt04.Intensity)
	pLBolt04.color = GrayedRGB(LBolt04.colorfull)
end sub

sub LBolt05_animate
	pLBolt05.blenddisablelighting = 1000 * (LBolt05.GetInPlayIntensity / LBolt05.Intensity)
	pLBolt05.color = GrayedRGB(LBolt05.colorfull)
end sub

sub LBolt06_animate
	pLBolt06.blenddisablelighting = 1000 * (LBolt06.GetInPlayIntensity / LBolt06.Intensity)
	pLBolt06.color = GrayedRGB(LBolt06.colorfull)
end sub


' large round inserts on shots
sub LSupreme01_animate
	pLSupreme01.blenddisablelighting = 1000 * (LSupreme01.GetInPlayIntensity / LSupreme01.Intensity)
	pLSupreme01.color = GrayedRGB(LSupreme01.colorfull)
end sub

sub LSupreme02_animate
	pLSupreme02.blenddisablelighting = 1000 * (LSupreme02.GetInPlayIntensity / LSupreme02.Intensity)
	pLSupreme02.color = GrayedRGB(LSupreme02.colorfull)
end sub

sub LSupreme03_animate
	pLSupreme03.blenddisablelighting = 1000 * (LSupreme03.GetInPlayIntensity / LSupreme03.Intensity)
	pLSupreme03.color = GrayedRGB(LSupreme03.colorfull)
end sub

sub LSupreme04_animate
	pLSupreme04.blenddisablelighting = 1000 * (LSupreme04.GetInPlayIntensity / LSupreme04.Intensity)
	pLSupreme04.color = GrayedRGB(LSupreme04.colorfull)
end sub

sub LSupreme05_animate
	pLSupreme05.blenddisablelighting = 1000 * (LSupreme05.GetInPlayIntensity / LSupreme05.Intensity)
	pLSupreme05.color = GrayedRGB(LSupreme05.colorfull)
end sub

sub LSupreme06_animate
	pLSupreme06.blenddisablelighting = 1000 * (LSupreme06.GetInPlayIntensity / LSupreme06.Intensity)
	pLSupreme06.color = GrayedRGB(LSupreme06.colorfull)
end sub

'eb


sub LExtraBall01_animate
	pLExtraBall01.blenddisablelighting = 1000 * (LExtraBall01.GetInPlayIntensity / LExtraBall01.Intensity)
	pLExtraBall01.color = GrayedRGB(LExtraBall01.colorfull)
end sub


'10K scores

sub L10KScore02_animate
	pL10KScore02.blenddisablelighting = 1000 * (L10KScore02.GetInPlayIntensity / L10KScore02.Intensity)
	pL10KScore02.color = GrayedRGB(L10KScore02.colorfull)
end sub

sub L5KScore01_animate
	pL5KScore01.blenddisablelighting = 1000 * (L5KScore01.GetInPlayIntensity / L5KScore01.Intensity)
	pL5KScore01.color = GrayedRGB(L5KScore01.colorfull)
end sub

sub L10KScore07_animate
	pL10KScore07.blenddisablelighting = 1000 * (L10KScore07.GetInPlayIntensity / L10KScore07.Intensity)
	pL10KScore07.color = GrayedRGB(L10KScore07.colorfull)
end sub

sub L5KScore02_animate
	pL5KScore02.blenddisablelighting = 1000 * (L5KScore02.GetInPlayIntensity / L5KScore02.Intensity)
	pL5KScore02.color = GrayedRGB(L5KScore02.colorfull)
end sub

sub L10KScore09_animate
	pL10KScore09.blenddisablelighting = 1000 * (L10KScore09.GetInPlayIntensity / L10KScore09.Intensity)
	pL10KScore09.color = GrayedRGB(L10KScore09.colorfull)
end sub





sub L25KScore01_animate
	pL25KScore01.blenddisablelighting = 1000 * (L25KScore01.GetInPlayIntensity / L25KScore01.Intensity)
	pL25KScore01.color = GrayedRGB(L25KScore01.colorfull)
end sub

sub L25KScore02_animate
	pL25KScore02.blenddisablelighting = 1000 * (L25KScore02.GetInPlayIntensity / L25KScore02.Intensity)
	pL25KScore02.color = GrayedRGB(L25KScore02.colorfull)
end sub

sub L10KScore06_animate
	pL10KScore06.blenddisablelighting = 1000 * (L10KScore06.GetInPlayIntensity / L10KScore06.Intensity)
	pL10KScore06.color = GrayedRGB(L10KScore06.colorfull)
end sub

sub L25KScore03_animate
	pL25KScore03.blenddisablelighting = 1000 * (L25KScore03.GetInPlayIntensity / L25KScore03.Intensity)
	pL25KScore03.color = GrayedRGB(L25KScore03.colorfull)
end sub

sub L10KScore08_animate
	pL10KScore08.blenddisablelighting = 1000 * (L10KScore08.GetInPlayIntensity / L10KScore08.Intensity)
	pL10KScore08.color = GrayedRGB(L10KScore08.colorfull)
end sub

sub L25KScore04_animate
	pL25KScore04.blenddisablelighting = 1000 * (L25KScore04.GetInPlayIntensity / L25KScore04.Intensity)
	pL25KScore04.color = GrayedRGB(L25KScore04.colorfull)
end sub





'arrows
sub LArrow01_animate
	pLArrow01.blenddisablelighting = 1000 * (LArrow01.GetInPlayIntensity / LArrow01.Intensity)
	pLArrow01.color = GrayedRGB(LArrow01.colorfull)
end sub

sub LArrow02_animate
	pLArrow02.blenddisablelighting = 1000 * (LArrow02.GetInPlayIntensity / LArrow02.Intensity)
	pLArrow02.color = GrayedRGB(LArrow02.colorfull)
end sub

sub LArrow03_animate
	pLArrow03.blenddisablelighting = 1000 * (LArrow03.GetInPlayIntensity / LArrow03.Intensity)
	pLArrow03.color = GrayedRGB(LArrow03.colorfull)
end sub

sub LArrow04_animate
	pLArrow04.blenddisablelighting = 1000 * (LArrow04.GetInPlayIntensity / LArrow04.Intensity)
	pLArrow04.color = GrayedRGB(LArrow04.colorfull)
end sub

sub LArrow05_animate
	pLArrow05.blenddisablelighting = 1000 * (LArrow05.GetInPlayIntensity / LArrow05.Intensity)
	pLArrow05.color = GrayedRGB(LArrow05.colorfull)
end sub

sub LArrow06_animate
	pLArrow06.blenddisablelighting = 1000 * (LArrow06.GetInPlayIntensity / LArrow06.Intensity)
	pLArrow06.color = GrayedRGB(LArrow06.colorfull)
end sub

sub LArrow07_animate
	pLArrow07.blenddisablelighting = 1000 * (LArrow07.GetInPlayIntensity / LArrow07.Intensity)
	pLArrow07.color = GrayedRGB(LArrow07.colorfull)
end sub

sub LArrow08_animate
	pLArrow08.blenddisablelighting = 1000 * (LArrow08.GetInPlayIntensity / LArrow08.Intensity)
	pLArrow08.color = GrayedRGB(LArrow08.colorfull)
end sub

sub LArrow09_animate
	pLArrow09.blenddisablelighting = 1000 * (LArrow09.GetInPlayIntensity / LArrow09.Intensity)
	pLArrow09.color = GrayedRGB(LArrow09.colorfull)
end sub


'top lanes
sub LStar01_animate
	pLStar01.blenddisablelighting = 1000 * (LStar01.GetInPlayIntensity / LStar01.Intensity)
	pLStar01.color = GrayedRGB(LStar01.colorfull)
end sub

sub LStar02_animate
	pLStar02.blenddisablelighting = 1000 * (LStar02.GetInPlayIntensity / LStar02.Intensity)
	pLStar02.color = GrayedRGB(LStar02.colorfull)
end sub

sub LStar03_animate
	pLStar03.blenddisablelighting = 1000 * (LStar03.GetInPlayIntensity / LStar03.Intensity)
	pLStar03.color = GrayedRGB(LStar03.colorfull)
end sub


Function GrayedRGB(color)
	Dim red, green, blue

	color = round(color,0)

	red = color mod 256
	green = (color \ 256) mod 256
	blue = (color \ 65536) mod 256

'	red = ((color And &H000000FF)\&H00000001) And 255
'	green = ((color And &H0000FF00)\&H00000100) And 255
'	blue = ((color And &H00FF0000)\&H00010000) And 255

'	debug.print "in: " & red & " " & green & " " & blue

	red = (red + 28) / 1.22
	green = (green + 28) / 1.22
	blue = (blue + 28) / 1.22

'	debug.print "--------------->: " & red & " " & green & " " & blue

	GrayedRGB = RGB(red, green, blue)
End Function

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'  WHY THE HELL ARE YOU READING ALL THE WAY DOWN HERE!!!  YOU MUST BE SOFA KING WE TODD ID!!! 
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

Dim objIEDebugWindow
Sub Dbg( myDebugText )
' Uncomment the next line to turn off debugging
Exit Sub

If Not IsObject( objIEDebugWindow ) Then
Set objIEDebugWindow = CreateObject( "InternetExplorer.Application" )
objIEDebugWindow.Navigate "about:blank"
objIEDebugWindow.Visible = True
objIEDebugWindow.ToolBar = False
objIEDebugWindow.Width = 600	
objIEDebugWindow.Height = 900
objIEDebugWindow.Left = 4100
objIEDebugWindow.Top = 100
Do While objIEDebugWindow.Busy
Loop
objIEDebugWindow.Document.Title = "My Debug Window"
objIEDebugWindow.Document.Body.InnerHTML = "<b>MRDOOM Debug Window -TimeStamp: " & GameTime& "</b></br>"
End If

objIEDebugWindow.Document.Body.InnerHTML = objIEDebugWindow.Document.Body.InnerHTML & myDebugText & " --TimeStamp:<b> " & GameTime & "</b><br>" & vbCrLf
End Sub




'//////////////////// PINUP PLAYER: STARTUP & CONTROL SECTION //////////////////////////

' This is used for the startup and control of Pinup 
'************ PuP-Pack Startup **************

Sub PuPStart(cPuPPack)
    If PUPStatus=true then Exit Sub
    If UsePuPEvents=true then
       ' Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")
        If PuPlayer is Nothing Then
            UsePuPEvents=false
            PUPStatus=false
        Else
            PuPlayer.B2SInit "",cPuPPack 'start the Pup-Pack
            PUPStatus=true
        End If
    End If
End Sub

Sub PuPEvent(EventNum)
    if (UsePuPEvents=false or PUPStatus=false) then Exit Sub
	Dbg "Event: " &EventNum
    PuPlayer.B2SData "E"&EventNum,1  'send event to Pup-Pack
End Sub

'===========================================
' vpwQueueManager
' This class manages a queue of
' vpwQueueItems and executes them.
'===========================================
Class vpwQueueManager
	Public qItems ' A dictionary of vpwQueueItems in the queue (do NOT use native Scripting.Dictionary.Add/Remove; use the vpwQueueManager's Add/Remove methods instead!)
	Public preQItems ' A dictionary of vpwQueueItems pending to be added to qItems
	Public debugOn 'Null = no debug. String = activate debug by using this unique label for the queue. REQUIRES baldgeek's error logs.
	
	'----------------------------------------------------------
	' vpwQueueManager.qCurrentItem
	' This contains a string of the key currently active / at
	' the top of the queue. An empty string means no items are
	' active right now.
	' This is an important property; it should be monitored
	' in another timer or routine whenever you Add a queue item
	' with a -1 (indefinite) preDelay or postDelay. Then, for
	' preDelay, ExecuteCurrentItem should be called to run the
	' queue item. And for postDelay, DoNextItem should be
	' called to move to the next item in the queue.
	'
	' For example, let's say you add a queue item with the
	' key "kickTheBall" and an indefinite preDelay. You want
	' to wait until another timer fires before this queue item
	' executes and kicks the ball out of a scoop. In the other
	' timer, you will monitor qCurrentItem. Once it equals
	' "kickTheBall", call ExecuteCurrentItem, which will run
	' the queue item and presumably kick out the ball.
	'
	' WARNING!: If you do not properly execute one of these
	' callback routines on an indefinite delayed item, then
	' the queue will effectively freeze / stop until you do.
	'---------------------------------------------------------
	Public qCurrentItem
	
	Public preDelayTime ' The GameTime the preDelay for the qCurrentItem was started
	Public postDelayTime ' The GameTime the postDelay for the qCurrentItem was started
	
	Private onQueueEmpty ' A string or object to be called every time the queue empties (use the QueueEmpty property to get/set this)
	Private queueWasEmpty ' Boolean to determine if the queue was already empty when firing DoNextItem
	Private preDelayTransfer ' Number of milliseconds of preDelay to transfer over to the next queue item when doNextItem is called
	
	Private Sub Class_Initialize
		Set qItems = CreateObject("Scripting.Dictionary")
		Set preQItems = CreateObject("Scripting.Dictionary")
		qCurrentItem = ""
		onQueueEmpty = ""
		queueWasEmpty = True
		debugOn = Null
		preDelayTransfer = 0
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.Tick
	' This is where all the magic happens! Call this method in
	' your timer's _timer routine to check the queue and
	' execute the necessary methods. We do not iterate over
	' every item in the queue here, which allows for superior
	' performance even if you have hundreds of items in the
	' queue.
	'----------------------------------------------------------
	Public Sub Tick()
		Dim item
		If qItems.Count > 0 Then ' Don't waste precious resources if we have nothing in the queue
			
			' If no items are active, or the currently active item no longer exists, move to the next item in the queue.
			' (This is also a failsafe to ensure the queue continues to work even if an item gets manually deleted from the dictionary).
			If qCurrentItem = "" Or Not qItems.Exists(qCurrentItem) Then
				DoNextItem
			Else ' We are good; do stuff as normal
				Set item = qItems.item(qCurrentItem)
				
				If item.Executed Then
					' If the current item was executed and the post delay passed, go to the next item in the queue
					If item.postDelay >= 0 And GameTime >= (postDelayTime + item.postDelay) Then
						DebugLog qCurrentItem & " - postDelay of " & item.postDelay & " passed."
						DoNextItem
					End If
				Else
					' If the current item expires before it can be executed, go to the next item in the queue
					If item.timeToLive > 0 And GameTime >= (item.queuedOn + item.timeToLive) Then
						DebugLog qCurrentItem & " - expired (Time To live). Moving To the Next queue item."
						DoNextItem
					End If
					
					' If the current item was not executed yet and the pre delay passed, then execute it
					If item.preDelay >= 0 And GameTime >= (preDelayTime + item.preDelay) Then
						DebugLog qCurrentItem & " - preDelay of " & item.preDelay & " passed. Executing callback."
						item.Execute
						preDelayTime = 0
						postDelayTime = GameTime
					End If
				End If
			End If
		End If
		
		' Loop through each item in the pre-queue to find any that is ready to be added
		If preQItems.Count > 0 Then
			Dim k, key
			k = preQItems.Keys
			For Each key In k
				Set item = preQItems.Item(key)
				
				' If a queue item was pre-queued and is ready to be considered as actually in the queue, add it
				If GameTime >= (item.queuedOn + item.preQueueDelay) Then
					DebugLog key & " (preQueue) - preQueueDelay of " & item.preQueueDelay & " passed. Item added To the main queue."
					preQItems.Remove key
					Me.Add key, item.Callback, item.priority, 0, item.preDelay, item.postDelay, item.timeToLive, item.executeNow
				End If
			Next
		End If
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.DoNextItem
	' Goes to the next item in the queue and deletes the
	' currently active one.
	'----------------------------------------------------------
	Public Sub DoNextItem()
		If Not qCurrentItem = "" Then
			If qItems.Exists(qCurrentItem) Then qItems.Remove qCurrentItem ' Remove the current item from the queue if it still exists
			qCurrentItem = ""
		End If
		
		If qItems.Count > 0 Then
			Dim k, key
			Dim nextItem
			Dim nextItemPriority
			Dim item
			nextItemPriority = 0
			nextItem = ""
			
			' Find which item needs to run next based on priority first, queue order second (ignore items with an active preQueueDelay)
			k = qItems.Keys
			For Each key In k
				Set item = qItems.Item(key)
				
				If item.preQueueDelay <= 0 And item.priority > nextItemPriority Then
					nextItem = key
					nextItemPriority = item.priority
				End If
			Next
			
			If qItems.Exists(nextItem) Then
				Set item = qItems.Item(nextItem)
				DebugLog "DoNextItem - checking " & nextItem & " (priority " & item.priority & ")"
				
				' Make sure the item is not expired and not already executed. If it is, remove it and re-call doNextItem
				If (item.timeToLive > 0 And GameTime >= (item.queuedOn + item.timeToLive + preDelayTransfer)) Or item.executed = True Then
					DebugLog "DoNextItem - " & nextItem & " expired (Time To live) Or already executed. Removing And going To the Next item."
					qItems.Remove nextItem
					DoNextItem
					Exit Sub
				End If
				
				'Transfer preDelay time when applicable
				If preDelayTransfer > 0 And item.preDelay > -1 Then
					DebugLog "DoNextItem " & nextItem & " - Transferred remaining postDelay of " & preDelayTransfer & " milliseconds from previously overridden queue item To its preDelay And timeToLive"
					qItems.Item(nextItem).preDelay = item.preDelay + preDelayTransfer
					If item.timeToLive > 0 Then qItems.Item(nextItem).timeToLive = item.timeToLive + preDelayTransfer
					preDelayTransfer = 0
				End If
				
				' Set item as current / active, and execute if it has no pre-delay (otherwise Tick will take care of pre-delay)
				qCurrentItem = nextItem
				If item.preDelay = 0 Then
					DebugLog "DoNextItem - " & nextItem & " Now active. It has no preDelay, so executing callback immediately."
					item.Execute
					preDelayTime = 0
					postDelayTime = GameTime
				Else
					DebugLog "DoNextItem - " & nextItem & " Now active. Waiting For a preDelay of " & item.preDelay & " before executing."
					preDelayTime = GameTime
					postDelayTime = 0
				End If
			End If
		ElseIf queueWasEmpty = False Then
			DebugLog "DoNextItem - Queue Is Now Empty; executing queueEmpty callback."
			CallQueueEmpty() ' Call QueueEmpty if this was the last item in the queue
		End If
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.ExecuteCurrentItem
	' Helper routine that can be used when the current item is
	' on an indefinite preDelay. Call this when you are ready
	' for that item to execute.
	'----------------------------------------------------------
	Public Sub ExecuteCurrentItem()
		If Not qCurrentItem = "" And qItems.Exists(qCurrentItem) Then
			DebugLog "ExecuteCurrentItem - Executing the callback For " & qCurrentItem & "."
			Dim item
			Set item = qItems.Item(qCurrentItem)
			item.Execute
			preDelayTime = 0
			postDelayTime = GameTime
		End If
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.Add
	' REQUIRES Class vpwQueueItem
	'
	' Add an item to the queue.
	'
	' PARAMETERS:
	'
	' key (string) - Unique name for this queue item
	' WARNING: Specifying a key that already exists will
	' overwrite the item in the queue. This is by design. Also
	' note the following behaviors:
	' * Tickers / clocks for tracking delay times will NOT be
	' restarted for this item (but the total duration will be
	' updated. For example, if the old preDelay was 3 seconds
	' and 2 seconds elapsed, but Add was called to update
	' preDelay to 5 seconds, then the queue item will now
	' execute in 3 more seconds (new preDelay - time elapsed)).
	' However, timeToLive WILL be restarted.
	' * Items will maintain their same place in the queue.
	' * If key = qCurrentItem (overwriting the currently active
	' item in the queue) and qCurrentItem already executed
	' the callback (but is waiting for a postDelay), then the
	' current queue item's remaining postDelay will be added to
	' the preDelay of the next item, and this item will be
	' added to the bottom of the queue for re-execution.
	' If you do not want it to re-execute, then add an If
	' guard on your call to the Add method checking
	' "If Not vpwQueueManager.qCurrentItem = key".
	'
	' qCallback (object|string) - An object to be called,
	' or string to be executed globally, when this queue item
	' runs. I highly recommend making sub routines for groups
	' of things that should be executed by the queue so that
	' your qCallback string does not get long, and you can
	' easily organize your callbacks. Also, use double
	' double-quotes when the call itself has quotes in it
	' (VBScript escaping).
	' Example: "playsound ""Plunger"""
	'
	' priority (number) - Items in the queue will be executed
	' in order from highest priority to lowest. Items with the
	' same priority will be executed in order according to
	' when they were added to the queue. Use any number
	' greater than 0. My recommendation is to make a plan for
	' your table on how you will prioritize various types of
	' queue items and what priority number each type should
	' have. Also, you should reserve priority 1 (lowest) to
	' items which should wait until everything else in the
	' queue is done (such as ejecting a ball from a scoop).
	'
	' preQueueDelay (number) - The number of
	' milliseconds before the queue actually considers this
	' item as "in the queue" (pretend you started a timer to
	' add this item into the queue after this delay; this
	' logically works in a similar way; the only difference is
	' timeToLive is still considered even when an item is
	' pre-queued.) Set to 0 to add to the queue immediately.
	' NOTE: this should be less than timeToLive.
	'
	' preDelay (number) - The number of milliseconds before
	' the qCallback executes once this item is active (top)
	' in the queue. Set this to 0 to immediately execute the
	' qCallback when this item becomes active.
	' Set this to -1 to have an indefinite delay until
	' vpwQueueManager.ExecuteCurrentItem is called (see the
	' comment for qCurrentItem for more information).
	' NOTE: this should be less than timeToLive. And, if
	' timeToLive runs out before preDelay runs out, the item
	' will be removed and will not execute.
	'
	' postDelay (number) - After the qCallback executes, the
	' number of milliseconds before moving on to the next item
	' in the queue. Set this to -1 to have an indefinite delay
	' until vpwQueueManager.DoNextItem is called (see the
	' comment for qCurrentItem for more information).
	'
	' timeToLive (number) - After this item is added to the
	' queue, the number of milliseconds before this queue item
	' expires / is removed if the qCallback is not executed by
	' then. Set to 0 to never expire. NOTE: If not 0, this
	' should be greater than preDelay + preQueueDelay or the
	' item will expire before the qCallback is executed.
	' Example use case: Maybe a player scored a jackpot, but
	' it would be awkward / irrelevant to play that jackpot
	' sequence if it hasn't played after a few seconds (e.g.
	' other items in the queue took priority).
	'
	' executeNow (boolean) - Specify true if this item
	' should interrupt the queue and run immediately. This
	' will only happen, however, if the currently active item
	' has a priority less than or equal to the item you are
	' adding. Note this does not bypass preQueueDelay nor
	' preDelay if set.
	' Example: If a player scores an extra ball, you might
	' want that to interrupt everything else going on as it
	' is an important milestone.
	'----------------------------------------------------------
	Public Sub Add(key, qCallback, priority, preQueueDelay, preDelay, postDelay, timeToLive, executeNow)
		DebugLog "Adding queue item " & key
		
		'Construct the item class
		Dim newClass
		Set newClass = New vpwQueueItem
		With newClass
			.Callback = qCallback
			.priority = priority
			.preQueueDelay = preQueueDelay
			.preDelay = preDelay
			.postDelay = postDelay
			.timeToLive = timeToLive
			.executeNow = executeNow
		End With
		
		'If we are attempting to overwrite the current queue item which already executed, take the remaining postDelay and add it to the preDelay of the next item. And set us up to immediately go to the next item while re-adding this item to the queue.
		If preQueueDelay <= 0 And qItems.Exists(key) And qCurrentItem = key Then
			If qItems.Item(key).executed = True Then
				DebugLog key & " (Add) - Attempting To overwrite the current queue item which already executed. Immediately re-queuing this item To the bottom of the queue, transferring the remaining postDelay To the Next item, And going To the Next item."
				If qItems.Item(key).postDelay >= 0 Then
					preDelayTransfer = ((postDelayTime + qItems.Item(key).postDelay) - GameTime)
				End If
				
				'Remove current queue item so we can go to the next item, this can be re-queued to the bottom, and the remaining postDelay transferred to the preDelay of the next item
				qItems.Remove qCurrentItem
				qCurrentItem = ""
			End If
		End If
		
		' Determine execution stuff if this item does not have a pre-queue delay
		If preQueueDelay <= 0 Then
			If executeNow = True Then
				' Make sure this item does not immediately execute if the current item has a higher priority
				If Not qCurrentItem = "" And qItems.Exists(qCurrentItem) Then
					Dim item
					Set item = qItems.Item(qCurrentItem)
					If item.priority <= priority Then
						DebugLog key & " (Add) - Execute Now was Set To True And this item's priority (" & priority & ") Is >= the active item's priority (" & item.priority & " from " & qCurrentItem & "). Making it the current active queue item."
						qCurrentItem = key
						If preDelay = 0 And preDelayTransfer = 0 Then
							DebugLog key & " (Add) - No pre-delay. Executing the callback immediately."
							newClass.Execute
							preDelayTime = 0
							postDelayTime = GameTime
						Else
							DebugLog key & " (Add) - Waiting For a pre-delay of " & (preDelay + preDelayTransfer) & " before executing the callback."
							preDelayTime = GameTime
							postDelayTime = 0
						End If
					Else
						DebugLog key & " (Add) - Execute Now was Set To True, but this item's priority (" & priority & ") Is Not >= the active item's priority (" & item.priority & " from " & qCurrentItem & "). This item will Not be executed Now And will be added To the queue normally."
					End If
				Else
					DebugLog key & " (Add) - Execute Now was Set To True And no item was active In the queue. Making it the current active queue item."
					qCurrentItem = key
					If preDelay = 0 Then
						DebugLog key & " (Add) - No pre-delay. Executing the callback immediately."
						preDelayTransfer = 0 'No preDelay transfer if we are immediately re-executing the same queue item
						newClass.Execute
						preDelayTime = 0
						postDelayTime = GameTime
					Else
						DebugLog key & " (Add) - Waiting For a pre-delay of " & preDelay & " before executing the callback."
						preDelayTime = GameTime
						postDelayTime = 0
					End If
				End If
			End If
			If qItems.Exists(key) Then 'Overwrite existing item in the queue if it exists
				DebugLog key & " (Add) - Already exists In the queue. Updating the item With the new parameters passed In Add."
				Set qItems.Item(key) = newClass
			Else
				DebugLog key & " (Add) - Added To the queue."
				qItems.Add key, newClass
			End If
			queueWasEmpty = False
		Else
			If preQItems.Exists(key) Then 'Overwrite existing item in the preQueue if it exists
				DebugLog key & " (Add) - Already exists In the preQueue. Updating the item With the new parameters passed In Add."
				Set preQItems.Item(key) = newClass
			Else
				DebugLog key & " (Add) - Added To the preQueue."
				preQItems.Add key, newClass
			End If
		End If
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.Remove
	'
	' Removes an item from the queue. It is better to use this
	' than to remove the item from qItems directly as this sub
	' will also call DoNextItem to advance the queue if
	' the item removed was the active item.
	' NOTE: This only removes items from qItems; to remove
	' an item from preQItems, use the standard
	' Scripting.Dictionary Remove method.
	'
	' PARAMETERS:
	'
	' key (string) - Unique name of the queue item to remove.
	'----------------------------------------------------------
	Public Sub Remove(key)
		If qItems.Exists(key) Then
			DebugLog key & " (Remove)"
			qItems.Remove key
			If qCurrentItem = key Or qCurrentItem = "" Then DoNextItem ' Ensure the queue does not get stuck
		End If
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueManager.RemoveAll
	'
	' Removes all items from the queue / clears the queue.
	' It is better to call this sub than to remove all items
	' from qItems directly because this sub cleans up the queue
	' to ensure it continues to work properly.
	'
	' PARAMETERS:
	'
	' preQueue (boolean) - Also clear the pre-queue.
	'----------------------------------------------------------
	Public Sub RemoveAll(preQueue)
		DebugLog "Queue was emptied via RemoveAll."
		
		' Loop through each item in the queue and remove it
		Dim k, key
		k = qItems.Keys
		For Each key In k
			qItems.Remove key
		Next
		qCurrentItem = ""
		
		If queueWasEmpty = False Then CallQueueEmpty() ' Queue is now empty, so call our callback if applicable
		
		If preQueue Then
			k = preQItems.Keys
			For Each key In k
				preQItems.Remove key
			Next
		End If
	End Sub
	
	'----------------------------------------------------------
	' Get vpwQueueManager.QueueEmpty
	' Get the current callback for when the queue is empty.
	'----------------------------------------------------------
	Public Property Get QueueEmpty()
		If IsObject(onQueueEmpty) Then
			Set QueueEmpty = onQueueEmpty
		Else
			QueueEmpty = onQueueEmpty
		End If
	End Property
	
	'----------------------------------------------------------
	' Let vpwQueueManager.QueueEmpty
	' Set the callback to call every time the queue empties.
	' This could be useful for setting a sub routine to be
	' called each time the queue empties for doing things such
	' as ejecting balls from scoops. Unlike using the Add
	' method, this callback is immune from getting removed by
	' higher priority items in the queue and will be called
	' every time the queue is emptied, not just once.
	'
	' PARAMETERS:
	'
	' callback (object|string) - The callback to call every
	' time the queue empties.
	'----------------------------------------------------------
	Public Property Let QueueEmpty(callback)
		If IsObject(callback) Then
			Set onQueueEmpty = callback
		ElseIf VarType(callback) = vbString Then
			onQueueEmpty = callback
		End If
	End Property
	
	'----------------------------------------------------------
	' Get vpwQueueManager.CallQueueEmpty
	' Private method that actually calls the QueueEmpty
	' callback.
	'----------------------------------------------------------
	Private Sub CallQueueEmpty()
		If queueWasEmpty = True Then Exit Sub
		queueWasEmpty = True
		
		If IsObject(onQueueEmpty) Then
			Call onQueueEmpty(0)
		ElseIf VarType(onQueueEmpty) = vbString Then
			If onQueueEmpty > "" Then ExecuteGlobal onQueueEmpty
		End If
	End Sub
	
	'----------------------------------------------------------
	' DebugLog
	' Log something if debugOn is not null.
	' REQUIRES / uses the WriteToLog sub from Baldgeek's
	' error log library.
	'----------------------------------------------------------
	Private Sub DebugLog(message)
		If Not IsNull(debugOn) Then
			WriteToLog "VPW Queue " & debugOn, message
		End If
	End Sub
End Class

'===========================================
' vpwQueueItem
' Represents a single item for the queue
' system. Do NOT use this class directly.
' Instead, use the vpwQueueManager.Add
' routine.

' You can, however, access an individual
' item in the queue via
' vpwQueueManager.qItems and then modify
' its properties while it is still in the
' queue.
'===========================================
Class vpwQueueItem  ' Do not construct this class directly; use vpwQueueManager.Add instead, and vpwQueueManager.qItems.Item(key) to modify an item's properties.
	Public priority ' The item's set priority
	Public timeToLive ' The item's set timeToLive milliseconds requested
	Public preQueueDelay ' The item's pre-queue milliseconds requested
	Public preDelay ' The item's pre delay milliseconds requested
	Public postDelay ' The item's post delay milliseconds requested
	Public executeNow ' Whether the item was set to Execute immediately
	Private qCallback ' The item's callback object or string (use the Callback property on the class to get/set it)
	
	Public executed ' Whether or not this item's qCallback was executed yet
	Public queuedOn ' The game time this item was added to the queue
	Public executedOn ' The game time this item was executed
	
	Private Sub Class_Initialize
		' Defaults
		priority = 0
		timeToLive = 0
		preQueueDelay = 0
		preDelay = 0
		postDelay = 0
		qCallback = ""
		executeNow = False
		
		queuedOn = GameTime
		executedOn = 0
	End Sub
	
	'----------------------------------------------------------
	' vpwQueueItem.Execute
	' Executes the qCallback on this item if it was not yet
	' already executed.
	'----------------------------------------------------------
	Public Sub Execute()
		If executed Then Exit Sub ' Do not allow an item's qCallback to ever Execute more than one time
		
		'Mark as execute before actually executing callback; that way, if callback recursively adds the item back into the queue, then we can properly handle it.
		executed = True
		executedOn = GameTime
		
		' Execute qCallback
		If IsObject(qCallback) Then
			Call qCallback(0)
		ElseIf VarType(qCallback) = vbString Then
			If qCallback > "" Then ExecuteGlobal qCallback
		End If
	End Sub
	
	Public Property Get Callback()
		If IsObject(qCallback) Then
			Set Callback = qCallback
		Else
			Callback = qCallback
		End If
	End Property
	
	Public Property Let Callback(cb)
		If IsObject(cb) Then
			Set qCallback = cb
		ElseIf VarType(cb) = vbString Then
			qCallback = cb
		End If
	End Property
End Class

'***************************************************************
' END VPIN WORKSHOP ADVANCED QUEUING SYSTEM
'***************************************************************

Sub QueueTimer_Timer()
	BallHandlingQueue.Tick
	GeneralPupQueue.Tick
	LightQueue.Tick
	AudioQueue.Tick
End Sub

Sub RTP
Dim TempTopStr
    TempTopStr = "YOUR NAME:"

DMDTopSplash "YOUR NAME:", 9999, 1

Dim TempBotStr
    TempBotStr = TempBotStr & " <A    "
    'dLine(1) = ExpandLine(TempBotStr)
	DMDBigText TempBotStr,9999,0
End Sub

Sub DbgTimer_Timer()
	Dbg "Video: " &PlayVideo
End Sub

Sub dmdattract_Timer()
	DMDintroloop
End Sub

' Superhac function to load song data dynamically
Function HasArrayElements(arr)
    HasArrayElements = False
    If IsArray(arr) Then
        On Error Resume Next
        Dim ub : ub = UBound(arr)
    If (Err.Number = 0) And (ub >= 0) Then HasArrayElements = True
    End If  
    On Error Goto 0 ' reset to default error catching
End Function

Function mp3PathArray()
  Dim matchedFiles()
  Dim rootPath:rootPath = MusicDirectory &"\MFDOOM"
  Dim objFile,bMatch,curFile
  
  Set re = New RegExp
  re.Global     = True
  re.IgnoreCase = False
  re.Pattern    = ".*\.mp3"

  Set fso = CreateObject("Scripting.FileSystemObject")
  set folder = fso.GetFolder(rootPath)
  Set files = folder.Files

  For Each objFile in files
    bMatch = re.Test(objFile.Name)
    curFile = objFile.Name

    If bMatch Then
     if NOT HasArrayElements(matchedFiles) Then
       ReDim matchedFiles(1)
       matchedFiles(0) = curFile
     Else
    ReDim Preserve matchedFiles(Ubound(matchedFiles)+1)
    matchedFiles(Ubound(matchedFiles)-1) = curFile
     End If
      'msgbox(curFile)

    End If
  Next

mp3PathArray = matchedFiles ' return values
End Function




' example call
'dim mp3Paths
'mp3Files = mp3PathArray("c:\users\pinball\vbtesting")
'msgbox mp3Files(1)



'**************************
'   SCORBIT
'**************************
'==================================================================================================================
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  SCORBIT Interface
' To Use:
' 1) Define a timer tmrScorbit
' 2) Call DoInit at the end of PupInit or in Table Init if you are nto using pup with the appropriate parameters
'     Replace 389 with your TableID from Scorbit 
'     Replace GRWvz-MP37P from your table on OPDB - eg: https://opdb.org/machines/2103
'		if Scorbit.DoInit(389, "PupOverlays", "1.0.0", "GRWvz-MP37P") then 
'			tmrScorbit.Interval=2000
'			tmrScorbit.UserValue = 0
'			tmrScorbit.Enabled=True 
'		End if 
' 3) Customize helper functions below for different events if you want or make your own 
' 4) Call 
'		DoInit - After Pup/Screen is setup (PuPInit)
'		StartSession - When a game starts (ResetForNewGame)
'		StopSession - When the game is over (Table1_Exit, EndOfGame)
'		SendUpdate - called when Score Changes (AddScore)
'			SendUpdate(P1Score, P2Score, P3Score, P4Score, CurrentBall, CurrentPlayer, NumberPlayers)
'			Example:  Scorbit.SendUpdate Score(0), Score(1), Score(2), Score(3), Balls, CurrentPlayer+1, PlayersPlayingGame
'		SetGameMode - When different game events happen like starting a mode, MB etc.  (ScorbitBuildGameModes helper function shows you how)
' 5) Drop the binaries sQRCode.exe and sToken.exe in your Pup Root so we can create session tokens and QRCodes.
'	- Drop QRCode Images (QRCodeS.png, QRcodeB.png) in yur pup PuPOverlays if you want to use those 
' 6) Callbacks 
'		Scorbit_Paired   	- Called when machine is successfully paired.  Hide QRCode and play a sound 
'		Scorbit_PlayerClaimed	- Called when player is claimed.  Hide QRCode, play a sound and display name 
'		ScorbitClaimQR		- Call before/after plunge (swPlungerRest_Hit, swPlungerRest_UnHit)
' 7) Other 
'		Set Pair QR Code	- During Attract
'			if (Scorbit.bNeedsPairing) then 
'				PuPlayer.LabelSet pDMDFull, "ScorbitQR_a", "PuPOverlays\\QRcode.png",1,"{'mt':2,'width':32, 'height':64,'xalign':0,'yalign':0,'ypos':5,'xpos':5}"
'				PuPlayer.LabelSet pDMDFull, "ScorbitQRIcon_a", "PuPOverlays\\QRcodeS.png",1,"{'mt':2,'width':36, 'height':85,'xalign':0,'yalign':0,'ypos':3,'xpos':3,'zback':1}"
'			End if 
'		Set Player Names 	- Wherever it makes sense but I do it here: (pPupdateScores)
'		   if ScorbitActive then 
'			if Scorbit.bSessionActive then
'				PlayerName=Scorbit.GetName(CurrentPlayer+1)
'				if PlayerName="" then PlayerName= "Player " & CurrentPlayer+1 
'			End if 
'		   End if 
'
'
'>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
' TABLE CUSTOMIZATION START HERE 

Sub Scorbit_Paired()								' Scorbit callback when new machine is paired 
dbg "Scorbit PAIRED"
	PlaySound "scorbit_login"
	hideScorbit

End Sub 

Sub Scorbit_PlayerClaimed(PlayerNum, PlayerName)	' Scorbit callback when QR Is Claimed 
dbg "Scorbit LOGIN"
	PlaySound "scorbit_login"
	ScorbitClaimQR(False)
	
End Sub 


Sub ScorbitClaimQR(bShow)	
dbg "In ScorbitClaimQR: " &bShow					'  Show QRCode on first ball for users to claim this position
	if Scorbit.bSessionActive=False then Exit Sub 
	if ScorbitShowClaimQR=False then Exit Sub
	if Scorbit.bNeedsPairing then exit sub 

	pupevent 401

	if bShow and CurrBall=1 and bGameInPlay and Scorbit.GetName(CurrentPlayer+1)="" then 
		'PuPlayer.LabelSet pBackglass, "ScorbitQRIcon2", "PuPOverlays\\QRcodeB.png",1,"{'mt':2,'width':25, 'height':50,'xalign':0,'yalign':0,'ypos':3,'xpos':3,'zback':1}"
		PuPlayer.LabelSet pBackglass, "ScorbitQR2", "PuPOverlays\\QRclaim.png",1,"{'mt':2,'width':19.61, 'height':36,'xalign':0,'yalign':0,'ypos':32,'xpos':74.6}"
		'pbackglasslabelshow "ScorbitQR2"
	Else 
		dbg "Hiding QR claim"
		hideScorbit
		'PuPlayer.LabelSet pBackglass, "ScorbitQR2", "PuPOverlays\\clear.png",0,""
		'pupevent 800
	End if 
End Sub 

Sub StopScorbit
	Scorbit.StopSession Score(0), Score(1), Score(2), Score(3), PlayersPlayingGame   ' Stop updateing scores
End Sub

Sub ScorbitBuildGameModes(MysMode)		' Custom function to build the game modes for better stats 
	dim GameModeStr
	if Scorbit.bSessionActive=False then Exit Sub 
	GameModeStr="NA:"

	if BallsRemaining(CurrentPlayer) <= 0 Then	'no balls left
		GameModeStr="NA{red}:YOU FAILED!!!"
	Else										'game on
		Select Case MysMode
			Case "VIK VAUGHN":
				GameModeStr="NA{purple}:VIK VAUGHN Completed"
			Case "GEEDORAH":
				GameModeStr="NA{green}:GEEDORAH Completed"
			Case "MAD VILLAIN":
				GameModeStr="NA{red}:MAD VILLAIN Completed"
			Case "ZEV LOVE X":
				GameModeStr="NA{orange}:ZEV LOVE X Completed"
			Case "DANGER DOOM":
				GameModeStr="NA{green}:DANGER DOOM Completed"
			Case "JJ DOOM":
				GameModeStr="NA{purple}:JJ DOOM Completed"
			Case "NEHRUVIAN":
				GameModeStr="NA{yellow}:NEHRUVIAN Completed"
		End Select


	End If ' endif balls remaining
	Scorbit.SetGameMode(GameModeStr)

End Sub 






' END ----------

Sub Scorbit_LOGUpload(state)	' Callback during the log creation process.  0=Creating Log, 1=Uploading Log, 2=Done 
	Select Case state 
		case 0:
			dbg "CREATING LOG"
		case 1:
			dbg "Uploading LOG"
		case 2:
			dbg "LOG Complete"
	End Select 
End Sub 
'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
' TABLE CUSTOMIZATION END HERE - NO NEED TO EDIT BELOW THIS LINE


dim Scorbit : Set Scorbit = New ScorbitIF
' Workaround - Call get a reference to Member Function
Sub tmrScorbit_Timer()								' Timer to send heartbeat 
	Scorbit.DoTimer(tmrScorbit.UserValue)
	tmrScorbit.UserValue=tmrScorbit.UserValue+1
	if tmrScorbit.UserValue>5 then tmrScorbit.UserValue=0
End Sub 
Function ScorbitIF_Callback()
	Scorbit.Callback()
End Function 
Class ScorbitIF

	Public bSessionActive
	Public bNeedsPairing
	Private bUploadLog
	Private bActive
	Private LOGFILE(10000000)
	Private LogIdx

	Private bProduction

	Private TypeLib
	Private MyMac
	Private Serial
	Private MyUUID
	Private TableVersion

	Private SessionUUID
	Private SessionSeq
	Private SessionTimeStart
	Private bRunAsynch
	Private bWaitResp
	Private GameMode
	Private GameModeOrig		' Non escaped version for log
	Private VenueMachineID
	Private CachedPlayerNames(4)
	Private SaveCurrentPlayer

	Public bEnabled
	Private sToken
	Private machineID
	Private dirQRCode
	Private opdbID
	Private wsh

	Private objXmlHttpMain
	Private objXmlHttpMainAsync
	Private fso
	Private Domain

	Public Sub Class_Initialize()
		bActive="false"
		bSessionActive=False
		bEnabled=False 
	End Sub 

	Property Let UploadLog(bValue)
		bUploadLog = bValue
	End Property

	Sub DoTimer(bInterval)	' 2 second interval
		dim holdScores(4)
		dim i
		if bInterval=0 then 
			SendHeartbeat()
		elseif bRunAsynch And bSessionActive = True then ' Game in play (Updated for TNA to resolve stutter in CoopMode)
			Scorbit.SendUpdate Score(1), Score(2), Score(3), Score(4), Balls, CurrentPlayer, PlayersPlayingGame
		End if 
	End Sub 

	Function GetName(PlayerNum)	' Return Parsed Players name  
		if PlayerNum<1 or PlayerNum>4 then 
			GetName=""
		else 
			GetName=CachedPlayerNames(PlayerNum-1)
		End if 
	End Function 

	Function DoInit(MyMachineID, Directory_PupQRCode, Version, opdb)
		dim Nad
		Dim EndPoint
		Dim resultStr 
		Dim UUIDParts 
		Dim UUIDFile

		bProduction=1
'		bProduction=0
		SaveCurrentPlayer=0
		VenueMachineID=""
		bWaitResp=False 
		bRunAsynch=False 
		DoInit=False 
		opdbID=opdb
		dirQrCode=Directory_PupQRCode
		MachineID=MyMachineID
		TableVersion=version
		bNeedsPairing=False
		if bProduction then 
			domain = "api.scorbit.io"
		else 
			domain = "staging.scorbit.io"
			domain = "scorbit-api-staging.herokuapp.com"
		End if 
		Set fso = CreateObject("Scripting.FileSystemObject")
		dim objLocator:Set objLocator = CreateObject("WbemScripting.SWbemLocator")
		Dim objService:Set objService = objLocator.ConnectServer(".", "root\cimv2")
		Set objXmlHttpMain = CreateObject("Msxml2.ServerXMLHTTP")
		Set objXmlHttpMainAsync = CreateObject("Microsoft.XMLHTTP")
		objXmlHttpMain.onreadystatechange = GetRef("ScorbitIF_Callback")
		Set wsh = CreateObject("WScript.Shell")

		' Get Mac for Serial Number 
		dim Nads: set Nads = objService.ExecQuery("Select * from Win32_NetworkAdapter where physicaladapter=true")
		for each Nad in Nads
			if not isnull(Nad.MACAddress) then
				if left(Nad.MACAddress, 6)<>"00090F" then ' Skip over forticlient MAC
dbg "Using MAC Addresses:" & Nad.MACAddress & " From Adapter:" & Nad.description   
					MyMac=replace(Nad.MACAddress, ":", "")
					Exit For 
				End if 
			End if 
		Next
		Serial=eval("&H" & mid(MyMac, 5))
		if Serial<0 then Serial=eval("&H" & mid(MyMac, 6))		' Mac Address Overflow Special Case 
		if MyMachineID<>2108 then 			' GOTG did it wrong but MachineID should be added to serial number also
			Serial=Serial+MyMachineID
		End if 
'		Serial=123456
		dbg "Serial:" & Serial

		' Get System UUID
		set Nads = objService.ExecQuery("SELECT * FROM Win32_ComputerSystemProduct")
		for each Nad in Nads
			dbg "Using UUID:" & Nad.UUID   
			MyUUID=Nad.UUID
			Exit For 
		Next

		if MyUUID="" then 
			MsgBox "SCORBIT - Can get UUID, Disabling."
			Exit Function
		elseif MyUUID="03000200-0400-0500-0006-000700080009" or ScorbitAlternateUUID then
			If fso.FolderExists(UserDirectory) then 
				If fso.FileExists(UserDirectory & "ScorbitUUID.dat") then
					Set UUIDFile = fso.OpenTextFile(UserDirectory & "ScorbitUUID.dat",1)
					MyUUID = UUIDFile.ReadLine()
					UUIDFile.Close
					Set UUIDFile = Nothing
				Else 
					MyUUID=GUID()
					Set UUIDFile=fso.CreateTextFile(UserDirectory & "ScorbitUUID.dat",True)
					UUIDFile.WriteLine MyUUID
					UUIDFile.Close
					Set UUIDFile=Nothing
				End if
			End if 
		End if

		' Clean UUID
		UUIDParts=split(MyUUID, "-")
		MyUUID=LCASE(Hex(eval("&h" & UUIDParts(0))+MyMachineID) & UUIDParts(1) &  UUIDParts(2) &  UUIDParts(3) & UUIDParts(4))		 ' Add MachineID to UUID
		MyUUID=LPad(MyUUID, 32, "0")
'		MyUUID=Replace(MyUUID, "-",  "")
		dbg "MyUUID:" & MyUUID 


		' Authenticate and get our token 
		if getStoken() then 
			bEnabled=True 
'			SendHeartbeat
			DoInit=True
		End if 
	End Function 

	Sub Callback()
		Dim ResponseStr
		Dim i 
		Dim Parts
		Dim Parts2
		Dim Parts3
		if bEnabled=False then Exit Sub 

		if bWaitResp and objXmlHttpMain.readystate=4 then 
'			dbg "CALLBACK: " & objXmlHttpMain.Status & " " & objXmlHttpMain.readystate
			if objXmlHttpMain.Status=200 and objXmlHttpMain.readystate = 4 then 
				ResponseStr=objXmlHttpMain.responseText
				'dbg "RESPONSE: " & ResponseStr

				' Parse Name 
				If bSessionActive = True Then
					if CachedPlayerNames(SaveCurrentPlayer-1)="" then  ' Player doesnt have a name
						if instr(1, ResponseStr, "cached_display_name") <> 0 Then	' There are names in the result
							Parts=Split(ResponseStr,",{")							' split it 
							if ubound(Parts)>=SaveCurrentPlayer-1 then 				' Make sure they are enough avail
								if instr(1, Parts(SaveCurrentPlayer-1), "cached_display_name")<>0 then 	' See if mine has a name 
									CachedPlayerNames(SaveCurrentPlayer-1)=GetJSONValue(Parts(SaveCurrentPlayer-1), "cached_display_name")		' Get my name
									CachedPlayerNames(SaveCurrentPlayer-1)=Replace(CachedPlayerNames(SaveCurrentPlayer-1), """", "")
									Scorbit_PlayerClaimed SaveCurrentPlayer, CachedPlayerNames(SaveCurrentPlayer-1)
	'								dbg "Player Claim:" & SaveCurrentPlayer & " " & CachedPlayerNames(SaveCurrentPlayer-1)
								End if 
							End if
						End if 
					else												    ' Check for unclaim 
						if instr(1, ResponseStr, """player"":null")<>0 Then	' Someone doesnt have a name
							Parts=Split(ResponseStr,"[")						' split it 
	'dbg "Parts:" & Parts(1)
							Parts2=Split(Parts(1),"}")							' split it 
							for i = 0 to Ubound(Parts2)
	'dbg "Parts2:" & Parts2(i)
								if instr(1, Parts2(i), """player"":null")<>0 Then
									CachedPlayerNames(i)=""
								End if 
							Next 
						End if 
					End if
				End If

				'Check heartbeat
				HandleHeartbeatResp ResponseStr
			End if 
			bWaitResp=False
		End if 
	End Sub

	Public Sub StartSession()
		if bEnabled=False then Exit Sub 
		dbg "Scorbit Start Session" 
		CachedPlayerNames(0)=""
		CachedPlayerNames(1)=""
		CachedPlayerNames(2)=""
		CachedPlayerNames(3)=""
		bRunAsynch=True 
		bActive="true"
		bSessionActive=True
		SessionSeq=0
		SessionUUID=GUID()
		SessionTimeStart=GameTime
		LogIdx=0
		SendUpdate 0, 0, 0, 0, 1, 1, 1
	End Sub

	' Custom method for TNA to work around coop mode stuttering
	Public Sub ForceAsynch(enabled)
		if bEnabled=False then Exit Sub
		if bSessionActive=True then Exit Sub 'Sessions should always control asynch when active
		bRunAsynch=enabled
	End Sub

	Public Sub StopSession(P1Score, P2Score, P3Score, P4Score, NumberPlayers)
		StopSession2 P1Score, P2Score, P3Score, P4Score, NumberPlayers, False
	End Sub 

	Public Sub StopSession2(P1Score, P2Score, P3Score, P4Score, NumberPlayers, bCancel)
		Dim i
		dim objFile
		if bEnabled=False then Exit Sub 
		bRunAsynch=False 'Asynch might have been forced on in TNA to prevent coop mode stutter
		if bSessionActive=False then Exit Sub 
dbg "Scorbit Stop Session" 

		bActive="false" 
		SendUpdate P1Score, P2Score, P3Score, P4Score, -1, -1, NumberPlayers
		bSessionActive=False
'		SendHeartbeat

		if bUploadLog and LogIdx<>0 and bCancel=False then 
			dbg "Creating Scorbit Log: Size" & LogIdx
			Scorbit_LOGUpload(0)
			Set objFile = fso.CreateTextFile(puplayer.getroot&"\" & cGameName & "\sGameLog.csv")
			For i = 0 to LogIdx-1 
				objFile.Writeline LOGFILE(i)
			Next 
			objFile.Close
			LogIdx=0
			Scorbit_LOGUpload(1)
			pvPostFile "https://" & domain & "/api/session_log/", puplayer.getroot&"\" & cGameName & "\sGameLog.csv", False
			Scorbit_LOGUpload(2)
			on error resume next
			fso.DeleteFile(puplayer.getroot&"\" & cGameName & "\sGameLog.csv")
			on error goto 0
		End if 

	End Sub 

	Public Sub SetGameMode(GameModeStr)
		GameModeOrig=GameModeStr
		GameMode=GameModeStr
		GameMode=Replace(GameMode, ":", "%3a")
		GameMode=Replace(GameMode, ";", "%3b")
		GameMode=Replace(GameMode, " ", "%20")
		GameMode=Replace(GameMode, "{", "%7B")
		GameMode=Replace(GameMode, "}", "%7D")
	End sub 

	Public Sub SendUpdate(P1Score, P2Score, P3Score, P4Score, CurrentBall, nPlayer, NumberPlayers)
		SendUpdateAsynch P1Score, P2Score, P3Score, P4Score, CurrentBall, nPlayer, NumberPlayers, bRunAsynch
	End Sub 

	Public Sub SendUpdateAsynch(P1Score, P2Score, P3Score, P4Score, CurrentBall, nPlayer, NumberPlayers, bAsynch)
		dim i
		Dim PostData
		Dim resultStr
		dim LogScores(4)

		if bUploadLog then 
			if NumberPlayers>=1 then LogScores(0)=P1Score
			if NumberPlayers>=2 then LogScores(1)=P2Score
			if NumberPlayers>=3 then LogScores(2)=P3Score
			if NumberPlayers>=4 then LogScores(3)=P4Score
			LOGFILE(LogIdx)=DateDiff("S", "1/1/1970", Now()) & "," & LogScores(0) & "," & LogScores(1) & "," & LogScores(2) & "," & LogScores(3) & ",,," &  nPlayer & "," & CurrentBall & ",""" & GameModeOrig & """"
			LogIdx=LogIdx+1
		End if

		if bSessionActive=False then Exit Sub 
		if bEnabled=False then Exit Sub 
		if bWaitResp then exit sub ' Drop message until we get our next response 

'		msgbox "currentplayer: " & CurrentPlayer
		SaveCurrentPlayer=nPlayer
		PostData = "session_uuid=" & SessionUUID & "&session_time=" & GameTime-SessionTimeStart+1 & _
					"&session_sequence=" & SessionSeq & "&active=" & bActive

		SessionSeq=SessionSeq+1
		if NumberPlayers > 0 then 
			for i = 0 to NumberPlayers-1
				PostData = PostData & "&current_p" & i+1 & "_score="
				if i <= NumberPlayers-1 then 
					if i = 0 then PostData = PostData & P1Score
					if i = 1 then PostData = PostData & P2Score
					if i = 2 then PostData = PostData & P3Score
					if i = 3 then PostData = PostData & P4Score
				else 
					PostData = PostData & "-1"
				End if 
			Next 

			PostData = PostData & "&current_ball=" & CurrentBall & "&current_player=" & nPlayer
			if GameMode<>"" then PostData=PostData & "&game_modes=" & GameMode
		End if 
		resultStr = PostMsg("https://" & domain, "/api/entry/", PostData, bAsynch)
		'if resultStr<>"" then dbg "SendUpdate Resp:" & resultStr    			'rtp12
	End Sub 

' PRIVATE BELOW 
	Private Function LPad(StringToPad, Length, CharacterToPad)
	  Dim x : x = 0
	  If Length > Len(StringToPad) Then x = Length - len(StringToPad)
	  LPad = String(x, CharacterToPad) & StringToPad
	End Function

	Private Function GUID()		
		Dim TypeLib
		Set TypeLib = CreateObject("Scriptlet.TypeLib")
		GUID = Mid(TypeLib.Guid, 2, 36)
	End Function

	Private Function GetJSONValue(JSONStr, key)
		dim i 
		Dim tmpStrs,tmpStrs2
		if Instr(1, JSONStr, key)<>0 then 
			tmpStrs=split(JSONStr,",")
			for i = 0 to ubound(tmpStrs)
				if instr(1, tmpStrs(i), key)<>0 then 
					tmpStrs2=split(tmpStrs(i),":")
					GetJSONValue=tmpStrs2(1)
					exit for
				End if 
			Next 
		End if 
	End Function

	Private Sub SendHeartbeat()
		Dim resultStr
		if bEnabled=False then Exit Sub 
		resultStr = GetMsgHdr("https://" & domain, "/api/heartbeat/", "Authorization", "SToken " & sToken)
		
		'Customized for TNA
		If bRunAsynch = False Then 
			dbg "Heartbeat Resp:" & resultStr
			HandleHeartbeatResp ResultStr
		End If
	End Sub 

	'TNA custom method
	Private Sub HandleHeartbeatResp(resultStr)
		dim TmpStr
		Dim Command
		Dim rc
		'Dim QRFile:QRFile=puplayer.getroot&"\" & cGameName & "\" & dirQrCode
		Dim QRFile:QRFile=puplayer.getroot & cGameName & "\" & dirQrCode
'dbg "QRFile: " &QRFile
		If VenueMachineID="" then
			If resultStr<>"" And Not InStr(resultStr, """machine_id"":" & machineID)=0 Then 'We Paired
				bNeedsPairing=False
				dbg "Scorbit: Paired"
				Scorbit_Paired()
			ElseIf resultStr<>"" And Not InStr(resultStr, """unpaired"":true")=0 Then 'We Did not Pair
				dbg "Scorbit: NOT Paired"
				bNeedsPairing=True
			Else
				' Error (or not a heartbeat); do nothing
			End If

			TmpStr=GetJSONValue(resultStr, "venuemachine_id")
			if TmpStr<>"" then 
				VenueMachineID=TmpStr
'dbg "VenueMachineID=" & VenueMachineID			
				'Command = """" & puplayer.getroot&"\" & cGameName & "\sQRCode.exe"" " & VenueMachineID & " " & opdbID & " """ & QRFile & """"
				Command = """" & puplayer.getroot & cGameName & "\sQRCode.exe"" " & VenueMachineID & " " & opdbID & " """ & QRFile & """"
				rc = wsh.Run(Command, 0, False)
			End if 
		End if
	End Sub

	Private Function getStoken()
		Dim result
		Dim results
'		dim wsh
		Dim tmpUUID:tmpUUID="adc12b19a3504453a7414e722f58736b"
		Dim tmpVendor:tmpVendor="vscorbitron"
		Dim tmpSerial:tmpSerial="999990104"
		'Dim QRFile:QRFile=puplayer.getroot&"\" & cGameName & "\" & dirQrCode
		Dim QRFile:QRFile=puplayer.getroot & cGameName & "\" & dirQrCode
		'Dim sTokenFile:sTokenFile=puplayer.getroot&"\" & cGameName & "\sToken.dat"
		Dim sTokenFile:sTokenFile=puplayer.getroot & cGameName & "\sToken.dat"

		' Set everything up
		tmpUUID=MyUUID
		tmpVendor="vpin"
		tmpSerial=Serial
		
		on error resume next
		fso.DeleteFile(sTokenFile)
		On error goto 0 

		' get sToken and generate QRCode
'		Set wsh = CreateObject("WScript.Shell")
		Dim waitOnReturn: waitOnReturn = True
		Dim windowStyle: windowStyle = 0
		Dim Command 
		Dim rc
		Dim objFileToRead

		'Command = """" & puplayer.getroot&"\" & cGameName & "\sToken.exe"" " & tmpUUID & " " & tmpVendor & " " &  tmpSerial & " " & MachineID & " """ & QRFile & """ """ & sTokenFile & """ " & domain
		Command = """" & puplayer.getroot & cGameName & "\sToken.exe"" " & tmpUUID & " " & tmpVendor & " " &  tmpSerial & " " & MachineID & " """ & QRFile & """ """ & sTokenFile & """ " & domain
dbg "RUNNING Command:" & Command
		rc = wsh.Run(Command, windowStyle, waitOnReturn)
dbg "Return:" & rc
		if FileExists(puplayer.getroot&"\" & cGameName & "\sToken.dat") and rc=0 then
			Set objFileToRead = fso.OpenTextFile(puplayer.getroot&"\" & cGameName & "\sToken.dat",1)
			result = objFileToRead.ReadLine()
			objFileToRead.Close
			Set objFileToRead = Nothing

			if Instr(1, result, "Invalid timestamp")<> 0 then 
				MsgBox "Scorbit Timestamp Error: Please make sure the time on your system is exact"
				getStoken=False
			elseif Instr(1, result, ":")<>0 then 
				results=split(result, ":")
				sToken=results(1)
				sToken=mid(sToken, 3, len(sToken)-4)
dbg "Got TOKEN:" & sToken
				getStoken=True
			Else 
dbg "ERROR:" & result
				getStoken=False
			End if 
		else 
dbg "ERROR No File:" & rc
		End if 

	End Function 

	private Function FileExists(FilePath)
		If fso.FileExists(FilePath) Then
			FileExists=CBool(1)
		Else
			FileExists=CBool(0)
		End If
	End Function

	Private Function GetMsg(URLBase, endpoint)
		GetMsg = GetMsgHdr(URLBase, endpoint, "", "")
	End Function

	Private Function GetMsgHdr(URLBase, endpoint, Hdr1, Hdr1Val)
		Dim Url
		Url = URLBase + endpoint & "?session_active=" & bActive
'dbg "Url:" & Url  & "  Async=" & bRunAsynch
		objXmlHttpMain.open "GET", Url, bRunAsynch
'		objXmlHttpMain.setRequestHeader "Content-Type", "text/xml"
		objXmlHttpMain.setRequestHeader "Cache-Control", "no-cache"
		if Hdr1<> "" then objXmlHttpMain.setRequestHeader Hdr1, Hdr1Val

'		on error resume next
			err.clear
			objXmlHttpMain.send ""
			if err.number=-2147012867 then 
				MsgBox "Multiplayer Server is down (" & err.number & ") " & Err.Description
				bEnabled=False
			elseif err.number <> 0 then 
				dbg "Server error: (" & err.number & ") " & Err.Description
			End if 
			if bRunAsynch=False then 
dbg "Status: " & objXmlHttpMain.status
				If objXmlHttpMain.status = 200 Then
					GetMsgHdr = objXmlHttpMain.responseText
				Else 
					GetMsgHdr=""
				End if 
			Else 
				bWaitResp=True
				GetMsgHdr=""
			End if 
'		On error goto 0

	End Function

	Private Function PostMsg(URLBase, endpoint, PostData, bAsynch)
		Dim Url

		Url = URLBase + endpoint
'dbg "PostMSg:" & Url & " " & PostData			'rtp12

		objXmlHttpMain.open "POST",Url, bAsynch
		objXmlHttpMain.setRequestHeader "Content-Type", "application/x-www-form-urlencoded"
		objXmlHttpMain.setRequestHeader "Content-Length", Len(PostData)
		objXmlHttpMain.setRequestHeader "Cache-Control", "no-cache"
		objXmlHttpMain.setRequestHeader "Authorization", "SToken " & sToken
		if bAsynch then bWaitResp=True 

		on error resume next
			objXmlHttpMain.send PostData
			if err.number=-2147012867 then 
				MsgBox "Multiplayer Server is down (" & err.number & ") " & Err.Description
				bEnabled=False
			elseif err.number <> 0 then 
				'dbg "Multiplayer Server error (" & err.number & ") " & Err.Description
			End if 
			If objXmlHttpMain.status = 200 Then
				PostMsg = objXmlHttpMain.responseText
			else 
				PostMsg="ERROR: " & objXmlHttpMain.status & " >" & objXmlHttpMain.responseText & "<"
			End if 
		On error goto 0
	End Function

	Private Function pvPostFile(sUrl, sFileName, bAsync)
'dbg "Posting File " & sUrl & " " & sFileName & " " & bAsync & " File:" & Mid(sFileName, InStrRev(sFileName, "\") + 1)
		Dim STR_BOUNDARY:STR_BOUNDARY  = GUID()
		Dim nFile  
		Dim baBuffer()
		Dim sPostData
		Dim Response

		'--- read file
		Set nFile = fso.GetFile(sFileName)
		With nFile.OpenAsTextStream()
			sPostData = .Read(nFile.Size)
			.Close
		End With


		'--- prepare body
		sPostData = "--" & STR_BOUNDARY & vbCrLf & _
			"Content-Disposition: form-data; name=""uuid""" & vbCrLf & vbCrLf & _
			SessionUUID & vbcrlf & _
			"--" & STR_BOUNDARY & vbCrLf & _
			"Content-Disposition: form-data; name=""log_file""; filename=""" & SessionUUID & ".csv""" & vbCrLf & _
			"Content-Type: application/octet-stream" & vbCrLf & vbCrLf & _
			sPostData & vbCrLf & _
			"--" & STR_BOUNDARY & "--"


		'--- post
		With objXmlHttpMain
			.Open "POST", sUrl, bAsync
			.SetRequestHeader "Content-Type", "multipart/form-data; boundary=" & STR_BOUNDARY
			.SetRequestHeader "Authorization", "SToken " & sToken
			.Send sPostData ' pvToByteArray(sPostData)
			If Not bAsync Then
				Response= .ResponseText
				pvPostFile = Response
dbg "Upload Response: " & Response
			End If
		End With

	End Function

	Private Function pvToByteArray(sText)
		pvToByteArray = StrConv(sText, 128)		' vbFromUnicode
	End Function

End Class 
'  END SCORBIT 
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

