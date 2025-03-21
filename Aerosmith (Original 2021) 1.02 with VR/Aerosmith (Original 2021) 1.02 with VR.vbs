' Allknowing2012 - Please dont redistribute at this time...
' 2023
' v1.02 now with VR
' My Channel: https://www.youtube.com/channel/UCi9jCs_DmgtHWqLVPf5_7rA
'
' rel 1.01 - changed the COMMENTED OUT code for the nudge buttons to use the VPM keycodes (for those looking for nudge Keys)
'       - remove docallouts - notused
'		- Namestr undefined bug
'		- flexdmd option now has backglass. 
'		- Bug Scorbit sending updates when not paired
'		- ball eject from kicker bug
'		- ScorBit is now OFF by default Line 135
'  1.02 - VR Updates from MikeDaSpike
'		- TODO after 2 player high score, score is not shown	
'		- TODO Add callouts	
'		- apron image update from drik333
'		- Todo no music after 1st ball	
'		- TODO Nailbuster fixes..to PUP.zip 
'
' Code Framework based heavily on the Guardians of the Galaxy 2.x table
' Credits there include 
'
' By: Pincredibles
' Special thanks: 
' Arngrim, MPT3k, Mr H, Nailbuster, Rik, Harlan, Orbital Group, VAAS, wAxx, Ashtone6, Al, Terry
' VPW Team, namely Apophis, PinStratsDan, BountyBob, Sixtoe, Wylte, Hauntfreaks and many more...
'
'
'                                   __,▄▄▄█▄▄_                                __µ▄▄█▓▓
'                               _╓▄█▓▓▓▓▓▓▓▓▓█                          _,▄▄█▓▓▓▓▓▓▓▓▓
'                           _ç▄█▓▓▓▓▓▓▓▓▓▓▓▓▓▀                      µ▄█▓▓▓▓▓▓▓▓▓▓▓▓▓▓
'                       _,▄█▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓╜                   _╓▄█▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▀
'                     _▄█▓▓▓▓▓▓▓▓▓▓▓▀▀"                     ,▄█▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▀
'                   ▄█▓▓▓▓▓▓▓▀╙`                          ▄█▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▀      
'                ,█▓▓▓▓▓▓▓▀                            ¿█▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▀
'             _¡█▓▓▓▓▓▓▓╜                           _¿█▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
'            ,█▓▓▓▓▓▓▓╙                            ,█▓▓▓▓▓▓▓▓▓▓▀²▓▓▓▓▓▓▓▓▀   
'          _▄▓▓▓▓▓▓▓▀                            _▄▓▓▓▓▓▓▓▓▓▓╙ _▐▓▓▓▓▓▓▓"
'         ,▓▓▓▓▓▓▓▓                              █▓▓▓▓▓▓▓▓▓    ╓▓▓▓▓▓▓▓
'       _▄▓▓▓▓▓▓▓▀                             ¡▓▓▓▓▓▓▓▓▓"    ]▓▓▓▓▓▓▓`
'      _█▓▓▓▓▓▓▓▀                             ]▓▓▓▓▓▓▓▓▀     _▓▓▓▓▓▓▓M
'     _█▓▓▓▓▓▓▓▌                             ¡▓▓▓▓▓▓▓▓╛     _█▓▓▓▓▓▓▌
'     █▓▓▓▓▓▓▓▓                    _,,▄▄▄▄▄▄▄▓▓▓▓▓▓▓▓       ▐▓▓▓▓▓▓▓
'    ▐▓▓▓▓▓▓▓▓▓               _µ▄██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓       ]▓▓▓▓▓▓▓M
'   _▓▓▓▓▓▓▓▓▓▓,          _╓▄█▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓█µ      ▓▓▓▓▓▓▓▓                        V1.02 2023
'   ▄▓▓▓▓▓▓▓▓▓▓▓▄    _,▄██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▀▀▀▓▓▓▓▓▓▓▓▓▓█▄_  ]▓▓▓▓▓▓▓`
'   ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▀▀_     ▐▓▓▓▓▓▓▀▓▓▓▓▓█µ_▓▓▓▓▓▓▓▓
'   ]▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▀`        ,▓▓▓▓▓▓▌ _Ñ▓▓▓▓▓█▓▓▓▓▓▓▓M
'   _Ñ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▀            █▓▓▓▓▓▓    ]▓▓▓▓▓▓▓▓▓▓▓▓
'     ²▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▀╜_            _█▓▓▓▓▓▓      ▓▓▓▓▓▓▓▓▓▓▓▌
'       _▀▓▓▓▓▓▓▓▓▓▓▀╙_                █▓▓▓▓▓▓Ω      ▐▓▓▓▓▓▓▓▓▓▓_
'              _,▄▓▓▓▓▓▓            ▄▓▓▓▓▓▓▓▓       _▓▓▓▓▓▓▓▓▓▓▓_
'             ╓█▓▓▓▓▓▓▓          _▄▓▓▓▓▓▓▓▓▓        ▐▓▓▓▓▓▓▓▓▓▓▓_
'           ç█▓▓▓▓▓▓▓▓▓µ      _¿█▓▓▓▓▓▓▓▓▓▓       _▄▓▓▓▓▓▓▓▓▓▓▓▓█_
'         _█▓▓▓▓▓▓▓▓▓▓▓▓▓█▄▄██▓▓▓▓▓▓▓▓▓▓▓▓       ¿█▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓███▄
'         █▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▀     _╓█▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
'        █▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓Ö    _▄█▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▀
'       _▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓    _ç█▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▀"
'       _▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▀    _█▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▀_
'        ▐▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▀      ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓Ö
'         ╙▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▀       ]▓▓▓▓▓▓▓▓▓▓▓▓▓▀╙_
'          _²▀▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▀`_         ²▓▓▓▓▓▓▓▓▀╜╙╙
'              _²╙▀▀▀▀▀▀▀╙`                ╙╙╙╙╙    ╙
'    
'    
'
'
' .10 - add flexdmd code, skillshot
' .11 - add smartbutton, scorbit, shotmultiplier, rework Rats Arrows, clear Super Modes, Upgrade for Dude
' .12 - Fix Sweet, Saddle, Extraball for 3 n 6 Modes, ElevatorMB + SuperJackpot, PupOverlay
' .13 - Fix Toybox MB
' .14 - Shot Multipliers, Wizrd Medley Mode, AddABall fix, FinalTour Mode
'  15 - More Flexdmd and Non DMD info, Medley Tour fixes
'
'
' Todo Combo is more points more coins
'      Final Tour Last Shot is Scoop lock flippers  and add ball to lane    "PLAYER 1 KEEP SHOOTING"
'
'  Todo queueflush queueflushpriority
'  Todo Make VIP harder after each success
'


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
'    INSTALL NOTES
' if you are NOT using PUP you STILL need to get the music from the pup pack and place it in <pinball folder>\Music\aerosmith
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' MODES 
' Toys - Gather shots while in toy box multiball
' Love - Gather shots while in Elevator multiball
' Rats - 1 From Left then 1 From Right, then 2 From Left then 2 From Right, etc - Super Spinner  
' Dude - Center, Elevator - Super Lanes
' Back - Right Orbit (f) - Super Pops   50 hits 275K each
' Sweet  - Jester,Elevator,L (f) /R Lock Balls - Super Scoring - Hard Mode: resets when not 100% complete
'    Update -  The 6 “easiest” shots are available at mode start. Once completed 6 more “difficult” shots are added.
' Walk - l/R Orbits and Center (f) or Right Ramp (f) - Super Ramps
'    Update - Now 7 shots are available after Cranking Up the mode. Make 10 shots to complete the mode.
' Same - Elevator (f) (then both L/R Target banks) - Super Targets
' Last - Center/ Left (f) & Right Orbit (f) - Super Orbits
'
'
' MedleyMB - 
'	Last Child = 2 shots required to finish, shots are not extinguished.
'	Walk this Way = 4 shots required to finish, shots are not extinguished until the last shot.
'	Same Old Song and Dance = 9 target shots required to finish, shots are extinguished.
'	Sweet Emotion = 6 shots required to finish, shots are extinguished.
'	Dude Looks Like a Lady = 4 shots (1 scoop, 3 lanes) required to finish, shots are extinguished.
'	Back in the Saddle = 25 pop hits required to finish.
'	Rats in the Cellar = 25 spinner hits required to finish.
'	Toys in the Attic = 6 shots (toy box) required to finish, progress shown on 6 toy box inserts.
'	Love in an Elevator = 3 shots required to finish, shots are not extinguished.
'
' Notes:
' Elevator - l+R orbit then elevator x3, then 1 Shot + Elevator x 9 times for Super Jackpot and 100% complete
'           - continues prexisting mode but does NOT start a new mode
' 1 hit flashes the toy box light, then If you sink it changes solid .. 
'       -- gets harder in later levels - 
' shoot box to light lock (green) then it can be thrown in to box  -  Done
' Shoot any shot to light crank it up, then score it to add 20s & 500K -- Done
' shoot left & right of toybox to light VIP Pass (ball Save)  - done
' can abort 3 ball multiball - 3 second countdown BEFORE you throw the ball -- done
' spell aer-os-mith to get shot  multiplier
' 3 dude lights increases bonus multiplier  - done


'
' Shot X Multipliers - Complete the AEROSMITH targets to qualify the “X” inserts at all major shots. 
'      The next major shot you make with a flashing “X” insert scores double for the remainder of the ball, 
'       and qualifying every “X” insert on a single ball enables a roving “X” insert for 3x
' Updates: 
'      - completing Same Old Song and Dance will also light the shot multipliers.

Option Explicit
Randomize

Dim ScorbitActive:ScorbitActive	= 0		' Is Scorbit Active

Const bUsePlungerForSternKey 	= False	' Defaults to Right Magna Button but you can use Plunger button also	
Const kBallSearchTimeout 		= 10000	'  Start ball search after 15 seconds of no activity
Const DMDMode					= 2 	' 0=None, 1 = Flex/Ultra, 2=PUP (make sure you set PuPDMDDriverType below)
TextBox.Visible					= False ' Set to True if you have nothing, pup, flex, (debug for Me)
Const bFlipperSkipEnabled 		= True	' Skip scenes
Const FontScale					= 1		' Scales the PupFonts up/down for different sized DMDs 		[Desktop 0.75]
Const FontScaleDmd				= 0.5	' Scales the SlimDMDFonts up/down for different sized DMDs  [Desktop 0.5]
	  	
Const ScorbitShowClaimQR		= 1 	' If Scorbit is active this will show a QR Code in the bottom left on ball 1 that allows player to claim the active player from the app
Const ScorbitClaimSmall			= 0 	' Make Claim QR Code smaller for high res backglass 
Const ScorbitUploadLog			= 0 	' Store local log and upload after the game is over 
Const ScorbitAlternateUUID  	= 0 	' Force Alternate UUID from Windows Machine and saves it in VPX Users directory (C:\Visual Pinball\User\ScorbitUUID.dat)
Const osbactive					= 0 	' Orbital Scoreboard: Set to 0 for off, 1 for only player 1 to be sent, 2 for all scores to be sent.	
										' See link to create obs.vbs: https://docs.orbitalpin.com/vpx-user-settings
Const VRCabinet					= 0     ' Change cabinet style : 0= Pro, 1= Premium, 2=Limited Edition

Dim AutoQA:AutoQa=False                	'Main QA Testing FLAG setting to false will disable all this stuff.

Dim AutoAI:AutoAI=False				:debugWall2.IsDropped=True:DebugWall1.IsDropped=True:DebugWall.IsDropped=True
'Dim AutoAI:AutoAI=True				:debugWall2.IsDropped=True:debugWall1.IsDropped=True
'Dim AutoAI:AutoAI=False

'AutoQA=True

If AutoAI then TurnOnAI

If DMDMode <> 2 then ScorbitActive = 0
		
Dim shotval
Dim shotvalRight

Const bHardMode=False			' Hard Same -- Same Old Song and Dance - Aerosmith lights come in after CIU
								'           -- mode is 30s with CIU of another 20s


Const DigitFont="DIGIT LCD"
Const DMDScrFont="Donnie Solid Narrow"  ' "Impact"
Const DMDMainFont="Donnie Solid Narrow"

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

'**************************
'   UltraDMD USER Config
'**************************
Const UseFullColor = "True" 			'	ULTRA: Enable full Color on UltraDMD "True" / "False"
Const UltraDMDVideos = True				'	ULTRA: Works on my DMDv3 but seems it causes issues on others
'**************************
'   PinUp Player USER Config
'**************************
dim PuPDMDDriverType: PuPDMDDriverType=0   	' 0=LCD DMD, 1=RealDMD (For FULLDMD use the batch scripts in the pup pack)
dim useRealDMDScale : useRealDMDScale=0    	' 0 or 1 for RealDMD scaling.  Choose which one you prefer.
dim useDMDVideos    : useDMDVideos=True	   	' true or false to use DMD splash videos.
Dim pGameName       : pGameName="aerosmith"	' pupvideos foldername, probably set to cGameName in realworld
'***********TABLE VOLUME LEVELS ********* 
' [Value is from 0 to 1 where 1 is full volume. 
' NOTE: you can go past 1 to amplify sounds]

Const VolBGMusic = 0.7  ' Volume for Video Clips   
Const VolMusic = 0.7    ' Volume for the Songs

Const VolDef = 0.8		' Default volume for callouts 
Const VolSfx = 0.6		' Volume for table Sound effects 

' VolumeDial:
' VolumeDial is the actual global volume multiplier for the mechanical sounds.
' Values smaller than 1 will decrease mechanical sounds volume.
' Recommended values should be no greater than 1.
Const VolumeDial = 0.8

Const BallSize = 50    ' 50 is the normal size
Const BallMass = 1.0     ' 1 is normal ball

'********* UltraDMD **************
Dim FlexDMD
Dim FontScoreInactive, FontScoreActive

Dim UltraDMD:UltraDMD=0
Const UltraDMD_VideoMode_Stretch = 0
Const UltraDMD_VideoMode_Top = 1
Const UltraDMD_VideoMode_Middle = 2
Const UltraDMD_VideoMode_Bottom = 3
Const UltraDMD_Animation_FadeIn = 0
Const UltraDMD_Animation_FadeOut = 1
Const UltraDMD_Animation_ZoomIn = 2
Const UltraDMD_Animation_ZoomOut = 3
Const UltraDMD_Animation_ScrollOffLeft = 4
Const UltraDMD_Animation_ScrollOffRight = 5
Const UltraDMD_Animation_ScrollOnLeft = 6
Const UltraDMD_Animation_ScrollOnRight = 7
Const UltraDMD_Animation_ScrollOffUp = 8
Const UltraDMD_Animation_ScrollOffDown = 9
Const UltraDMD_Animation_ScrollOnUp = 10
Const UltraDMD_Animation_ScrollOnDown = 11
Const UltraDMD_Animation_None = 14
Const UltraDMD_deOn = 1500

'********* End UltraDMD **************

Const BlinkIntFast   = 70
Const BlinkIntDef    = 125
Const BlinkIntSlow   = 500
Const BlinkPatternDef  = 10
Const BlinkPatternSlow = 100

Dim luts, lutpos
luts = array("ColorGradeLUT256x16_1to1", "ColorGradeLUT_Bright2", "ColorGradeLUT_Bright", "ColorGradeLUT256x16_ConSat", "ColorGradeLUT256x16_HalfSat" )
lutpos = 0	

' Main Code
' Define any Constants
Const cGameName = "aerosmith"		' Match DOF Config Tool
Const TableName = "aerosmith"
Const myVersion = "1.0.0"
Const MaxPlayers = 4		
Dim MusicDir	

'VR_
Dim VR_Room : If RenderingMode = 2 Then VR_Room=True Else VR_Room=False

'*****************************************
'      Structure to save/restore all player data before moving to a new ones
' ***************************************
Const kStack_Pri0 = 0		' base mode
Const kStack_Pri1 = 1		' toybox, elevator mb
Const kStack_Pri2 = 2		' wizard
Const kStack_Pri3 = 3		' Skillshot

Class cArrowState
	Public ArrowColor
	Public ArrowState
	Public MultipState
	Public NameState
End Class

' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(4)			
Dim BonusHeldPoints(4)
Dim BonusMultiplier
Dim PlayMultiplier
Dim ModePoints
Dim ModePointsSave

Dim WizardModePoints
Dim WizardBonusPoints
			
Dim bBonusHeld
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Score(4)
Dim HighScore(5)   ' GC, 1,2,3, Med-GC
Dim HighScoreName(5)
Dim TiltCount(4)
Dim Jackpot
Dim VipValue
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim mToyBoxBalls2Eject
Dim SkillshotValue
Dim bCreatedBall	' we just created a ball
Dim bAutoPlunger	' Auto fire 
Dim bAutoPlunged	' Did AutoPlunger Fire
Dim bInstantInfo
Dim BumperMultiplier 
dim bLaneSaverEnabled
Dim FlipperSkipCmd		' Command to when you FlipperSkip
Dim BallSearchCnt
Dim bResetCurrentGame
Dim SongNum
Dim ModeCountdown
Dim modeRampColor
Dim bDebounce
Dim bPauseTimer
Dim Multiplier3x
Dim RatsPos1 'Hold the random Rats position for the start of a ball
Dim RatsPos2 'Hold the random Rats position
Dim RatsPos3 'Hold the random Rats position
Dim RatsStep 

Dim baLightsSaved
Dim baSaveLightsSaved
Dim baRampLightsSaved

Dim bShotMultiplierSelect

Dim modesStarted
Dim ModesCompleted

Dim bFinalTourDone
Dim bFinalTourReady
Dim FinalTourCount
Dim bWizardMode

Dim bMedleyTourProgress(8)	'
Dim bMedleyTourDone			
Dim bMedleyTourReady		' ready to start - progressed thru the prior modes
Dim bMedleyTourCount		' how many tour modes completed

Dim Coins 
Dim HiddenJesters
Dim Dice
Dim SpinHits
Dim SpinScore
Dim SpinScoreCurrent
Dim SpinValue

Dim SpinnerBonus
Dim TargetBonus
Dim BumperBonus
Dim LoopBonus
Dim RampBonus
Dim LaneBonus

Dim PopHits
Dim PopScore
Dim PopScoreCurrent
Dim PopValue
Dim PopLevel
Dim SwitchHitCount

Dim BonusModeTotal(8)		' Holds the total bonus during gameplay
Dim BonusModeValue(8)		' Holds the bonus value that will get added to the next mode bonus
Dim ModeNames(8)
Dim ModeNamesShort(8)
Dim Shots(8)
Dim HoldAerosmithLights(9)

Dim ModeProgress(8)
Dim ModeOrder(8)			' retain what order they were attempted in as this is used in the Medely/Final Tour
Dim ModePercent(8)
dim Mode2Percent(8)
dim Mode2Progress(8)
Dim Mode2Value(8)
Dim Mode2Total(8)

Dim bModeProgressUpgraded

Dim ToyBoxMBFinished			' Did we complete ToyBoxMB
Dim ToyBoxMultiBallCount		' How many times have we completed ToyBox MB
Dim ToyBoxMBLocks				' How many ToyBox Locks we have done during this toybox MB. We can only lock it a total of 3 times in MB
Dim ToyBoxMultiBallToggle
Dim ToyBoxMBJackpotHits
Dim ToyBoxMBJackpot
Dim ToyBoxMBJackpotBase
Dim ToyBoxMBJackpotTotal
Dim ToyBoxMBAttempts
Dim bToyBoxBonus3x
Dim ElevTarget1Toggle
Dim ElevMBJackpot
Dim ElevMBJackpotTotal
Dim bElevMultiBall
Dim ElevMultiBallAttempts

' Define Game Control Variables
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim RealBallsInLock
Dim SpellAerosmith
Dim MultiplierShot
Dim bShotMultiplier
Dim bSecondMode
Dim bBonusMode
Dim	PlayerMode2

' Define Game Flags
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim BallSaverActiveBuffer		' Allows lanes to kick in ball saver 
Dim bBallSaverReady
Dim bMultiBallMode
Dim bMusicOn
Dim ElevMultiBallCount			' How many balls locked in elevator

dim SuperSkillShotIndex
Dim bSkillshotsReady(2)

Dim bPlayerModeSelect
Dim PlayerMode
Dim SaveMode
Dim bRndModeShot

Dim bExtraBallWonThisBall
Dim bJustStarted
Dim bSkipWaitVideo

Dim plungerIM 'used mostly as an autofire plunger
Dim SmartButtonCount
Dim bTableReady
Dim bUseUltraDMD
Dim bUseFlexDMD
Dim bUsePUPDMD
Dim bPupStarted
Dim PlayerState(4)
Dim RampLightsSaveColor()
Dim LightsSaveColor()
Dim saveColor()

Const defBallSaverTime = 8		' In seconds
Dim BallSaverTime
BallSaverTime = defBallSaverTime
Const BallsPerGame = 3   ' 3 or 5
Const MaxMultiballs = 5  ' max number of balls during multiballs
Const MaxMultiplier=10

'***************************************************************************
'      Structure to save/restore all player data before moving to a new ones
' **************************************************************************

Const StackSize = 4		' How Many Modes can be stacked  - Higher priority wins    base, mb, wizard, skillshot

Class cStack
	Public bStackActive					' Is there a mode here actually stacked?
	Public ModeIndex					' What is the index of this mode?
	Public ArrowStates(9)				' What are the arrow states for this mode

	Private Sub Class_Initialize(  )
		dim i
		For i = 0 to 9
			set ArrowStates(i) = New cArrowState
		Next
	End Sub

	Public Sub Reset()
		dim i
		bStackActive = False
		ModeIndex = -1
		For i = 0 to 9
			ArrowStates(i).ArrowColor = ""
			ArrowStates(i).ArrowState = 0
			ArrowStates(i).MultipState = 0 	' 
			ArrowStates(i).NameState = 0 	' Color is always name color
		Next 
	End Sub

	Public Function GetArrowState(light)
		dim lIndex
		lIndex = GetModeIndex(light.name)
		GetArrowState = ArrowStates(lIndex).ArrowState
	End Function 

	Public Function GetArrowColor(light)
		dim lIndex
		lIndex = GetModeIndex(light.name)
		GetArrowColor = ArrowStates(lIndex).ArrowColor
	End Function 

	Public Sub SetArrowState(light, state)
		dim lIndex
		lIndex = GetModeIndex(light.name)
		ArrowStates(lIndex).ArrowState = state
	End Sub 

	Public Sub Enable (NewModeIndex) 
		bStackActive = True
		ModeIndex = NewModeIndex
	End Sub 

	Public Sub Disable() 
		Reset
	End Sub 
End Class 

Public StackState(4)			' Stores which modes are actually in play (stacked)


Class TableState
	' These should hold state of the current mode
	Public sBumperMultiplier
	Public bFirstBall
	Public lArrowStates(9,5)
	Public lLockLights(6)
	Public lLockLightsValue(6)
	Public lAerosmith(9)
	Public lShots(9)
	Public lMiscLights(20)
	Public lElevatorLights(3)
	Public lToyBoxMBState			' 
	Public bExtraball_1				' Triggered - Toy or Elev MB Extra Ball
	Public bExtraball_2				' Triggered 3 Modes are started
	Public bExtraball_3				' Triggered 6 Modes are complete
	Public sWizardMode
	Public sPopValue
	Public sPopLevel
	Public bElevMultiBallCount		' How many balls locked in elevator
	Public bElevMultiBallAttempts	' How many times we attempted to complete Love in An Elevator
	Public bElevMBJackpotHits       ' You need 6 floors to complete the Mode

	' Holds global info
	Public sModeProgress(9)			'
	Public sModeOrder(9)
	Public sModePercent(9)
	Public sMode2Progress(9)		'
	Public sMode2Percent(9)
	Public sMode2Value(9)
	Public sMode2Total(9)

	Public sAerosmithCount

	Public SFinalTourDone
	Public SFinalTourReady
	Public SFinalTourCount
	Public SFinalTourCompletions
	Public sMedleyTourReady

	Dim bCountDownEnabled		' Countdown Time info
	Dim CountdownValue

	Public bModeSelect
	Public SPlayerMode
	Public SPlayerMode2

	Public bSmartButtonCount
	Public bHiddenJesters

	Public Sub Reset()
		bFirstBall = True
		BumperMultiplier=1
		sAerosmithCount = 0
		PopValue=2000
		SpinHits=25
		SpinValue=200
		PopLevel=0
		PopHits=25
		bElevMultiBallCount = 0
		bElevMultiBallAttempts = 0
		bExtraball_1 = False
		bExtraball_2 = False
		bExtraball_3 = False
		bSmartButtonCount = 0

		SPlayerMode = -1
		SPlayerMode2 = -1
	End Sub

	Public Sub Save()
		dim i, a, astr
		sBumperMultiplier = BumperMultiplier
		sAerosmithCount = SpellAerosmith
		i=0:astr=""
		for each a in ArrowLights
			astr=astr & a.name & "=" & cstr(a.state) & " "
			lArrowStates(i,0) = a.name
			lArrowStates(i,1) = a.color
			lArrowStates(i,2) = a.colorfull
			lArrowStates(i,3) = a.state
			lArrowStates(i,4) = a.uservalue
			i=i+1
		Next
		'msgbox "Saving: " & astr
		i = 0
		for each a in aerosmithLights
			lAerosmith(i) = a.state
			i=i+1
		Next

		i=0
		for each a in elevatorLights
			lElevatorLights(i) = a.state
			i=i+1
		Next
		i=0
		for each a in aLockLights
			lLockLights(i) = a.state
			lLockLightsValue(i) = a.uservalue
			i=i+1
		Next
		i=0
		for each a in MiscLights
			lMiscLights(i) = a.state
			i=i+1
		Next
		i=0
		for each a in aShots
			lShots(i) = a.state
			i=i+1
		Next
		sPopValue = PopValue
		sPopLevel = PopLevel
		SFinalTourDone = bFinalTourDone
		SFinalTourReady= bFinalTourReady
		SFinalTourCount = FinalTourCount
		bSmartButtonCount = SmartButtonCount
		sWizardMode = bWizardMode

		For i = 0 to 8
			sModeProgress(i) = ModeProgress(i)
			sModePercent(i) = ModePercent(i)
			sMode2Percent(i) = Mode2Percent(i)
			sMode2Progress(i) = Mode2Progress(i)
			sMode2Value(i) = Mode2Value(i)
			sMode2Total(i) = Mode2Total(i)
			sModeOrder(i) = ModeOrder(i)
		Next

		bCountDownEnabled = ModeCountdownTimer.Enabled
		CountdownValue = ModeCountdownTimer.UserValue

		bModeSelect = bPlayerModeSelect
		SPlayerMode = PlayerMode
		SPlayerMode2 = PlayerMode2
		bElevMultiBallCount = ElevMultiBallCount
		bElevMultiBallAttempts = ElevMultiBallAttempts
		bHiddenJesters = HiddenJesters
		sMedleyTourReady = bMedleyTourReady
	End Sub

	Public Sub Restore()
		dim i,a,astr,costr
		If PlayersPlayingGame = 1 then exit sub
		BumperMultiplier = sBumperMultiplier
		SpellAerosmith = sAerosmithCount
		i=0:astr="":costr=""
		for each a in ArrowLights
		'	a.name = lArrowStates(i,0)
			a.color = lArrowStates(i,1)
			a.colorfull = lArrowStates(i,2)
			a.state = lArrowStates(i,3)
			a.uservalue = lArrowStates(i,4)
			astr=astr & a.name & "=" & cstr(a.state) & " "
			costr=costr & ":" & cstr(a.colorfull)
			i=i+1
		Next
		'msgbox "Restore: " & astr & "--" & costr
		i=0
		for each a in aerosmithLights
			a.state = lAerosmith(i)
			i=i+1
		Next
		i=0
		for each a in elevatorLights
			a.state = lElevatorLights(i)
			i=i+1
		Next
		i=0
		for each a in aLockLights
			a.state = lLockLights(i)
			a.uservalue = lLockLightsValue(i)
			i=i+1
		Next
		i=0
		for each a in MiscLights
			a.state = lMiscLights(i)
			i=i+1
		Next
		i=0
		for each a in aShots ' The Mode Lights
			a.state=lShots(i)
			i=i+1
		Next

		PopValue = sPopValue
		PopLevel = sPopLevel
		bFinalTourDone = SFinalTourDone
		bFinalTourReady= SFinalTourReady
		FinalTourCount = SFinalTourCount
		SmartButtonCount = bSmartButtonCount
		ElevMultiBallCount = bElevMultiBallCount
		ElevMultiBallAttempts = bElevMultiBallAttempts
		HiddenJesters = bHiddenJesters
		bWizardMode = sWizardMode
		bMedleyTourReady = sMedleyTourReady

		If I100.state = 2 Then	
			ToyBox_OpenLid()
		End If

		For i = 0 to 8
			ModeProgress(i) = sModeProgress(i)
			ModePercent(i) = sModePercent(i)
			Mode2Percent(i) = sMode2Percent(i)
			Mode2Progress(i) = sMode2Progress(i)
			Mode2Value(i) = sMode2Value(i)
			Mode2Total(i) = sMode2Total(i)
			ModeOrder(i) = sModeOrder(i)
		Next

		'ModeCountdownTimer.Enabled = bCountDownEnabled 		' Dont restore this since we start it once they hit the ball
		ModeCountdownTimer.UserValue = CountdownValue
		bPlayerModeSelect = bModeSelect
		PlayerMode = SPlayerMode
		PlayerMode2 = SPlayerMode2

		' Set other lights back up
		StopScoopLightSeq

		if ModeCountdownTimer.Enabled then
			SetBackglassTimer(ModeCountdownTimer.UserValue)
		End If
	End Sub

End Class

Sub TableState_Init(Index)
	Set PlayerState(Index) = New TableState
	PlayerState(Index).Reset
End Sub
Sub StackState_Init(Index)
	Set StackState(Index) = New cStack
	StackState(Index).Reset
End Sub 

'************************
' nFozzy Flipper Physics Vars
'************************
dim LF:Set LF = New FlipperPolarity
dim RF:Set RF = New FlipperPolarity

'************************
' Core Stuff
'************************
LoadCoreFiles
InitPolarity

Dim mMagnet
Set mMagnet = New cvpmTurnTable
With mMagnet
		.InitTurnTable HMagnet, 60
		.spinCW = False	
		.MotorOn = False
		.CreateEvents "mMagnet"
End With


'====================== Routines
Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox  "Can't open core.vbs"
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox  "You need the controller.vbs in order to run this table, available in the vp10 package"
    On Error Goto 0

	'============================'  Orbital Scoreboard'============================	
	If osbactive = 1 or osbactive = 2 Then		
		On Error Resume Next		
		ExecuteGlobal GetTextFile("osb.vbs")	
		On Error Goto 0
	End if
End Sub

'####################### CHECK FOR FILE #######################
' Version: 2.0
' Mar - 2022

' Add function to check for an external file

Dim fso
Function FileExists(FilePath)
    Set fso = CreateObject("Scripting.FileSystemObject")
    If fso.FileExists(FilePath) Then
        FileExists=CBool(1)
    Else
        FileExists=CBool(0)
    End If
End Function

'Example:
'If FileExists("\IamHere.txt") Then DoWhatYouWant

'Examples to check for a file:

Dim PuPPlayer
Dim PUPStatus
Dim PUPEnabled 
Dim PuPPack_folder
Dim PupPack_version
Dim PupPack_verified

PuPPack_folder = "aerosmith"
PupPack_verified = "1.0.0" ' ONLY CHANGE WITH PUP PACK


'####################### END CHECK FOR FILE #######################

'===================== Common Routines
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

'Methods:
'.TimeDelay - Delay before trigger shuts off automatically. Default = 80 (ms)
'.AddPoint - "Polarity", "Velocity", "Ycoef" coordinate points. Use one of these 3 strings, keep coordinates sequential. x = %position on the flipper, y = output
'.Object - set to flipper reference. Optional.
'.StartPoint - set start point coord. Unnecessary, If .object is used.

'Called with flipper - 
'ProcessBalls - catches ball data. 
' - OR - 
'.Fire - fires flipper.rotatetoend automatically + processballs. Requires .Object to be set to the flipper.

'******************************************************
'******************************************************
'******************************************************
'******************************************************
'******************************************************
'		FLIPPER CORRECTION SUPPORTING FUNCTIONS
'******************************************************

RightFlipper.timerinterval=1
rightflipper.timerenabled=True

sub RightFlipper_timer()

	If leftflipper.currentangle = leftflipper.endangle and LFPress = 1 then 
		leftflipper.eostorqueangle = EOSAnew
		leftflipper.eostorque = EOSTnew
		LeftFlipper.rampup = EOSRampup
		If LFCount = 0 Then LFCount = GameTime
		If GameTime - LFCount < LiveCatch Then
			leftflipper.Elasticity = 0.1
			If LeftFlipper.endangle <> LFEndAngle Then leftflipper.endangle = LFEndAngle
		Else	
			leftflipper.Elasticity = FElasticity
		end if
	elseIf leftflipper.currentangle > leftflipper.startangle - 0.05  Then
		leftflipper.rampup = SOSRampup
		leftflipper.endangle = LFEndAngle - 3
		leftflipper.Elasticity = FElasticity
		LFCount = 0
	elseIf leftflipper.currentangle > leftflipper.endangle + 0.01 Then 
		leftflipper.eostorque = EOST
		leftflipper.eostorqueangle = EOSA
		LeftFlipper.rampup = Frampup
		leftflipper.Elasticity = FElasticity
	end if

	If rightflipper.currentangle = rightflipper.endangle and RFPress = 1 then
		rightflipper.eostorqueangle = EOSAnew
		rightflipper.eostorque = EOSTnew
		RightFlipper.rampup = EOSRampup
		If RFCount = 0 Then RFCount = GameTime
		If GameTime - RFCount < LiveCatch Then
			rightflipper.Elasticity = 0.1
			If RightFlipper.endangle <> RFEndAngle Then rightflipper.endangle = RFEndAngle
		Else
			rightflipper.Elasticity = FElasticity
		end if
	elseIf rightflipper.currentangle < rightflipper.startangle + 0.05 Then
		rightflipper.rampup = SOSRampup 
		rightflipper.endangle = RFEndAngle + 3
		rightflipper.Elasticity = FElasticity
		RFCount = 0 
	elseIf rightflipper.currentangle < rightflipper.endangle - 0.01 Then 
		rightflipper.eostorque = EOST
		rightflipper.eostorqueangle = EOSA
		RightFlipper.rampup = Frampup
		rightflipper.Elasticity = FElasticity
	end if

end sub

dim LFPress, RFPress, EOST, EOSA, EOSTnew, EOSAnew
dim FStrength, Frampup, FElasticity, EOSRampup, SOSRampup
dim RFEndAngle, LFEndAngle, LFCount, RFCount, LiveCatch

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
FStrength = LeftFlipper.strength
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
EOSTnew = 1.0 'FEOST
EOSAnew = 0.2
EOSRampup = 1.5 
SOSRampup = 8.5 
LiveCatch = 8

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle


Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub


Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart, FlipperEnd, LR, PartialFlipCoef
	Private Balls(20), balldata(20)
	
	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub
	
	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
	Public Property Let StartPoint(aInput) : If IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end If : End Property
	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
	Public Property Let EndPoint(aInput) : If IsObject(aInput) then FlipperEnd = aInput.x else FlipperEnd = aInput : end If : End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property
	
	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			Case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
		If gametime > 100 then Report aChooseArray
	End Sub 

	Public Sub Report(aChooseArray) 	'debug, reports all coords in tbPL.text
		If not DebugOn then exit sub
		dim a1, a2 : Select Case aChooseArray
			Case "Polarity" : a1 = PolarityIn : a2 = PolarityOut
			Case "Velocity" : a1 = VelocityIn : a2 = VelocityOut
			Case "Ycoef" : a1 = YcoefIn : a2 = YcoefOut 
			Case else :tbpl.text = "wrong string" : exit sub
		End Select
		dim str, x : for x = 0 to uBound(a1) : str = str & aChooseArray & " x: " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		tbpl.text = str
	End Sub
	
	Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : If IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end If : Next  : End Sub

	Private Sub RemoveBall(aBall)
		dim x : for x = 0 to uBound(balls)
			If TypeName(balls(x) ) = "IBall" then 
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
		dim x : for x = 0 to uBound(balls)
			If not IsEmpty(balls(x) ) then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next		
	End Property

	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			If not IsEmpty(balls(x) ) then
				balldata(x).Data = balls(x)
				If DebugOn then StickL.visible = True : StickL.x = balldata(x).x		'debug TODO
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
		If abs(Flipper.currentAngle - Flipper.EndAngle) < 30 Then
			PartialFlipCoef = 0
		End If
	End Sub
	Private Function FlipperOn() : If gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function	'Timer shutoff for polaritycorrect
	
	Public Sub PolarityCorrect(aBall)
		If FlipperOn() then 
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1
			dim teststr : teststr = "Cutoff"
			tmp = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
			If tmp < 0.1 then 'If real ball position is behind flipper, exit Sub to prevent stucks	'Disabled 1.03, I think it's the Mesh that's causing stucks, not this
				If DebugOn then TestStr = "real pos < 0.1 ( " & round(tmp,2) & ")" : tbpl.text = Teststr 
				'RemoveBall aBall
				'Exit Sub
			end if

			'y safety Exit
			If aBall.VelY > -8 then 'ball going down
				If DebugOn then teststr = "y velocity: " & round(aBall.vely, 3) & "exit sub" : tbpl.text = teststr
				RemoveBall aBall
				exit Sub
			end if
			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				If aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					'TB.TEXT = balldata(x).id & " " & BALLDATA(X).X & VBNEWLINE & FLIPPERSTART & " " & FLIPPEREND
					If ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				end if
			Next

			'Velocity correction
			If not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				If DebugOn then set tmp = new spoofball : tmp.data = aBall : End If
				If IsEmpty(BallData(idx).id) and aBall.VelY < -12 then 'If tip hit with no collected data, do vel correction anyway
					If PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1) > 1.1 then 'adjust plz
						VelCoef = LinearEnvelope(5, VelocityIn, VelocityOut)
						If partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
						If Enabled then aBall.Velx = aBall.Velx*VelCoef'VelCoef
						If Enabled then aBall.Vely = aBall.Vely*VelCoef'VelCoef
						If DebugOn then teststr = "tip protection" & vbnewline & "velcoef: " & round(velcoef,3) & vbnewline & round(PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1),3) & vbnewline
						'debug.print teststr
					end if
				Else
		 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
					If Enabled then aBall.Velx = aBall.Velx*VelCoef
					If Enabled then aBall.Vely = aBall.Vely*VelCoef
				end if
			End If

			'Polarity Correction (optional now)
			If not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity If left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				If Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
			'debug
			If DebugOn then
				TestStr = teststr & "%pos:" & round(BallPos,2)
				If IsEmpty(PolarityOut(0) ) then 
					teststr = teststr & vbnewline & "(Polarity Disabled)" & vbnewline
				else 
					teststr = teststr & "+" & round(1 *(AddX*ycoef*PartialFlipcoef),3)
					If BallPos >= PolarityOut(uBound(PolarityOut) ) then teststr = teststr & "(MAX)" & vbnewline else teststr = teststr & vbnewline end if	
					If Ycoef < 1 then teststr = teststr &  "ycoef: " & ycoef & vbnewline
					If PartialFlipcoef < 1 then teststr = teststr & "PartialFlipcoef: " & round(PartialFlipcoef,4) & vbnewline				
				end if

				teststr = teststr & vbnewline & "Vel: " & round(BallSpeed(tmp),2) & " -> " & round(ballspeed(aBall),2) & vbnewline
				teststr = teststr & "%" & round(ballspeed(aBall) / BallSpeed(tmp),2)
				tbpl.text = TestSTR
			end if
		Else
			'If DebugOn then tbpl.text = "td" & timedelay
		End If
		RemoveBall aBall
	End Sub
End Class

'================================
'Helper Functions
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)	'Shuffle objects in a temp array
		If not IsEmpty(aArray(x) ) Then
			If IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	If offset < 0 then offset = 0
	redim aArray(aCount-1+offset)	'Resize original array
	for x = 0 to aCount-1		'set objects back into original array
		If IsObject(a(x)) then 
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub


Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

Function NullFunctionZ(aEnabled):End Function	'1 argument null function placeholder	 TODO move me or replac eme

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


Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		If xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	If xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	'Clamp If on the boundry lines
	'If L=1 and Y < yLvl(LBound(yLvl) ) then Y = yLvl(lBound(yLvl) )
	'If L=uBound(xKeyFrame) and Y > yLvl(uBound(yLvl) ) then Y = yLvl(uBound(yLvl) )
	'clamp 2.0
	If xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	If xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
End Function

'===================================================================================

Sub StartInstantInfo(keycode)
	D2 "Start Instant " & keycode & " " & bInstantInfo
	If bInstantInfo = False and tmrSkillshot.Enabled=False  Then ' I am already in instantinfo
		InstantInfoTimer.Interval = 8000
		InstantInfoTimer.Enabled = True
		InstantInfoTimer.UserValue=keycode
	End If 
End Sub 


Sub InstantInfoTimer_Timer
    InstantInfoTimer.Enabled = False
    bInstantInfo = True
	D "InstantInfotimer Expired"
	PuPlayer.LabelShowPage pOverVid, 1,0,""
	pCurAttractPos=0
	pInAttract=true
	pAttractNext
End Sub


Sub EndFlipperStatus(keycode)
    If bInstantInfo Then
		D "EndInstantInfo check" & keycode & " " & InstantInfoTimer.UserValue
		If (keycode=InstantInfoTimer.UserValue) then 	' They let go of the key
			D "EndInstantInfo"
			InstantInfoTimer.Enabled = False
			bInstantInfo=False
			PuPlayer.LabelShowPage pBackglass,1,0,""
			pInAttract=false
			playclear pOverVid
			PuPlayer.LabelSet pOverVid,"OverMessage1"," ",1,""
			PuPlayer.LabelSet pOverVid,"OverMessage2"," ",1,""
			PuPlayer.LabelSet pOverVid,"OverMessage3"," ",1,""
			RefreshPlayerMode

			If bPlayerModeSelect Then
				If modecountdowntimer.enabled Then
					pDMDEvent(P+1)
				Else
					pDMDEvent(P)
				End If
			End If 
		Else ' They pressed the other flipper so cycle faster 
			PriorityReset=2000
			pAttractNext
		End If 
	Else
		D2 "Stop Instant " & keycode
		InstantInfoTimer.Enabled = False
    End If
End Sub

'a1a1
Sub UpdatePlayerMode() 'Updates the DMD & lights with the chosen skillshots
	Dim a, i, x
	'D "Sub UpdatePlayerMode " & PlayerMode 
    LightSeqSkillshot.Play SeqAllOff
    'turn off various mode lights
	BumperLight001.State = 0
	BumperLight002.State = 0
	BumperLight003.State = 0
    I15.State = 0:I15.BlinkInterval=BlinkIntDef:I15.BlinkPattern=BlinkPatternDef  'MODE LIGHTS ON PF
    I16.State = 0:I16.BlinkInterval=BlinkIntDef:I16.BlinkPattern=BlinkPatternDef
    I17.State = 0:I17.BlinkInterval=BlinkIntDef:I17.BlinkPattern=BlinkPatternDef
    I18.State = 0:I18.BlinkInterval=BlinkIntDef:I18.BlinkPattern=BlinkPatternDef
    I19.State = 0:I19.BlinkInterval=BlinkIntDef:I19.BlinkPattern=BlinkPatternDef
    I20.State = 0:I20.BlinkInterval=BlinkIntDef:I20.BlinkPattern=BlinkPatternDef
    I21.State = 0:I21.BlinkInterval=BlinkIntDef:I21.BlinkPattern=BlinkPatternDef

	
	'PuPlayer.playlistplayex pMusic,"audioclear","clear.mp3",100, 1
	SaveMode=PlayerMode
	if playermode <> -1 Then PlaySong "Song-" & PlayerMode & ".mp3"

	ScorbitBuildGameModes()

	dim time
	time = 2000
	If bUsePUPDMD then time = -1

	'***********************  Turn off last player *****************

	D "UpdatePlayerMode - turn off shot lights"
	SetLightColor I35, "white", 0	
	SetLightColor I62, "white", 0
	SetLightColor I81, "white", 0
	SetLightColor I86, "white", 0
	SetLightColor I91, "white", 0
	SetLightColor I97, "white", 0
	SetLightColor I103, "white", 0
	SetLightColor I107, "white", 0

	For each a in aerosmithLights
		SetLightColor a, "orange", 0
	Next

    DMDFlush
	Dim Line1
	If bUsePupDMD Then
		Line1 = "USE FLIPS TO SELECT SONG"
	Else
		Line1 = ">Select Song<"
	End If
    Select Case PlayerMode
		Case -1:' No Mode is selected yet
			DisplayDMDText2 "SELECT", "A SONG", "", eNone, eNone, eNone, time, False, ""   
        Case 0:  ' last
			DMD Line1, ModeNamesShort(PlayerMode), "", eNone, eNone, eNone, time, False, "":I15.State = 2 ' flash left orbits, solid center ramp & right ramp
			modeRampColor = "cyan"
			SetLightColor  I62, modeRampColor, 2
			SetLightColor I107, modeRampColor, 2
			SetLightColor  I86, modeRampColor, 1
			SetLightColor I103, modeRampColor, 1
			ModeCountdown = 50 ' seconds
			If bHardMode Then ModeCountdown = 30
			SmartButtonFlash modeRampColor, True
			pDMDEvent(kDMD_last) 'select image
			RefreshPlayerMode
          Case 1: ' walk
			DMD Line1, ModeNamesShort(PlayerMode), "", eNone, eNone, eNone, time, False, "":I16.State = 2' solid orbits, flash ramps
			modeRampColor = "orange"
			SetLightColor I62, modeRampColor, 1
			SetLightColor I107, modeRampColor, 1
			SetLightColor I86, modeRampColor, 2
			ModeCountdown = 50 ' seconds
			If bHardMode Then ModeCountdown = 30
			SmartButtonFlash modeRampColor, True
			pDMDEvent(kDMD_walk)
			RefreshPlayerMode
        Case 2:  ' same
			DMD Line1, ModeNamesShort(PlayerMode), "", eNone, eNone, eNone, time, False, "":I17.State = 2 ' flash elevator
			modeRampColor = "yellow"
			SetLightColor aRampLights(bRndModeShot), modeRampColor, 2
			ModeCountdown = 50 ' seconds
			If bHardMode Then ModeCountdown = 30
			If bHardMode=False Then
				i=0
				For each a in aerosmithLights
					HoldAerosmithLights(i) = a.state ' save so we can restore
					SetLightColor a, "orange", 2
					i=i+1
				Next
			End If
			SmartButtonFlash modeRampColor, True
			pDMDEvent(kDMD_same)
			RefreshPlayerMode
        Case 3:  ' sweet
			DMD Line1, ModeNamesShort(PlayerMode), "", eNone, eNone, eNone, time, False, "":I18.State = 2   ' flash CIU, solid box, elevator, lock
			modeRampColor = "red"
			SetLightColor I35, modeRampColor, 2
			SetLightColor I81, modeRampColor, 2
			SetLightColor I91, modeRampColor, 2
			SetLightColor I97, modeRampColor, 2
			ModeCountdown = 50 ' seconds
			If bHardMode Then ModeCountdown = 30
			SmartButtonFlash modeRampColor, True
			pDMDEvent(kDMD_sweet)
			RefreshPlayerMode
        Case 4: ' dude
			DMD Line1, ModeNamesShort(PlayerMode), "", eNone, eNone, eNone, time, False, "":I19.State = 2   ' solid center, solid elevator
			modeRampColor = "blue"
			SetLightColor I86, modeRampColor, 1
			SetLightColor I91, modeRampColor, 1
			ModeCountdown = 50 ' seconds
			If bHardMode Then ModeCountdown = 30
			SmartButtonFlash modeRampColor, True
			pDMDEvent(kDMD_dude)
			RefreshPlayerMode
        Case 5: ' back
			DMD Line1, ModeNamesShort(PlayerMode), "", eNone, eNone, eNone, time, False, "":I20.State = 2   '  flash right orbit
			modeRampColor = "purple"
			SetLightColor I107, modeRampColor, 2
			ModeCountdown = 50 ' seconds
			If bHardMode Then ModeCountdown = 30
			SmartButtonFlash modeRampColor, True
			pDMDEvent(kDMD_back)
			RefreshPlayerMode
        Case 6:  ' rats  1 from left, 1 from right, then 2 on the left, 2 on the right
			DMD Line1, ModeNamesShort(PlayerMode), "", eNone, eNone, eNone, time, False, "":I21.State = 2   '  flash center ramp  -- then it lights elevator and toys
			modeRampColor = "red"
			' random 0-3 ramp which is a left ramp
			RatsPos1 = INT(RND*4)+1 ' Find a random light for rats mode
			i=1
			for each a in aRampLights
				if i = RatsPos1 Then
					SetLightColor a, modeRampColor, 2
					Exit For
				End If
				i=i+1
			Next
		
			ModeCountdown = 50 ' seconds  
			If bHardMode Then ModeCountdown = 30
			SmartButtonFlash modeRampColor, True
			pDMDEvent(kDMD_rats)
			RefreshPlayerMode
    End Select
	If PlayerMode <> -1 Then
		SetBackglassTimer(ModeCountdown)
	End If
	D "UpdatePlayermode calling SetModeLights"
	SetModeLights
End Sub

Sub SetModeLights
	Dim i
	' Of any of the modes are 1/2 done then Flash
	if bWizardMode Then Exit Sub

	modesStarted=0
	modesCompleted=0
	D "Sub SetModeLights Started:" & modesStarted & " Comp:" & modesCompleted _
                  & " _2:" & PlayerState(CurrentPlayer).bExtraball_2 _
                  &  " _3:" & PlayerState(CurrentPlayer).bExtraball_3

	SetModeLightComplete 0, I15
	SetModeLightComplete 1, I16
	SetModeLightComplete 2, I17
	SetModeLightComplete 3, I18
	SetModeLightComplete 4, I19
	SetModeLightComplete 5, I20
	SetModeLightComplete 6, I21
	SetModeLightComplete 7, I22
	SetModeLightComplete 8, I23  
	
	D "SetModeLights Started:" & modesStarted & " Comp:" & modesCompleted _
                  & " _2:" & PlayerState(CurrentPlayer).bExtraball_2 _
                  &  " _3:" & PlayerState(CurrentPlayer).bExtraball_3


	If modesStarted >=3 and PlayerState(CurrentPlayer).bExtraball_2 = False  then ' started 3 modes 
		PlayerState(CurrentPlayer).bExtraball_2 = True
		setExtraBallLight(True)
	ElseIf modesCompleted >=6 and PlayerState(CurrentPlayer).bExtraball_3 = False  then ' Finished 6 modes 
		PlayerState(CurrentPlayer).bExtraball_3 = True
		setExtraBallLight(True)
	End If 
End Sub

Sub RefreshPlayerMode()
	D "RefreshPlayerMode :" & bPlayerModeSelect
	If bUsePUPDMD then 
		Select Case PlayerMode
		Case -1:
			PuPlayer.LabelSet pBackglass, "ModeProgress", "PuPOverlays\\Progress8-0.png",0,"{'mt':2,'color':111111,'width':12, 'height':23, 'yalign':0,'ypos':4.0,'xpos':5,'pagenum':1}"
		Case 0:
			PuPlayer.LabelSet pBackglass, "ModeProgress", "PuPOverlays\\Progress8-"&ModeProgress(PlayerMode)&".png",1,"{'mt':2,'color':111111,'width':12, 'height':23, 'yalign':0,'ypos':4.0,'xpos':5,'pagenum':1}"
		Case 1:
			PuPlayer.LabelSet pBackglass, "ModeProgress", "PuPOverlays\\Progress10-"&ModeProgress(PlayerMode)&".png",1,"{'mt':2,'color':111111,'width':12, 'height':23, 'yalign':0,'ypos':4.0,'xpos':5,'pagenum':1}"
		Case 2:
			PuPlayer.LabelSet pBackglass, "ModeProgress", "PuPOverlays\\Progress10-"&ModeProgress(PlayerMode)&".png",1,"{'mt':2,'color':111111,'width':12, 'height':23, 'yalign':0,'ypos':4.0,'xpos':5,'pagenum':1}"
		Case 3:
			PuPlayer.LabelSet pBackglass, "ModeProgress", "PuPOverlays\\Progress12-"&ModeProgress(PlayerMode)&".png",1,"{'mt':2,'color':111111,'width':12, 'height':23, 'yalign':0,'ypos':4.0,'xpos':5,'pagenum':1}"
		Case 4:
			PuPlayer.LabelSet pBackglass, "ModeProgress", "PuPOverlays\\Progress10-"&ModeProgress(PlayerMode)&".png",1,"{'mt':2,'color':111111,'width':12, 'height':23, 'yalign':0,'ypos':4.0,'xpos':5,'pagenum':1}"
		Case 5:
			PuPlayer.LabelSet pBackglass, "ModeProgress", "PuPOverlays\\Progress8-"&ModeProgress(PlayerMode)&".png",1,"{'mt':2,'color':111111,'width':12, 'height':23, 'yalign':0,'ypos':4.0,'xpos':5,'pagenum':1}"
		Case 6:
			PuPlayer.LabelSet pBackglass, "ModeProgress", "PuPOverlays\\Progress8-"&ModeProgress(PlayerMode)&".png",1,"{'mt':2,'color':111111,'width':12, 'height':23, 'yalign':0,'ypos':4.0,'xpos':5,'pagenum':1}"
		End Select
	End If 
	If bUsePUPDMD Then
		PuPlayer.LabelNew pBackglass,"CoinImage",DMDScrFont,		7*FontScale,RGB(255, 255, 255)	,0,1,0 ,0,0     ,1,1
		PuPlayer.LabelSet pBackglass,"CoinImage","PupOverlays\\Coin.png",1,"{'mt':2,'width':100,'height':100,'xpos':0,'ypos':0}" 

		PuPlayer.LabelNew pBackglass,"JesterImage",DMDScrFont,		7*FontScale,RGB(255, 255, 255)	,0,1,0 ,0,0     ,1,1	
		PuPlayer.LabelSet pBackglass,"JesterImage","PupOverlays\\Jester.png",1,"{'mt':2,'width':100,'height':100,'xpos':0,'ypos':0}"

		PuPlayer.LabelNew pBackglass,"DiceImage",DMDScrFont,		7*FontScale,RGB(255, 255, 255)	,0,1,0 ,0,0     ,1,1
		PuPlayer.LabelSet pBackglass,"DiceImage","PupOverlays\\Dice.png",1,"{'mt':2,'width':100,'height':100,'xpos':0,'ypos':0}"
	End If
End Sub

'sub CheckWizardModeStart
Function CheckWizardModeStart(trigger_name) 
	Dim a,i,b,BallsToRelease
	D "Function CheckWizardModeStart :" & trigger_name & " Final:" & bFinalTourReady & " Medley:" & bMedleyTourReady
	CheckWizardModeStart=False
	If trigger_name="I35" Then  ' I34 should already be blinking
		If bMedleyTourReady and not bToyBoxMultiball Then ' Dont kick into Medley during ToyBox
			D "Start MedleyTour - bMedleyTourReady already set TIMER:" & ScoopEject.TimerEnabled
			tmrMedleyTour.Enabled=True
			ScoopDelay=20000
			StopScoopLightSeq

			setModeSelectLight(False)
			ModecountdownTimer.UserValue = 0
			ModecountdownTimer.Enabled = False
			PuPlayer.LabelSet pBackglass,"Time", " ",0,""

			aSaveLights() ' capture the ToyBoxLights, Multipliers
			for each a in saveLights
				SetLightColor a, "white", 0
			Next
			
			SetLightColor F147,"white", 0  ' Toys Light
			SetLightColor I110,"white", 0  ' Elevator Lights
			SetLightColor I94, "white", 0
			SetLightColor I65, "white", 0
			SetLightColor I100,"white", 0  ' lock

			bMedleyTourReady = False
			bMedleyTourCount=0
			QueueScene "SceneClearPlayMessage '", 0, 1
			QueueScene "SceneClearPlayMessage '", 0, 2
' Might have to leave this alone so you can finish ToysInTheAttic
			D "Starting Medley - CLOSE THE TOYBOX TIMER:" & ScoopEject.TimerEnabled
			ToyBox_CloseLid()
			FlashEffect(2)
			StartWizardModeBonus()

'If bHardMode=True then you need atleast 1 shot in each mode .. no 0% modes
' turn off all inserts
'ball Save - 40s
'3 ball MB_Multiplier
'blink one mode
'songs played in order of completion in game (v1.07)
'skip songs that were completed to 100% in game mode
'"SONG COMPLETED 1M" - with generic background, 2M for next song (and Shots)
' add a ball upon a song completion


' similar to super modes - show "Loops Needed in background"
'can use smart button
'down to 1 ball return to regular mode and super shots are enabled again
'show mode total at end as well
'upon completion - drain all but 1 ball before returning to regular mode


			bWizardMode = True
			bAddedABall = False
			WizardModePoints=0
			WizardBonusPoints=0
			WizardModesComplete=0  	' how many we actual completed
			WizardModeStage=0		' index position in the queue
			PlayerMode3=0
			WizardHits=0  ' for modes that need a required number of hits

			' Turn off all the Mode Lights
			I15.State = 0:I16.State = 0:I17.State = 0:I18.State = 0
			I19.State = 0:I20.state = 0:I21.State = 0:I22.State = 0:I23.state = 0
			StackState(kStack_Pri2).Enable(-1)
			if ModePercent(0)>=100 Then SetLightColor I15, "cyan", 1
			if ModePercent(1)>=100 Then SetLightColor I16, "orange", 1
			if ModePercent(2)>=100 Then SetLightColor I17, "yellow", 1
			if ModePercent(3)>=100 Then SetLightColor I18, "red", 1
			if ModePercent(4)>=100 Then SetLightColor I19, "blue", 1
			if ModePercent(5)>=100 Then SetLightColor I20, "purple", 1
			if ModePercent(6)>=100 Then SetLightColor I21, "red", 1
			if ModePercent(7)>=100 Then SetLightColor I22, "green", 1
			if ModePercent(8)>=100 Then SetLightColor I23, "pink", 1

			CheckWizardModeStart = True

			ScorbitBuildGameModes()

			NextWizardStage()
			setMysteryLight(True)
		ElseIf bFinalTourReady Then
			I2 "Start Final Tour"
			StopScoopLightSeq
			ScoopDelay=3000

			if bUsePUPDMD then
				QueueScene "SceneClearMessage '", 0, 1
				QueueScene "ScenePlayMessage ""Video-0x0010.mp4"", """","""","""" '", 5500, 1
				QueueScene "SceneClearPlayMessage '", 0, 1
			Else
				DisplayDMDText2 "FINAL TOUR","WIZARD MODE", 1000, 11, 0
			end if

			setModeSelectLight(False)
			setCIULight(False)
			tmrFinalTour.Enabled=True

			' Multi Step Light Sequence
			LightSeqFinalTour.UserValue=0
			LightSeqFinalTour.Play SeqDownOn, 30, 3

			bFinalTourReady=False
			FinalTourCount=0   
			bWizardMode=True
			bFinalTourDone=False
			WizardModePoints=0
			WizardBonusPoints=0
			bAddedABall=False  
			modeRampColor = "yellow"
	
			' Stop ballsaver if it is currently running 
			BallSaverTimerCancel
			playclear pAudio

			' Setup new Ball Saver  
			BallSaverTime = 40						' Note: initially you get 40 seconds
			bBallSaverReady = True
			bMultiBallMode=True

			For each a in aLockLights
				SetLightColor a, "green",0
				a.uservalue = 1   ' only 1 hit to make it solid
			Next

			SetLightColor F147,"white", 0  ' Toys Light
			SetLightColor I110,"white", 0  ' Elevator Lights
			SetLightColor I94, "white", 0
			SetLightColor I65, "white", 0
			SetLightColor I100,"white", 0  ' lock

			ToyBox_CloseLid()

			'QueueScene2  "PlaySoundVol ""sfx_FinalTourStart"", VolSfx '", 0, 1, True 
			QueueScene2  "StartPlayerModeVideo False '", 3000, 1, True 
			QueueScene2  "ScoopKickOut '", 100, 1, True 
			QueueScene2  "AddMultiball 3 - BallsOnPlayfield '", 0, 1, True

			StackState(kStack_Pri2).Enable(-1)

			For each a in aRampLights				
				SSetLightColor kStack_Pri2, a, "yellow", 2
				a.UserValue = 0
			Next 

			CheckWizardModeStart = True
			ScorbitBuildGameModes()
		End If
	End If
End Function

Sub StartWizardModeContinue(nballs)	' delayed until vid and scoop eject
	' Stop ballsaver if it is currently running 
	FlashEffect(0) ' Turn Off Blinky Blinky
	BallSaverTimerCancel

	' Setup new Ball Saver 
	BallSaverTime = 10						' Note: initially you get 40 seconds   TODO
	bBallSaverReady = True
	D "Add balls:" & nballs & " BOP:" & BallsOnPlayfield
	AddMultiball nballs - BallsOnPlayfield
	bMultiBallMode=True

	D "remove the timer and bubble"
	PuPlayer.LabelSet pBackglass,"Time", " ",0,""

	pDMDEvent(P)
End Sub

Dim WizardStagesComplete, WizardModesComplete, PlayerMode3, WizardModeStage, WizardHits

Sub NextWizardStage()
	Dim a,i, foundone
	' find first mode in ModeOrder that is not done
	
	WizardModeStage=0
	foundone=False
	D "sub NextWizardStage WizardModeStage=" & WizardModeStage & " TIMER:" & ScoopEject.TimerEnabled
D "Debugging"
for a = 0 to 8
	D "ModeOrder(" & a & ") = " & ModeOrder(a)
Next
	Do Until WizardModeStage >= 9
	D "NextWizardStage A WizardModeStage=" & WizardModeStage
		if ModePercent(ModeOrder(WizardModeStage)) < 100 Then	' Dont have to do wizard modes on stages already completed in regular play
			D "Found InComplete ModeOrder()=" & ModeOrder(WizardModeStage)
			FoundOne=True
			Exit Do
		End If
		WizardModeStage=WizardModeStage+1
		D "NextWizardStage B WizardModeStage=" & WizardModeStage
	Loop  

	If FoundOne=False then
		D "Completed Wizard Mode TIMER:" & ScoopEject.TimerEnabled
		EndWizardMode(1)   	
		bFinalTourReady=True  ' 
	Else
		D "Starting stage=" & WizardModeStage & " PlayerMode3=Mode(x)=" & ModeOrder(WizardModeStage)
		PlayerMode3=ModeOrder(WizardModeStage)
		SetWizardMode()
	End If
End Sub

Sub SetWizardMode()
	Dim a, Line1, time, i
	D "SetWizardMode PlayerMode3=" & PlayerMode3
	time=1000

	For each a in aRampLights				
		SSetLightColor kStack_Pri2, a, "white", 0
		a.UserValue = 0
	Next
	i=0
	for each a in aerosmithLights
		HoldAerosmithLights(i) = a.state
		SetLightColor a, "orange", 0
		i=i+1
	Next

	SetLightColor F141, "red", 0
	SetLightColor F144, "purple", 0

	SetLightColor I75,  "green",0 ' toybox locks
	SetLightColor I76,  "green",0
	SetLightColor I77,  "green",0
	SetLightColor I78,  "green",0
	SetLightColor I79,  "green",0
	SetLightColor I80,  "green",0
	SetLightColor F152, "white",0  'dude
	SetLightColor F153, "white",0
	SetLightColor F154, "white",0
	SetLightColor I100, "green",0 ' Lock
	ToyBox_CloseLid()
	
	Line1 = "Medley Wizard Mode"

'same - 9 flashing targets that go out as they are Hit, Score each target regardless If lit
'toys - shoot toybox 6 times , flashing to solid, CR and Elev solid - these shots just go out
'last - 2 shots - not extinguished - flash
'walk - 4 shots - not extinguised except when down to 1 - flash then 1 solid
'sweet - 6 shots - extinguished - solid
'dude - 1 scoop, 3 lanes - shots are extinguished - solid
'back - 25 pop hits
'rats - 25 spinners
'love - 3 shots - not exitinguished - flash

	StartPlayerModeVideo False
	if playermode <> -1 Then PlaySong "Song-" & PlayerMode3 & ".mp3"

	select case PlayerMode3
		case 0: ' last
			DMD Line1, CL(0, ModeNamesShort(PlayerMode3)), "", eNone, eNone, eNone, time, False, "":I15.State = 2 ' orbits
			SetLightColor I62, "cyan", 2
			SetLightColor I107, "cyan", 2
			WizardHits=2
		case 1: ' walk
			DMD Line1, CL(0, ModeNamesShort(PlayerMode3)), "", eNone, eNone, eNone, time, False, "":I16.State = 2 ' orbits and ramps
			SetLightColor I62, "orange", 2
			SetLightColor I107, "orange", 2
			SetLightColor I86, "orange", 2
			SetLightColor I103, "orange", 2
			WizardHits=4
		case 2: ' same
			DMD Line1, CL(0, ModeNamesShort(PlayerMode3)), "", eNone, eNone, eNone, time, False, "":I17.State = 2 ' aerosmith targets
			i=0
			for each a in aerosmithLights
				HoldAerosmithLights(i) = a.state
				SetLightColor a, "orange", 2
				i=i+1
			Next
			WizardHits=9
		case 3: ' sweet
			DMD Line1, CL(0, ModeNamesShort(PlayerMode3)), "", eNone, eNone, eNone, time, False, "":I18.State = 2 ' shots no orbits
			SetLightColor I35, "red", 2
			SetLightColor I81, "red", 2
			SetLightColor I86, "red", 2
			SetLightColor I91, "red", 2
			SetLightColor I97, "red", 2
			SetLightColor I103, "red", 2
			WizardHits=6
		case 4: ' dude
			DMD Line1, CL(0, ModeNamesShort(PlayerMode3)), "", eNone, eNone, eNone, time, False, "":I19.State = 2 ' scoop and dude lights
			SetLightColor I35, "blue", 2
			SetLightColor F152, "blue", 2
			SetLightColor F153, "blue", 2
			SetLightColor F154, "blue", 2
			WizardHits=4
		case 5: ' back
			DMD Line1, CL(0, ModeNamesShort(PlayerMode3)), "", eNone, eNone, eNone, time, False, "":I20.State = 2 ' pops
			SSetLightColor kStack_Pri2, F144, "purple", 2
			WizardHits=25
		case 6: ' rats
			DMD Line1, CL(0, ModeNamesShort(PlayerMode3)), "", eNone, eNone, eNone, time, False, "":I21.State = 2 ' spinners
			SetLightColor F141, "red", 2
			WizardHits=25
		case 7: ' toys
			DMD Line1, CL(0, ModeNamesShort(PlayerMode3)), "", eNone, eNone, eNone, time, False, "":I22.State = 2 ' toybox
			SetLightColor I75, "green", 2
			SetLightColor I76, "green", 2
			SetLightColor I77, "green", 2
			SetLightColor I78, "green", 2
			SetLightColor I79, "green", 2
			SetLightColor I80, "green", 2

'			SetLightColor I86, "green", 1 		' Need to confirm
'			SetLightColor I91, "green", 1
'			SSetLightColor kStack_Pri2, I86, "green", 1
'			SSetLightColor kStack_Pri2, I91, "green", 1
			SetLightColor F147, "white",2 'toybox
			WizardHits=6
		case 8: ' love
			DMD Line1, CL(0, ModeNamesShort(PlayerMode3)), "", eNone, eNone, eNone, time, False, "":I23.State = 2 ' elevator
			SSetLightColor kStack_Pri2, I91, "pink", 2
			WizardHits=3
	End Select
End Sub

'4todo
Sub CheckWizardModeProgress(trigger_name)
	dim a,b,i
	Dim bValidHit
	dim bFinalShot
	dim hitLight
	Dim holdScore

	D "Sub CheckWizardModeProgress " & trigger_name & " Mode:" & PlayerMode3 & " Complete:" & _
		WizardModesComplete & " Med:" & tmrMedleyTour.Enabled & " Final:" & tmrFinalTour.Enabled

	if bDebounce then exit sub 

	If tmrMedleyTour.Enabled Then
		Select case PlayerMode3
			case 0: ' last
				if trigger_name = "I62" and StackState(kStack_Pri2).GetArrowState(I62) <> 0 Then
					SSetLightColor kStack_Pri2, I62, "white", 0
					AddScore WizardModesComplete*1000000
					WizardHits=WizardHits-1
				End if
				if trigger_name = "I107" and StackState(kStack_Pri2).GetArrowState(I107) <> 0 Then
					SSetLightColor kStack_Pri2, I107, "white", 0
					AddScore WizardModesComplete*1000000
					WizardHits=WizardHits-1
				End if
				if WizardHits <= 0 then
					I15.state=1:ModePercent(PlayerMode3)=100
					EndWizardStage()
				End if
			case 1: ' walk
				if trigger_name = "I62"  and StackState(kStack_Pri2).GetArrowState(I62)  <> 0 Then WizardHits=WizardHits-1:AddScore WizardModesComplete*1000000
				if trigger_name = "I107" and StackState(kStack_Pri2).GetArrowState(I107) <> 0 Then WizardHits=WizardHits-1:AddScore WizardModesComplete*1000000
				if trigger_name = "I86"  and StackState(kStack_Pri2).GetArrowState(I86)  <> 0 Then WizardHits=WizardHits-1:AddScore WizardModesComplete*1000000
				if trigger_name = "I103" and StackState(kStack_Pri2).GetArrowState(I103) <> 0 Then WizardHits=WizardHits-1:AddScore WizardModesComplete*1000000
				If WizardHits <= 0 Then
					I16.state=1:ModePercent(PlayerMode3)=100
					EndWizardStage()
				End If
			case 2: ' same
				For each a in aerosmithLights
					If a.name=trigger_name and a.state=2 Then
						a.state=0:AddScore WizardModesComplete*1000000
						WizardHits=WizardHits-1
						exit for
					End If
				Next
				If WizardHits <= 0 Then		
					I17.state=1:ModePercent(PlayerMode3)=100
					EndWizardStage()
				End If
			case 3: ' sweet
				if trigger_name = "I35" and StackState(kStack_Pri2).GetArrowState(I35) <> 0 Then
					SSetLightColor kStack_Pri2, I35, "white", 0:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
				End if
				if trigger_name = "I81" and StackState(kStack_Pri2).GetArrowState(I81) <> 0 Then
					SSetLightColor kStack_Pri2, I81, "white", 0:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
				End if
				if trigger_name = "I86" and StackState(kStack_Pri2).GetArrowState(I86) <> 0 Then
					SSetLightColor kStack_Pri2, I86, "white", 0:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
				End if
				if trigger_name = "I91" and StackState(kStack_Pri2).GetArrowState(I91) <> 0 Then
					SSetLightColor kStack_Pri2, I91, "white", 0:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
				End if
				if trigger_name = "I97" and StackState(kStack_Pri2).GetArrowState(I97) <> 0 Then
					SSetLightColor kStack_Pri2, I97, "white", 0:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
				End if
				if trigger_name = "I103" and StackState(kStack_Pri2).GetArrowState(I103) <> 0 Then
					SSetLightColor kStack_Pri2, I103, "white", 0:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
				End if
				if WizardHits <= 0 Then
					I18.state=1:ModePercent(PlayerMode3)=100
					EndWizardStage()
				End If
			case 4: ' dude
				If trigger_name = "I35" and StackState(kStack_Pri2).GetArrowState(I35) <> 0 Then
					SSetLightColor kStack_Pri2, I35, "white", 0:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
				End if
				if trigger_name = "sw2" and F152.state<>0 Then
					F152.state=0:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
				End If
				if trigger_name = "sw3" and F153.state<>0 Then
					F153.state=0:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
				End If
				if trigger_name = "sw5" and F154.state<>0 Then
					F154.state=0:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
				End If
				If WizardHits <= 0 Then
					I19.state=1:ModePercent(PlayerMode3)=100
					EndWizardStage()
				End If
			case 5: ' back
				If trigger_name = "bumper" Then
					AddScore WizardModesComplete*100000
					WizardHits=WizardHits-1
					if WizardHits <= 0 Then	
						I20.state=1:ModePercent(PlayerMode3)=100
						EndWizardStage()
					End If
				End If
			case 6: ' rats
				If trigger_name = "sw38" Then
					AddScore WizardModesComplete*100000
					WizardHits=WizardHits-1
					if WizardHits <= 0 Then	
						I21.state=1:ModePercent(PlayerMode3)=100
						EndWizardStage()
					End If
				End If
			case 7: ' toys
				if trigger_name = "I86" and StackState(kStack_Pri2).GetArrowState(I86) <> 0 Then
					SSetLightColor kStack_Pri2, I86, "white", 0
					AddScore WizardModesComplete*1000000
				end if
				if trigger_name = "I91" and StackState(kStack_Pri2).GetArrowState(I91) <> 0 Then
					SSetLightColor kStack_Pri2, I91, "white", 0
					AddScore WizardModesComplete*1000000
				end if
				If trigger_name = "I81" Then
					If I75.state=2 Then
						I75.state=1:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
					Elseif I76.state=2 Then
						I76.state=1:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
					Elseif I77.state=2 Then
						I77.state=1:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
					Elseif I78.state=2 Then
						I78.state=1:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
					Elseif I79.state=2 Then
						I79.state=1:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
					Elseif I80.state=2 Then
						I80.state=1:AddScore WizardModesComplete*1000000:WizardHits=WizardHits-1
					End If
				End If
				If WizardHits <= 0 Then
					I22.state=1:ModePercent(PlayerMode3)=100
					EndWizardStage()  
					I75.state=0:I76.state=0:I77.state=0:I78.state=0:I79.state=0:I80.state=0
					SetLightColor F147, "white",0 'toybox
				End If
			case 8: ' love
					If trigger_name = "I91" and StackState(kStack_Pri2).GetArrowState(I91)<>0 Then
						WizardHits=WizardHits-1:AddScore WizardModesComplete*1000000
					End If
					if WizardHits <= 0 Then	
						I23.state=1:ModePercent(PlayerMode3)=100
						EndWizardStage()
					End If
		End Select

	Elseif tmrFinalTour.Enabled and bFinalTourDone=False Then
		D "Final Tour - check " & trigger_name
		if trigger_name = "I35" and I35.UserValue>=4 then		' Final Tour Super Jackpot
			bFinalTourDone=True 
			AddScore 100000000 * Multiplier3x * MultiplierShot * PlayMultiplier							' 100M for FinalTour complete 
			FinalTourCompletions=FinalTourCompletions+1
			LightSeqFinalTourFinale.UserValue = 0
			LightSeqFinalTourFinale.Play SeqUpOn, 30, 1
			'PlayDMDScene "Video-0x0091.wmv", 5040														' Pink light show on DMD
			QueueScene "DoFinalTourComplete'", 5000, 1													' Show Pink light show
			QueueScene "DoFinalTourClear'", 0, 1
			QueueScene2 "ShowPlayerModeComplete(2) '", 2000, 1, True				' Show Save FinalTour Total / Show Mode total
			QueueScene2 "EndFinalTour '", 1, 1, True								' End the Mode 
 		End If
		bValidHit = False
		For each a in aRampLights		' Need to hit flashing arrows
			if a.name = trigger_name and StackState(kStack_Pri2).GetArrowState(a)>=1 then
				bValidHit = True
				set hitLight=a
				a.UserValue = a.UserValue+1
				exit for
			End If
		Next

		if bValidHit Then
			PlayProgress2 trigger_name, False   

			if StackState(kStack_Pri2).GetArrowState(hitLight) = 2 Then		' Add Score
				DOF 128, DOFPulse
				holdScore=10000000
				If FinalTourCount > 8 then  holdScore=20000000
				If FinalTourCount > 16 then holdScore=30000000
				FinalTourCount=FinalTourCount+1
				AddScore holdScore
				SSetLightColor kStack_Pri2, hitLight, modeRampColor, 1
				holdScore = holdScore * MultiplierShot * PlayMultiplier
				QueueFlushPriority 2		' Clear other scores waiting 
				QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", """&Cities(FinalTourCount)&""","""&FormatScore(holdscore)&""","""" '", 1300, 2
				QueueScene "SceneClearMessage '", 0, 2 
				QueueScene "ScenePlayMessage ""Video-0x0047.mp4"",""FINAL TOUR"","""",""CITIES LEFT ""&cstr(24-FinalTourCount) '", 1300, 2
				'	QueueScene "DoAnimateScore kModeXandar, " & GetModeIndex(a.name)+10 & ", " & holdScore & " '", 2000, 2
				'	QueueScene "DoAnimateScore kModeXandar, " & GetModeIndex(a.name) & ", " & holdScore & " '", 2000, 2

				D Cities(FinalTourCount) & " " & holdScore
			elseif StackState(kStack_Pri2).GetArrowState(hitLight) = 1 Then
				DOF 128, DOFPulse
				holdScore=10000000
				If FinalTourCount > 8 then  holdScore=20000000
				If FinalTourCount > 16 then holdScore=30000000
				FinalTourCount=FinalTourCount+1
				SSetLightColor kStack_Pri2, hitLight, modeRampColor, 2
				'PlaySoundVol "sfx_BaseHit13", VolSfx
				holdScore = holdScore * MultiplierShot * PlayMultiplier
				AddScore holdScore
				QueueFlushPriority 2		' Clear other scores waiting 
				QueueScene "ScenePlayMessage ""Video-0x0049.mp4"", """&Cities(FinalTourCount)&""","""&FormatScore(holdscore)&""","""" '", 1300, 2
				QueueScene "SceneClearMessage '", 0, 2 
				QueueScene "ScenePlayMessage ""Video-0x0047.mp4"",""FINAL TOUR"","""",""CITIES LEFT ""&cstr(24-FinalTourCount) '", 1300, 2
				'QueueScene "DoAnimateScore kModeXandar, " & GetModeIndex(a.name)+10 & ", " & holdScore & " '", 2000, 2
				D Cities(FinalTourCount)  & " " & holdScore
			End If

			If FinalTourCount=9 Then
				modeRampColor = "orange"
				For each a in aRampLights
					if a.UserValue < 3 Then
						SSetLightColor kStack_Pri2, a, modeRampColor, StackState(kStack_Pri2).GetArrowState(a)
					End If
				Next
				DisplayDMDText2 "FINAL TOUR","Add A Ball", 1000, 11, 0
				AddMultiball 1							' Add a ball for completing 
			End If
			If FinalTourCount=17 Then
				modeRampColor = "red"
				For each a in aRampLights
					if a.UserValue < 3 Then
						SSetLightColor kStack_Pri2, a, modeRampColor, StackState(kStack_Pri2).GetArrowState(a)
					End If
				Next
				DisplayDMDText2 "FINAL TOUR","Add A Ball", 1000, 11, 0	
				AddMultiball 1							' Add a ball for completing 
			End If

			if hitLight.UserValue >= 3 Then		' three hits turns it off
'				EnableBallSaverForce 5
				SSetLightColor kStack_Pri2, hitLight, modeRampColor, 0
debug.print "Finished: " & hitLight.name & " " & hitLight.UserValue 
			End If

			bFinalShot=True		' See if we got all of the arrows 
			For each a in aRampLights
				if a.UserValue < 3 Then
debug.print "Not Finished: " & a.name & " " & a.UserValue & " " & StackState(kStack_Pri2).GetArrowState(a)
					bFinalShot=False
					exit For
				End If
			Next
			if bFinalShot then	' Light Super Jackpot
				PlaySoundVol "gg_SuperJackpot", VolDef
				' Flash Scoop
				I35.UserValue = 4
				SSetLightColor kStack_Pri2, I35, modeRampColor, 2
			End If 
		End If
	End If
End Sub

Sub EndWizardStage()
	Dim ScoreVal
	
	WizardModesComplete=WizardModesComplete+1  ' # of stages we actually completed no freebies 
	WizardModeStage=WizardModeStage+1
	D "SONGS COMPLETE " & WizardModesComplete & " Mode:" & PlayerMode3 & " %" & ModePercent(PlayerMode3)
	ScoreVal=1000000*WizardModesComplete
	AddScore ScoreVal
	QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", ""SONG COMPLETE"","""&FormatScore(ScoreVal)&""","""" '", 1300, 2
	QueueScene "SceneClearMessage '", 0, 2
	If BallsOnPlayfield <= 6 Then
		AddMultiball 1
	End If
	NextWizardStage()
End Sub

'5todo
Sub EndWizardMode(mode)
	Dim i,a
	D "EndWizardMode Mode:" & mode 
	If tmrMedleyTour.Enabled Then
		tmrMedleyTour.Enabled=False

		for each a in aRampLights	' Disable the Medley lights
			SSetLightColor kStack_Pri2, a, "white", 0
			SSetLightColor kStack_Pri0, a, "white", 0   ' should be off anyways prior to Medley Tour
		Next
		StackState(kStack_Pri2).Disable 

		if playermode <> -1 Then PlaySong "Song-" & SaveMode & ".mp3"

		aRestoreLights()

		playclear pBackglass
		playmedia "Video-0x0000.mp4", "PupVideos", pBackglass, "", -1, "", 1, 1

		bMedleyTourDone=True
		ShowPlayerModeComplete(2)		' Show Mode total
		bWizardMode = False 

		' Turn off all the Mode Lights
		I15.State = 0:I16.State = 0:I17.State = 0:I18.State = 0
		I19.State = 0:I20.state = 0:I21.State = 0
		D "StopPlayerMode calling SetModeLights"
		SetModeLights()			' Set mode lights based on progress incl wizard mode completions

		Playermode3 = -1
		
		CheckWizardModesReady 

		For i = 0 to 6	' check if one of the modes is not completed then allow them to select
			if ModePercent(i) < 100 then
				 D "Still another song available: ModePercent C i=" & i & " %" & ModePercent(i)
				setModeSelectLight(True)
				exit For
			end If
		Next
		if I100.state=2 Then
			ToyBox_OpenLid()
		End If
	Else 
		If bMedleyTourReady then ' Turn off the Medley arrow until triggerred on the new ball
			D "Waiting for Medley - turn off the light at EOB"
			SetLightColor I35, "white", 0
		End If
		D "No wizard mode active"
	End If

D "End WizardMode Done."
	ScorbitBuildGameModes()

End Sub

Dim WizardModeBonusCnt

Sub StartWizardModeBonus()
	D "sub StartWizardModeBonus()"
	tmrWizardModeBonus.Interval=200
	tmrWizardModeBonus.UserValue=-1
	tmrWizardModeBonus.Enabled=True
	WizardModeBonusCnt=0
End Sub


Dim MedC
Sub tmrWizardModeBonus_Timer()
	Dim ScoreVal
	tmrWizardModeBonus.Interval=2000
	ScoreVal=10000000

	D "tmrWizardModeBonus " & tmrWizardModeBonus.UserValue
	Select Case tmrWizardModeBonus.UserValue
		case -1:
			tmrWizardModeBonus.Interval=5800  ' Wizard Medley Screen
			if bUsePUPDMD then
				QueueFlush()
				GiEffect 3
				LightEffect 3
				QueueScene "SceneBMessage """","""","""" '", 100,2
				QueueScene "ScenePlayMessage ""Video-0x003F.mp4"", """","""","""" '", 5500, 1  
				'QueueScene "SceneClearPlayMessage '", 0, 1
				D "ChangeVol pBackglass Volume for Callouts"
				PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pBackglass&", ""FN"":11, ""VL"":60 }"
				PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pMusic&",     ""FN"":11, ""VL"":60 }"
				PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pAudio&",     ""FN"":11, ""VL"":60 }"
			Else
				DisplayDMDText2 "MEDLEY TOUR","WIZARD MODE", 1000, 11, 0
			end if
			tmrWizardModeBonus.UserValue=0
		Case 0:
			D "Last Child Bonus"
			if ModePercent(0) < 100 Then
				tmrWizardModeBonus.Interval=100
				MedC=0
			Else
				MedC=1
				WizardModeBonusCnt=WizardModeBonusCnt+1:Scoreval=10000000*WizardModeBonusCnt
				AddScore ScoreVal
				if bUsePuPDMD then
					QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", """&ModeNames(0)&""","""&FormatScore(ScoreVal)&""",""BONUS"" '", 1500, 1
					PuPlayer.playlistplayex pCallouts,"audio-modes","Sound-0x0459Last.mp3",80, 1
				Else
					DisplayDMDText2 ModeNamesShort(0), FormatScore(ScoreVal), 1000, 11, 0
				End If
				'playmedia "","audio-modes",pCallouts,"Sound-0x0459Last.mp3",1000,"",1,1 
			End If
			tmrWizardModeBonus.UserValue=1
		Case 1: 
			D "Walk This Way Bonus"
			if ModePercent(1) < 100 Then
				tmrWizardModeBonus.Interval=100
			Else
				tmrWizardModeBonus.Interval=3000
				MedC=MedC+1
				WizardModeBonusCnt=WizardModeBonusCnt+1:Scoreval=10000000*WizardModeBonusCnt
				AddScore ScoreVal
				QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", """&ModeNames(1)&""","""&FormatScore(ScoreVal)&""",""BONUS"" '", 1500, 1		
				if bUsePuPDMD then
					PuPlayer.playlistplayex pCallouts,"audio-modes","Sound-0x0458Walk.mp3",80, 1
				Else
					DisplayDMDText2 ModeNamesShort(1), FormatScore(ScoreVal), 1000, 11, 0
				End If
				'playmedia "","audio-modes",pCallouts,"Sound-0x0458Walk.mp3",1000,"",1,1  ' 
			End If

			tmrWizardModeBonus.UserValue=2
		Case 2:
			D "Same Old Song Bonus"
			if ModePercent(2) < 100 Then
				tmrWizardModeBonus.Interval=100
			Else
				tmrWizardModeBonus.Interval=3000
				MedC=MedC+1				
				WizardModeBonusCnt=WizardModeBonusCnt+1:Scoreval=10000000*WizardModeBonusCnt
				AddScore ScoreVal
				If bUsePUPDMD Then
					QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", """&ModeNames(2)&""","""&FormatScore(ScoreVal)&""",""BONUS"" '", 1500, 1
				Else
					DisplayDMDText2 ModeNamesShort(2), FormatScore(ScoreVal), 1000, 11, 0
				End If
				tmrWizardModeBonus.Interval=3500
				PuPlayer.playlistplayex pCallouts,"audio-modes","Sound-0x0457Same.mp3",80, 1
			End If
			tmrWizardModeBonus.UserValue=3
		Case 3:
			D "Sweet Emotion Bonus"
			if ModePercent(3) < 100 Then
				tmrWizardModeBonus.Interval=100
			Else
				tmrWizardModeBonus.Interval=3000
				MedC=MedC+1
				WizardModeBonusCnt=WizardModeBonusCnt+1:Scoreval=10000000*WizardModeBonusCnt
				AddScore ScoreVal
				if bUsePuPDMD then
					QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", """&ModeNames(3)&""","""&FormatScore(ScoreVal)&""",""BONUS"" '", 1500, 1
				Else
					DisplayDMDText2 ModeNamesShort(3), FormatScore(ScoreVal), 1000, 11, 0
				End If
				PuPlayer.playlistplayex pCallouts,"audio-modes","Sound-0x0456Sweet.mp3",80, 1
			End If
			tmrWizardModeBonus.UserValue=4
		Case 4:
			D "Dude Bonus"
			if ModePercent(4) < 100 Then
				tmrWizardModeBonus.Interval=100
			Else
				MedC=MedC+1
				tmrWizardModeBonus.Interval=3000
				WizardModeBonusCnt=WizardModeBonusCnt+1:Scoreval=10000000*WizardModeBonusCnt
				AddScore ScoreVal
				if bUsePuPDMD then
					QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", """&ModeNames(4)&""","""&FormatScore(ScoreVal)&""",""BONUS"" '", 1500, 1
				Else
					DisplayDMDText2 ModeNamesShort(4), FormatScore(ScoreVal), 1000, 11, 0
				End If
				PuPlayer.playlistplayex pCallouts,"audio-modes","Sound-0x0453Dude.mp3",80, 1
			End If
			tmrWizardModeBonus.UserValue=5
		Case 5:
			D "Back in the Saddle Bonus"
			if ModePercent(5) < 100 Then
				tmrWizardModeBonus.Interval=100
			Else
				MedC=MedC+1
				WizardModeBonusCnt=WizardModeBonusCnt+1:Scoreval=10000000*WizardModeBonusCnt
				AddScore ScoreVal
				if bUsePuPDMD then
					QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", """&ModeNames(5)&""","""&FormatScore(ScoreVal)&""",""BONUS"" '", 1500, 1
				Else
					DisplayDMDText2 ModeNamesShort(5), FormatScore(ScoreVal), 1000, 11, 0
				End If
				PuPlayer.playlistplayex pCallouts,"audio-modes","Sound-0x0454Back.mp3",80, 1 
			End If
			tmrWizardModeBonus.UserValue=6
		Case 6:
			D "Rats in the Cellar Bonus"
			if ModePercent(6) < 100 Then
				tmrWizardModeBonus.Interval=100
			Else
				MedC=MedC+1
				WizardModeBonusCnt=WizardModeBonusCnt+1:Scoreval=10000000*WizardModeBonusCnt
				AddScore ScoreVal
				if bUsePuPDMD then
					QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", """&ModeNames(6)&""","""&FormatScore(ScoreVal)&""",""BONUS"" '", 1500, 1
				Else
					DisplayDMDText2 ModeNamesShort(6), FormatScore(ScoreVal), 1000, 11, 0
				End If
				PuPlayer.playlistplayex pCallouts,"audio-modes","Sound-0x0452Rats.mp3",80, 1
			End If
			tmrWizardModeBonus.UserValue=7
		Case 7:
			D "Toys in the Attic Bonus"
			if ModePercent(7) < 100 Then
				tmrWizardModeBonus.Interval=100
			Else
				MedC=MedC+1
				WizardModeBonusCnt=WizardModeBonusCnt+1:Scoreval=10000000*WizardModeBonusCnt
				AddScore ScoreVal
				if bUsePuPDMD then
					QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", """&ModeNames(7)&""","""&FormatScore(ScoreVal)&""",""BONUS"" '", 1500, 1
				Else
					DisplayDMDText2 ModeNamesShort(7), FormatScore(ScoreVal), 1000, 11, 0
				End If
				PuPlayer.playlistplayex pCallouts,"audio-modes","Sound-0x0450Toys.mp3",80, 1
			End If
			tmrWizardModeBonus.UserValue=8
		Case 8:
			D "Love in an Elevator Bonus"
			if ModePercent(8) < 100 Then
				tmrWizardModeBonus.Interval=100
			Else
				MedC=MedC+1
				WizardModeBonusCnt=WizardModeBonusCnt+1:Scoreval=10000000*WizardModeBonusCnt
				AddScore ScoreVal
				if bUsePuPDMD then
					QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", """&ModeNames(8)&""","""&FormatScore(ScoreVal)&""",""BONUS"" '", 1500, 1
				Else
					DisplayDMDText2 ModeNamesShort(8), FormatScore(ScoreVal), 1000, 11, 0
				End If
				PuPlayer.playlistplayex pCallouts,"audio-modes","Sound-0x0451Love.mp3",80, 1
			End If
			tmrWizardModeBonus.UserValue=9
		Case 9: 
			tmrWizardModeBonus.Interval=100
			if MedC = 9 then
				ScoreVal = 200000000
				AddScore ScoreVal
				if bUsePuPDMD then
					QueueScene "PlaySoundVol ""Sound-0x0472Jackpot"", VolSfx '", 0, 1
					QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", ""PERFECT BONUS"","""&FormatScore(200000000)&""","""" '", 3300, 2
					QueueScene "ScenePlayMessage """", """","""","""" '", 100, 2
				Else
					DisplayDMDText2 "PERFECT BONUS", FormatScore(ScoreVal), 1000, 11, 0
				End If
			End If
D "kickout 2"
			vpmtimer.addtimer 100, "ScoopKickOut '"
			tmrWizardModeBonus.UserValue=10
		Case 10:
			tmrWizardModeBonus.Enabled=False
			QueueScene "SceneClearMessage '", 0, 1
			StartWizardModeContinue 3 
			turnitbackup()
	End Select
	D "tmrWizardModeBonus Finish Interval:" & tmrWizardModeBonus.Interval
End Sub
'*************
' Pause Table
'*************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub table1_Exit
	Scorbit.StopSession2 Score(0), Score(1), Score(2), Score(3), PlayersPlayingGame, True 	' In case you stop mid game 
	If b2son then Controller.Stop
	If bUseFlexDMD then 
		If Not FlexDMD is Nothing Then FlexDMD.Run = False
	End If
	Savehs
End Sub

'********************
'     Flippers
'********************
Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
    If Enabled Then
		LF.fire 
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If
		RotateLaneLightsLeft
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
        RF.fire 
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
        RotateLaneLightsRight
    Else
        RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
    End If
End Sub


' flippers hit Sound

Sub LeftFlipper_Collide(parm)
	If bUseUltraDMD then AddScore 1	' Keep the display from blanking
    LeftFlipperCollide parm
End Sub


Sub RightFlipper_Collide(parm)
	If bUseUltraDMD then AddScore 1	' Keep the display from blanking
    RightFlipperCollide parm
End Sub


Sub RotateLaneLightsLeft()
	Dim TempState
	TempState = F152.State
	F152.State = F153.State
	F153.State = F154.State
	F154.State = TempState
	F152.blinkinterval = 50
	F153.blinkinterval = 50
	F154.blinkinterval = 50
End Sub

Sub RotateLaneLightsRight()
	Dim TempState
	TempState = F154.State
	F154.State = F153.State
	F153.State = F152.State
	F152.State = TempState
	F152.blinkinterval = 50
	F153.blinkinterval = 50
	F154.blinkinterval = 50
End Sub

'*********
' Queue - This could be used for anything but I use it to queue  priority=1 items up with the option to have 1 Priority=2 item queued or running 
'				Thought here is Pri 1 items need to be shown, Pri 2 items can be shown if an item is running 
'
' 	NOTE - Since VPMtimer is limited to 20 concurrent timers you need a timer called tmrQueue to ensure items dont get dropped 
'	QueueScene
'		Command=vbscript command    ex:   "RunFunction ""Test"", 123  '"
'		Length=milliseconds before running the next item in the queue
'		Priority=Number, 0 being highest
'
'*********
dim PupQueue(20, 4)			' Size=20,  Fields are 0=Command, 1=Priority, 2=time, 3=MustRun
dim PupQueueEndPos			' Size of the queue (-1 = Empty)
dim QueueActive				' We are actively running something 
Dim QueueCurrentTime		' How much time is this one going to run (Just used for gtting the queue time)
QueueActive=False
PupQueueEndPos=-1
Sub QueueFlush()
	QueueFlushPriority -1
End Sub 

Sub QueueFlushPriority(priority)		' Only flush ones with this priority
	Dim time
	dim xx
	dim nextFree

	nextFree=-1
	for xx = 0 to PupQueueEndPos
		if PupQueue(xx, 3)=True or (PupQueue(xx, 1)<>priority and priority<>-1) then		' MustRun=True,  keep it 
			if nextFree=-1 then 
				nextFree=0
			else 
				nextFree=nextFree+1
			End if 
			PupQueue(nextFree, 0 )=PupQueue(xx,0 )
			PupQueue(nextFree, 1 )=PupQueue(xx,1 )
			PupQueue(nextFree, 2 )=PupQueue(xx,2 )
			PupQueue(nextFree, 3 )=PupQueue(xx,3 )
		End if 
	Next 
	PupQueueEndPos=nextFree

	' See if one is actively running
	time=0
	if QueueActive and QueueCurrentTime <> 0 then time = (DateDiff("s", now, QueueCurrentTime) * 1000)

	if nextFree= -1 and time=0 then
'debug.print "QueueFlush Empty Deactivated"
		QueueActive=False
		tmrQueue.Enabled = False
	End if 

End Sub
Function getQueueTime()		' Returns how much time left on queue
	Dim time,i 
	time = 0
'debug.print "GetQueueTime:" & now 
'debug.print "GetQueueTime:" & QueueCurrentTime & " " & QueueActive
	if QueueActive and QueueCurrentTime <> 0 then time = (DateDiff("s", now, QueueCurrentTime) * 1000)
'debug.print "GetQueueTime Active:" & time

	for i = 0 to PupQueueEndPos
		time = time + PupQueue(i, 2) 
	Next
	getQueueTime = time
'debug.print "GetQueueTime ret:" & time
End Function

Sub QueuePop()
	if PupQueueEndPos = -1 then exit sub 
	PupQueue(0, 1 )=99
	SortPupQueue
	PupQueue(PupQueueEndPos,0 )=""
	PupQueue(PupQueueEndPos,2 )=0
	PupQueue(PupQueueEndPos,3 )=False
	PupQueueEndPos=PupQueueEndPos-1

Debug.print "--Q-Dump Pop---"
	Dim xx
	for xx = 0 to PupQueueEndPos
		debug.print xx & " " & PupQueue(xx, 0) & " " & PupQueue(xx, 1) & " " & PupQueue(xx, 2) & " " & PupQueue(xx, 3) 
	Next 
Debug.print "--Q-Dump Pop---"


End Sub
 

Sub QueueScene(Command, msecLen, priority) 
	QueueScene2 Command, msecLen, priority, False 
End Sub 

Sub QueueScene2(Command, msecLen, priority, bMustRun) 
debug.print "Queue Scene " & Command & " Len: " & msecLen
	if PupQueueEndPos < UBound(PupQueue, 1) then 
		PupQueueEndPos=PupQueueEndPos+1
	End if 
	' NOTE: If it is full we overwrite the lowest priority (Optionally we could make the queue bigger)
	PupQueue(PupQueueEndPos,0 )=Command
	PupQueue(PupQueueEndPos,1 )=priority
	PupQueue(PupQueueEndPos,2 )=msecLen
	PupQueue(PupQueueEndPos,3 )=bMustRun
	SortPupQueue
	
Debug.print "--Q-Dump---"
	Dim xx
	for xx = 0 to PupQueueEndPos
		debug.print xx & " " & PupQueue(xx, 0) & " " & PupQueue(xx, 1) & " " & PupQueue(xx, 2) & " " & PupQueue(xx, 3) 
	Next 
Debug.print "--Q-Dump---"

	RunQueue True
End Sub

Sub tmrQueue_Timer
	tmrQueue.Enabled = False 
	RunQueue False
End Sub 

Sub QueueSkip()						' Shortcycle the timer and move on
'debug.print "--Q Skip--"
	if tmrQueue.Enabled Then
		tmrQueue.Enabled = False
		RunQueue False
	End if
End Sub

Sub RunQueue(bNewItem)
	dim qCmd, qTime
debug.print "Run Queue " & QueueActive & " " & bNewItem & " " & Now
	if QueueActive = False or bNewItem=False then 	' Nothing is running Or we just finished running something 
		if PupQueueEndPos <> -1 then
			QueueActive = True
			qCmd=PupQueue(0, 0)
			qTime=PupQueue(0, 2)
debug.print "Exec " & qCmd
			PupQueue(0, 3)=True		' Set MustRun to True so it cant get deleted while running 
			Execute qCmd
'debug.print "Timer " & qTime
			if qTime > 0 then 
				QueueCurrentTime = DateAdd("s",qTime/1000, now)
'debug.print QueueCurrentTime
				tmrQueue.Interval = qTime
				tmrQueue.Enabled = True
				'vpmtimer.addtimer cInt(qTime), "RunQueue False '"
				QueuePop
			Else			' No timer just run the next item in the queue 
				QueueCurrentTime = 0
				QueuePop
				RunQueue False
			End If
		Else 
debug.print "Queue Empty Deactivated"
			QueueActive = False
		End If 
	End if
End Sub

Sub SortPupQueue
	dim a, j, temp1, temp2, temp3, temp4
	for a = PupQueueEndPos - 1 To 0 Step -1
		for j= 0 to a
			if PupQueue(j, 1)>PupQueue(j+1, 1) then
				temp1=PupQueue(j+1,0 )
				temp2=PupQueue(j+1,1 )
				temp3=PupQueue(j+1,2 )
				temp4=PupQueue(j+1,3 )
				PupQueue(j+1,0 )=PupQueue(j,0 )
				PupQueue(j+1,1 )=PupQueue(j,1 )
				PupQueue(j+1,2 )=PupQueue(j,2 )
				PupQueue(j+1,3 )=PupQueue(j,3 )
				PupQueue(j, 0 )=temp1
				PupQueue(j, 1 )=temp2
				PupQueue(j, 2 )=temp3
				PupQueue(j, 3 )=temp4
			end if
		next
	next 

End Sub


'*********
' TILT
'*********

Dim PauseBigScore:PauseBigScore=False
Dim TiltDangerWait
TiltDangerWait=False
Sub SceneTilt(Message)
	PauseBigScore=True
	TiltDangerWait=True
	playmedia "Video-0x0047.mp4", "PupBackgrounds", pOverVid, "", -1, "", 1, 1
	PuPlayer.LabelShowPage pOverVid, 1,0,""
	If TiltCount(CurrentPlayer) = 1 then 
		PuPlayer.LabelSet pBackglass,"BigScore"," ",0,""
		puPlayer.LabelSet pOverVid,"OverMessage1", ""	,1,""
		puPlayer.LabelSet pOverVid,"OverMessage2", Message	,1,""
		puPlayer.LabelSet pOverVid,"OverMessage3", ""	,1,""
	Else 
		PuPlayer.LabelSet pBackglass,"BigScore"," ",0,""
		puPlayer.LabelSet pOverVid,"OverMessage1", ""	,1,""
		puPlayer.LabelSet pOverVid,"OverMessage2", Message	,1,""
		puPlayer.LabelSet pOverVid,"OverMessage3", ""	,1,""
	End If 
End Sub


Sub SceneClearTilt()
	TiltDangerWait=False
	puPlayer.LabelSet pOverVid,"OverMessage1", " "	,0,""
	puPlayer.LabelSet pOverVid,"OverMessage2", " "	,0,""
	puPlayer.LabelSet pOverVid,"OverMessage3", " "	,0,""
	playclear pOverVid
	PauseBigScore=False
End Sub


Sub SceneClearPlayMessage()
	PuPlayer.LabelSet pOverVid,"OverMessage1"," ",0,""
	PuPlayer.LabelSet pOverVid,"OverMessage2"," ",0,""
	PuPlayer.LabelSet pOverVid,"OverMessage3"," ",0,""
	playclear pOverVid
	PauseBigScore=False
End Sub


Sub ScenePlayMessage(Media, Message1, Message2, Message3)
	PauseBigScore=True
	If bUseUltraDMD Then
		UltraDMD.DisplayScene00 "", Message1, 15, Message3, 15, 14, duration, 14
	ElseIf bUsePUPDMD Then 
		PuPlayer.LabelSet pBackglass,"BigScore"," ",0,""
		If Media <> "" 	Then playmedia Media, "PupVideos", pOverVid, "", -1, "", 1, 1
		PuPlayer.LabelShowPage pOverVid, 1,0,""

		If Len(Message1) > 15 then  
			PuPlayer.LabelSet pOverVid,"OverMessage1", Message1	,1,"{'mt':2,'size': " & 8*FontScale & " }"   
		ElseIf Len(Message1) > 10 then
			PuPlayer.LabelSet pOverVid,"OverMessage1", Message1	,1,"{'mt':2,'size': " & 12*FontScale & " }" 
		Else
			PuPlayer.LabelSet pOverVid,"OverMessage1", Message1	,1,"{'mt':2,'size': " & 15*FontScale & " }" 
		End if

		If Len(Message2) > 9 then  
			PuPlayer.LabelSet pOverVid,"OverMessage2", Message2	,1,"{'mt':2,'size': " & 8*FontScale & " }"   
		Else
			PuPlayer.LabelSet pOverVid,"OverMessage2", Message2	,1,"{'mt':2,'size': " & 10*FontScale & " }" 
		End if
		puPlayer.LabelSet pOverVid,"OverMessage3", Message3	,1,""
	ElseIf bUseFlexDMD Then
		SternScoreboard Message3, "", CL(0,Message1), CL(1,Message2)
	End If
	If NOT bUsePUPDMD Then
		If Message1 = "" or Message1 = "_" then 
			I2 Message2
		Else
			I2 Message1 & chr(13) & Message2
		End if
	End If
End Sub 

Sub SceneClearMessage()
	If bUsePUPDMD Then
		PuPlayer.LabelSet pBackglass,"BigScore"," ",0,""
		PuPlayer.LabelSet pBackglass,"Message1"," ",0,""
		PuPlayer.LabelSet pBackglass,"Message2"," ",0,""
		PuPlayer.LabelSet pBackglass,"Message3"," ",0,""
	End If
	PauseBigScore=False
End Sub


Sub SceneMessage(Message1, Message2, Message3)  
	'D "SceneMessage " & Message1 & ":" & Message2
	PauseBigScore=True
	If bUseUltraDMD Then
		UltraDMD.DisplayScene00 "", Message1, 15, Message3, 15, 14, duration, 14
	ElseIf bUsePUPDMD Then
		If bPupStarted then 
			PuPlayer.LabelSet pBackglass,"BigScore"," ",0,""

			If Len(Message1) > 15 then
				PuPlayer.LabelSet pBackglass,"Message1", Message1	,1,"{'mt':2,'size': " & 10*FontScale & " }"   
			ElseIf Len(Message1) > 10 then
				PuPlayer.LabelSet pBackglass,"Message1", Message1	,1,"{'mt':2,'size': " & 12*FontScale & " }" 
			Else
				PuPlayer.LabelSet pBackglass,"Message1", Message1	,1,"{'mt':2,'size': " & 15*FontScale & " }" 
			End if

			If Len(Message2) > 9 then
				PuPlayer.LabelSet pBackglass,"Message2", Message2	,1,"{'mt':2,'size': " & 12*FontScale & " }"   
			Else
				PuPlayer.LabelSet pBackglass,"Message2", Message2	,1,"{'mt':2,'size': " & 18*FontScale & " }" 
			End if

			puPlayer.LabelSet pBackglass,"Message3", Message3	,1,""
		End If 
	ElseIf bUseFlexDMD Then
		SternScoreboard Message3, "", CL(0,Message1), CL(1,Message2)
	End If
	If NOT bUsePUPDMD Then
		If Message1 = "" or Message1 = "_" then 
			I2 Message2
		Else
			I2 Message1 & chr(13) & Message2
		End if
	End If
End Sub 

Sub SceneBMessage(Message1, Message2, Message3)  ' Super Bonus Messages   - All 1 Color
	'D "SceneMessage " & Message1
	PauseBigScore=True
	If bUseUltraDMD Then
		UltraDMD.DisplayScene00 "", Message1, 15, Message3, 15, 14, duration, 14
	ElseIf bUsePUPDMD Then
		If bPupStarted then 
			PuPlayer.LabelSet pBackglass,"BigScore"," ",0,""

			If Len(Message1) > 15 then
				PuPlayer.LabelSet pBackglass,"Message1", Message1	,1,"{'mt':2,'size': " & 10*FontScale & " }"   
			ElseIf Len(Message1) > 10 then
				PuPlayer.LabelSet pBackglass,"Message1", Message1	,1,"{'mt':2,'size': " & 12*FontScale & " }" 
			Else
				PuPlayer.LabelSet pBackglass,"Message1", Message1	,1,"{'mt':2,'size': " & 15*FontScale & " }" 
			End if

			If Len(Message2) > 9 then
				PuPlayer.LabelSet pBackglass,"Message2", Message2	,1,"{'mt':2,'size': " & 12*FontScale & " }"   
			Else
				PuPlayer.LabelSet pBackglass,"Message2", Message2	,1,"{'mt':2,'size': " & 18*FontScale & " }" 
			End if

			puPlayer.LabelSet pBackglass,"Message3", Message3	,1,""
		End If 
	ElseIf bUseFlexDMD Then
		SternScoreboard "", "", CL(0,Message1), CL(1,Message2)
	End If
	If NOT bUsePUPDMD Then
		If Message1 = "" or Message1 = "_" then 
			I2 Message2
		Else
			I2 Message1 & chr(13) & Message2
		End if
	End If
End Sub 

Sub ShowPlayerModeComplete(ExtraMode) ' Show the total for the player mode (ExtraMode: 0=ToyMB, 1=ElevMB, 2=WizardMode, -1=DefaultMode)
	D "ShowPlayerModeComplete " & ExtraMode & " PlayerMode:" & PlayerMode & " bSecondMode:" & bSecondMode
	dim ModeStr
	Dim ScoreVal
	Dim SecondStr
	dim thisMode
	ModeStr=""
	If ExtraMode = 0 Then				' ToyMB
		ModeStr = "TOYBOX MULTIBALL"
		ScoreVal = ToyBoxMBJackpotTotal
	elseIf ExtraMode = 1 Then 			' ElevatorMB
		ModeStr = "ELEVATOR MULTIBALL"
		ScoreVal = ElevMBJackpotTotal
	elseIf PlayerMode > -1 Then
		thisMode = PlayerMode
		ScoreVal=ModePoints
		ModeStr=ModeNames(thisMode)
	elseif ExtraMode= 2 then 
		ScoreVal=WizardModePoints	
		ModeStr = "MEDLEY TOUR"
	elseif ExtraMode = 3 then 
			ModeStr = "FINAL TOUR"
	End If

	If ModeStr <> "" then 
		' Queue up PlayerModeComplete scene
		QueueScene "SceneMessage """&ModeStr&""","""&FormatScore(ScoreVal)&""",""TOTAL"" '", 2000, 2
		QueueScene "SceneClearMessage '", 0, 2

		If ExtraMode = -1 Then
			If PlayerMode <> -1 and bSecondMode=False Then
				If ModePercent(PlayerMode) >= 100 Then
					QueueScene "SceneFinishMode " & PlayerMode & " '", 6000, 2
					QueueScene "SceneClearFinishMode '", 0, 2
				End If
			End If
		elseIf ExtraMode = 0 and I22.state = 1 then				' We completed it 
			QueueScene "SceneFinishMode 7 '", 6000, 2
			QueueScene "SceneClearFinishMode '", 0, 2			
		elseIf ExtraMode = 1 and I23.state = 1 then				' We completed it  
			QueueScene "SceneFinishMode 8 '", 6000, 2
			QueueScene "SceneClearFinishMode '", 0, 2			
		End If 
	End If 
End Sub


'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round
Sub CheckTilt                                    'Called when table is nudged
    If NOT bGameInPlay Then Exit Sub
	If TiltDangerWait then Exit Sub
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
	If Tilt > 15 and TiltCount(CurrentPlayer) < 2 Then 
		TiltCount(CurrentPlayer) = TiltCount(CurrentPlayer) + 1
		
		QueueScene "SceneTilt ""DANGER"" '", 3000, 1
		QueueScene "SceneClearTilt '", 0, 1
	ElseIf Tilt > 15 Then 'If more that 15 then TILT the table
        Tilted = True
		PuPlayer.playlistplayex pCallouts,"audioevents","tilted-Sound-0x03A8.wav",100, 1
		SceneTilt "TILT"
		vpmtimer.addtimer 3000, "SceneClearTilt '"

        DMDFlush
		DisplayDMDText "TILT","", 3000
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
    End If
End Sub


Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt > 0 Then
        Tilt = Tilt - 0.1
    Else
        TiltDecreaseTimer.Enabled = False
    End If
End Sub


Sub DisableTable(Enabled)
    If Enabled Then
        'turn off GI and turn off all the lights
        GiOff
        LightSeqTilt.Play SeqAllOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart

        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
		mMagnet.MotorOn = False
    Else
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        'clean up the buffer display
        DMDFlush
		mMagnet.MotorOn = True
    End If
End Sub


Sub TiltRecoveryTimer_Timer()
    ' If all the balls have been drained then..
    If(BallsOnPlayfield = 0)Then
        ' do the normal end of ball thing (this doesn't give a bonus If the table is tilted)
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub



' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************


Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function


Const tnob = 20 ' total number of balls
Const lob = 0   'number of locked balls
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
    Dim BOT, b, ballpitch, ballvol
    BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next

    ' exit the sub If no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

	'debug.print lob & " " & UBound(BOT)

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
		'debug.print "b"

		aBallShadow(b).X = BOT(b).X
		aBallShadow(b).Y = BOT(b).Y

		If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If

		'***Ball Drop Sounds***
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
    Next
End Sub


' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board
'
Sub AddScore(points)
	dim multiplierVal
	dim tPoints
	ResetBallSearch
    If(Tilted = False)Then
		'D "AddScore: " & points & "Mode3%:" & Mode2Percent(3) & " PlayerMode2=" & PlayerMode2 & " (3)Progress=" & Mode2Progress(3)
		If MultiplierShot > 1 then D "***2X SHOT***"
		If Multiplier3x > 1 then D "** 3X SHOT **"
		multiplierVal = Multiplier3x * MultiplierShot * PlayMultiplier
		tPoints=(points * multiplierVal)

		If Mode2Percent(3) <> -1 and Mode2Percent(3) < 100 and tPoints > 0  Then
			Mode2Progress(3) = Mode2Progress(3)+1
			Mode2Percent(3) = CINT((Mode2Progress(3)  / 20) * 100)
			tPoints=int(tPoints*1.50)
			'D "**SUPER SCORE**"

			ShowPlayerMode2(3)
			If (Mode2Progress(3) >= 20) Then
				Mode2Percent(3) = 100
			End If
		End If
		ModePoints = ModePoints + tPoints
		if bWizardMode then WizardModePoints = WizardModePoints + tPoints

        ' add the points to the current players score variable
        Score(CurrentPlayer) = Score(CurrentPlayer) + tPoints
        ' update the score displays

		If Multiplier3x <> 1 then 
			Multiplier3x = 1	' 3x only last for next score
			SetLightColor I32, "green", I32.UserValue
			SetLightColor I46, "green", I46.UserValue
			SetLightColor I67, "green", I67.UserValue
			SetLightColor I84, "green", I84.UserValue
			SetLightColor I95, "green", I95.UserValue
			SetLightColor I101, "green",I101.UserValue
			SetLightColor I89, "green",  I89.UserValue
			SetLightColor I112, "green",I112.UserValue
		End If

		Scorbit.SendUpdate Score(0), Score(1), Score(2), Score(3), Balls, CurrentPlayer+1, PlayersPlayingGame

        DMDScore
    End if
End Sub


'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
	' Load Orbital scores 
	GetScores

    Dim x
    x = LoadValue(TableName, "HighScore1")
    If(x <> "")Then HighScore(0) = CDbl(x)Else HighScore(0) = 100000 End If
    x = LoadValue(TableName, "HighScore1Name")
    If(x <> "")Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(TableName, "HighScore2")
    If(x <> "")then HighScore(1) = CDbl(x)Else HighScore(1) = 100000 End If
    x = LoadValue(TableName, "HighScore2Name")
    If(x <> "")then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(TableName, "HighScore3")
    If(x <> "")then HighScore(2) = CDbl(x)Else HighScore(2) = 100000 End If
    x = LoadValue(TableName, "HighScore3Name")
    If(x <> "")then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(TableName, "HighScore4")
    If(x <> "")then HighScore(3) = CDbl(x)Else HighScore(3) = 100000 End If
    x = LoadValue(TableName, "HighScore4Name")
    If(x <> "")then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(TableName, "HighScore5Name")
    If(x <> "")then HighScoreName(4) = x Else HighScoreName(4) = "MED" End If
    x = LoadValue(TableName, "Credits")
    If(x <> "")then Credits = CInt(x)Else Credits = 0 End If

    'x = LoadValue(TableName, "Jackpot")
    'If(x <> "") then Jackpot = CDbl(x) Else Jackpot = 200000 End If
    x = LoadValue(TableName, "TotalGamesPlayed")
    If(x <> "")then TotalGamesPlayed = CInt(x)Else TotalGamesPlayed = 0 End If
	Score(0) = HighScore(0)
'HighScore(0)=10
'HighScore(1)=8
'HighScore(2)=6
'HighScore(3)=4
'msgbox "Games played=" & TotalGamesPlayed
End Sub

Sub Savehs
    SaveValue TableName, "HighScore1", HighScore(0)
    SaveValue TableName, "HighScore1Name", HighScoreName(0)
    SaveValue TableName, "HighScore2", HighScore(1)
    SaveValue TableName, "HighScore2Name", HighScoreName(1)
    SaveValue TableName, "HighScore3", HighScore(2)
    SaveValue TableName, "HighScore3Name", HighScoreName(2)
    SaveValue TableName, "HighScore4", HighScore(3)
    SaveValue TableName, "HighScore4Name", HighScoreName(3)
    SaveValue TableName, "HighScore5", HighScore(4)
    SaveValue TableName, "HighScore5Name", HighScoreName(4)
    SaveValue TableName, "Credits", Credits
    SaveValue TableName, "TotalGamesPlayed", TotalGamesPlayed
End Sub

Sub Clearhs
	HighScore(0) = 100000
	HighScoreName(0) = "AAA"
	HighScore(1) = 100000
	HighScoreName(1) = "BBB"
	HighScore(2) = 100000
	HighScoreName(2) = "CCC"
	HighScore(3) = 100000
	HighScoreName(3) = "DDD"
    SaveValue TableName, "HighScore1", HighScore(0)
    SaveValue TableName, "HighScore1Name", HighScoreName(0)
    SaveValue TableName, "HighScore2", HighScore(1)
    SaveValue TableName, "HighScore2Name", HighScoreName(1)
    SaveValue TableName, "HighScore3", HighScore(2)
    SaveValue TableName, "HighScore3Name", HighScoreName(2)
    SaveValue TableName, "HighScore4", HighScore(3)
    SaveValue TableName, "HighScore4Name", HighScoreName(3)
End Sub

' ***********************************************************
'  High Score Initals Entry Functions - based on Black's code
' ***********************************************************

Dim hsbModeActive
Dim hsEnteredName
Dim hsEnteredDigits(3)
Dim hsCurrentDigit
Dim hsValidLetters
Dim hsCurrentLetter
Dim hsLetterFlash

Sub CheckHighscore()
	D "CheckHighScore"
	osbtempscore = Score(CurrentPlayer)

    If Score(CurrentPlayer) > HighScore(0) Then 'add 1 credit for beating the highscore
        AwardSpecial
    End If
	' Bail out on high score when auto testing 
    If AutoQa=false and AutoAI=false and Score(CurrentPlayer) >= HighScore(3) Then	' Overwrite the lowest
        HighScore(3) = Score(CurrentPlayer)
        'enter player's name
        HighScoreEntryInit()
    Else
		If osbactive <> 0 then 
			' Submit to Orbital
			osbtemp = osbdefinit
			SubmitOSBScore
		End If 
        EndOfBallComplete()
    End If
End Sub

Sub HighScoreEntryInit()
	D "HighScoreEntryInit()"
Dim NameStr: NameStr=""
	If bUsePUPDMD then 
		PuPlayer.LabelShowPage pBackglass, 3,0,""
		pDMDEvent(kDMD_Attract)
		if ScorbitActive then 
			if Scorbit.bSessionActive then
				NameStr=Scorbit.GetName(CurrentPlayer+1)
				if NameStr<>"" then 
					puPlayer.LabelSet pBackglass,"EnterHS1", NameStr	,1,""
				Else 
					puPlayer.LabelSet pBackglass,"EnterHS1", "PLAYER " & CurrentPlayer+1	,1,""
				End if 
			End if 
		Else 
			puPlayer.LabelSet pBackglass,"EnterHS1", "PLAYER " & CurrentPlayer+1	,1,""
		End If
		puPlayer.LabelSet pBackglass,"EnterHS2", "ENTER INITIALS" ,1,""
	End If
 
    hsbModeActive = True
    hsLetterFlash = 0

    hsEnteredDigits(0) = " "
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 0

    hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ<+-0123456789"    ' < is used to delete the last letter
    hsCurrentLetter = 1
    DMD_Clearforhighscore()
	DMDId "hsc", "Enter", "Your Name", 999999
    HighScoreDisplayName()
End Sub

Sub EnterHighScoreKey(keycode)
    If keycode = LeftFlipperKey Then
        PlaySoundVol "fx_Previous", VolDef
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0)then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayName()
    End If

    If keycode = RightFlipperKey Then
        PlaySoundVol "fx_Next", VolDef
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter > len(hsValidLetters))then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayName()
    End If

    If keycode = StartGameKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<")then
            PlaySoundVol "fx_Enter", VolDef
			
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3)then
                HighScoreCommitName()
            else
                HighScoreDisplayName()
            end if
        else
			PlaySoundVol "fx_Esc", VolDef
			
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit > 0)then
                hsCurrentDigit = hsCurrentDigit - 1
            end if
            HighScoreDisplayName()
        end if
    end if
End Sub


Sub HighScoreDisplayName()
	Dim i, TempStr
	Dim bugFIX

    TempStr = " >"
    if(hsCurrentDigit> 0) then TempStr = TempStr & hsEnteredDigits(0)
    if(hsCurrentDigit> 1) then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit> 2) then TempStr = TempStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3) then
        if(hsLetterFlash <> 0) then
            TempStr = TempStr & "_"
        else
            TempStr = TempStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit <1) then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit <2) then TempStr = TempStr & hsEnteredDigits(2)

    TempStr = TempStr & "< "
	bugFIX=""
	If bUseUltraDMD then		' Not sure why but ULTRA doesnt update unless both lines change
		If (hsCurrentLetter mod 2) = 0 then 
			bugFIX="-"
		else
			bugFIX=" -"
		End If
	End If
	DMDMod "hsc", "YOUR NAME" & bugFIX, Mid(TempStr, 2, 5), 999999
	If bUsePUPDMD then puPlayer.LabelSet pBackglass,"EnterHS3", TempStr,1,""
End Sub

Sub HighScoreCommitName()
    hsbModeActive = False

    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ")then
        hsEnteredName = "YOU"
    end if

    HighScoreName(3) = hsEnteredName
    SortHighscore
	DMD_Clearforhighscore

	' Submit to Orbital
	osbtemp = hsEnteredName
	SubmitOSBScore

	If bUsePUPDMD then 
		puPlayer.LabelSet pBackglass,"EnterHS1", "",1,""
		puPlayer.LabelSet pBackglass,"EnterHS2", "",1,""
		puPlayer.LabelSet pBackglass,"EnterHS3", "",1,""
	End If 
	pBGGamePlay
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
End Sub


Sub DMD_Clearforhighscore()
	If (bUseUltraDMD) then
		UltraDMD.CancelRendering
		UltraDMD.Clear
	End If
End Sub


Sub AwardSpecial()
	QueueScene "playmedia ""Video-0x0054.mp4"", ""PupVideos"", pOverVid , """", -1, """", 1, 1 '", 5000, 2
	DMD "EXTRA GAME WON", "", "", eNone, eBlink, eNone, 1000, True, ""
	KnockerSolenoid
	Credits = Credits + 1
	DOF 140, DOFOn
	DOF 115, DOFPulse
	GiEffect 1
	LightEffect 1
	DOF 400, DOFPulse   'DOF MX - Special
End Sub


Sub AddBonusMultiplier(n)
    ' If not at the maximum bonus level
    if(BonusMultiplier + n <= MaxMultiplier)then
        SetBonusMultiplier(BonusMultiplier + n)
		If bUsePUPDMD then 
			QueueScene "ScenePlayMessage """&"Video-0x006A-" & Cstr(BonusMultiplier) &".mp4"", """","""","""" '", 3000, 2
			QueueScene "SceneClearPlayMessage '", 0, 2
		Else 
			DisplayDMDText2 "BONUS MULTIPLIER",BonusMultiplier & "x", 1000, 11, 0
		End If 
    End if
End Sub


Sub SetBonusMultiplier(Level)
    ' Set the multiplier to the specified level
    BonusMultiplier = Level
End Sub


Sub AwardExtraBall()
	D "Sub Award ExtraBall " & bExtraBallWonThisBall
    If NOT bExtraBallWonThisBall Then
		D "Award ExtraBall Awarded"
		QueueScene "playmedia ""Video-0x0014.mp4"", ""PupVideos"", pOverVid , """", -1, """", 1, 1 '", 5000, 2
        DMD "EXTRA BALL WON", "", "", eNone, eBlink, eNone, 1000, True, SoundFXDOF("Knocker_1", 122, DOFPulse, DOFKnocker)
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
        GiEffect 1
        LightEffect 2
		SetLightColor I14, "yellow", 2
    END If
End Sub


Sub LoadFlexDMD
'	Dim WshShell
'	Set WshShell = CreateObject("WScript.Shell")
'	WshShell.RegWrite "HKCU\Software\UltraDMD\fullcolor",UseFullColor,"REG_SZ"  'UseFullColor
	' FlexDMD API is found at https://github.com/vbousquet/flexdmd/blob/f3d67a6667b8c35637eec0e90a0f23b4647f533b/docs/FlexDMD_API.md 

	'On Error Resume Next
	Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
	if Not FlexDMD is Nothing Then
		FlexDMD.TableFile = Table1.Filename & ".vpx"
		FlexDMD.RenderMode=FlexDMD_RenderMode_DMD_GRAY_4
		FlexDMD.Width = 128
		FlexDMD.Height = 32
		FlexDMD.Clear=True
		FlexDMD.GameName = cGameName   
		FlexDMD.Run = True
		FlexDMD.Color = RGB(255,88, 32) '&hCA1BCC
		'Set DMDScene = FlexDMD.NewGroup("Scene")
		'DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.bkempty")
		'DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
		'Set UltraDMD = FlexDMD.NewUltraDMD()
		'UseColoredDMD = True
		'CreateScoreSceneSternStyle()
		CreateScoreSceneWilliamsStyle()
	Else
        MsgBox  "No FlexDMD found.  This table MAY run without it."
		bUseUltraDMD=False
        Exit Sub
    End If
	On Error Goto 0
End Sub

Sub DMDClearQueue				' It looks like If we call this too fast it will cancel the ULTRA DMD logo scene
	If bUseUltraDMD and UltraDMDVideos Then
		If UltraDMD.IsRendering Then
			UltraDMD.CancelRendering
		End If
	End If
End Sub

Sub PlayDMDScene(video, toptext, bottomtext, timeMs)
	If bUseUltraDMD and UltraDMDVideos Then
		UltraDMD.DisplayScene00 video, toptext, 15, bottomtext, 15, UltraDMD_Animation_None, timeMs, UltraDMD_Animation_None
	End If
	If bUseFlexDMD Then
		SternScoreboard "", "", CL(0, toptext), CL(1, bottomtext)
	End If
	If NOT bUsePUPDMD Then
		I2 toptext & chr(13) & bottomtext
	End if
End Sub

Sub DisplayDMDText(Line1, Line2, duration)
	'D "Sub DisplayDMDText " & Line1 & " " & Line2 & " Duration:" & duration
	If bUseUltraDMD Then
		UltraDMD.DisplayScene00 "", Line1, 15, Line2, 15, 14, duration, 14
	ElseIf bUsePUPDMD Then
		If bPupStarted then 
			If Line1 = "" or Line1 = "_" then 
				pupDMDDisplay "-", Line2, "" ,Duration/1000, 0, 10
			else
				pupDMDDisplay "-", Line1 & "^" & Line2, "" ,Duration/1000, 0, 10
			End If 
		End If 
	ElseIf bUseFlexDMD Then
		SternScoreboard "", "", CL(0,Line1), CL(1,Line2)
	End If
	If NOT bUsePUPDMD Then
		If Line1 = "" or Line1 = "_" then 
			I2 Line2
		Else
			I2 Line1 & chr(13) & Line2
		End if
	End If
End Sub

Sub DisplayDMDText2(Line1, Line2, duration, pri, blink)
D "Sub DisplayDMDText2 :" & Line1 & ":" & Line2 & "<"

	If bUseUltraDMD Then
		UltraDMD.DisplayScene00 "", Line1, 15, Line2, 15, 14, duration, 14
	ElseIf bUsePUPDMD Then
		If bPupStarted then 
			If Line1 = "" or Line1 = "_" then 
				pupDMDDisplay "-", Line2, "" ,Duration/1000, blink, pri
			else
				pupDMDDisplay "-", Line1 & "^" & Line2, "" ,Duration/1000, blink, pri
			End If 
		End If 
	ElseIf bUseFlexDMD Then
D "Calling SternScoreboard " & Line1 & " " & Line2
		SternScoreboard "", "", CL(0,Line1), CL(1,Line2)
	End If
	If NOT bUsePUPDMD Then
		If Line1 = "" or Line1 = "_" then 
			I2 Line2
		Else
			I2 Line1 & chr(13) & Line2
		End if
	End If
End Sub


Sub DMDId(id, toptext, bottomtext, duration) 'used in the highscore entry routine
	If bUseUltraDMD then 
		UltraDMD.DisplayScene00ExwithID id, false, "", toptext, 15, 0, bottomtext, 15, 0, 14, duration, 14
	ElseIf bUsePUPDMD Then
		If bPupStarted then pupDMDDisplay "default", toptext & "^" & bottomtext, "" ,Duration/1000, 0, 10
	End If 
End Sub

Sub DMDMod(id, toptext, bottomtext, duration) 'used in the highscore entry routine
	If bUseUltraDMD then 
		UltraDMD.ModifyScene00Ex id, toptext, bottomtext, duration
	ElseIf bUsePUPDMD Then
		If bPupStarted then pupDMDDisplay "default", toptext & "^" & bottomtext, "" ,Duration/1000, 0, 10
	End If 
End Sub


' *************************************************************************
'   JP's Reduced Display Driver Functions (based on script by Black)
' only 5 effects: none, scroll left, scroll right, blink and blinkfast
' 3 Lines, treats all 3 lines as text. 3rd line is just 1 character
' Example format:
' DMD "text1","text2","backpicture", eNone, eNone, eNone, 250, True, "sound"
' Short names:
' dq = display queue
' de = display effect
' *************************************************************************

Const eNone = 0        ' Instantly displayed
Const eScrollLeft = 1  ' scroll on from the right
Const eScrollRight = 2 ' scroll on from the left
Const eBlink = 3       ' Blink (blinks for 'TimeOn')
Const eBlinkFast = 4   ' Blink (blinks for 'TimeOn') at user specified intervals (fast speed)

Const dqSize = 64

Dim dqHead
Dim dqTail
Dim deSpeed
Dim deBlinkSlowRate
Dim deBlinkFastRate

Dim dCharsPerLine(2)
Dim dLine(2)
Dim deCount(2)
Dim deCountEnd(2)
Dim deBlinkCycle(2)

Dim dqText(2, 64)
Dim dqEffect(2, 64)
Dim dqTimeOn(64)
Dim dqbFlush(64)
Dim dqSound(64)

Sub DMD_Init() 'default/startup values
    Dim i, j

	If bUseFlexDMD then LoadFlexDMD()

    DMDFlush()
    deSpeed = 20
    deBlinkSlowRate = 5
    deBlinkFastRate = 2
    dCharsPerLine(0) = 26
    dCharsPerLine(1) = 26
    dCharsPerLine(2) = 3
	if bUseFlexDMD Then
		dCharsPerLine(0) = 18
		dCharsPerLine(1) = 18 
		dCharsPerLine(2) = 3
	End if
    For i = 0 to 2
        dLine(i) = Space(dCharsPerLine(i))
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
        dqTimeOn(i) = 0
        dqbFlush(i) = True
        dqSound(i) = ""
    Next
    For i = 0 to 2
        For j = 0 to 64
            dqText(i, j) = ""
            dqEffect(i, j) = eNone
        Next
    Next
    DMD dLine(0), dLine(1), dLine(2), eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDFlush()
	DMDClearQueue
End Sub

Sub DMDScore()
	Dim TimeStr, Astr
	TimeStr = ""

	If bUseFlexDMD Then
		if tmrMedleyTour.Enabled  then
			if WizardHits > 0 Then
				Select case PlayerMode3:
					case 0:	TimeStr = "LOOPS LEFT:" & WizardHits
					case 1:	TimeStr = "SHOTS LEFT:" & WizardHits
					case 2: TimeStr = "TARGETS LEFT:" & WizardHits
					case 3: TimeStr = "RAMPS LEFT:" & WizardHits
					case 4: TimeStr = "LANES LEFT:" & WizardHits
					case 5: TimeStr = "POPS LEFT:" & WizardHits
					case 6: TimeStr = "SPINS LEFT:" & WizardHits
					case 7: TimeStr = "TOYS LEFT:" & WizardHits
					case 8: TimeStr = "ELEVATOR LEFT:" & WizardHits
				End Select
			End If
		elseif tmrFinalTour.Enabled  then 
			TimeStr = "CITIES LEFT:" & 24 - FinalTourCount
		elseif PlayerMode = -1 Then
			If bSecondMode then
				If PlayerMode2 = -1 then 
					TimeStr = ""
				Else 
					TimeStr = Mode2Percent(PlayerMode2) & "%  HITS:" & SwitchHitCount & " 2nd"
				End If 
			else 
				TimeStr = "Select Mode"
			End If
			if bToyBoxMultiball Then
				if ToyBoxMBJackpotHits < 18 then
					TimeStr = "TOYS LEFT:" & 18 - ToyBoxMBJackpotHits
				End If 
			End If
		End If
		If Score(CurrentPlayer) < 2300000000 then 		' UltraDMD cant handle alything greater than 2.4 billion ???? sux
			if ModecountdownTimer.Enabled and PlayerMode > -1 then
				if ModePercent(PlayerMode) >= 0 Then
					Astr = "Time:" & ModeCountdownTimer.UserValue & " M:" & ModePercent(PlayerMode)
					I2 "Score:" & Score(CurrentPlayer) & chr(13) &  "Time:" & ModeCountdownTimer.UserValue & " " & ModePercent(PlayerMode) & "%"
				Else
					Astr = "Credits " & cstr(Credits)
					if playermode <> -1 then
						I2 "Score:" & Score(CurrentPlayer) & chr(13) & "PlayerMode:" & PlayerMode & " BOP:" & BallsOnPlayfield
					Else
						I2 "Score:" & Score(CurrentPlayer) & chr(13) & "BOP:" & BallsOnPlayfield & " " & TimeStr
					End If
				End If
			else
				Astr = "Credits " & cstr(Credits)
				if playermode <> -1 then
					I2 "Score:" & Score(CurrentPlayer) & chr(13) & "PlayerMode:" & PlayerMode & " BOP:" & BallsOnPlayfield
				Else
					I2 "Score:" & Score(CurrentPlayer) & chr(13) & "BOP:" & BallsOnPlayfield & " " & TimeStr
					Astr = TimeStr
				End If
			End If
			SternScoreboard AStr, "", "", ""
		End If 
	Else
		if ModecountdownTimer.Enabled and PlayerMode > -1 Then
			if ModePercent(PlayerMode) >= 0 Then
				Astr = "Time:" & ModeCountdownTimer.UserValue & " " & ModePercent(PlayerMode) & "%"
			Else
				Astr = "Credits " & cstr(Credits)
			End If
		elseif tmrMedleyTour.Enabled  then 
				Astr = "" 'Hits:" & 1234 & " Sw:" & 1234  Based on Mode you are in    
		elseif tmrFinalTour.Enabled  then 
				Astr = "CITIES LEFT:" & 24 - FinalTourCount
		elseif PlayerMode = -1 Then
			If bSecondMode then
				If PlayerMode2 = -1 then 
					Astr = ""
				Else 
					Astr = Mode2Percent(PlayerMode2) & "%  HITS:" & SwitchHitCount & " 2nd"
				End If 
			else 
				Astr = "Select Mode"
			End If 
		Else
			Astr = "Credits " & cstr(Credits)
		End If
		I2 "Score:" & Score(CurrentPlayer) & chr(13) & "PlayerMode:" & PlayerMode & " BOP:" & BallsOnPlayfield & _
			chr(13) & Astr
		
	End If
End Sub

Sub SternScoreboard(val, titlestr, line1, line2)
	if FlexDMD is Nothing then Exit Sub
	Dim i, n, x, y, label
	FlexDMD.LockRenderThread

	if Len(ltrim(titlestr))=0 then titlestr = ""
	if Len(ltrim(line1))=0 then line1 = "": 
	if Len(ltrim(line2))=0 then line2 = "": 


	'D "SternScoreboard:" & titlestr & ":" & line1 & ":" & line2


			if val = "" then val = "Credits " & cstr(Credits)
			For i = 1 to 4
				Set label = FlexDMD.Stage.GetLabel("Score_" & i)
				If i = CurrentPlayer+1 Then
					label.Font = FontScoreActive
				Else
					label.Font = FontScoreInactive
				End If
				if i <= PlayersPlayingGame and (len(titlestr & line1 & line2) < 5) Then
					label.Text = FormatNumber(Score(i-1), 0, -1, 0, -1)
				Else
					label.Text = ""
				End if
			Next

			'For i = 1 to 4
			'	Set label = FlexDMD.Stage.GetLabel("Score_" & i)
			'	label.SetAlignedPosition 45, 1 + (i - 1) * 6, FlexDMD_Align_TopRight
			'Next

			if line1 <> "" AND line2 <> "" Then
				FlexDMD.Stage.GetLabel("Ball").Text = ""
				FlexDMD.Stage.GetLabel("Credit").Text = ""
			Else
				FlexDMD.Stage.GetLabel("Ball").Text = "Ball " & cstr(Balls)
				FlexDMD.Stage.GetLabel("Credit").Text = val

			End if
			if titlestr = "" and (line1="" or line1="_") then
				FlexDMD.Stage.GetLabel("Title").Text = line2
				FlexDMD.Stage.GetLabel("Line1").Text = ""
				FlexDMD.Stage.GetLabel("Line2").Text = ""
			Elseif titlestr = "" and (line2="" or line2="_") then
				FlexDMD.Stage.GetLabel("Title").Text = line1
				FlexDMD.Stage.GetLabel("Line1").Text = ""
				FlexDMD.Stage.GetLabel("Line2").Text = ""
			Else
				FlexDMD.Stage.GetLabel("Title").Text = titlestr
				FlexDMD.Stage.GetLabel("Line1").Text = line1
				FlexDMD.Stage.GetLabel("Line2").Text = line2
			End if

			FlexDMD.Stage.GetLabel("Score_1").SetAlignedPosition 0, 0, FlexDMD_Align_TopLeft
			FlexDMD.Stage.GetLabel("Score_2").SetAlignedPosition 128, 0, FlexDMD_Align_TopRight
			FlexDMD.Stage.GetLabel("Score_3").SetAlignedPosition 0, 24, FlexDMD_Align_BottomLeft
			FlexDMD.Stage.GetLabel("Score_4").SetAlignedPosition 128, 24, FlexDMD_Align_BottomRight
			FlexDMD.Stage.GetLabel("Ball").SetAlignedPosition 2, 33, FlexDMD_Align_BottomLeft
			FlexDMD.Stage.GetLabel("Credit").SetAlignedPosition 128, 33, FlexDMD_Align_BottomRight

	FlexDMD.UnlockRenderThread
End Sub
Sub DMDScoreNow
    DMDFlush
    DMDScore
End Sub

Sub CreateScoreSceneSternStylexx()
	Dim i
	DotMatrix.color = RGB(255, 88, 32)

	Set FontScoreActive = FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", vbWhite, vbWhite, 0)
	Set FontScoreInactive = FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", RGB(128, 128, 128), vbWhite, 0)
	Dim FontBig : Set FontBig = FlexDMD.NewFont("FlexDMD.Resources.bm_army-12.fnt", vbWhite, vbWhite, 0)
	Dim font : Set font = FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", vbWhite, vbWhite, 0)
	'Dim Zxfont : Set font = FlexDMD.NewFont("FlexDMD.Resources.zx_spectrum-7.fnt", vbWhite, vbWhite, 0)
	Dim UDMDfont7x13 : Set UDMDfont7x13 = FlexDMD.NewFont("FlexDMD.Resources.udmd-f7by13.fnt", vbWhite, vbWhite, 0)
	Dim UDMDfont7x5 : Set UDMDfont7x5 = FlexDMD.NewFont("FlexDMD.Resources.udmd-f7by5.fnt", vbWhite, vbWhite, 0)
	Dim UDMDfont6x12 : Set UDMDfont6x12 = FlexDMD.NewFont("FlexDMD.Resources.udmd-f6by12.fnt", vbWhite, vbWhite, 0)
	Dim UDMDfont5x7 : Set  UDMDfont5x7 = FlexDMD.NewFont("FlexDMD.Resources.udmd-f5by7.fnt", vbWhite, vbWhite, 0)
	Dim UDMDfont : Set UDMDfont = FlexDMD.NewFont("FlexDMD.Resources.udmd-f4by5.fnt", vbWhite, vbWhite, 0)

	Dim scene : Set scene = FlexDMD.NewGroup("Score")
	For i = 1 to 4
		scene.AddActor FlexDMD.NewLabel("Score_" & i, FontScoreInactive, "0")
	Next

	'scene.AddActor FlexDMD.NewFrame("VSeparator")
	'scene.GetFrame("VSeparator").Thickness = 1
	'scene.GetFrame("VSeparator").SetBounds 45, 0, 1, 32

	scene.AddActor FlexDMD.NewGroup("Content")
	scene.GetGroup("Content").Clip = True
	scene.GetGroup("Content").SetBounds 47, 0, 81, 32 

	scene.AddActor FlexDMD.NewGroup("FullContent")
	scene.GetGroup("FullContent").SetBounds 1, 0, 81, 32


	'Dim title : Set title = FlexDMD.NewLabel("Title", UDMDfont, "AEROSMITH")
	'Dim af : Set af = title.ActionFactory
	'Dim list : Set list = af.Sequence()
	'list.Add af.MoveTo(1, 10, 0)
	'list.Add af.Wait(1.0)
	'list.Add af.MoveTo(128, 10,  5.0)
	'list.Add af.Wait(0.5)
	'list.Add af.MoveTo(-128, 10, 5.0)
	'list.Add af.Wait(0.5)
	'title.AddAction af.Repeat(list, -1)

	scene.GetGroup("FullContent").AddActor FlexDMD.NewLabel("Title", UDMDfont6x12, "")

	scene.GetGroup("Content").AddActor FlexDMD.NewLabel("Ball", FontScoreActive, "Ball 1")
	scene.GetGroup("Content").AddActor FlexDMD.NewLabel("Credit", FontScoreActive, "Credits " & cstr(Credits))

	scene.GetGroup("FullContent").AddActor FlexDMD.NewLabel("Line1", UDMDfont6x12, "Line1")  '7x5 is not bad
	scene.GetGroup("FullContent").AddActor FlexDMD.NewLabel("Line2", UDMDfont6x12, "Line2")

	FlexDMD.LockRenderThread
	FlexDMD.RenderMode = FlexDMD_RenderMode_DMD_GRAY_4
	FlexDMD.Stage.RemoveAll
	FlexDMD.Stage.AddActor scene
	FlexDMD.Stage.GetLabel("Title").SetAlignedPosition 1, 10, FlexDMD_Align_Center
	FlexDMD.Stage.GetLabel("Ball").SetAlignedPosition 0, 33, FlexDMD_Align_BottomLeft
	FlexDMD.Stage.GetLabel("Credit").SetAlignedPosition 81, 33, FlexDMD_Align_BottomRight
	FlexDMD.Stage.GetLabel("Line1").SetAlignedPosition 0, 7, FlexDMD_Align_Left
	FlexDMD.Stage.GetLabel("Line2").SetAlignedPosition 0, 21, FlexDMD_Align_Left
	FlexDMD.Show = True
	FlexDMD.UnlockRenderThread
End Sub

Sub CreateScoreSceneWilliamsStyle()
	DotMatrix.color = RGB(255, 88, 32)
	Dim i
	'Set FontScoreActive = FlexDMD.NewFont("FlexDMD.Resources.udmd-f7by13.fnt", vbWhite, vbWhite, 0)
	Set FontScoreActive = FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", vbWhite, vbWhite, 0)
	Set FontScoreInactive = FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", RGB(128, 128, 128), vbWhite, 0)
	Dim UDMDfont6x12 : Set UDMDfont6x12 = FlexDMD.NewFont("FlexDMD.Resources.udmd-f6by12.fnt", vbWhite, vbWhite, 0)

	Dim scene : Set scene = FlexDMD.NewGroup("Score")
	For i = 1 to 4
		scene.AddActor FlexDMD.NewLabel("Score_" & i, FontScoreInactive, "0")
	Next
	scene.AddActor FlexDMD.NewLabel("Ball", FontScoreInactive, "Ball 1")
	scene.AddActor FlexDMD.NewLabel("Credit", FontScoreInactive, "Credit " & cstr(Credits))

	scene.AddActor FlexDMD.NewLabel("Title", UDMDfont6x12, "")
	scene.AddActor FlexDMD.NewLabel("Line1", UDMDfont6x12, "")  '7x5 is not bad
	scene.AddActor FlexDMD.NewLabel("Line2", UDMDfont6x12, "")
	

	FlexDMD.LockRenderThread
	FlexDMD.RenderMode = FlexDMD_RenderMode_DMD_GRAY_4
	FlexDMD.Stage.RemoveAll
	FlexDMD.Stage.AddActor scene

	FlexDMD.Stage.GetLabel("Title").SetAlignedPosition 1, 10, FlexDMD_Align_Center
	FlexDMD.Stage.GetLabel("Line1").SetAlignedPosition 0, 3, FlexDMD_Align_Left
	FlexDMD.Stage.GetLabel("Line2").SetAlignedPosition 0, 17, FlexDMD_Align_Left
	FlexDMD.Show = True
	FlexDMD.UnlockRenderThread
End Sub

Sub DMD(Text0, Text1, Text2, Effect0, Effect1, Effect2, TimeOn, bFlush, Sound)
	'D "Sub DMD() :" & text0 & ":" & text1 & ":" & text2 & " TimeOn=" & TimeOn
	if (Text1="" or Text1="_") AND (Text2="" or Text2="_") then
		'D "Sub DMD() Step1"
		DisplayDMDText CL(0,ltrim(Text0)), "",  TimeOn
	else
		'D "Sub DMD() Step2:" & CL(0,ltrim(Text0)) & ":" & CL(1,ltrim(Text1))
		DisplayDMDText CL(0,ltrim(Text0)), CL(1,ltrim(Text1)), TimeOn
	end if
	PlaySoundVol Sound, VolDef
	if Text0 = "" or Text0 = "_" Then
		I2 Text1 & chr(13) & Text2
	Else
		I2 Text0 & chr(13) & Text1 & chr(13) & Text2
	End If
End Sub

Function ExpandLine(TempStr, id) 'id is the number of the dmd line
    If TempStr = "" Then
        TempStr = Space(dCharsPerLine(id))
    Else
        if(Len(TempStr) > Space(dCharsPerLine(id)))Then
            TempStr = Left(TempStr, Space(dCharsPerLine(id)))
        Else
            if(Len(TempStr) < dCharsPerLine(id))Then
                TempStr = TempStr & Space(dCharsPerLine(id)- Len(TempStr))
            End If
        End If
    End If
    ExpandLine = TempStr
End Function

Function FormatScore(ByVal Num) 'it returns a string with commas (as in Black's original font)
    dim i
    dim NumString

    NumString = CStr(abs(Num))

	For i = Len(NumString)-3 to 1 step -3
		If IsNumeric(mid(NumString, i, 1))then
			NumString = left(NumString, i) & "," & right(NumString, Len(NumString)-i)
		end if
	Next

    FormatScore = NumString
End function

Function CL(id, NumString) 'center line
    Dim Temp, TempStr
	NumString = LEFT(NumString, dCharsPerLine(id))
    Temp = (dCharsPerLine(id)- Len(NumString)) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL = TempStr
End Function

Function RL(id, NumString) 'right line
    Dim Temp, TempStr
	NumString = LEFT(NumString, dCharsPerLine(id))
    Temp = dCharsPerLine(id)- Len(NumString)
    TempStr = Space(Temp) & NumString
    RL = TempStr
End Function

Function FL(id, aString, bString) 'fill line
    Dim tmp, tmpStr
	aString = LEFT(aString, dCharsPerLine(id))
	bString = LEFT(bString, dCharsPerLine(id))
    tmp = dCharsPerLine(id)- Len(aString)- Len(bString)
	If tmp <0 Then tmp = 0
    tmpStr = aString & Space(tmp) & bString
    FL = tmpStr
End Function

Function LPad(StringToPad, Length, CharacterToPad)
  Dim x : x = 0
  If Length > Len(StringToPad) Then x = Length - len(StringToPad)
  LPad = String(x, CharacterToPad) & StringToPad
End Function

Function RPad(StringToPad, Length, CharacterToPad)
  Dim x : x = 0
  If Length > Len(StringToPad) Then x = Length - len(StringToPad)
  RPad = StringToPad & String(x, CharacterToPad)
End Function

'****************************************
' Real Time updatess using the GameTimer
'****************************************
'used for all the real time updates

Sub GameTimer_Timer
    RollingUpdate
	pRightGate.RotX = RightGate.CurrentAngle
	pLeftGate.RotX = LeftGate.CurrentAngle
End Sub

'********************************************************************************************
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
'********************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState) 'thanks gtxjoe for the first myVersion
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
' 11 colors: red, orange, amber, yellow...
'******************************************

Sub SetLightColor(n, col, stat)
	D2 "SetLightColor " & n.name & " " & col & " " & stat
    Select Case col
		Case "cyan"
			n.color = RGB(0, 128, 128)
			n.colorfull = RGB(0, 231, 231)
        Case "red"
            n.color = RGB(200, 0, 0)
            n.colorfull = RGB(255, 0, 0)
        Case "orange"
            n.color = RGB(18, 3, 0)
            n.colorfull = RGB(255, 64, 0)
        Case "yellow"
            n.color = RGB(200, 200, 0)
            n.colorfull = RGB(255, 255, 0)
        Case "green"
            n.color = RGB(0, 200, 0)
            n.colorfull = RGB(0, 255, 0)
		Case "green-intense"
            n.color = RGB(0, 255, 0)
            n.colorfull = RGB(0, 255, 0)
        Case "blue"
            n.color = RGB(0, 0, 200)
            n.colorfull = RGB(0, 0, 255)
        Case "blue-light"
            n.color = RGB(18, 18, 18)
            n.colorfull = RGB(18, 18, 255)
        Case "white"
            n.color = RGB(255, 252, 224)  ' value is used on trigers so dont change
            n.colorfull = RGB(193, 91, 0)
        Case "purple"
            n.color = RGB(128, 0, 128)
            n.colorfull = RGB(255, 0, 255)
        Case "amber"
            n.color = RGB(193, 49, 0)
            n.colorfull = RGB(255, 153, 0)        
		Case "pink"
            n.color = RGB(255,192, 203)
            n.colorfull = RGB(255,204, 255)
		Case Else
			msgbox  "Unknown color " & col
    End Select
    If stat <> -1 Then
        n.State = 0
		n.BlinkInterval = BlinkIntDef
        n.State = stat
    End If
End Sub

Sub SetLightColorRestore(light, lIndex)
	Dim finalColor
	Dim finalState 
	Dim i
	D2 "SetLightColorRestore " & light.name & " " & lIndex
	If lIndex = -1 then 					' Got get it If they didnt already have it 
		lIndex = GetModeIndex(light.name)
	End If 

	finalState = 0
	finalColor = "white"					' Should never need this line but just in Case 
	for i = 0 to StackSize-1				' Set the real color and state based on the stack  (higher in the stack is higher prioroty)
		If StackState(i).bStackActive then 
			If StackState(i).ArrowStates(lIndex).ArrowState <> 0 then 
				finalState = StackState(i).ArrowStates(lIndex).ArrowState
				finalColor = StackState(i).ArrowStates(lIndex).ArrowColor
				D2 "Stack: " & i & " lindex:" & lIndex & " state:" & finalState & " color:" & finalColor
			End If 
		End If  
	Next
	D2 "SetLightColorRestore Setting it to.." & light.name & " state:" & finalState & " color:" & finalColor
	SetLightColor light, finalColor, finalState
End Sub 

Sub SSetLightColor(stackIndex, light, color, state)		' StackSetLightColor		
	dim lIndex
	lIndex = GetModeIndex(light.name)
	If lIndex <> -1 then 
		StackState(stackIndex).ArrowStates(lIndex).ArrowColor = color
		StackState(stackIndex).ArrowStates(lIndex).ArrowState = state
		D "SSetLightColor " & stackIndex & " idx:" & lIndex & " Light:" & light.name & " state:" & state & " Color:" & color
		SetLightColorRestore light, lIndex
	else 
		D2 "Light Color Error:" & light.name & " idx:" & stackIndex & " " & lIndex
	End If 
End Sub

Sub SSetLightColorName(stackIndex, lightName, color, state)		' StackSetLightColor		
	dim lIndex, a
	D "SSetLightColorName "  & lightName & " idx:" & stackIndex & " color:" & color

	lIndex = GetModeIndex(lightName)
	If lIndex <> -1 then 
		StackState(stackIndex).ArrowStates(lIndex).ArrowColor = color
		StackState(stackIndex).ArrowStates(lIndex).ArrowState = state
		D "SSetLightColor " & stackIndex & " idx:" & lIndex & " Light:" & lightName & " state:" & state & " Color:" & color
		For Each a in aRampLights
			If UCase(a.name) = UCase(lightName) then 
				SetLightColorRestore a, lIndex
			End If
		Next

	else 
		D "Light Color Name Error:" & lightName & " idx:" & stackIndex & " " & lIndex
	End If 
End Sub

Sub flshCIU_Timer()
    If Lampz.State(2) then 
		Lampz.SetLamp 2, False
	Else
		Lampz.SetLamp 2, True
	End If 
End Sub

Sub setCIULight(bValue)
	If bValue then 
		flshCIU.TimerEnabled = True
		QueueScene "SceneMessage ""CRANK IT UP"",""IS LIT"","""" '", 1000, 2
		QueueScene "SceneClearMessage '", 0, 2
		PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x04F1cranklit.wav",100, 1
	Else 
		flshCIU.TimerEnabled = False
		Lampz.SetLamp 2, False
	End If 
End Sub 

Sub setModeSelectLight(bValue)
	D "sub setModeSelectLight() " & bValue & " BOP:" & BallsOnPlayfield
	If bValue Then
		SetLightColor I34, "white", 2
	Else
		SetLightColor I34, "white", 0
	End if
End Sub

Sub setExtraBallLight(bValue)
	D "sub setExtraBallLight() " & bValue
	If bValue Then
		SetLightColor I33, "orange", 2
	Else
		SetLightColor I33, "orange", 0
	End if
End Sub

Sub setMysteryLight(bValue)
	If bValue Then
		SetLightColor I38, "white", 2
	Else
		SetLightColor I38, "white", 0
	End if
End Sub


Sub flshDude_Timer()
    If Lampz.State(1) then 
		Lampz.SetLamp 1, False
	Else
		Lampz.SetLamp 1, True
	End If 
End Sub

Sub setDudeLight(bValue)
	If bValue then 
		flshDude.TimerEnabled = True
	Else 
		flshDude.TimerEnabled = False
		Lampz.SetLamp 1, False
	End If 
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Orbital Scoreboard Code
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

'****************************
' POST SCORES
'****************************
Dim osbtemp
Dim osbtempscore:osbtempscore = 0
If osbactive = 1 or osbactive = 2 Then osbtemp = osbdefinit
Sub SubmitOSBScore
	dim vers
	Dim ver
	vers=split(myVersion,".")			' Convert x.y.z to x.y
	ver=vers(0) & "." & vers(1)

	On Error Resume Next
	If osbactive = 1 or osbactive = 2 Then
		Dim objXmlHttpMain, Url, strJSONToSend 

		Url = "https://hook.integromat.com/82bu988v9grj31vxjklh2e4s6h97rnu0"
		strJSONToSend = "{""auth"":""" & osbkey &""",""player id"": """ & osbid & """,""player initials"": """ & osbtemp &""",""score"": " & CStr(osbtempscore) & ",""table"":"""& TableName & """,""version"":""" & ver & """}"

		Set objXmlHttpMain = CreateObject("Msxml2.ServerXMLHTTP")
		objXmlHttpMain.open "PUT",Url, False
		objXmlHttpMain.setRequestHeader "Content-Type", "application/json"
		objXmlHttpMain.setRequestHeader "application", "application/json"

		objXmlHttpMain.send strJSONToSend
	end if
End Sub	

'****************************
' GET SCORES
'****************************
dim worldscores

Sub GetScores()
	If osbactive =0 then exit sub 
	If osbkey="" then exit sub
	On Error Resume Next
	Dim objXmlHttpMain, Url, strJSONToSend 

	Url = "https://hook.integromat.com/kj765ojs42ac3w4915elqj5b870jrm5c"

	strJSONToSend = "{""auth"":"""& osbkey &""", ""table"":"""& TableName & """}"

	Set objXmlHttpMain = CreateObject("Msxml2.ServerXMLHTTP")
	objXmlHttpMain.open "PUT",Url, False
	objXmlHttpMain.setRequestHeader "Content-Type", "application/json"
	objXmlHttpMain.setRequestHeader "application", "application/json"

	objXmlHttpMain.send strJSONToSend

	worldscores = objXmlHttpMain.responseText
	vpmtimer.addtimer 3000, "showsuccess '"
	debug.print "Got OSB scores"
	'debug.print worldscores
	splitscores
End Sub	

Dim scorevar(22)
Dim dailyvar(22)
Dim weeklyvar(22)
Dim alltimevar(42)
Sub emptyscores
	dim i		
	For i = 0 to 42
		alltimevar(i) = "0"
	Next
	For i = 0 to 22
		weeklyvar(i) = "0"
		dailyvar(i) = "0"
	Next
End Sub

EmptyScores()


Sub splitscores
	On Error Resume Next
	dim a,scoreset,subset,subit,myNum,daily,weekly,alltime,x
	a = Split(worldscores,": {") 
	subset = Split(a(1),"[")

'		debug.print subset(1)
'		debug.print subset(2)
'		debug.print subset(3)
' daily scores
	myNum = 0
	daily = Split(subset(1),": ")
	for each x in daily
		myNum = MyNum + 1
		x = Replace(x, vbCr, "")
		x = Replace(x, vbLf, "")
		x = Replace(x, ",", "")
		x = Replace(x, """", "")
		x = Replace(x, "{", "")
		x = Replace(x, "}", "")
		x = Replace(x, "score", "")
		x = Replace(x, "initials", "")
		x = Replace(x, "weekly", "")
		x = Replace(x, "]", "")
		x = Replace(x, "alltime", "")
		dailyvar(MyNum) = x
		If dailyvar(MyNum) = "" Then
			If MyNum = 2 or 4 or 6 or 8 or 10 or 12 or 14 or 16 or 18 or 20 Then
				dailyvar(MyNum) = "OBS"
			Else
				dailyvar(MyNum) = 0
			end if
		end if
		debug.print "dailyvar(" &MyNum & ")=" & x
	Next

' weekly scores
	myNum = 0
	weekly = Split(subset(2),": ")
	for each x in weekly
		myNum = MyNum + 1
		x = Replace(x, vbCr, "")
		x = Replace(x, vbLf, "")
		x = Replace(x, ",", "")
		x = Replace(x, """", "")
		x = Replace(x, "{", "")
		x = Replace(x, "}", "")
		x = Replace(x, "score", "")
		x = Replace(x, "initials", "")
		x = Replace(x, "weekly", "")
		x = Replace(x, "]", "")
		x = Replace(x, "alltime", "")
		weeklyvar(MyNum) = x
		If weeklyvar(MyNum) = "" Then
			If MyNum = 2 or 4 or 6 or 8 or 10 or 12 or 14 or 16 or 18 or 20 Then
				weeklyvar(MyNum) = "OBS"
			Else
				weeklyvar(MyNum) = 0
			end if
		end if
		debug.print "weeklyvar(" &MyNum & ")=" & x
	Next

' alltime scores
	myNum = 0
	alltime = Split(subset(3),": ")
	for each x in alltime
		myNum = MyNum + 1
		x = Replace(x, vbCr, "")
		x = Replace(x, vbLf, "")
		x = Replace(x, ",", "")
		x = Replace(x, """", "")
		x = Replace(x, "{", "")
		x = Replace(x, "}", "")
		x = Replace(x, "score", "")
		x = Replace(x, "initials", "")
		x = Replace(x, "weekly", "")
		x = Replace(x, "]", "")
		x = Replace(x, "alltime", "")
		alltimevar(MyNum) = x
		If alltimevar(MyNum) = "" Then
			If MyNum = 2 or 4 or 6 or 8 or 10 or 12 or 14 or 16 or 18 or 20 or 22 or 24 or 26 or 28 or 30 or 32 or 34 or 36 or 38 or 40 Then
				alltimevar(MyNum) = "OBS"
			Else
				alltimevar(MyNum) = "0"
			end if
		end if
		debug.print "alltimevar(" &MyNum & ")=" & x
	Next

End Sub

sub showsuccess
	'pNote "Scoreboard","Updated"
	pupDMDDisplay "-","Scoreboard^Updated","",3,0,10
end sub

Class PinupNULL	' Dummy Pinup class so I dont have to keep adding If Cases when people dont choose pinup
	Public Sub LabelShowPage(screen, pagenum, vis, Special)
	End Sub
	Public Sub LabelSet(screen, label, text, vis, Special)
	End Sub
	Public Sub playlistplayex(screen, dir, fname, volume, priority)
	End Sub 
End Class 


'********************* START OF PUPDMD FRAMEWORK v1.0 *************************
'******************** DO NOT MODIFY STUFF BELOW   THIS LINE!!!! ***************
'******************************************************************************
'*****   Create a PUPPack within PUPPackEditor for layout config!!!  **********
'******************************************************************************
'
'
'  Quick Steps:
'      1>  create a folder in PUPVideos with Starter_PuPPack.zip and call the folder "yourgame"
'      2>  above set global variable pGameName="yourgame"
'      3>  copy paste the settings section above to top of table script for user changes.
'      4>  on Table you need to create ONE timer only called pupDMDUpdate and set it to 250 ms enabled on startup.
'      5>  go to your tabe1_init or table first startup function and call PUPINIT function
'      6>  Go to bottom on framework here and setup game to call the appropriate events like pStartGame (call that in your game code where needed)...etc
'      7>  attractmodenext at bottom is setup for you already,  just go to each Case and add/remove as many as you want and setup the messages to show.  
'      8>  Have fun and use pDMDDisplay(xxxx)  sub all over where needed.  remember its best to make a bunch of mp4 with text animations... looks the best for sure!
'
'
'Note:  for *Future Pinball* "pupDMDupdate_Timer()" timer needs to be renamed to "pupDMDupdate_expired()"  and then all is good.
'       and for future pinball you need to add the follow lines near top
'Need to use BAM and have com idll enabled.
'				Dim icom : Set icom = xBAM.Get("icom") ' "icom" is name of "icom.dll" in BAM\Plugins dir
'				If icom is Nothing then MSGBOX "Error cannot run without icom.dll plugin"
'				Function CreateObject(className)       
'   					Set CreateObject = icom.CreateObject(className)   
'				End Function


Const HasPuP = True   'dont set to false as it will break pup

Const pTopper=0
Const pDMD=1
Dim pBackglass:pBackglass=5
Const pPlayfield=3
Const pMusic=4
Const pAudio=14
Const pCallouts=6
Const pBackglass2=7
Const pPup0=8
Const pBonusScreen = 12
Const pOverVid=13
Const pPopUP1=11
Const pPopUP2=12
Const pPopUP3=13
Const pPopUP4=14
Const pPopUP5=15
Const pPopUP6=16
Const pPopUP7=17
Const pPopUP8=18
Const pPopUP9=19

'pages
Const pDMDBlank=0
Const pScores=1
Const pBigLine=2
Const pThreeLines=3
Const pTwoLines=4
Const pLargerLetters=5

'dmdType
Const pDMDTypeLCD=0
Const pDMDTypeReal=1
Const pDMDTypeFULL=2


Dim PuPlayer
Dim PUPDMDObject  'for realtime mirroring.
Dim pDMDlastchk: pDMDLastchk= -1    'performance of updates
Dim pDMDCurPage: pDMDCurPage= 0     'default page is empty.
Dim pInAttract : pInAttract=false   'pAttract mode

ModeNames(0)="LAST CHILD"
ModeNamesShort(0)="Last Child"
ModeNames(1)="WALK THIS WAY"
ModeNamesShort(1)="Walk This Way"
ModeNames(2)="SAME OLD SONG AND DANCE"
ModeNamesShort(2)="Same Old Song"
ModeNames(3)="SWEET EMOTION"
ModeNamesShort(3)="Sweet Emotion"
ModeNames(4)="DUDE LOOKS LIKE A LADY"
ModeNamesShort(4)="Dude"
ModeNames(5)="BACK IN THE SADDLE"
ModeNamesShort(5)="Back In The Saddle"
ModeNames(6)="RATS IN THE CELLAR"
ModeNamesShort(6)="Rats"
ModeNames(7)="TOYS IN THE ATTIC"
ModeNamesShort(7)="Toys In The Attic"
ModeNames(8)="LOVE IN AN ELEVATOR"
ModeNamesShort(8)="Love"

dim bFlash1Enabled
dim bFlash2Enabled
dim bFlash3Enabled
dim bFlash4Enabled
Dim bFlashFastEnabled

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()

    Dim i
    Randomize

	Redim LightsSaveColor(aLights.count, 4)
	Redim RampLightsSaveColor(aRampLights.count, 4)
	Redim saveColor(saveLights.count, 4)

	baLightsSaved=False
	baRampLightsSaved = False
	baSaveLightsSaved = True
	bTableReady=False
	bUseUltraDMD=False
	bUsePUPDMD=False
	bPupStarted=False

	If DMDMode = 1 then 
		bUseUltraDMD= False
		bUseFlexDMD = True
		set PuPlayer = New PinupNULL
	elseIf DMDMode = 2 Then
		bUsePUPDMD = True
	Else 
		set PuPlayer = New PinupNULL
	End If 

	if bUseFlexDMD Then
		FlexDMDTimer.interval = -1
		FlexDMDTimer.enabled = True
		LoadEM   
	end if

	If b2son then 
		Controller.B2SSetData 1,1
		Controller.B2SSetData 2,1
		Controller.B2SSetData 3,1
		Controller.B2SSetData 4,1
		Controller.B2SSetData 5,1
		Controller.B2SSetData 6,1
		Controller.B2SSetData 7,1
		Controller.B2SSetData 8,1
	End If 

    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 45 ' Plunger Power
    Const IMTime = 1.1        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFX("KickerKick", DOFContactors), SoundFX("Plunger_Release_No_Ball", DOFContactors)
        .CreateEvents "plungerIM"
    End With


    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    'load saved values, highscore, names, jackpot
    Loadhs

	'Set FSO = CreateObject("Scripting.FileSystemObject")

	'Set objFile = FSO.CreateTextFile("E:\aerosmith.txt",True)
    ' initalise the DMD display
    DMD_Init

    'Init main variables
	TableState_Init(0)
	TableState_Init(1)
	TableState_Init(2)
	TableState_Init(3)
	TableState_Init(4)

	for i = 0 to StackSize
		StackState_Init(i)
	Next

    ' freeplay or coins
    bFreePlay = False ' coins yes or no?

    ' initialse any other flags
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
	BallSaverActiveBuffer = 0
    bBallSaverReady = False
    bMultiBallMode = False
    bGameInPlay = False
	bCreatedBall = False
    bAutoPlunger = False
	bAutoPlunged = False
    bMusicOn = True
    BallsOnPlayfield = 0
	RealBallsInLock = 0
    CurrentPlayer = 0  ' needed to set early so Ultradmd doesnt crash
    PlayersPlayingGame = 0
    LastSwitchHit = ""
	SpellAerosmith = 0
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bBonusHeld = False
    bJustStarted = True
    bInstantInfo = False

	DOF 142, DOFOff

	If bUsePUPDMD Then 
		PuPInit
		bPupStarted=True
	Else
		pupDMDUpdate.Enabled = False
	End if

	If bUseUltraDMD Then  ' Show an Intro Video on the DMD
		PlayDMDScene "Video-0x0098.wmv", "","", 9000
		vpmtimer.addtimer 4500, "bTableReady = True '"
	Else
		bTableReady = True
	End If

If VR_Room Then
		'First Hide a lot of stuff that was on the original table
		DotMatrix.visible = False
		pscrew027.visible = False 
		Light001.visible = False 
		Light001.ShowBulbMesh = False
		LeftWallDecal.SideVisible = False 
		RightWallDecal.SideVisible = False 
		Ramp17.visible = False
		FlasherFlash5_5.visible = False
		FlasherFlash5.visible = False
		FlasherBase5.visible = False
		SmartButton.visible = False
		DesktopApronButton.visible = False
		RightRail.visible = False
		LeftRail.visible = False
		Wall2Hide.visible = False
		'Right card is floating. Set to left card
		pcardright.Z = pCardLeft.Z
		'Set the Decal, metals and flippers by the type of cabinet layout
		Dim objVRmetal
			select Case VRCabinet
				Case 0 'Pro
					For Each objVRmetal in VRMetalColors
						objVRmetal.DisableLighting = False
						objVRmetal.Color=RGB(0,5,66)
					Next
					Pincab_FirePlate_Image.Color=RGB(64,107,255)
					PinCab_Right_Rail_Blade.visible = False
					PinCab_Right_Rail_Screw_1.visible = False
					PinCab_Right_Rail_Screw_2.visible = False
					PinCab_Right_Rail_Screw_3.visible = False
					PinCab_Left_Rail_Blade.visible = False
					PinCab_Left_Rail_Screw_1.visible = False
					PinCab_Left_Rail_Screw_2.visible = False
					PinCab_Left_Rail_Screw_3.visible = False
					PinCab_Flipper_Button_Left.material = "Plastic Black"
					PinCab_Flipper_Button_Right.material = "Plastic Black"
					PinCab_Button_Rings.material = "Plastic Black"
					PinCab_Button_Rings.Size_X = 3 
					PinCab_Flipper_Button_Left.x = PinCab_Flipper_Button_Left.x +4
					PinCab_Flipper_Button_Right.x = PinCab_Flipper_Button_Right.x -4
					PinCab_Button_Rings.x = PinCab_Button_Rings.x -22
					Pincab_Backglass.image = "Translite"
					PinCab_Backbox.image = "VR_Pincab_BackBox_Pro"
					PinCab_Cabinet.image = "VR_Pincab_Cabinet_Pro"
					VRFlyerPrim.image = "VRFlyer_Pro"
				Case 1 'Premium
					For Each objVRmetal in VRMetalColors
						objVRmetal.DisableLighting = False
						objVRmetal.Color=RGB(1,1,1)
					Next
					Pincab_FirePlate_Image.Color=RGB(1,1,1)
					PinCab_Right_Rail_Blade.visible = False
					PinCab_Right_Rail_Screw_1.visible = False
					PinCab_Right_Rail_Screw_2.visible = False
					PinCab_Right_Rail_Screw_3.visible = False
					PinCab_Left_Rail_Blade.visible = False
					PinCab_Left_Rail_Screw_1.visible = False
					PinCab_Left_Rail_Screw_2.visible = False
					PinCab_Left_Rail_Screw_3.visible = False
					PinCab_Button_Rings.material = "Plastic White"
					PinCab_Button_Rings.Size_X = 3 
					PinCab_Flipper_Button_Left.x = PinCab_Flipper_Button_Left.x +4
					PinCab_Flipper_Button_Right.x = PinCab_Flipper_Button_Right.x -4
					PinCab_Button_Rings.x = PinCab_Button_Rings.x -22
					PinCab_Flipper_Button_Left.material = "Plastic White"
					PinCab_Flipper_Button_Right.material = "Plastic White"
					Pincab_Backglass.image = "VR_Pincab_backImage_LE"
					PinCab_Backbox.image = "VR_Pincab_BackBox_LE"
					PinCab_Cabinet.image = "VR_Pincab_Cabinet_LE"
					Pincab_Backglass.image = "VR_Pincab_backImage_LE"
					PinCab_Backbox.image = "VR_Pincab_BackBox_LE"
					PinCab_Cabinet.image = "VR_Pincab_Cabinet_LE"
					Pincab_Backglass.image = "VR_Pincab_backImage_Premium"
					PinCab_Backbox.image = "VR_Pincab_BackBox_Premium"
					PinCab_Cabinet.image = "VR_Pincab_Cabinet_Premium"
					VRFlyerPrim.image = "VRFlyer_Premium"
				Case 2 'LE
					For Each objVRmetal in VRMetalColors
						objVRmetal.DisableLighting = False
						objVRmetal.Color=RGB(80,45,91)
					Next
					Pincab_FirePlate_Image.Color=RGB(255,92,64)
					PinCab_Right_Rail_Blade.visible = True
					PinCab_Right_Rail_Screw_1.visible = True
					PinCab_Right_Rail_Screw_2.visible = True
					PinCab_Right_Rail_Screw_3.visible = True
					PinCab_Left_Rail_Blade.visible = True
					PinCab_Left_Rail_Screw_1.visible = True
					PinCab_Left_Rail_Screw_2.visible = True
					PinCab_Left_Rail_Screw_3.visible = True
					PinCab_Flipper_Button_Left.material = "Plastic Black"
					PinCab_Flipper_Button_Right.material = "Plastic Black"
					PinCab_Button_Rings.material = "Plastic Black"
					Pincab_Backglass.image = "VR_Pincab_backImage_LE"
					PinCab_Backbox.image = "VR_Pincab_BackBox_LE"
					PinCab_Cabinet.image = "VR_Pincab_Cabinet_LE"
					VRFlyerPrim.image = "VRFlyer_LE"
			end Select
			'Set DMD. 
			Select Case DMDMode
				Case 0 'all off (not playable in VR
					PinCab_DMD.visible = False
				Case 1  'Flex
					PinCab_DMD.visible = True
				Case 2  'PUp
					PinCab_DMD.visible = True
			End Select
	Else
		Dim objVR
		For Each objVR in VRStuff
			objVR.visible = False
		Next
	End If


    EndOfGame()
End Sub

Sub Game_Init() 'called at the start of a new game
	Dim i,a
	bExtraBallWonThisBall = False

	If bUsePUPDMD then pUpdateScores

	for each a in  flasherlights
		SetLightColor a, "white", -1
	Next
	SetLightColor BumperLight001, "white", 0:SetLightColor BumperLight002, "white", 0:SetLightColor BumperLight003, "white", 0

	MusicDir="Audioevents"

	bAutoPlunger=False 
	bCreatedBall = False

	BallSearchCnt=0
	modesStarted = 0
	modesStarted = 0
	modesCompleted = 0
	SaveMode = -1

	bAutoPlunger=False
	SongNum = 1
	baLightsSaved = False
	baRampLightsSaved = False
	baSaveLightsSaved = False
	ballInLock=False
	ballInScoop=False
	RealBallsInLock=0
	bAddedABall = False

	' Reset game progress
	For i = 0 to 8
		ModeProgress(i) = 0
		ModePercent(i) = -1  ' 0 means you atleast attempted the mode
		Mode2Percent(i) = -1
		Mode2Progress(i) = 0
		Mode2Value(i) = 0
		Mode2Total(i) = 0
		ModeOrder(i) = -1
	Next

	modeRampColor = "white"
	bDebounce = False
	TurnOffPlayfieldLights()
	setCIULight(False)   
	setModeSelectLight(False)
	setExtraBallLight(False)
	setMysteryLight(False)
	setDudeLight(False)

	bMedleyTourCount=0
	bMedleyTourDone = False
	bMedleyTourReady = False

	For i = 0 to 8
		bMedleyTourProgress(i)=False
	Next

	bFinalTourDone=False
	bFinalTourReady=False
	FinalTourCount=0

	SwitchHitCount = 0
	ModecountdownTimer.UserValue = 0
	ModecountdownTimer.Enabled = False
	SmartButtonCount = 1 ' One Freebie
	BumperMultiplier = 1

	ToyBoxMultiBallCount = 0
	ToyBoxMBJackpot = 0
	ToyBoxMBJackpotBase = 0   ' More per jackpot depending on how many balls locked
	ToyBoxMBJackpotTotal = 0
	ToyBoxMBJackpotHits = 0
	ToyBoxMBAttempts = 0
	ElevMultiBallCount = 0 
	ElevMultiBallAttempts = 0

	For each a in aLockLights
		SetLightColor a, "green",0
		a.uservalue = 1   ' only 1 hit to make it solid
	Next

	SetLightColor F147,"white", 2  ' Toys Light
	SetLightColor I110,"white", 2  ' Elevator Lights
	SetLightColor I65, "white", 2

'For Testing
	'SetLightColor I100,"green", 2 
	'SetLightColor I75, "green", 1 
	'SetLightColor I76, "green", 1  
	'SetLightColor I77, "green", 2 
	'realballsinlock=2

	ElevMBJackpot = 0
	ElevMBJackpotTotal = 0
	bElevMultiBall = False
	bShotMultiplierSelect = False
    MultiplierShot = 1
    Multiplier3x = 1
	bToyBoxBonus3x = False
	tmrToyBoxMBBonus.Enabled = False

	bPauseTimer = False


	EnablePlayerSelect() 

	PlayerMode = -1
	bSecondMode = False
	bBonusMode = False
	PlayerMode2 = -1
	bWizardMode = False

	Coins=0
	HiddenJesters=25
	SpinHits=25
	SpinValue=200
	SpinScore=0
	SpinScoreCurrent=0
	PopScore=0
	PopValue=0

	tmrShotMultiplierStrobe.Enabled = False

	' Reset all the player states
	PlayerState(0).Reset		
	PlayerState(1).Reset
	PlayerState(2).Reset
	PlayerState(3).Reset
	PlayerState(0).Save
	PlayerState(1).Save
	PlayerState(2).Save
	PlayerState(3).Save	

	DMDClearQueue		
	pDMDStartGame

	bFlash1Enabled = False
	bFlash2Enabled = False
	bFlash3Enabled = False
	bFlash4Enabled = False
	bFlashFastEnabled=False
End Sub


Sub EnablePlayerSelect()
	D "sub EnablePlayerSelect"
	bPlayerModeSelect = True
	bRndModeShot = Int(7*Rnd())  ' Use this to lock in a mode shot for random shots
	pBGPlayerSelect
End sub


Sub pBGPlayerSelect 
	D "sub pBGPlayerSelect"
	If bUsePUPDMD then 

		If modecountdowntimer.enabled Then
			pDMDEvent(P+1)
		Else
			pDMDEvent(P)
		End If
		PuPlayer.LabelSet pBackglass,"ModeProgress", " ",0,"{'mt':2,'color':111111,'width':12, 'height':23,'yalign':0,'ypos':4.0,'xpos':0,'pagenum':1}"
		PuPlayer.LabelSet pBackglass,"Time"," ",0,""
		PuPlayer.LabelSet pBackglass,"Aerosmith"," ",0,""
		PuPlayer.LabelShowPage pBackglass,1,0,""

		vpmtimer.addtimer 1500, "pBGPlayerInstruct '"
	End If
End Sub


Sub pBGPlayerInstruct
	If bUsePUPDMD then
		PuPlayer.LabelSet pBackglass,"MessageT","USE FLIPPERS TO SELECT SONG", 1,"{'mt':1,'at':1,'fq':250}"
	End If
End Sub

Sub pClearMessageT
	If bUsePUPDMD then 
		PuPlayer.LabelSet pBackglass,"MessageT"," ", 1,"{'mt':1,'at':1,'fq':250}"
	End If
End Sub

Sub pBGGamePlay
	'D "pBGGamePlay"
	If bUsePUPDMD then
		PuPlayer.LabelShowPage pBackglass,1,0,""
		PuPlayer.LabelSet pBackglass, "Aerosmith", "PuPOverlays\\aerosmith-"&"&SpellAerosmith"&".png",1,"{'mt':2,'color':111111,'width':31, 'height':18,'yalign':0,'ypos':0.0,'xpos':50.4}"

		If modecountdowntimer.enabled Then
			pDMDEvent(P+1)
		Else
			pDMDEvent(P)
		End If
		pClearMessageT()
	End If
End Sub


Sub pBgShowBonus(Message1, Message2)
	If bUsePUPDMD then
		If Message1 ="" and Message2 ="" Then
			PuPlayer.LabelSet pBonusScreen,"Message1", "" ,0,""
			PuPlayer.LabelSet pBonusScreen,"Message2", "" ,0,""
		Else
			PuPlayer.LabelSet pBonusScreen,"Message1", Message1 ,1,""
			PuPlayer.LabelSet pBonusScreen,"Message2", Message2 ,1,""
		End If
	End If
End Sub


' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

' lost a ball ;-( check to see how many balls are on the playfield.
' If only one then decrement the remaining count AND test for End of game
' If more than 1 ball (multi-ball) then kill of the ball but don't create
' a new one
'
Sub Drain_Hit()

	Dim tmpBallSaver
	Dim queueTime
	D ">>>>>>>>Drain_Hit<<<<< Was BallsOnPlayfield: " & BallsOnPlayfield & " RealBallsInLock: " & RealBallsInLock & _
		" BSActive:" & bBallSaverActive & "BallSaverActiveBuffer:" & BallSaverActiveBuffer & _
        " CreateMultiball:" & CreateMultiballTimer.Enabled

	tmpBallSaver=BallSaverActiveBuffer <> 0
	If BallSaverActiveBuffer>0 then BallSaverActiveBuffer = BallSaverActiveBuffer-1


	If (I14.state = 2) and bBallSaverActive=False and ExtraBallsAwards(CurrentPlayer) = 0 then 
		D "ERROR Ball SAVE"
	End If 

    ' Destroy the ball
    Drain.DestroyBall
    BallsOnPlayfield = BallsOnPlayfield - 1

    ' pretend to knock the ball into the ball storage mech
    RandomSoundDrain Drain
    'If Tilted the end Ball modes
    If Tilted Then
        StopEndOfBallModes
    End If
    ' If there is a game in progress AND it is not Tilted
    If(bGameInPlay = True)AND(Tilted = False)Then
        ' is the ball saver active (or did the switch catch us before the timer ran out)
        If bBallSaverActive or tmpBallSaver Then
            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in Case the multiballs are being ejected
			D "Ball Save multiball"
            AddMultiball 1
            ' we kick the ball with the autoplunger
            bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
			DoBallSave
        Else
			D "Drain BOP:" & BallsOnPlayfield & " Mode:" & bMultiBallMode _
				& " Real: " & RealBallsInLock & " toyboxTimer" & tmrToyBoxMBBonus.Enabled

			If ToyBoxMB_End(True)	then D "Drain EXITSUB": exit Sub ' See If we need to throw more balls back into play

            ' cancel any multiball If on last ball (ie. lost all other balls)
            If((BallsOnPlayfield-RealBallsInLock = 1) and not bElevMultiBall and _
				Not tmrMedleyTour.Enabled and Not tmrFinalTour.Enabled) or _
               (BallsOnPlayfield=1 and tmrMedleyTour.Enabled) or _
               (BallsOnPlayfield=1 and tmrFinalTour.Enabled) Then
                If(bMultiBallMode = True)then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ChangeGi "white"
                    ' turn off any multiball specific lights                    
                    ToyBoxMB_End(False)
					EndWizardMode(1)
					EndFinalTour()
                End If
            End If

			If(BallsOnPlayfield-RealBallsInLock <= 1) Then	' Stop Elev Multiball
				If (bElevMultiBall) then 					' Reset Elevator Stuff
					D "Ending ElevMB BOP:" & BallsOnPlayfield & " Real:" & RealBallsInLock
					bMultiBallMode = False					' Release last locked ball. Stop Multiball
					StopElevMB
				end if
			End If
			
            ' was that the last ball on the playfield and there isnt another coming out real soon?
            If(BallsOnPlayfield = 0 and NOT CreateMultiballTimer.Enabled and NOT tmrToyBoxMBBonus.enabled)Then
				D "====BallsOnPlayfield = 0 Condition====="
                ' End Modes and timers
				ToyBoxMB_End(False)
				EndWizardMode(0)
				StopPlayerMode2
                StopEndOfBallModes
				If Mode2Percent(3)>0 then Mode2Percent(3)=100: D "SuperScoring is Ended" ' Super Scoring Ends when ball ends
                ChangeGi "white"

				queueTime = getQueueTime					' See If we need to let animations finish playing 
				If queueTime = 0 then queueTime = 2500

                ' handle the end of ball (count bonus, change player, high score entry etc..)				
				If PlayerMode <> -1 Then			' Pause the countdown timer 
					ModeCountdownTimer.Enabled = False
				End If
                vpmtimer.addtimer queueTime, "EndOfBall '"
            End If
        End If
    End If
End Sub

Sub LightSeqFinalTour_PlayDone()
	if (LightSeqFinalTour.UserValue=0) Then
		LightSeqFinalTour.UserValue=1
		LightSeqFinalTour.TimerInterval = 40
		LightSeqFinalTour.Play SeqBlinking, 0, 15, 40		' 20 time, 40ms on/off
	End If
End Sub

Sub LightSeqFinalTourFinale_PlayDone()
	if (LightSeqFinalTourFinale.UserValue=0) Then
		LightSeqFinalTourFinale.UserValue = 1
		LightSeqFinalTourFinale.Play SeqDownOn, 30, 1
	Elseif (LightSeqFinalTourFinale.UserValue=1) Then
		LightSeqFinalTourFinale.UserValue = 2
		LightSeqFinalTourFinale.Play SeqUpOn, 30, 1
	Elseif (LightSeqFinalTourFinale.UserValue=2) Then
		LightSeqFinalTourFinale.UserValue = 3
		LightSeqFinalTourFinale.Play SeqDownOn, 30, 1
	Elseif (LightSeqFinalTourFinale.UserValue=3) Then
		LightSeqFinalTourFinale.UserValue = 4
		LightSeqFinalTourFinale.Play SeqRandom, 30, 1, 3000
	End If
End Sub


Sub StopEndOfBallModes
	D "sub StopEndofBallModes()"
	PauseBigScore=True
	puPlayer.LabelSet pBackglass,"BigScore"," ",0,""
	SceneClearPlayMessage
End Sub


Sub StopElevMB()
	Dim a
	I2 "StopElevMB : BallsonPF:" & BallsOnPlayfield & " RealLock:" & RealBallsInLock
	D "StopElevMB : BallsonPF:" & BallsOnPlayfield & " RealLock:" & RealBallsInLock
	ShowPlayerModeComplete(1)	' Show Elevator MB Total
	bElevMultiBall=False
	SetLightColor F147, "white",2 'toybox
	If I100.state = 2 then
		ToyBox_OpenLid()
	Else	
		ToyBox_CloseLid()
	End If
	for each a in aRampLights
		SSetLightColor kStack_Pri1, a, "pink", 0
	Next
	StackState(kStack_Pri1).Disable 
	if playermode <> -1 Then 
		PlaySong "Song-" & SaveMode & ".mp3"
	Else
		PlaySong "clear.mp3"
		If BallsOnPlayfield < 2 then setModeSelectLight(True)  ' Have to drained to 1 ball before starting next mode
	End If

	SetLightColor I110,"white", 2   ' Elevator Lights go back on
	SetLightColor I65, "white", 2
	'If ModeCountdownTimer.Enabled = False Then ' turn off the Petals and the TimerBubble
	'	PuPlayer.LabelSet pBackglass,"Time", " ",0,""
	'	pDMDEvent(kDMD_PlayerMode+(CurrentPlayer)+(PlayersPlayingGame*(PlayersPlayingGame-1)))
	'	ShowPlayerModeComplete(-1)			' Show Mode total
	'End If
	SetModeLights()
	CheckWizardModesReady
End Sub

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded
Dim tmpBonusTotal

Sub tmrEndOfBallBonus_Timer()
	dim i
	Dim XMultiplier
	dim FastBonus
	i=0
	FastBonus = 0
	tmrEndOfBallBonus.Interval = 1000
	tmrEndOfBallBonus.Enabled = False

	If LFPress = 1 or RFPress = 1 then 		' Holding flippers speed up bonus 
		FastBonus = 500
		tmrEndOfBallBonus.Interval = 300
	End If

' spinners
' Targets
' bumpers
' loops
' ramps
' lanes
' 1x, 2x ,3x
' total bonus
 

	'D "Start " & tmrEndOfBallBonus.UserValue
	Select Case INT(tmrEndOfBallBonus.UserValue)
		Case 0	'
			tmpBonusTotal = 0
			pBgShowBonus " "," "
			tmrEndOfBallBonus.UserValue = 1
			If PlayerMode <> -1 Then
				PuPlayer.LabelSet pBackglass,"Message1",ModeNames(PlayerMode),1,""
				PuPlayer.LabelSet pBackglass,"Message2",FormatScore(ModePoints),1,""
				PuPlayer.LabelSet pBackglass,"Message3","TOTAL",1,""
				DMD FormatScore(ModePoints), ModeNamesShort(PlayerMode), "", eBlink, eNone, eNone, 2000, False, ""
			Else
				tmrEndOfBallBonus.Interval = 100
			End If
		Case 1	' 
			If PlayerMode <> -1 Then
				PuPlayer.LabelSet pBackglass,"Message1"," ",0,""
				PuPlayer.LabelSet pBackglass,"Message2"," ",0,""
				PuPlayer.LabelSet pBackglass,"Message3"," ",0,""
				DMD "", "", "", eBlink, eNone, eNone, 2000, False, ""
			End If
			If SpinnerBonus = 0 Then
				tmrEndOfBallBonus.Interval = 100
			Else
				AddScore SpinnerBonus*2000
				tmpBonusTotal=tmpBonusTotal+(SpinnerBonus*2000)
				DMD "SPINNER X " & SpinnerBonus, FormatScore(SpinnerBonus*2000), "", eBlink, eNone, eNone, 1000, False, ""
				pBgShowBonus "SPINNER X " & SpinnerBonus, FormatScore(SpinnerBonus*2000)
			End If
			tmrEndOfBallBonus.UserValue = 2
		Case 2
			If TargetBonus = 0 Then
				tmrEndOfBallBonus.Interval = 100
			Else 
				AddScore TargetBonus*2000
				tmpBonusTotal=tmpBonusTotal+(TargetBonus*2000)
				DMD "TARGETS X " & TargetBonus, FormatScore(TargetBonus*2000), "", eBlink, eNone, eNone, 1000, False, ""
				pBgShowBonus "TARGETS X " & TargetBonus, FormatScore(TargetBonus*2000)	
			End If
			tmrEndOfBallBonus.UserValue = 3
		Case 3
			If BumperBonus = 0 Then
				tmrEndOfBallBonus.Interval = 100
			Else
				AddScore BumperBonus*2000
				tmpBonusTotal=tmpBonusTotal+(BumperBonus*2000)
				DMD "BUMPERS X " & BumperBonus, FormatScore(BumperBonus*2000), "", eBlink, eNone, eNone, 1000, False, ""
				pBgShowBonus "BUMPERS X " & BumperBonus, FormatScore(BumperBonus*2000)	
			End If
			tmrEndOfBallBonus.UserValue = 4
		Case 4 
			If LoopBonus = 0 Then
				tmrEndOfBallBonus.Interval = 100
			Else
				AddScore LoopBonus*2000
				tmpBonusTotal=tmpBonusTotal+(LoopBonus*2000)
				DMD "LOOPS X " & LoopBonus, FormatScore(LoopBonus*2000), "", eBlink, eNone, eNone, 1000, False, ""
				pBgShowBonus "LOOPS X " & LoopBonus, FormatScore(LoopBonus*2000)	
			End If
			tmrEndOfBallBonus.UserValue = 5
		Case 5
			If RampBonus = 0 Then
				tmrEndOfBallBonus.Interval = 100
			Else
				AddScore RampBonus*5000
				tmpBonusTotal=tmpBonusTotal+(RampBonus*5000)
				DMD "RAMPS X " & LoopBonus, FormatScore(RampBonus*5000), "", eBlink, eNone, eNone, 1000, False, ""
				pBgShowBonus "RAMPS X " & RampBonus, FormatScore(RampBonus*5000)	
			End If
			tmrEndOfBallBonus.UserValue = 6
		Case 6
			If LaneBonus = 0 Then
				tmrEndOfBallBonus.Interval = 100
			Else
				AddScore LaneBonus*5000
				tmpBonusTotal=tmpBonusTotal+LaneBonus*5000
				DMD "LANES X " & LaneBonus, FormatScore(LaneBonus*5000), "", eBlink, eNone, eNone, 1000, False, ""
				pBgShowBonus "LANES X " & LaneBonus, FormatScore(LaneBonus*5000)	
			End If
			tmrEndOfBallBonus.UserValue = 7.1
		Case 7
			XMultiplier = round((tmrEndOfBallBonus.UserValue - 7) * 10)
			DMD "TOTAL BONUS X " & XMultiplier, FormatScore(tmpBonusTotal * XMultiplier), "", eBlink, eNone, eNone, 1000, False, ""
			pBgShowBonus "TOTAL BONUS X " & XMultiplier, FormatScore(tmpBonusTotal * XMultiplier)	
			If BonusMultiplier > XMultiplier Then
				tmrEndOfBallBonus.UserValue=tmrEndOfBallBonus.UserValue+.1
				tmrEndOfBallBonus.Interval = 1000 - (XMultiplier*75)
				If tmrEndOfBallBonus.Interval < 300 then tmrEndOfBallBonus.Interval=300
			Else
				tmrEndOfBallBonus.UserValue = 8
			End If
		Case 8	' Show Total Bonus
			tmrEndOfBallBonus.UserValue = 9
			tmrEndOfBallBonus.Interval = 1000 - FastBonus
			AddScore (tmpBonusTotal * BonusMultiplier)
			DMD "TOTAL BONUS", FormatScore(tmpBonusTotal * BonusMultiplier), "", eBlink, eNone, eNone, 1000, False,  ""
			pBgShowBonus "TOTAL BONUS", FormatScore(tmpBonusTotal * BonusMultiplier)
			SetBonusMultiplier 1		' Push it back down to 1
		Case 9:
			If bUsePUPDMD then
				PuPlayer.LabelShowPage pBackglass,1,0,""

				pDMDEvent(P)
				pUpdateScores()
			End If
			tmrEndOfBallBonus.UserValue = 10
			tmrEndOfBallBonus.Interval = 2000 - FastBonus			
		Case 10
			pBgShowBonus "", ""
			pDMDEvent(kDMD_BonusClear)
			playclear pBonusScreen

			pBGGamePlay			' Change the backglass to the play mode
			pDMDSetPage(pScores)
			WaitPlayerMode

			vpmtimer.addtimer 400, "EndOfBall2 '"
			pDMDEvent(kDMD_BonusClear)
			Exit Sub
	End Select 
	tmrEndOfBallBonus.Enabled = True
End Sub

Sub WaitPlayerMode()	' chooses the correct wait video 
	D "WaitPlayerMode playermode:" & PlayerMode
	'Dim VideoName 
	If PlayerMode <> -1 then
		If bUsePupDMD then 
'msgbox  "Try to remove the paused screen1"
			PuPlayer.playstop pOverVid
'msgbox  "Try to remove the paused screen2"
			PuPlayer.SetBackGround pOverVid, 0
'msgbox  "Try to remove the paused screen3"
			PuPlayer.SetLoop pOverVid, 0
'msgbox  "Try to remove the paused screen4"
			PuPlayer.playstop pOverVid
		End If 
'msgbox  "Try to remove the paused screen5"
		Select Case PlayerMode
			Case 0
				pDMDEvent(kDMD_last+10)
			Case 1
				pDMDEvent(KDMD_walk+10)
			Case 2
				pDMDEvent(kDMD_same+10)
			Case 3
				pDMDEvent(kDMD_sweet+10)
			Case 4
				pDMDEvent(kDMD_dude+10)
			Case 5
				pDMDEvent(kDMD_back+10)
			Case 6
				pDMDEvent(kDMD_rats+10)
		End Select
		'playmedia VideoName, "PupVideos", pOverVid, "", -1, "", 1, 1
'		If bUsePupDMD then PuPlayer.SetLoop pOverVid, 1
	End If 
End sub 


Sub EndOfBall()
    Dim AwardPoints, TotalBonus
	D "EndOfBall()"

    AwardPoints = 0
    TotalBonus = 0
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False
	If PlayerMode <> -1 Then			' Pause the countdown timer 
		ModeCountdownTimer.Enabled = False
		PuPlayer.LabelSet pBackglass,"Time", " ",0,""
	End If

	SceneClearMessage()	
	if NOT bUsePUPDMD then
		EndMusic
	End If

    ' only process any of this If the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

    If(Tilted = False) Then
        ' Count the bonus. This table uses several bonus
        DMDflush
		QueueFlush()

		If bUsePupDMD then PuPlayer.LabelShowPage pBonusScreen,1,0,"":PuPlayer.LabelShowPage pBackglass, 2,0,""
		
		pDMDEvent(kDMD_BonusBG)
		playmedia "Video-0x0000.mp4", "PupVideos", pBonusScreen, "", -1, "", 1, 1

        ' add a bit of a delay to allow for the bonus points to be shown & added up
		tmrEndOfBallBonus.Interval = 200
		tmrEndOfBallBonus.UserValue = 0		' Timer will start EndOfBall2 when it is done
		tmrEndOfBallBonus.Enabled = true
    Else
        vpmtimer.addtimer 100, "EndOfBall2 '"
    End If
End Sub


' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see If there are any extra balls for this player.
' If not, then check to see If this was the last ball (of the currentplayer)
'
Sub EndOfBall2()
	dim i
	dim thisMode
	'D "sub EndOfBall2 Playermode=" & playermode
    ' If were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful If we are changing player LOL
    Tilted = False
    Tilt = 0
	PlayMultiplier = 1
	mMagnet.MotorOn = False
    DisableTable False 'enable again bumpers and slingshots

	If PlayerMode = -1 and NOT bFinalTourReady Then	
		EnablePlayerSelect()
	End If


    ' has the player won an extra-ball ? (might be multiple outstanding)
    If(ExtraBallsAwards(CurrentPlayer) <> 0) and bBallInPlungerLane=False Then	' Save Extra ball for later If there is a ball in the plunger lane
        D "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

        ' If no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
            I14.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        DMD "EXTRA BALL", "", "", eNone, eBlink, eNone, 1000, True, "vo_extraball"

'		If INT(RND * 2) = 0 then 
'			pDMDEvent(kDMD_ExtraBall)
'		Else
'			pDMDEvent(kDMD_ShootAgain)
'		End If 

        ' Create a new ball in the shooters lane
		D "created for ExtraBallsAwards.." & ExtraBallsAwards(CurrentPlayer)
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1
		ToyBox_CloseLid()

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0) Then				' GAME OVER
            D "No More Balls, High Score Entry"
			If bUsePupDMD then 
				PuPlayer.PlayStop pOverVid						' Stop overlay If there is one
				PuPlayer.SetLoop pOverVid, 0
			End If
			playclear pAudio
			StopPlayerMode
			StopPlayerMode2

			' Turn off DOF so we dont accidently leave it on
			PlaySoundAt SoundFXDOF("Flipper_Left_Down_3", 101, DOFOff, DOFFlippers), LeftFlipper
			LeftFlipper.RotateToStart
			PlaySoundAt SoundFXDOF("Flipper_Right_Down_3", 102, DOFOff, DOFFlippers), RightFlipper
			RightFlipper.RotateToStart

            ' Submit the currentplayers score to the High Score system
            CheckHighScore()
			' you may wish to play some music at this point
        Else
            ' not the last ball (for that player)
            ' If multiple players are playing then move onto the next one
            EndOfBallComplete()
        End If
    End If
End Sub


' This function is called when the end of bonus display
' (or high score entry finished) AND it either end the game or
' move onto the next player (or the next ball of the same player)
'all of the same player)
'
Sub EndOfBallComplete()
    Dim NextPlayer
	dim Match
	Dim NameStr:NameStr=""

    D "EndOfBall - Complete"
    ' are there multiple players playing this game ?
    If(PlayersPlayingGame > 1)Then
        ' then move to the next player
        NextPlayer = CurrentPlayer + 1
        ' are we going from the last player back to the first
        ' (ie say from player 4 back to player 1)
        If(NextPlayer > PlayersPlayingGame-1)Then
            NextPlayer = 0
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

	'D "Sub EndofBallComplete step1:" & "Current Player = " & CurrentPlayer

    ' is it the end of the game ? (all balls been lost for all players)
    If((BallsRemaining(CurrentPlayer) <= 0)AND(BallsRemaining(NextPlayer) <= 0))Then
        ' set the machine into game over mode
		vpmtimer.addtimer getqueueTime+500, "EndOfGame() '"
    Else
        ' set the next player
		PlayerState(CurrentPlayer).bFirstBall = False

		PlayerState(CurrentPlayer).Save 
        CurrentPlayer = NextPlayer
		UpdateNumberPlayers				' Update the Score Sizes
        ' make sure the correct display is up to date
        AddScore 0

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

		D "Sub EndofBallComplete step2 playermode=" & PlayerMode & "Current Player = " & CurrentPlayer & " #players=" & PlayersPlayingGame
		PlayerState(CurrentPlayer).Restore 

		ScorbitBuildGameModes()

        ' AND create a new ball
		D "created for EndofBallComplete() ..."
        CreateNewBall()

        ' play a sound If more than 1 player
        If PlayersPlayingGame > 1 Then
			Select Case CurrentPlayer
				Case 0
					PuPlayer.playlistplayex pCallouts,"audioevents","player1-Sound-0x03E0.wav",100, 1
				Case 1
					PuPlayer.playlistplayex pCallouts,"audioevents","player2-Sound-0x03E1.wav",100, 1
				Case 2
					PuPlayer.playlistplayex pCallouts,"audioevents","player3-Sound-0x03E2.wav",100, 1
				Case 3
					PuPlayer.playlistplayex pCallouts,"audioevents","player4-Sound-0x03E3.wav",100, 1
			End Select
			if ScorbitActive then 
				if Scorbit.bSessionActive then
					NameStr=Scorbit.GetName(CurrentPlayer+1)
					if NameStr<>"" then 
						DMD "", NameStr, "", eNone, eNone, eNone, 800, True, ""
					Else 
						DMD "", "PLAYER " &CurrentPlayer+1, "", eNone, eNone, eNone, 800, True, ""
					End if 
				End if 
			Else 
				DMD "", "PLAYER " &CurrentPlayer+1, "", eNone, eNone, eNone, 800, True, ""
			End If
        End If
    End If
End Sub


' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    D "EndOfGame"	
	StopPlayerMode
	StopPlayerMode2
    bGameInPlay = False	
	tmrBallSearch.Enabled = False
    bJustStarted = False
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0
	LFPress=0
	RFPress=0

	Scorbit.StopSession Score(0), Score(1), Score(2), Score(3), PlayersPlayingGame

    ' set any lights for the attract mode
    GiOff
	bFlash1Enabled = True
	bFlash2Enabled = True
	bFlash3Enabled = True
	bFlash4Enabled = True
	pDMDGameOver
End Sub


' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table

Sub AddMultiball(nballs)
    mBalls2Eject = mBalls2Eject + nballs
	D "Sub AddMultiball :" & nBalls
	CreateMultiballTimer.Interval = 2500
    CreateMultiballTimer.Enabled = True
End Sub


' Eject the ball after the delay, AddMultiballDelay
Sub CreateMultiballTimer_Timer()
    ' wait If there is a ball in the plunger lane
    If bBallInPlungerLane Then
        Exit Sub
    Else
        If BallsOnPlayfield < MaxMultiballs Then
			D "created for createMBtimer " & mBalls2Eject-1
            CreateNewBall()
            mBalls2Eject = mBalls2Eject -1
            If mBalls2Eject = 0 Then 'If there are no more balls to eject then stop the timer
                Me.Enabled = False
            End If
        Else 'the max number of multiballs is reached, so stop the timer
            mBalls2Eject = 0
            Me.Enabled = False
        End If
    End If
End Sub

Sub AddToyBoxMultiball(nballs)
    mToyBoxBalls2Eject = mToyBoxBalls2Eject + nballs
	D "Sub AddToyBoxMultiball :" & nBalls
	CreateToyBoxMultiballTimer.Interval = 1000
    CreateToyBoxMultiballTimer.Enabled = True
End Sub


' Eject the ball after the delay, AddMultiballDelay
Sub CreateToyBoxMultiballTimer_Timer()
    If BallsOnPlayfield < MaxMultiballs Then
        CreateNewToyBoxBall()
        mToyBoxBalls2Eject = mToyBoxBalls2Eject -1
        If mToyBoxBalls2Eject = 0 Then 'If there are no more balls to eject then stop the timer
            Me.Enabled = False
			vpmtimer.addtimer 1000, "ToyBox_CloseLid '"
        End If
    Else 'the max number of multiballs is reached, so stop the timer
        mToyBoxBalls2Eject = 0
        Me.Enabled = False
		vpmtimer.addtimer 1000, "ToyBox_CloseLid '"
    End If

End Sub


Sub DoBallSave()
	D "sub DoBallSave"
	If bToyBoxMultiball or bElevMultiBall or tmrMedleyTour.Enabled or tmrFinalTour.Enabled then exit sub 	

	If bUseUltraDMD then 
		PlayDMDScene "vidBallSave.wmv", "", "", 4000
	Else
		pDMDEvent(582)
	End If
	DMD "BALL SAVED", "", "", eNone, eBlinkfast, eNone, 800, True, ""
End Sub 


' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see If a ball saver mechanism is needed and If so fire it up.


Sub swPlungerRest_Hit()
	Dim i
    'D "Sub swPlungerHit: ball in plunger lane: Sel:" & bPlayerModeSelect & " Auto:" & bAutoPlunger & " Created:" & bCreatedBall
    ' some sound according to the ball position
    PlaySoundAt "Rubber_1", swPlungerRest
    bBallInPlungerLane = True
    ' kick the ball in play If the bAutoPlunger flag is on

	If bCreatedBall = False and AutoAI = False and AutoQA=False then	' If we didnt create a ball this must have gone up and back so kick it out automatically  
		bAutoPlunger = True
	End If 
	bCreatedBall = False
   
	If (bPlayerModeSelect) Then
		If PlayerMode = -1 then 
			If AutoAi or AutoQA Then ' force to select a random mode
				i=INT(10*RND())+2
				PlayerMode = (PlayerMode + i)
				If (PlayerMode > 6) Then PlayerMode=PlayerMode-6
				i=7 
				DO
					i=i-1
					PlayerMode = (PlayerMode + 1) 
D "Mode: " & PlayerMode & " %:" & ModePercent(PlayerMode)
					If (PlayerMode > 6) Then PlayerMode=0
					If i=0 then exit do
				Loop While ModePercent(PlayerMode) = 100
				If ModePercent(PlayerMode)=100 Then PlayerMode=-1
			Else
				SelectPlayerMode LeftFlipperKey ' force to select a mode
			End if
			D "Playermode Selected: " & PlayerMode
			if PlayerMode <> -1 Then UpdatePlayerMode()
		End If
		If PlayerState(CurrentPlayer).bFirstBall then 
			Select Case CurrentPlayer
				Case 0
					PuPlayer.playlistplayex pCallouts,"audioevents","player1-Sound-0x03E0.wav",100, 1
				Case 1
					PuPlayer.playlistplayex pCallouts,"audioevents","player2-Sound-0x03E1.wav",100, 1
				Case 2
					PuPlayer.playlistplayex pCallouts,"audioevents","player3-Sound-0x03E2.wav",100, 1
				Case 3
					PuPlayer.playlistplayex pCallouts,"audioevents","player4-Sound-0x03E3.wav",100, 1
			End Select
		End If 
	ElseIf PlayerMode <> -1 Then
		if bUsePUPDMD then
			playclear pAudio  
			D2 "ResumeMedia and Video"
			resumemedia pBackglass
			resumemedia pMusic
		end if
	End If 

    If bAutoPlunger Then
        PlungerIM.AutoFire
        DOF 125, DOFPulse
		DOF 112, DOFPulse
        bAutoPlunger = False
		bAutoPlunged = True
    End If

    ' remember last trigger hit by the ball.
    LastSwitchHit = "swPlungerRest"
End Sub

' The ball is released from the plunger turn off some flags and check for skillshot
Sub swPlungerRest_UnHit()
	'D "swPlungerRest_UnHit"
    bBallInPlungerLane = False
	bPauseTimer=False

	ResetBallSearch

	If (bAutoPlunged = False) then		' Only start the skillshot If it is a manual plunge
		If LeftFlipper.CurrentAngle = LeftFlipper.EndAngle and bWizardMode = False Then	' No skill shot on Wizard Modes 

			bSkillshotsReady(1) = True		' Enable the super skillshot 
			StackState(kStack_Pri3).Enable(-1)
			SuperSkillShotIndex = INT(3 * RND)
			D "Skillshot " & SuperSkillShotIndex
			Select Case SuperSkillShotIndex
				Case 0
					SSetLightColor kStack_Pri3, I35, "white", 2
				Case 1
					SSetLightColor kStack_Pri3, I62, "white", 2
				Case 2
					SSetLightColor kStack_Pri3, I86, "white", 2
			End Select
		End If 
		tmrSkillShot.interval=8000:tmrSkillshot.Enabled = True
	End If
	bAutoPlunged =  False

	If (bFinalTourReady) then			' There are no modes to start
		PlayerMode = -1
	End If 

	' Check Mode
	If (bPlayerModeSelect) Then
		pBGGamePlay						' Change the backglass to the play mode
										' Always start the skillshot
		StartPlayerMode()				' Start the mode and clear bPlayerModeSelect
	elseIf PlayerMode <> -1 then 		' If a mode is selected we should start the timer back Update
		vpmtimer.addtimer cInt(defBallSaverTime/4), "ModeCountdownTimer.Enabled = True '" ' delay the countdown
		' If we are paused resume media
		
		If bUsePupDMD then 
			PuPlayer.PlayStop pOverVid
			PuPlayer.SetLoop pOverVid, 0
		End If 

		RefreshPlayerMode

		playclear pAudio  ' turn off the callout
		'D "ResumeMedia and Video"
		'resumemedia pBackglass
		'resumemedia pMusic
	End If

	' If there is a need for a ball saver, then start off a timer
    ' only start If it is ready, and it is currently not running, else it will reset the time period
	D "Ballsaver Ready:" &  bBallSaverReady & " T:" & BallSaverTime & " Active:" & bBallSaverActive
    If(bBallSaverReady = True)AND(BallSaverTime <> 0)And(bBallSaverActive = False)Then
        EnableBallSaver BallSaverTime
    End If
End Sub


Sub tmrSkillshot_Timer()
	bSkillshotsReady(0) = False		' toybox kicker
	bSkillshotsReady(1) = False		' Hold Left, drop down and hit CIU, Left Orb, Center Ramp
	D "Restore the Lights .. skillshot is over"
	StackState(kStack_Pri3).Disable
	Select Case SuperSkillShotIndex	' Turn it off 
		Case 0
			SetLightColorRestore I35, -1 'lsccop
		Case 1
			SetLightColorRestore I62, -1 'lorbit
		Case 2
			SetLightColorRestore I86, -1 'center ramp
	End Select
	tmrSkillshot.Enabled = False
End Sub


Sub EnableBallSaver(seconds)
    D "Ballsaver started " & seconds
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' start the timer
    BallSaverTimerExpired.Interval = 1000 * seconds
    BallSaverTimerExpired.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
    BallSaverSpeedUpTimer.Enabled = True
    ' If you have a ball saver light you might want to turn it on at this point (or make it flash)
    I14.BlinkInterval = 160
    SetLightColor I14, "yellow", 2
End Sub


' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimerExpired_Timer()
	D2 "Ballsaver ended"
	BallSaverTimerExpired.Enabled = False
	vpmtimer.addtimer 1500, "bBallSaverActive = False '"    	' clear the flag after a slight buffer 
    ' If you have a ball saver light then turn it off at this point
    I14.State = 0
	BallSaverTime = defBallSaverTime
End Sub

Sub BallSaverTimerCancel()
	D "Ballsaver Cancel"
	BallSaverTimerExpired.Enabled = False
	bBallSaverActive = False
    ' If you have a ball saver light then turn it off at this point
    I14.State = 0
	BallSaverTime = defBallSaverTime	
End Sub 

Sub BallSaverSpeedUpTimer_Timer()
    D "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    I14.BlinkInterval = 80
    I14.State = 2
End Sub

Function P  ' Determine what background image to show
	P = kDMD_PlayerMode+(CurrentPlayer*2)+(PlayersPlayingGame*(PlayersPlayingGame-1))
End Function

Sub SetBackglassTimer(value)
	If bUsePUPDMD then
		If modecountdowntimer.enabled Then
			pDMDEvent(P+1)
			PuPlayer.LabelSet pBackglass,"Time", value		,1,""
		Else
			pDMDEvent(P)
		End If
	End If 
End Sub


dim ModeCountdownTimerGrace
Sub ModeCountdownTimer_Timer()
	If bPauseTimer then exit Sub

	If ModeCountdownTimer.UserValue >0 then ModeCountdownTimer.UserValue = ModeCountdownTimer.UserValue -1
	SetBackglassTimer(ModeCountdownTimer.UserValue)

	If ModecountdownTimer.UserValue = 5 Then
		PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x030Btime.wav",100, 1
	End If
	If ModeCountdownTimer.UserValue <= 5 then 
		PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x01DB.wav",100, 1
	End If 
	If ModeCountdownTimer.UserValue = 1 then ModeCountdownTimerGrace=2	' setup 2 second grace period 
	If (ModeCountdownTimer.UserValue <= 0) Then
		ModeCountdownTimerGrace=ModeCountdownTimerGrace - 1
		If ModeCountdownTimerGrace <= 0 then 
			PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x030Ctimesup.wav",100, 1
'			If modecountdowntimer.enabled Then ' remove timer bubble       ' KEEP BUBBLE FOR THE secondMode

			pDMDEvent(P)
			StopPlayerMode()
		End If 
	End If
End Sub


'******
' Keys
'******
Sub tmrHoldKey_Timer()		' Reset the game with Ball in lane
	tmrHoldKey.Enabled = False
	' TBD Destroy balls
	
	If bBallInPlungerLane and BallsOnPlayfield = 1 Then
		PlaySoundVol "start", VolDef
		bResetCurrentGame=True
		If bUsePupDMD then 
			PuPlayer.PlayStop pOverVid						' Stop overlay If there is one
			PuPlayer.SetLoop pOverVid, 0
		End If
		ResetForNewGame()
	End If
End Sub


Sub Table1_KeyDown(ByVal Keycode)

	dim musicMode
	If bTableReady=False then Exit Sub	' If the ultraDMD hasnt finished intro then things look weird 
	If bBallInPlungerLane and keycode = StartGameKey then 
		tmrHoldKey.Enabled = False
		tmrHoldKey.Enabled = True
	End If

    If Keycode = AddCreditKey Then
		D "AddCreditKey"
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
		Credits = Credits + 1
		if Credits > 30 then Credits = 30
		DOF 140, DOFOn
		If(Tilted = False)Then
			DMDFlush
			DMD "CREDITS " & Credits, "", "", eNone, eNone, eNone, 500, True, "coin_In_1"
		End If
    End If
 
    If keycode = PlungerKey Then 
        Plunger.Pullback:SoundPlungerPull()
    End If

	If keycode = LeftFlipperKey Then lfPress = 1
	If keycode = RightFlipperKey Then rfPress = 1	

    'If keycode = LeftTiltKey Then Nudge 90, 6:SoundNudgeLeft():CheckTilt
    'If keycode = RightTiltKey Then Nudge 270,6:SoundNudgeRight():CheckTilt


    If keycode = CenterTiltKey Then 
		Nudge 0, 7:SoundNudgeCenter():CheckTilt
	End If

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

	If bGameInPlay AND NOT Tilted Then
		' Normal flipper action
        If keycode = LeftFlipperKey Then SolLFlipper 1:StartInstantInfo(keycode)
        If keycode = RightFlipperKey Then SolRFlipper 1:StartInstantInfo(keycode)

		' Select Player Mode
		If (bPlayerModeSelect) and bInstantInfo=False Then	
			SelectPlayerMode(keycode)
		end If


		If keycode = RightMagnaSave or keycode = LockBarKey Then
			If bStartMB Then 
				ToyBox_Cancel()
			Else
				if bGameInPlay and BallsOnPlayfield > 0 Then
					CheckSmartButton True ' The routine will determine If it can be used
				End if
			End if
		End if

		'  Start Mode
		If keycode = RightMagnaSave or keycode = LockBarKey or _  
			(keycode = PlungerKey and bUsePlungerForSternKey) Then

			If bPlayerModeSelect and bBallInPlungerLane = False then	' They selected a new player mode (kick the ball from the scoop)
				StartPlayerMode()
				setModeSelectLight(False)  
D "kickout 3"
				vpmtimer.addtimer 2000, "ScoopKickOut '"
				exit sub
			elseIf bAutoPlunger=False and bBallInPlungerLane = True then	' Auto fire ball with stern key
				PlungerIM.AutoFire
				plungerIM.Strength = 45
			End if
		End if


        If keycode = StartGameKey Then		' startkey
            If((PlayersPlayingGame < MaxPlayers) AND (bOnTheFirstBall = True)) Then
                If(bFreePlay = True) Then
					soundStartButton()
                    PlayersPlayingGame = PlayersPlayingGame + 1

					If modecountdowntimer.enabled Then
						pDMDEvent(P+1)
					Else
						pDMDEvent(P)
					End If
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMDFlush
                    DMD PlayersPlayingGame & " PLAYERS", "", "", eNone, eBlink, eNone, 500, True, "start"
					I2 cstr(PlayersPlayingGame) + " PLAYERS"
                Else
                    If(Credits > 0)then
						soundStartButton()
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
						If Credits < 1 Then DOF 140, DOFOff
                        DMD PlayersPlayingGame & " PLAYERS", "", "", eNone, eBlink, eNone, 500, True, "start"
						I2 cstr(PlayersPlayingGame) + " PLAYERS"
                    Else
                        ' Not Enough Credits to start a game.
                        DMDFlush
                        DMD "CREDITS " & Credits, "INSERT COIN", "", eNone, eBlink, eNone, 500, True, ""
						I2 "CREDITS " & cstr(Credits) &  " INSERT COIN"
                    End If
                End If
				UpdateNumberPlayers	' Update the screen layout on pup for multiple players			
            End If
        End If
    ElseIf bGameInPlay=False Then
		If keycode = StartGameKey Then
			If(bFreePlay = True)Then
				If(BallsOnPlayfield = 0)Then
					ResetForNewGame()
				End If
			Else
				If(Credits > 0)Then
					If(BallsOnPlayfield = 0)Then
						Credits = Credits - 1
						If Credits < 1 Then DOF 140, DOFOff
						ResetForNewGame()
					End If
				Else
					' Not Enough Credits to start a game.
					DMDFlush
					DMD "CREDITS " & Credits, "INSERT COIN", "", eNone, eBlink, eNone, 500, True, ""
				End If
			End If
		End If 
    End If ' If (GameInPlay)
	'VR Keys animation
	If keycode = LeftFlipperKey Then PinCab_Flipper_Button_Left.X = PinCab_Flipper_Button_Left.X +8
	If keycode = RightFlipperkey Then PinCab_Flipper_Button_Right.X = PinCab_Flipper_Button_Right.X -8
	If Keycode = StartGameKey Then  PinCab_Start_Button.y = PinCab_Start_Button.y -2: PinCab_Start_Button_Inner_Ring.Y = PinCab_Start_Button_Inner_Ring.Y -2
	If keycode = RightMagnaSave or keycode = LockBarKey or (keycode = PlungerKey and bUsePlungerForSternKey) Then PinCab_SmartButton_1.Z =PinCab_SmartButton_1.Z -6	:PinCab_SmartButton_2.Z =PinCab_SmartButton_2.Z -6 :PinCab_SmartButton_inner.Z = PinCab_SmartButton_inner.Z -6
	If keycode = PlungerKey Then TimerVRPlunger.enabled = True : TimerVRPlunger2.enabled = False
	If Credits > 0 then Pincab_TimserStartButton.enabled = True
End Sub


Sub Table1_KeyUp(ByVal keycode)
	Dim NextCmd
	tmrHoldKey.Enabled = False
    If keycode = PlungerKey Then
        Plunger.Fire
		If bBallInPlungerLane then 
			SoundPlungerReleaseBall()
		Else	
			SoundPlungerReleaseNoBall()
		End If
    End If

    If hsbModeActive Then
		InstantInfoTimer.Enabled = False
		bInstantInfo = False
        Exit Sub
    End If

    If bGameInPlay AND NOT Tilted Then
		If LFPress=1 and RFPress = 1 then	' Pressed both at the same time
			If bFlipperSkipEnabled and FlipperSkipCmd<>"" then
				NextCmd = FlipperSkipCmd
				FlipperSkipCmd=""
				Execute NextCmd
			End If 
		End If 

		If keycode = LeftFlipperKey Then
			lfpress = 0
			leftflipper.eostorqueangle = EOSA
			leftflipper.eostorque = EOST
		End If
		If keycode = RightFlipperKey Then 
			rfpress = 0
			rightflipper.eostorqueangle = EOSA
			rightflipper.eostorque = EOST
		End If

        If keycode = LeftFlipperKey Then
			If bSkillshotsReady(1) = True Then tmrSkillshot_Timer()	' lose the super skill shot
            SolLFlipper 0
			EndFlipperStatus(keycode)
        End If
        If keycode = RightFlipperKey Then
            SolRFlipper 0
			EndFlipperStatus(keycode)
        End If

	Else
		If keycode = LeftFlipperKey Then lfpress = 0
		If keycode = RightFlipperKey Then rfpress = 0
    End If

	'VR Keys animation
	If keycode = LeftFlipperKey Then PinCab_Flipper_Button_Left.X = PinCab_Flipper_Button_Left.X -8
	If keycode = RightFlipperkey Then PinCab_Flipper_Button_Right.X = PinCab_Flipper_Button_Right.X +8
	If Keycode = StartGameKey Then  PinCab_Start_Button.y = PinCab_Start_Button.y +2: PinCab_Start_Button_Inner_Ring.Y = PinCab_Start_Button_Inner_Ring.Y +2
	If keycode = RightMagnaSave or keycode = LockBarKey or (keycode = PlungerKey and bUsePlungerForSternKey) Then PinCab_SmartButton_1.Z =PinCab_SmartButton_1.Z +6	:PinCab_SmartButton_2.Z =PinCab_SmartButton_2.Z +6 : PinCab_SmartButton_inner.Z = PinCab_SmartButton_inner.Z +6
	If keycode = PlungerKey Then TimerVRPlunger.enabled = False : TimerVRPlunger2.enabled = True

End Sub


Sub UpdateNumberPlayers
	dim text1Size, pos1
	dim text2Size, pos2
	dim text3Size, pos3
	dim text4Size, pos4

	If bUsePUPDMD = False then 
		Exit Sub 	
	End If
	
  ' xAlign, 0 = horizontal left align, 1 = center horizontal, 2 = right horizontal
  ' yAlign, 0 = top, 1 = center, 2=bottom vertical alignment

	Select Case PlayersPlayingGame 
		Case 1:
			text1Size=8*FontScale
			puPlayer.LabelSet pBackglass,"Play1score",FormatScore(score(0)),1,"{'mt':2,'size':  "&text1Size&", 'ypos': 100, 'xpos': 52, 'xalign': 1}"
			puPlayer.LabelSet pBackglass,"Play2score","",0,""
			puPlayer.LabelSet pBackglass,"Play3score","",0,""
			puPlayer.LabelSet pBackglass,"Play4score","",0,""
		Case 2:
			pos1=30:pos2=80
			If CurrentPlayer = 0 then text1Size=8*FontScale else text1Size=6*FontScale
			If CurrentPlayer = 1 then text2Size=8*FontScale else text2Size=6*FontScale
			Select Case CurrentPlayer:
				Case 0: pos1=30:pos2=80
				Case 1: pos1=28:pos2=75
			End Select
			puPlayer.LabelSet pBackglass,"Play1score",FormatScore(score(0)),1,"{'mt':2,'size':  "&text1Size&", 'ypos': 99, 'xpos': "&pos1&", 'xalign': 1}"
			puPlayer.LabelSet pBackglass,"Play2score",FormatScore(score(1)),1,"{'mt':2,'size':  "&text2Size&", 'ypos': 99, 'xpos': "&pos2&", 'xalign': 1}"
			puPlayer.LabelSet pBackglass,"Play3score","0",0,""
			puPlayer.LabelSet pBackglass,"Play4score","0",0,""
		Case 3:
			If CurrentPlayer = 0 then text1Size=6*FontScale else text1Size=5*FontScale
			If CurrentPlayer = 1 then text2Size=6*FontScale else text2Size=5*FontScale
			If CurrentPlayer = 2 then text3Size=6*FontScale else text3Size=5*FontScale
			Select Case CurrentPlayer:
				Case 0: pos1=20:pos2=57:pos3=85
				Case 1: pos1=16:pos2=53:pos3=87
				Case 2: pos1=18:pos2=46:pos3=80
			End Select
			puPlayer.LabelSet pBackglass,"Play1score",FormatScore(score(0)),1,"{'mt':2,'size':  "&text1Size&", 'ypos': 98, 'xpos': "&pos1&", 'xalign': 1}"
			puPlayer.LabelSet pBackglass,"Play2score",FormatScore(score(1)),1,"{'mt':2,'size':  "&text2Size&", 'ypos': 98, 'xpos': "&pos2&", 'xalign': 1}"
			puPlayer.LabelSet pBackglass,"Play3score",FormatScore(score(2)),1,"{'mt':2,'size':  "&text3Size&", 'ypos': 98, 'xpos': "&pos3&", 'xalign': 1}"
			puPlayer.LabelSet pBackglass,"Play4score","0",0,""
		Case 4:
			If CurrentPlayer = 0 then text1Size=6*FontScale else text1Size=5*FontScale
			If CurrentPlayer = 1 then text2Size=6*FontScale	else text2Size=5*FontScale
			If CurrentPlayer = 2 then text3Size=6*FontScale else text3Size=5*FontScale
			If CurrentPlayer = 3 then text4Size=6*FontScale else text4Size=5*FontScale
			Select Case CurrentPlayer:
				Case 0: pos1=18:pos2=43:pos3=66:pos4=89
				Case 1: pos1=13:pos2=41:pos3=66:pos4=88
				Case 2: pos1=13:pos2=37:pos3=64:pos4=88
				Case 3: pos1=13:pos2=36:pos3=59:pos4=86
			End Select
			puPlayer.LabelSet pBackglass,"Play1score",FormatScore(score(0)),1,"{'mt':2, 'size':  "&text1Size&", 'ypos': 98, 'xpos': "&pos1&", 'xalign': 1}"
			puPlayer.LabelSet pBackglass,"Play2score",FormatScore(score(1)),1,"{'mt':2, 'size':  "&text2Size&", 'ypos': 98, 'xpos': "&pos2&", 'xalign': 1}"
			puPlayer.LabelSet pBackglass,"Play3score",FormatScore(score(2)),1,"{'mt':2, 'size':  "&text3Size&", 'ypos': 98, 'xpos': "&pos3&", 'xalign': 1}"
			puPlayer.LabelSet pBackglass,"Play4score",FormatScore(score(3)),1,"{'mt':2, 'size':  "&text4Size&", 'ypos': 98, 'xpos': "&pos4&", 'xalign': 1}"
	End Select
End Sub 


Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp > BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a  
'
Sub ResetForNewGame()
    Dim i
	D "ResetForNewGame"
    bGameInPLay = True
	tmrBallSearch.Interval = kBallSearchTimeout
	BallSearchCnt = 0
	tmrBallSearch.Enabled = True

    'resets the score display, and turn off attrack mode
    StopAttractMode
	pBgShowBonus " ", " "
	If bUsePUPDMD then
		playclear pOverVid
		PuPlayer.LabelSet pOverVid,"OverMessage1"," ",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2"," ",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage3"," ",1,""
	End If

    GiOn
	SmartButtonFlash "", False

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 0
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
	PlayMultiplier = 1
    For i = 0 To MaxPlayers-1
        Score(i) = 0
        BonusPoints(i) = 0
        BonusHeldPoints(i) = 0
        BonusMultiplier = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
		TiltCount(i) = 0
    Next

    ' initialise any other flags
    Tilt = 0

    ' initialise Game variables
    Game_Init()

	'debug_medleytour() ' dwe

	UpdateNumberPlayers		' Do the first one 

	Scorbit.StartSession()
	ScorbitBuildGameModes()
    ' set up the start delay to handle any Start of Game Attract Sequence
    vpmtimer.addtimer 1500, "FirstBall '"
End Sub

' This is used to delay the start of a game to allow any attract sequence to
' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
	D "FirstBall()"
    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
	If bResetCurrentGame = False then 
		D "created for FirstBall..."
		CreateNewBall()
	End If
	bResetCurrentGame = False
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (If multiple are playing))

Sub ResetForNewPlayerBall()
	D "ResetForNewPlayerBall BALL " & Balls
    ' make sure the correct display is upto date
    AddScore 0
	mMagnet.MotorOn = True

	SetBonusMultiplier 1
    ' set the current players bonus multiplier back down to 1X
    MultiplierShot = 1
    Multiplier3x = 1
	bShotMultiplierSelect = False
'	bWizardMode = False

	RatsStep=1 ' Go from 1L,0R to 0L,1R to 2L,0R, etc

	' Clear shot multipliers 
	I32.UserValue =0
	I67.UserValue =0
	I84.UserValue =0
	I95.UserValue =0
	I46.UserValue =0
	I112.UserValue=0
	I101.UserValue=0
	I89.UserValue =0

	coins=0

    ' reset any drop targets, lights, game modes etc..

    BonusPoints(CurrentPlayer) = 0
    bBonusHeld = False
    bExtraBallWonThisBall = False
    'Reset any table specific
    ResetNewBallVariables()
	ResetNewBallLights()
    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True
	PopValue = 5000
	SpinValue=200

	SpinnerBonus=0
	TargetBonus=0
	BumperBonus=0
	LoopBonus=0
	RampBonus=0
	LaneBonus=0

	D "Reset Skillshot"
	bSkillshotsReady(0) = True		' tbox kicker
	bSkillshotsReady(1) = False		' Hold Left, Loop around and hit Orb, Right Ramp or Right Orbit
	SuperSkillShotIndex=-1

	If I100.state=2 Then
		ToyBox_OpenLid()
	Else
		ToyBox_CloseLid()
	End If

	ScorbitBuildGameModes()
End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
    D "sub CreateNewBall()" ' create a ball in the plunger lane kicker.
	bCreatedBall = True
    BallRelease.CreateSizedball BallSize / 2

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
    BallRelease.Kick 90, 4
    SoundSaucerKick 1, BallRelease

' If there is 2 or more balls then set the multiball flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
    If BallsOnPlayfield-RealBallsInLock > 1 Then
		D "SETTING MULTIBALL MODE BOP:" & BallsOnPlayfield & " Real:" & RealBallsInLock & "!!!!!!!!!!!!!!!!!!"
        bMultiBallMode = True
        bAutoPlunger = True
    End If
End Sub

Sub CreateNewToyBoxBall()
    ' create a ball in the plunger lane kicker.
	bCreatedBall = True
    BallReleaseToyBox.CreateSizedball BallSize / 2

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
    BallReleaseToyBox.Kick 160, 10	
    SoundSaucerKick 1, BallReleaseToyBox

' If there is 2 or more balls then set the multiball flag (remember to check for locked balls and other balls used for animations)
    If BallsOnPlayfield-RealBallsInLock > 1 Then
		D "SETTING MULTIBALL MODE " & BallsOnPlayfield & " Real:" & RealBallsInLock & "!!!!!!!!!!!!!!!!!!"
        bMultiBallMode = True
    End If
End Sub

Sub ResetBallSearch()
	If BallSearchResetting then Exit Sub	' We are resetting just exit for now 
	'D "ResetBallSearch()"
	tmrBallSearch.Enabled = False	' Reset Ball Search
	BallSearchCnt=0
	tmrBallSearch.Enabled = True
End Sub 

Dim BallSearchResetting:BallSearchResetting=False

Sub tmrBallSearch_Timer()	' We timed out
	' See If we are in mode select, a flipper is up that might be holding the ball or a ball is in the lane 
	D "tmrBallSearchTimer()"
	If bGameInPlay and bPlayerModeSelect = False and _
		hsbModeActive = False and _ 
		tmrEndOfBallBonus.Enabled = False and _
		bBallInPlungerLane = False and _
		LeftFlipper.CurrentAngle <> LeftFlipper.EndAngle and _
		RightFlipper.CurrentAngle <> RightFlipper.EndAngle Then

		D "Ball Search - NO ACTIVITY " & BallSearchCnt

		If BallSearchCnt >= 3 Then
			dim Ball
			D "--- listing balls ---"
			For each Ball in GetBalls
				D "Ball: (" & Ball.x & "," & Ball.y & ")"
			Next
			D "--- listing balls ---"

			BallSearchCnt = 0
			If BallsOnPlayfield > 0 then 	' somehow we might have drained and didnt catch it??
				BallsOnPlayfield = BallsOnPlayfield - 1  ' We cant find the ball (remove one)
			End if
			I2 "Ball search .. add a ball"
			AddMultiball(1)
			DisplayDMDText "BALL SEARCH FAIL","", 1000
			Exit sub
		End if

		BallSearchResetting=True
		BallSearchCnt = BallSearchCnt + 1
		If BallSearchCnt > 2 Then
			DisplayDMDText "BALL SEARCH","", 1000
			DOF 113, DOFPulse
			DOF 112, DOFPulse
			mMagnet.MotorOn = False
		End If
		vpmtimer.addtimer 3000, "BallSearchResetting = False '"
	Else 
		ResetBallSearch()
	End If 
End Sub

'*************  starts PUP system,  must be called AFTER b2s/controller running so put in last line of table1_init
Sub PuPInit
	on error resume next
	Dim fso
	Dim objFileToRead

	Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")  
	Set fso = CreateObject("Scripting.FileSystemObject")

	' Check Version Stuff
	if FileExists(puplayer.getroot & "\" & PuPPack_folder & "\version.txt") then
		Set objFileToRead = fso.OpenTextFile(puplayer.getroot & PuPPack_folder & "\version.txt",1)
		PupPack_version = objFileToRead.ReadLine()
		objFileToRead.Close
		Set objFileToRead = Nothing

		'use whatever command VPX uses to import external "version.txt" file (using the pup path above) to be used as vbs script
		if PupPack_version = PupPack_verified then
			if FileExists(puplayer.getroot & "\" & PuPPack_folder & "\Option Selected.txt") then

				Set objFileToRead = fso.OpenTextFile(puplayer.getroot & "\" & PuPPack_folder & "\Option Selected.txt",1)
				PupOption = objFileToRead.ReadLine()
				objFileToRead.Close
				Set objFileToRead = Nothing

				if PupOption = 0 or PupOption=2 then   ' Desktop & 3 screen
					ScreenType=0
				elseif PupOption = 3 then   ' 5x4
					ScreenType=3
				Else 
					ScreenType=1
				End if 

			   'use whatever command VPX uses to import external "Option Selected.txt" file (using the pup path above) to be used as vbs script
			   'make changes needed in script based on pup-pack option file found (like pupdmddrivertype,etc)
			end if
'			PuPlayer.B2SInit "",PuPPack_folder 'start the Pup-Pack
			PUPStatus = true
		Else
			MsgBox "PupPack version mismatch.  Please download the latest pupPack version " & PupPack_verified & "><" & PupPack_version
			'pup-pack is not found or is incorrect version
			PUPStatus = false
			PUPEnabled = false
			'use FlexDMD or other option if supported on the table
		end if
	Else
		msgbox "Warning! Missing the PupPack version.txt file"
	end if

	PuPlayer.B2SInit "", pGameName
	on error goto 0
	If not IsObject(PuPlayer) then bUsePUPDMD=False


	If (PuPDMDDriverType=pDMDTypeReal) and (useRealDMDScale=1) Then 
       PuPlayer.setScreenEx pDMD,0,0,128,32,0  'If hardware set the dmd to 128,32
	End if

	PuPlayer.LabelInit pBackglass
	PuPlayer.LabelInit pDMD

	If PuPDMDDriverType=pDMDTypeReal then
		Set PUPDMDObject = CreateObject("PUPDMDControl.DMD") 
		PUPDMDObject.DMDOpen
		PUPDMDObject.DMDPuPMirror
		PUPDMDObject.DMDPuPTextMirror
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":33 }"             'set pupdmd for mirror and hide behind other pups
		PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":32, ""FQ"":3 }"   'set no antialias on font render If real
	'Else
	'	PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":33 }"             'set pupdmd for mirror and hide behind other pups
	End If

	pSetPageLayouts
	pDMDSetPage(pDMDBlank)   'set blank text overlay page.
	pDMDStartUP				 ' firsttime running for a startup video..

	pupDMDUpdate.Enabled = True
	pupDMDUpdate.interval = 250

	if Scorbitactive then 
		if Scorbit.DoInit(2152, "PupOverlays", myVersion, "Gr16e-MnKEX-VPIN") then 	' from Jay in discord 3862 Munsters and 3067 (G4qX5-Mz2Pp-VPIN) for kiss
			debug.print "Running scorebit!"
			tmrScorbit.Interval=2000
			tmrScorbit.UserValue = 0
			tmrScorbit.Enabled=True 
			Scorbit.UploadLog = ScorbitUploadLog
		End if 
	End if 
End Sub 'end PUPINIT


'PinUP Player DMD Helper Functions

Sub pDMDLabelHide(labName)
	PuPlayer.LabelSet pDMD,labName,"",0,""   
End sub


Sub pDMDScrollBig(msgText,timeSec,mColor)
	PuPlayer.LabelShowPage pDMD,2,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
End sub


Sub pDMDScrollBigV(msgText,timeSec,mColor)
	PuPlayer.LabelShowPage pDMD,2,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'yps':1,'ype':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
End sub


Sub pDMDSplashScore(msgText,timeSec,mColor)
	PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':1,'fq':250,'len':"& (timeSec*1000) &",'fc':" & mColor & "}"
End Sub


Sub pDMDSplashScoreScroll(msgText,timeSec,mColor)
	PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':"& (timeSec*1000) &", 'mlen':"& (timeSec*1000) &",'tt':0, 'fc':" & mColor & "}"
End Sub


Sub pDMDZoomBig(msgText,timeSec,mColor)  'new Zoom
	PuPlayer.LabelShowPage pDMD,2,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':3,'hstart':5,'hend':80,'len':" & (timeSec*1000) & ",'mlen':" & (timeSec*500) & ",'tt':5,'fc':" & mColor & "}"
End sub


Sub pDMDTargetLettersInfo(msgText,msgInfo, timeSec)  'msgInfo = '0211'  0= layer 1, 1=layer 2, 2=top layer3.
'this function is when you want to hilite spelled words.  Like B O N U S but have O S hilited as already hit markers... see example.
	PuPlayer.LabelShowPage pDMD,5,timeSec,""  'show page 5
	Dim backText
	Dim middleText
	Dim flashText
	Dim curChar
	Dim i
	Dim offchars:offchars=0
	Dim spaces:spaces=" "  'set this to 1 or more depends on font space width.  only works with certain fonts
                          'If using a fixed font width then set spaces to just one space.

	For i=1 To Len(msgInfo)
		curChar="" & Mid(msgInfo,i,1)
		If curChar="0" Then
            backText=backText & Mid(msgText,i,1)
            middleText=middleText & spaces
            flashText=flashText & spaces          
            offchars=offchars+1
		End If
		If curChar="1" Then
            backText=backText & spaces
            middleText=middleText & Mid(msgText,i,1)
            flashText=flashText & spaces
		End If
		If curChar="2" Then
            backText=backText & spaces
            middleText=middleText & spaces
            flashText=flashText & Mid(msgText,i,1)
		End If   
	Next 

	If offchars=0 Then 'all litup!... flash entire string
		backText=""
		middleText=""
		FlashText=msgText
	End If  

	PuPlayer.LabelSet pDMD,"Back5"  ,backText  ,1,""
	PuPlayer.LabelSet pDMD,"Middle5",middleText,1,""
	PuPlayer.LabelSet pDMD,"Flash5" ,flashText ,0,"{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) & "}"   
End Sub


Sub pDMDSetPage(pagenum)
	If (bUsePUPDMD) then
		D "Changing Page: " & pagenum
		PuPlayer.LabelShowPage pDMD,pagenum,0,""   'set page to blank 0 page If want off
		PDMDCurPage=pagenum
	End if
End Sub


Sub pHideOverlayText(pDisp)
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& pDisp &", ""FN"": 34 }"             'hideoverlay text during next videoplay on DMD auto return
End Sub


Sub pDMDShowLines3(msgText,msgText2,msgText3,timeSec)
	Dim vis:vis=1
	If pLine1Ani<>"" Then vis=0
	PuPlayer.LabelShowPage pDMD,3,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash3a",msgText,vis,pLine1Ani
	PuPlayer.LabelSet pDMD,"Splash3b",msgText2,vis,pLine2Ani
	PuPlayer.LabelSet pDMD,"Splash3c",msgText3,vis,pLine3Ani
End Sub


Sub pDMDShowLines2(msgText,msgText2,timeSec)
	Dim vis:vis=1
	If pLine1Ani<>"" Then vis=0
	PuPlayer.LabelShowPage pDMD,4,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash4a",msgText,vis,pLine1Ani
	PuPlayer.LabelSet pDMD,"Splash4b",msgText2,vis,pLine2Ani
End Sub

Sub pDMDShowCounter(msgText,msgText2,msgText3,timeSec)
	Dim vis:vis=1
	If pLine1Ani<>"" Then vis=0
	PuPlayer.LabelShowPage pDMD,6,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash6a",msgText,vis, pLine1Ani
	PuPlayer.LabelSet pDMD,"Splash6b",msgText2,vis,pLine2Ani
	PuPlayer.LabelSet pDMD,"Splash6c",msgText3,vis,pLine3Ani
End Sub


Sub pDMDShowBig(msgText,timeSec, mColor)
	Dim vis:vis=1
	If pLine1Ani<>"" Then vis=0
	PuPlayer.LabelShowPage pDMD,2,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash",msgText,vis,pLine1Ani
End sub


Sub pDMDShowHS(msgText,msgText2,msgText3,timeSec) 'High Score
	Dim vis:vis=1
	If pLine1Ani<>"" Then vis=0
	PuPlayer.LabelShowPage pDMD,7,timeSec,""
	PuPlayer.LabelSet pDMD,"Splash7a",msgText,vis,pLine1Ani
	PuPlayer.LabelSet pDMD,"Splash7b",msgText2,vis,pLine2Ani
	PuPlayer.LabelSet pDMD,"Splash7c",msgText3,vis,pLine3Ani
End Sub


Sub pDMDSetBackFrame(fname)
	PuPlayer.playlistplayex pDMD,"PUPFrames",fname,0,1    
End Sub

Sub pDMDStartBackLoop(fPlayList,fname)
	PuPlayer.playlistplayex pDMD,fPlayList,fname,0,1
	PuPlayer.SetBackGround pDMD,1
End Sub

Sub pStartBackLoop(fPlayList,fname)
	D "pStartBackLoop " & fPlayList & " " & fname	
	If bUsePupDMD then ' 01d
		PuPlayer.playlistplayex pBackglass,fPlayList,fname,100,1
		PuPlayer.SetBackGround pBackglass,1
	End IF
end Sub

Sub pDMDStopBackLoop
	If bUsePupDMD then ' 01d
		PuPlayer.SetBackGround pDMD,0
		PuPlayer.playstop pDMD
	End If
End Sub


Dim pNumLines

'Theme Colors for Text (not used currenlty,  use the |<colornum> in text labels for colouring.
Dim SpecialInfo
Dim pLine1Color : pLine1Color=8454143  
Dim pLine2Color : pLine2Color=8454143
Dim pLine3Color :  pLine3Color=8454143
Dim curLine1Color: curLine1Color=pLine1Color  'can change later
Dim curLine2Color: curLine2Color=pLine2Color  'can change later
Dim curLine3Color: curLine3Color=pLine3Color  'can change later


Dim pDMDCurPriority: pDMDCurPriority =-1
Dim pDMDDefVolume: pDMDDefVolume = 0   'default no audio on pDMD

Dim pLine1
Dim pLine2
Dim pLine3
Dim pLine1Ani
Dim pLine2Ani
Dim pLine3Ani

Dim PriorityReset:PriorityReset=-1
DIM pAttractReset:pAttractReset=-1
DIM pAttractBetween: pAttractBetween=2000 '1 second between calls to next attract page
DIM pDMDVideoPlaying: pDMDVideoPlaying=false


'************************ where all the MAGIC goes,  pretty much call this everywhere  ****************************************
'****************************************   DONT TOUCH THIS CODE   ************************************************************

Sub pupDMDDisplay(pEventID, pText, VideoName, TimeSec, pAni, pPriority)
' pEventID = reference If application,  
' pText = "text to show" separate lines by ^ in same string
' VideoName "gameover.mp4" will play in background  "@gameover.mp4" will play and disable text during gameplay.
' 			also global variable useDMDVideos=true/false If user wishes only TEXT
' TimeSec how long to display msg in Seconds
' animation If any 0=none 1=Flasher
' also,  now can specify color of each line (when no animation).  "sometext|12345"  will set label to "sometext" and set color to 12345
	DIM curPos
	'D "pupDMDDisplay " & pEventID & " " & pText & " " & VideoName

	If pDMDCurPriority>pPriority then 
		Exit Sub  'If something is being displayed that we don't want interrupted.  same level will interrupt.
		D "DMD Skipping - Hi Pri"
	End If 
	pDMDCurPriority=pPriority
	If timeSec=0 then timeSec=1 'don't allow page default page by accident

	pLine1=""
	pLine2=""
	pLine3=""
	pLine1Ani=""
	pLine2Ani=""
	pLine3Ani=""

	If pAni=1 Then  'we flashy now aren't we
		pLine1Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
		pLine2Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
		pLine3Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
	End If

	curPos=InStr(pText,"^")   'Lets break apart the string If needed
	If curPos>0 Then 
	   pLine1=Left(pText,curPos-1) 
	   pText=Right(pText,Len(pText) - curPos)
	   
	   curPos=InStr(pText,"^")   'Lets break apart the string
	   If curPOS>0 Then
		  pLine2=Left(pText,curPos-1) 
		  pText=Right(pText,Len(pText) - curPos)

		  curPos=InStr("^",pText)   'Lets break apart the string   
		  If curPos>0 Then
			 pline3=Left(pText,curPos-1) 
		  Else 
			If pText<>"" Then pline3=pText 
		  End If 
	   Else 
		  If pText<>"" Then pLine2=pText
	   End If    
	Else 
	  pLine1=pText  'just one line with no break 
	End if


	'lets see how many lines to Show
	pNumLines=0
	If pLine1<>"" then pNumLines=pNumlines+1
	If pLine2<>"" then pNumLines=pNumlines+1
	If pLine3<>"" then pNumLines=pNumlines+1

	If pDMDVideoPlaying Then 
		If bUsePUPDMD then
			PuPlayer.playstop pDMD
			pDMDVideoPlaying=False
		End if
	End if


	If (VideoName<>"") and (useDMDVideos) Then  'we are showing a splash video instead of the text.	
		if bUsePUPDMD then
			PuPlayer.playlistplayex pDMD,"DMDSplash",VideoName,pDMDDefVolume,pPriority  'should be an attract background (no text is displayed)
			pDMDVideoPlaying=true
		End if
	End If 'If showing a splash video with no text


	If StrComp(pEventID,"shownum",1)=0 Then              'check eventIDs
		pDMDShowCounter pLine1,pLine2,pLine3,timeSec
	ElseIf StrComp(pEventID,"target",1)=0 Then              'check eventIDs
		pDMDTargetLettersInfo pLine1,pLine2,timeSec
	ElseIf StrComp(pEventID,"highscore",1)=0 Then              'check eventIDs
		pDMDShowHS pLine1,pLine2,pline3,timeSec
	ElseIf (pNumLines=3) Then                'depends on # of lines which one to use.  pAni=1 will flash.
		pDMDShowLines3 pLine1,pLine2,pLine3,TimeSec
	ElseIf (pNumLines=2) Then
		pDMDShowLines2 pLine1,pLine2,TimeSec
	ElseIf (pNumLines=1) Then
		pDMDShowBig pLine1,timeSec, curLine1Color
	Else
		pDMDShowBig pLine1,timeSec, curLine1Color
	End if

	PriorityReset=TimeSec*1000
End Sub 'pupDMDDisplay message

Sub pupDMDupdate_Timer()
	pUpdateScores

    If PriorityReset>0 Then  'for splashes we need to reset current prioirty on timer
		PriorityReset=PriorityReset-pupDMDUpdate.interval
		If PriorityReset<=0 Then 
            pDMDCurPriority=-1            
            If pInAttract then pAttractReset=pAttractBetween ' pAttractNext  call attract next after 1 second
			pDMDVideoPlaying=false	
		End if
    End if

    If pAttractReset>0 Then  'for splashes we need to reset current prioirty on timer
		pAttractReset=pAttractReset-pupDMDUpdate.interval
		If pAttractReset<=0 Then 
            pAttractReset=-1            
            If pInAttract then pAttractNext
		End if
    End If 
End Sub

Sub PuPEvent(EventNum)
	If hasPUP=false then Exit Sub
	PuPlayer.B2SData "E"&EventNum,1  'sEnd event to puppack driver  
End Sub


'********************* END OF PUPDMD FRAMEWORK v1.0 *************************
'******************** DO NOT MODIFY STUFF ABOVE THIS LINE!!!! ***************
'****************************************************************************

'*****************************
' AUTO TESTING
' by:NailBuster 
' Global variable "AutoQA" below will switch all this on/off during testing.  
'
'*****************************
' NailBusters AutoQA Code and triggers..  
' add a timer called AutoQAStartGame.  you can run every 10000 interval.


Dim QACoinStartSec:QACoinStartSec=3 'TODO was 30   'timeout seconds for AutoCoinStartSec
Dim QANumberOfCoins:QANumberOfCoins=3 'number of coins to add for each start
Dim QASecondsDiff

Dim QALastFlipperTime:QALastFlipperTime=Now()
Dim AutoFlipperLeft:AutoFlipperLeft=false
Dim AutoFlipperRight:AutoFlipperRight=false
'If AutoQa or AutoAI then 
'	AutoQAStartGame.interval=5000
'	AutoQAStartGame.Enabled = True		' Start it up If this is enabled 
'end if

Sub AutoQAStartGame_Timer()                 'this is a timeout when sitting in attract with no flipper presses for 60 seconds, then add coins and start game.
 If AutoQA=false and AutoAI=False Then Exit Sub
 AutoQAStartGame.Enabled = False

 QASecondsDiff = DateDiff("s",QALastFlipperTime,NOW())
 If QASecondsDiff>QACoinStartSec Then
    'simulate quarters and start game keys
    Dim fx : fx=0
    Dim keydelay : keydelay=100
	Do While fx<QANumberOfCoins  
		vpmtimer.addtimer keydelay,"Table1_KeyDown(AddCreditKey) '"
        vpmtimer.addtimer keydelay+200,"Table1_KeyUp(AddCreditKey) '"
        keydelay=keydelay+500
		fx=fx+1
	Loop
	fx = 1+int(3*rnd()) ' Number of Players to QA
	Do While fx > 0
		vpmtimer.addtimer keydelay,"Table1_KeyDown(StartGameKey) '"
		vpmtimer.addtimer keydelay+200,"Table1_KeyUp(StartGameKey) '"
		keydelay=keydelay+500
		fx =fx-1
	Loop
    QALastFlipperTime=Now() 
	AutoFlipperLeft=false 	
    AutoFlipperRight=false
 End If  
  If QASecondsDiff>30 Then   'safety of stuck up flippers.
   AutoFlipperLeft=false 	
   AutoFlipperRight=false
  End if
  If bGameInPlay and QASecondsDiff>20 Then   'Press the magna
	vpmtimer.addtimer keydelay,"Table1_KeyDown(RightMagnaSave) '"
	vpmtimer.addtimer keydelay+200,"Table1_KeyUp(RightMagnaSave) '"
  End if
End Sub


Sub TriggerAutoPlunger_Hit()          'add a trigger in front of plunger.  adjust the delay timings If needed.    
    If AutoQA=false and AutoAI=false Then Exit Sub
	vpmtimer.addtimer 300,"Table1_KeyDown(PlungerKey) '"
    vpmtimer.addtimer int(1200+(8*RND())),"Table1_KeyUp(PlungerKey) '"   
End Sub


Sub FlipperUP(which)  'which=1 left 2 right
	QALastFlipperTime=Now()
	If which=1 Then
		Table1_KeyDown(LeftFlipperKey)   
		vpmtimer.addtimer 200+int(200*Rnd()),"Table1_KeyUP(LeftFlipperKey):AutoFlipperLeft=false  '"    
	Else
		Table1_KeyDown(RightFlipperKey)
		vpmtimer.addtimer 200+int(200*Rnd()),"Table1_KeyUP(RightFlipperKey):AutoFlipperRight=false  '"    
	End If
End Sub

'********************************** AI
Sub TurnOnAI
	TriggerLF.enabled=1:TriggerRF.enabled=1
	ZoneLeft.enabled=1:ZoneRight.enabled=1
	ZoneLeft1.enabled=1:ZoneRight1.enabled=1
	LeftFlipper.scatter=0
	RightFlipper.scatter=0
End Sub

Sub TurnOffAI
	TriggerLF.enabled=0:TriggerRF.enabled=0
	ZoneLeft.enabled=0:ZoneRight.enabled=0
	ZoneLeft1.enabled=0:ZoneRight1.enabled=0
End Sub

Sub  ZoneLeft_Hit
'	D "ZoneLeft_Hit() - check"
    If BallsOnPlayfield = 1 Then  
       If Activeball.velX<4 and Activeball.velX>-4 Then
		     If Activeball.velY<16.5 and Activeball.velY>-16.5 Then 
				'D "ZoneLeft_Hit() - press flipper"
				Table1_KeyDown(LeftFlipperKey)
				CradleLeft.Enabled=1
				TriggerLF.enabled=0
		    End If
	     End If
    End if
End Sub

Sub ZoneLeft1_Unhit
'	D "ZoneLeft_unhit - release flipper"
	Table1_KeyUp(LeftFlipperKey)
	CradleLeft.Enabled=0:CradleLeftTimer.Enabled=0
	TriggerLF.enabled=1
End Sub

Sub CradleLeft_Hit
	CradleLeftTimer.interval=750:CradleLeftTimer.Enabled=1
	If BallsOnPlayfield=1 Then bPauseTimer=True
End Sub

Sub CradleLeft_UnHit
	bPauseTimer=False
	CradleLeftTimer.Enabled=0
End Sub

Sub CradleLeftTimer_Timer
	CradleLeftTimer.Enabled=0
	Table1_KeyUp(LeftFlipperKey)
	TriggerLF.enabled=0
	CheckShotPriority 0
End Sub


Sub ZoneLeft1_Hit
	If BouncePassRTimer.Enabled = True Then Exit Sub
	'D "ZoneLeft1_Hit() velY=" & CStr(Activeball.velY) & "velX=" & CStr(Activeball.velX)
    If Activeball.velY>13.7 and Activeball.velY<18 and BallsOnPlayfield = 1 Then
		If Activeball.velX>-2 and Activeball.velX<2 Then  
			D "DeadflipL"
			TriggerLF.Enabled=0
			TriggerRF.Enabled=0
			ZoneLeft.Enabled=0
			ZoneRight.Enabled=0
			Leftdlight1.state=2
			BouncePassRTimer.interval=500:BouncePassRTimer.Enabled=1
	    End If
	End If
End Sub

Sub BouncePassLTimer_Timer
	'D "BouncePassLTimer"
    TriggerLF.Enabled=1 '(Gets Enabled above in ZoneLeft_Unhit)
    TriggerRF.Enabled=1
    ZoneRight.Enabled=1
    ZoneLeft.Enabled=1
	Rightdlight1.state=0
    BouncePassLTimer.Enabled=0
End Sub

Sub TriggerLF_Hit
'	D "TriggerLF_Hit - left flip"
	Table1_KeyDown(LeftFlipperKey)
	TimerLF.interval=150+int(RND()*70):TimerLF.Enabled=1
End Sub

Sub TimerLF_Timer
'	D "TimerLF"
	Table1_KeyUp(LeftFlipperKey)
    TimerLF.enabled=0
    ZoneLeft.Enabled=1
End Sub



' right
Sub  ZoneRight_Hit
	'D "ZoneRight_Hit() - check"
    If BallsOnPlayfield = 1 Then  
       If Activeball.velX<3.5 and Activeball.velX>-4 Then
		     If Activeball.velY<16.5 and Activeball.velY>-16.5 Then 
				'D "ZoneRight_Hit() - press flipper"
				Table1_KeyDown(RightFlipperKey)
				CradleRight.Enabled=1
				TriggerRF.enabled=0
		    End If
	     End If
    End if
End Sub

Sub ZoneRight1_Unhit
	D "ZoneRight_unhit - release flipper"
	Table1_KeyUp(RightFlipperKey)
	CradleRight.Enabled=0:CradleRightTimer.Enabled=0
	TriggerRF.enabled=1
End Sub

Sub CradleRight_Hit
	D "CradleRight_Hit"
	CradleRightTimer.interval=740:CradleRightTimer.Enabled=1  '750
	If BallsOnPlayfield=1 Then bPauseTimer=True
End Sub

Sub CradleRight_UnHit
	bPauseTimer=False
	CradleRightTimer.Enabled=0
End Sub

Sub CradleRightTimer_Timer
	D "CradleRightTimer_ release flipper"
	CradleRightTimer.Enabled=0
	Table1_KeyUp(RightFlipperKey)
	TriggerRF.enabled=0
	CheckShotPriority 1
End Sub


Sub ZoneRight1_Hit
	If BouncePassLTimer.Enabled = True Then Exit Sub
	'D "ZoneRight1_Hit() velY=" & CStr(Activeball.velY) & "velX=" & CStr(Activeball.velX)
    If Activeball.velY>13.7 and Activeball.velY<18 and BallsOnPlayfield = 1 Then
		If Activeball.velX>-4 and Activeball.velX<4 Then
			D "DeadflipR"
			TriggerLF.Enabled=0
			TriggerRF.Enabled=0
			ZoneLeft.Enabled=0
			ZoneRight.Enabled=0
			Rightdlight1.state=2
			BouncePassLTimer.interval=500:BouncePassLTimer.Enabled=1
	    End If
	End If
End Sub

Sub BouncePassRTimer_Timer
    TriggerLF.enabled=1 '(Gets Enabled above in ZoneLeft_Unhit)
    TriggerRF.Enabled=1
    ZoneRight.Enabled=1
    ZoneLeft.Enabled=1
	Leftdlight1.state=0
    BouncePassRTimer.Enabled=0
End Sub

Sub TriggerRF_Hit
	D "TriggerRF - Flipper UP!"
	Table1_KeyDown(RightFlipperKey)
	TimerRF.interval=100+int(RND()*70):TimerRF.Enabled=1
End Sub

Sub TimerRF_Timer
	D "TimerRF = Flipper Down!"
	Table1_KeyUp(RightFlipperKey)
    TimerRF.enabled=0
    ZoneRight.Enabled=1
End Sub

Dim LL:LL=137
Sub LeftPostPass()
	TimerLShot2.interval=LL:TimerLShot2.Enabled=1  '139, 141, 143, 147 149 153 152  works at 138
	TriggerRF.enabled=0
	TriggerLF.enabled=0
'LL=LL-1
End Sub

Sub PostPassLDelayTimer_Timer
	D "PostPassLDelayTimer_"
	PostPassLDelayTimer.Enabled=0
	Table1_KeyDown(RightFlipperKey)
    TriggerLF.enabled=1
End Sub

Sub CradleLogicLResetTimer_Timer
	D "CradleLogicLRestTimer_"
	ZoneLeft.Enabled=1:ZoneRight.Enabled=1
	Table1_KeyUp(LeftFlipperKey)
	CradleLogicLResetTimer.Enabled=0
	If AutoShot <> "SHOT099" Then Eval(AutoShot).intensity=0
End Sub

Dim RR: RR=118
Sub RightPostPass()
	TimerRShot2.interval=RR:TimerRShot2.Enabled=1   'was 140, 130
D "RR=" & RR
	TriggerRF.enabled=0
	TriggerLF.enabled=0
'RR=RR-1
End Sub

Sub PostPassRDelayTimer_Timer
	D "PostPassRDelayTimer_"
	PostPassRDelayTimer.Enabled=0
	Table1_KeyDown(LeftFlipperKey)
    TriggerRF.enabled=1
End Sub

Sub CradleLogicRResetTimer_Timer
	D "CradleLogicRRestTimer_"
	ZoneLeft.Enabled=1:ZoneRight.Enabled=1
	Table1_KeyUp(RightFlipperKey)
	CradleLogicRResetTimer.Enabled=0
	If AutoShot <> "SHOT119" Then Eval(AutoShot).intensity=0
End Sub

Sub TimerLShot2_Timer '(Post Pass)
	D "TimerLShot2_"
	TriggerRF.enabled=0
	Table1_KeyDown(LeftFlipperKey)
	CradleLogicLResetTimer.interval=440:CradleLogicLResetTimer.Enabled=1
	CradleRight.Enabled=1
	PostPassLDelayTimer.interval=340:PostPassLDelayTimer.Enabled=1   ' 1500  was 700 440
	TimerLShot2.Enabled=0
End Sub

Sub TimerRShot2_Timer '(Post Pass)
	D "TimerRShot2_"
	TimerRShot2.Enabled=0
	TriggerLF.enabled=0
	Table1_KeyDown(RightFlipperKey)
	CradleLogicRResetTimer.interval=440:CradleLogicRResetTimer.Enabled=1
	CradleLeft.Enabled=1
	PostPassRDelayTimer.interval=340:PostPassRDelayTimer.Enabled=1   ' 450
End Sub

'********************************** AI Shot Priority ****************************************

Dim NextShotR:NextShotR=1
Dim NextShotL:NextShotL=1
Dim AutoShot
Dim fudgefactor:fudgefactor=1

Sub CheckShotPriority(flipper) 'flipper: 0 = left flipper; 1 = right flipper  
	If flipper = 0 Then
		ZoneLeft.Enabled=0
	Else
		ZoneRight.Enabled=0
	End If

	If flipper = 0 Then
		NextShotL=4
		If i107.state=2 then 
			NextShotL=4 ' right orbit
		Else
			If i103.state<>0 Then
				NextShotL=5
			ElseIf i86.state<>0 or F149.state<>0 or F143.state<>0  Then
				NextShotL=8
			ElseIf i34.state<> 0 Then ' next Song
				NextShotL=99
			ElseIf i94.state<>0 Then  ' lock elevator
				NextShotL=7
			ElseIf i100.state<>0 Then  ' lock toybox
				NextShotL=6
			ElseIf i107.state<>0 Then  ' right orbit
				NextShotL=4
			ElseIf i97.state<>0 Then 
				NextShotL=6
			ElseIf i91.state<>0 Then 
				NextShotL=7
			ElseIf i45.state=0 Then  'aerosmith
				NextShotL=3
			ElseIf i81.state<>0 Then
				NextShotL=9
			ElseIf I80.state=0 Then ' go for ToyBox
				NextShotL=9
			ElseIf i44.state=0 Then
				NextShotL=2
			ElseIf i43.state=0 Then
				NextShotL=1
			Else 
				NextShotL=99
			End If
		End If
		'NextShotL=99 '' debug shot	
		If BallsOnPlayfield > 1 then NextShotL=8

		select Case NextShotL
			Case 1:shotval=602		' t8
			Case 2:shotval=599		' t7
			Case 3:shotval=595		' t6
			Case 4:shotval=590		' r orbit 573 580
			Case 5:shotval=562		' r ramp    '562  Good
			Case 6:shotval=536		' lock switch   ' 537
			Case 7:shotval=503		' elevator   ' 413 485 504
			Case 8:shotval=499		' c ramp   '495
			Case 9:shotval=450		' toy box
			Case 10:shotval=240		' t5   220
			Case 11:shotval=208		' t4
			Case 99:shotval=0 		' leftpostpass
		End Select
		AutoShot="SHOT" & LPAD(CSTR(NextShotL),3,"0")
		If NextShotL = 99 Then
			LeftPostPass
		Else
			SoundStrobe AutoShot
			'D "****SHOTTIMERLEFT=" & shotval
			TimerLshot1.interval=shotval*fudgefactor
			TimerLShot1.Enabled=1
		End If
	End If

	If flipper = 1 Then
		NextShotR=9
		If i91.state<>0 Then
			NextShotR=10
		ElseIf i97.state=2 Then
			NextShotR=11 ' switch
		ElseIf i81.state<>0 Then
			NextShotR=8
		ElseIf i62.state<>0 Then
			NextShotR=5
		ElseIf i34.state<> 0 Then ' next Song
			NextShotR=4
		ElseIf i35.state<>0 Then
			NextShotR=4
		ElseIf i86.state<>0 or F149.state <> 0 Then
			NextShotR=9 
		ElseIf i69.state=0 Then
			NextShotR=6
		ElseIf i70.state=0 Then
			NextShotR=7
 		ElseIf (i72.state=0 or i73.state=0) and I75.state=0 Then
			NextShotR=8
		ElseIf i31.state=0 Then
			NextShotR=3 ' 
		ElseIf i30.state=0 Then
			NextShotR=2
		ElseIf i29.state=0 Then
			NextShotR=1
		Else	
			NextShotR=99
		End If
		'NextShotR=99 '' debug shot
		If BallsOnPlayfield > 1 then NextShotR=9
		select Case NextShotR
			Case 1:shotvalRight=565		' t1
			Case 2:shotvalRight=560		' t2   533
			Case 3:shotvalRight=555 	' t3
			Case 4:shotvalRight=554		' CIU   550
			Case 5:shotvalRight=523   	' left orbit  489
			Case 6:shotvalRight=493		' t   
			Case 7:shotvalRight=490		' t   
			Case 8:shotvalRight=436		' toybox   427
			Case 9:shotvalRight=370		' c ramp   ' 370
			Case 10:shotvalRight=334	' elevator  330
			Case 11:shotvalRight=284	' switch    ' 279
			Case 99:shotval=0			' rightpostpass
		End Select
		AutoShot="SHOT" & LPAD(CSTR(20+NextShotR),3,"0")
		If NextShotR = 99 Then
			RightPostPass
		Else
			SoundStrobe AutoShot
			NextShotR=NextShotR+1
			'D "****SHOTTIMERRIGHT=" & shotvalRight		
			TimerRshot1.interval=shotvalRight*fudgefactor
			TimerRShot1.Enabled=1
		End If
	End If
End Sub

Sub TimerLShot1_Timer 
	Table1_KeyDown(LeftFlipperKey)
	If AutoShot <> "SHOT099" Then Eval(AutoShot).intensity=2000
	TimerLShot1.Enabled=0:CradleLogicLResetTimer.Enabled=1
End Sub

Sub TimerRShot1_Timer 
	Table1_KeyDown(RightFlipperKey)
	If AutoShot <> "SHOT119" Then Eval(AutoShot).intensity=2000
	TimerRShot1.Enabled=0:CradleLogicRResetTimer.Enabled=1
End Sub


Sub SoundStrobe(Shot)   
		Eval(Shot).intensity=1501
End Sub

'*****************************************************************
'   **********  PUPDMD  MODIFY THIS SECTION!!!  ***************
'PUPDMD Layout for each Table1
'Setup Pages.  Note If you use fonts they must be in FONTS folder of the pupVideos\tablename\FONTS  "Case sensitive exact naming fonts!"
'*****************************************************************

Sub pSetPageLayouts

	DIM dmddef
	DIM dmdalt
	DIM dmdscr
	DIM dmdfixed
	DIM digitLCD
	DIM dmdFont


'labelNew <screen#>, <Labelname>, <fontName>,<size%>,<colour>,<rotation>,<xalign>,<yalign>,<xpos>,<ypos>,<PageNum>,<visible>
'***********************************************************************'
'<screen#>, in standard wed set this to pDMD ( or 1)
'<Labelname>, your name of the label. keep it short no spaces (like 8 chars) although you can call it anything really. When setting the label you will use this labelname to access the label.
'<fontName> Windows font name, this must be exact match of OS front name. If you are using custom TTF fonts then double check the name of font names.
'<size%>, Height as a percent of display height. 20=20% of screen height.
'<colour>, integer value of windows color.
'<rotation>, degrees in tenths   (900=90 degrees)
'<xAlign>, 0= horizontal left align, 1 = center horizontal, 2= right horizontal
'<yAlign>, 0 = top, 1 = center, 2=bottom vertical alignment
'<xpos>, this should be 0, but If you want to force a position you can set this. it is a % of horizontal width. 20=20% of screen width.
'<ypos> same as xpos.
'<PageNum> IMPORTANT this will assign this label to this page or group.
'<visible> initial state of label. visible=1 show, 0 = off.

' Overlay
	'puPlayer.LabelInit pOverlayFrame

' Backglass - this is basically the FullLCD-DMD
'		------------------------------------------------------------------------
' Prgrs |           		  										 Ball
' 		|       						MessageT			    	MTimer
'		|  Time
'		| 		    
'		| 								 Message					Aerosmith
'		|							     							Timer
'		|	                            Play1Score
'		|	      Play1Score                            Play2Score
'		|	Play1Score                  Play2Score			       Play3Score
'		|	Play1Score       Play2Score			Play3Score	          Play4Score
'		-------------------------------------------------------------------------
'
	digitLCD=DigitFont
    dmdFont=DMDMainFont
  

'					   Scrn LblName    Fnt    			Size	Color	 						R,AxAy,X,Y,pagenum,Visible 
	PuPlayer.LabelInit pBackglass
	PuPlayer.LabelNew pBackglass,"ModeProgress", DMDScrFont,10*FontScale,RGB(255, 255, 255)  	,0,0,0 ,0,0    ,1,0					' Mode Progress Image (NOT VISIBLE)
	PuPlayer.LabelSet pBackglass,"ModeProgress", "",0,"{'mt':2,'color':111111,'width':2, 'height':2, 'yalign':0,'ypos':1.0,'xpos':0,'pagenum':1}"
	puPlayer.LabelNew pBackglass,"Aerosmith",DMDScrFont,	7*FontScale,RGB(255, 255, 255)	,0,1,0 ,0,0     ,1,1					


	puPlayer.LabelNew pBackglass,"MessageT",DMDScrFont,		8*FontScale,RGB(255, 255, 255)	,0,1,0 ,0,16    ,1,1						' Top middle message
	puPlayer.LabelNew pBackglass,"Ball"    ,DMDScrFont,		8*FontScale,RGB(255, 255, 255) 	,0,1,0 ,88,1    ,1,1
	puPlayer.LabelNew pBackglass,"MTimer"  ,DMDScrFont,		10*FontScale,RGB(255, 255, 255)	,0,2,0 ,85,18   ,1,1						' PF Multiplier


	PuPlayer.LabelNew pBackglass,"JesterImage",DMDScrFont,		7*FontScale,RGB(255, 255, 255)	,0,1,0 ,0,0     ,1,1	
	PuPlayer.LabelSet pBackglass,"JesterImage","PupOverlays\\clear.png",1,"{'mt':2,'width':100,'height':100,'xpos':0,'ypos':0}"

	PuPlayer.LabelNew pBackglass,"Jester"	,DMDScrFont,	 8*FontScale,RGB(255,255,80)	,0,2,0,7.2,45.2	,1,1

	PuPlayer.LabelNew pBackglass,"DiceImage",DMDScrFont,	 7*FontScale,RGB(255, 255, 255)	,0,1,0 ,0,0     ,1,1
	PuPlayer.LabelSet pBackglass,"DiceImage","PuPOverlays\\clear.png",1,"{'mt':2,'width':100,'height':100,'xpos':0,'ypos':0}"
	
	PuPlayer.LabelNew pBackglass,"SpinHits"	,DMDScrFont,	 8*FontScale,RGB(255,255,255)	,0,2,0,7.2,57.8 ,1,1

	PuPlayer.LabelNew pBackglass,"CoinImage",DMDScrFont,	7*FontScale,RGB(255, 255, 255)	,0,1,0 ,0,0     ,1,1
	PuPlayer.LabelSet pBackglass,"CoinImage","PupOverlays\\clear.png",1,"{'mt':2,'width':100,'height':100,'xpos':0,'ypos':0}" 

	PuPlayer.LabelNew pBackglass,"Coins"	,DMDScrFont,	 8*FontScale,RGB(255,255,255)	,0,2,0,98.4,32.2,1,1

	PuPlayer.LabelNew pBackglass,"SpinScore",DMDScrFont,	 5*FontScale,RGB(255,255,255)	,0,2,0,5.7,70	,1,1
	PuPlayer.LabelNew pBackglass,"PopHits"	,DMDScrFont,	 8*FontScale,RGB(255,255,80)	,0,2,0,99.5,47	,1,1
	PuPlayer.LabelNew pBackglass,"PopScore"	,DMDScrFont,	 5*FontScale,RGB(255,255,255)	,0,2,0,99.5,66	,1,1

	puPlayer.LabelNew pBackglass,"Timer"   ,DMDScrFont,		10*FontScale,RGB(255, 255, 255) ,0,1,0 ,0,75    ,1,1						' Used for the counddown on the hurry up
	puPlayer.LabelNew pBackglass,"Message" ,DMDScrFont,		18*FontScale,RGB(255, 255, 255) ,0,1,0 ,0,65    ,1,1						' Shows how many points we got on the hurry up timer
	puPlayer.LabelNew pBackglass,"Time"	   ,DMDScrFont,		10*FontScale,RGB(255, 255, 255) ,0,1,0,11.2,11.3,1,1						' Shows remaining time 

	puPlayer.LabelNew pBackglass,"Message1",DMDScrFont,		15*FontScale,RGB(181, 152, 206)	,0,1,1 ,0,25   ,1,1
	puPlayer.LabelNew pBackglass,"Message2",DMDScrFont,		18*FontScale,RGB(20, 20, 255)	,0,1,1 ,0,50   ,1,1
	puPlayer.LabelNew pBackglass,"Message3",DMDScrFont,		10*FontScale,RGB(255, 25, 0)	,0,1,1 ,0,70   ,1,1

	puPlayer.LabelNew pBackglass,"BigScore",DMDScrFont,		18*FontScale,RGB(255, 255, 255)	,0,1,1 ,50,50  ,1,1
	puPlayer.LabelNew pBackglass,"Play1Score",DMDScrFont,	 8*FontScale,RGB(255, 255, 255)	,0,1,2 ,0 ,98  ,1,1
	puPlayer.LabelNew pBackglass,"Play2Score",DMDScrFont,	 8*FontScale,RGB(255, 255, 255) ,0,1,2 ,30,98  ,1,1 
	puPlayer.LabelNew pBackglass,"Play3Score",DMDScrFont,	 8*FontScale,RGB(255, 255, 255) ,0,1,2 ,62,98  ,1,1
	puPlayer.LabelNew pBackglass,"Play4Score",DMDScrFont,	 8*FontScale,RGB(255, 255, 255) ,0,1,2 ,90,98  ,1,1

	PuPlayer.LabelNew pBackglass,"HighScore1",dmdFont,  	12*FontScale,RGB(255, 255, 255) ,0,1,0 ,0,25   ,2,1	
	PuPlayer.LabelNew pBackglass,"HighScore2",dmdFont,  	12*FontScale,RGB(255, 255, 255) ,0,1,0 ,0,45   ,2,1	
	PuPlayer.LabelNew pBackglass,"HighScore3",dmdFont,  	12*FontScale,RGB(255, 255, 255) ,0,1,0 ,0,75   ,2,1			
	PuPlayer.LabelShowPage pBackglass, 2,0,""

	PuPlayer.LabelNew pBackglass,"EnterHS1",dmdFont,   		14*FontScale,RGB(255, 255, 255) ,0,1,0 ,0,25   ,3,1				' HS Entry
	PuPlayer.LabelNew pBackglass,"EnterHS2",dmdFont,  		14*FontScale,RGB(255, 255, 255) ,0,1,0 ,0,45   ,3,1				' HS Entry
	PuPlayer.LabelNew pBackglass,"EnterHS3",dmdFont,  		14*FontScale,RGB(255, 255, 255) ,0,1,0 ,0,75   ,3,1				' HS Entry

	' Page 1 (OverVideo)
	PuPlayer.LabelInit pOverVid
	PuPlayer.LabelNew pOverVid,"OverMessage1",dmdFont,  	8*FontScale,RGB(250,250,250)  	,0,1,1 ,0,25   ,1,1			' Attract mode and other text over a video
	PuPlayer.LabelNew pOverVid,"OverMessage2",dmdFont,  	10*FontScale,RGB(250,250,250)  	,0,1,1 ,0,50   ,1,1			' 
	PuPlayer.LabelNew pOverVid,"OverMessage3",dmdFont,  	 8*FontScale,RGB(250,250,250)  	,0,1,1 ,0,70   ,1,1			' 
	PuPlayer.LabelShowPage pOverVid, 1,0,""

	' Page 1 (OverVideo)
	PuPlayer.LabelInit pPopUP5
	PuPlayer.LabelNew pPopUP5,"TOverMessage1",dmdFont,  	12*FontScale,RGB(255, 255, 255) ,0,1,1 ,0,25  ,1,1			' Transparent Over Video
	PuPlayer.LabelNew pPopUP5,"TOverMessage2",dmdFont,  	15*FontScale,RGB(255, 255, 255) ,0,1,1 ,0,50  ,1,1			' 
	PuPlayer.LabelNew pPopUP5,"TOverMessage3",dmdFont,  	 8*FontScale,RGB(255, 255, 255) ,0,1,1 ,0,70  ,1,1			' 
	PuPlayer.LabelShowPage pOverVid, 1,0,""

	puPlayer.LabelNew pOverVid,"ScorbitQR", 	dmdFont,		 1,RGB(247, 170, 51)			,0,2,0 ,0,0    ,1,1
	puPlayer.LabelNew pOverVid,"ScorbitQRIcon", dmdFont,		 1,RGB(247, 170, 51)			,0,2,0 ,0,0    ,1,1


' Page 3 (OverVideo - Finish Mode)
'		-------------------------------------------------------------------------
'		| 							 
'		|	Scattered 
'		|
'		-------------------------------------------------------------------------
	puPlayer.LabelNew pOverVid,"F0",dmdFont,				5,RGB(255, 255, 255)  	,0,0,0 ,0,0   ,3,1				' 
	puPlayer.LabelNew pOverVid,"F1",dmdFont,				5,RGB(255, 255, 255)  	,0,0,0 ,0,0   ,3,1				' 
	puPlayer.LabelNew pOverVid,"F2",dmdFont,				5,RGB(255, 255, 255)  	,0,0,0 ,0,0   ,3,1				' 
	puPlayer.LabelNew pOverVid,"F3",dmdFont,				5,RGB(255, 255, 255)  	,0,0,0 ,0,0   ,3,1				' 
	puPlayer.LabelNew pOverVid,"F4",dmdFont,				5,RGB(255, 255, 255)  	,0,0,0 ,0,0   ,3,1				' 
	puPlayer.LabelNew pOverVid,"F5",dmdFont,				5,RGB(255, 255, 255)  	,0,0,0 ,0,0   ,3,1				' 
	puPlayer.LabelNew pOverVid,"F6",dmdFont,				5,RGB(255, 255, 255)  	,0,0,0 ,0,0   ,3,1				' 
	puPlayer.LabelNew pOverVid,"F7",dmdFont,				5,RGB(255, 255, 255)  	,0,0,0 ,0,0   ,3,1				' 
	puPlayer.LabelNew pOverVid,"F8",dmdFont,				5,RGB(255, 255, 255)  	,0,0,0 ,0,0   ,3,1				' 




' Page 1 (Bonus)
'		-------------------------------------------------------------------------
'		| 							    MessageT							 
'		|  							    
'		|   
'		|
'		|								 Message1
'		|							     Message2
'		|
'		-------------------------------------------------------------------------
'
	puPlayer.LabelInit pBonusScreen

	puPlayer.LabelNew pBonusScreen,"Message1" ,DMDScrFont,		15*FontScale,RGB(255, 255, 0)  	,0,1,0 ,0,25   ,1,1						' Shows the Bonus
	puPlayer.LabelNew pBonusScreen,"Message2" ,DMDScrFont,		15*FontScale,RGB(255, 255, 255) ,0,1,0 ,0,70   ,1,1						' Shows the bonus 2nd line
	PuPlayer.LabelShowPage pBonusScreen,1,0,""

	If PuPDMDDriverType=pDMDTypeLCD THEN  'Using 4:1 Standard ratio LCD PuPDMD  ************ lcd **************
		D "pDMDTypeLCD"
		dmdalt="PKMN Pinball"    
		dmdfixed="Instruction"
		dmdscr="Impact"  'main score font
		dmddef="Impact"

	'Page 1 (default score display)
	'					   Scrn LblName    Fnt    Size	Color	 R AxAy X Y pagenum Visible 
		 PuPlayer.LabelNew pDMD,"Player"  ,dmddef,21*FontScaleDmd	,RGB(3, 57, 252)	,1,0,0, 0,0,	1,	0
		 PuPlayer.LabelNew pDMD,"Ball"    ,dmddef,21*FontScaleDmd	,RGB(3, 57, 252)	,1,2,0, 0,0,	1,	0
		 PuPlayer.LabelNew pDMD,"CurScore",dmddef,50*FontScaleDmd	,RGB(255, 255, 255),0,1,2, 0,60,	1,	0
		 PuPlayer.LabelNew pDMD,"Status"  ,dmddef,30*FontScaleDmd	,RGB(255, 255, 255)	,0,0,2, 0,0,	1,	0
 		 PuPlayer.LabelNew pDMD,"Credits" ,dmddef,21*FontScaleDmd	,RGB(3, 57, 252)	,0,2,2, 0,0,	1,	0

		puPlayer.LabelNew pDMD,"ScorbitQR",     dmddef,		 1,RGB(247, 170, 51)			,0,2,0 ,0,0    ,1,1
		puPlayer.LabelNew pDMD,"ScorbitQRicon", dmddef,		 1,RGB(247, 170, 51)			,0,2,0 ,0,0    ,1,1


	'Page 2 (default Text Splash 1 Big Line)
		PuPlayer.LabelNew pDMD,"Splash"  ,dmdalt,40*FontScaleDmd,33023,0,1,1,0,0,2,0

	'Page 3 (default Text 3 Lines)
		PuPlayer.LabelNew pDMD,"Splash3a",dmddef,30*FontScaleDmd,8454143,0,1,0,0,2,3,0
		PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,30*FontScaleDmd,33023,0,1,0,0,30,3,0
		PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,25*FontScaleDmd,33023,0,1,0,0,57,3,0


	'Page 4 (default Text 2 Line)
		PuPlayer.LabelNew pDMD,"Splash4a",dmddef,40*FontScaleDmd,8454143,0,1,0,0,0,4,0
		PuPlayer.LabelNew pDMD,"Splash4b",dmddef,30*FontScaleDmd,33023,0,1,2,0,75,4,0

	'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
		PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80*FontScaleDmd,8421504,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80*FontScaleDmd,65535  ,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80*FontScaleDmd,65535  ,0,1,1,0,0,5,0

	'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
		PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90*FontScaleDmd,65280,0,0,0,15,1,6,0
		PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50*FontScaleDmd,33023,0,1,0,60,0,6,0
		PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40*FontScaleDmd,33023,0,1,0,60,50,6,0

	'Page 7 (Show High Scores Fixed Fonts)
		PuPlayer.LabelNew pDMD,"Splash7a",dmddef,20*FontScaleDmd,8454143,0,1,0,0,2,7,0
		PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,40*FontScaleDmd,33023,0,1,0,0,20,7,0
		PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,40*FontScaleDmd,33023,0,1,0,0,50,7,0
	END IF  ' use PuPDMDDriver
End Sub


' Background base
const kDMD_Attract			=454  ' blank.png  -   gotg=default.mp4
const kDMD_PlayerMode		=456  ' PlayerOverlay

const kDMD_Mystery	=599


const kDMD_BonusBG			=479   ' blank.png
const kDMD_BonusClear		=480   ' blank.png

const kDMD_last		=521
const kDMD_walk		=522
const kDMD_same		=523
const kDMD_sweet	=524
const kDMD_dude		=525
const kDMD_back		=526
const kDMD_rats		=527
const kDMD_toys		=528
const kDMD_love		=529


'************************ called during gameplay to update Scores ***************************
Sub pUpdateScores 
Dim StatusStr, NameStr
	'D "Sub pUpdateScores PlayerMode:" & PlayerMode &  " 2:" & PlayerMode2 & "BonusMode:" & BonusMode

	if pDMDCurPage <> pScores then Exit Sub
	StatusStr=""

	if tmrMedleyTour.Enabled  then
		if WizardHits > 0 Then
		  Select case PlayerMode3:
			case 0:	StatusStr = "LOOPS LEFT:" & WizardHits
			case 1:	StatusStr = "SHOTS LEFT:" & WizardHits
			case 2: StatusStr = "TARGETS LEFT:" & WizardHits
			case 3: StatusStr = "RAMPS LEFT:" & WizardHits
			case 4: StatusStr = "LANES LEFT:" & WizardHits
			case 5: StatusStr = "POPS LEFT:" & WizardHits
			case 6: StatusStr = "SPINS LEFT:" & WizardHits
			case 7: StatusStr = "TOYS LEFT:" & WizardHits
			case 8: StatusStr = "ELEVATOR LEFT:" & WizardHits
		  End Select
		End If
	elseif tmrFinalTour.Enabled  then 
		StatusStr = "CITIES LEFT:" & 24 - FinalTourCount
	elseif bFinalTourReady then 
		StatusStr = " "
	elseif PlayerMode = -1 Then
		StatusStr="Select Song"
		if bToyBoxMultiball Then
			if ToyBoxMBJackpotHits < 18 then
				StatusStr = "TOYS LEFT:" & 18 - ToyBoxMBJackpotHits
			Else	
				StatusStr = ""
			End If
		elseIf bMedleyTourReady Then
			StatusStr = ""
		elseIf bSecondMode then
			StatusStr = ""
		elseIf  bBonusMode Then
			StatusStr = ""

		elseIf bMultiBallMode Then 
			StatusStr = ""
		else
			StatusStr = "Select Song"
		End If 
	else
		StatusStr = "Time:" & ModeCountdownTimer.UserValue & "  " & ModePercent(PlayerMode) & "%"
	end If
	PuPlayer.LabelSet pBackglass,"Ball",	"BALL " & CStr(Balls),1,""

	If bWizardMode Then
		PuPlayer.LabelSet pBackglass,"Coins"    ,"",0,""
		PuPlayer.LabelSet pBackglass,"Jester"   ,"",0,""
		PuPlayer.LabelSet pBackglass,"SpinHits" ,"",0,""
		PuPlayer.LabelSet pBackglass,"SpinScore","",0,""
		PuPlayer.LabelSet pBackglass,"PopHits"  ,"",0,""
		PuPlayer.LabelSet pBackglass,"PopScore" ,"",0,""
		PuPlayer.LabelSet pBackglass,"Coins"    ,"",0,""
	Else
		PuPlayer.LabelSet pBackglass,"Jester"   ,CStr(HiddenJesters),1,""
		PuPlayer.LabelSet pBackglass,"SpinHits" ,CStr(spinHits),1,""
		If SpinScore > 10000 Then
			PuPlayer.LabelSet pBackglass,"SpinScore" ,CSTR(int(SpinScore/1000))+"K",1,""
		Else
			PuPlayer.LabelSet pBackglass,"SpinScore" ,CStr(SpinScore),1,""
		End IF
		PuPlayer.LabelSet pBackglass,"PopHits"  ,CStr(PopHits),1,""
		PuPlayer.LabelSet pBackglass,"PopScore" ,CStr(PopScore),1,""
		PuPlayer.LabelSet pBackglass,"Coins" ,CStr(coins),1,""
	End If

	If playermode = -1 and not bToyBoxMultiBall and not bElevMultiBall and not bBallInPlungerLane and score(currentplayer) <> 0 then
		' todo need to be sure we are not showing a video
		If not PauseBigScore  and not bsecondMode  and not bWizardMode Then
			PuPlayer.LabelSet pBackglass,"BigScore",FormatScore(Score(CurrentPlayer)),1,""
		End If
	End If

	UpdateNumberPlayers() ' To align the values ..

	if ScorbitActive then 
		if Scorbit.bSessionActive then
			NameStr=Scorbit.GetName(CurrentPlayer+1)
			if NameStr<>"" then 
				puPlayer.LabelSet pDMD,"Player", NameStr	,1,""
			Else 
				puPlayer.LabelSet pDMD,"Player", "PLAYER " & CurrentPlayer+1	,1,""
			End if 
		End if 
	Else 
		PuPlayer.LabelSet pDMD,"Player",	"PLAYER " & CurrentPlayer+1				,1,""
	End if 

	PuPlayer.LabelSet pDMD,"Ball",		"BALL " & CStr(Balls)					,1,""
	PuPlayer.LabelSet pDMD,"CurScore",	FormatScore(Score(CurrentPlayer))		,1,""
	puPlayer.LabelSet pDMD,"Status",	StatusStr								,1,"1"
	PuPlayer.LabelSet pDMD,"Credits", 	"CRED " & Credits						,1,""
End Sub

' This is called when they hit the plunger and when they press start after ball is in the scoop
Sub StartPlayerMode()
	dim time, a, i
	D "Sub StartPlayerMode " & PlayerMode
	pDMDSetPage(pScores)
	setCIULight(False)    ' Gets set after the first shot

	bSecondMode = False
	bBonusMode = False

	time = 1000
	playclear pAudio
	ModePoints = 0
	bModeProgressUpgraded = False

	bPlayerModeSelect = False
	pBGGamePlay

	If bUsePUPDMD then 
		D2 ">>>>> Clear the MESSAGE <<<<<"
		vpmtimer.addtimer 1000, "pClearMessageT() '"
	end if

	If (PlayerMode <> -1) then 					' Add this to the stack
		D "Playermode - enable"
		StackState(kStack_Pri0).Enable(PlayerMode)
	End If

	if 	bFinalTourReady=True or (PlayerMode = -1 and bMedleyTourReady) Then
		setModeSelectLight(True)
	Else
		setModeSelectLight(False)
	End If

	Select Case PlayerMode
		Case -1:' No Mode Selected
		Case 0: 
			DMD "Start Mode", "LAST CHILD", "", eNone, eNone, eNone, time, False, ""
			SSetLightColor kStack_Pri0, I62,  modeRampColor, 2
			SSetLightColor kStack_Pri0, I107, modeRampColor, 2
			SSetLightColor kStack_Pri0, I86,  modeRampColor, 1
			SSetLightColor kStack_Pri0, I103, modeRampColor, 1
		Case 1: 
			DMD "Start Mode", "WALK THIS WAY", "", eNone, eNone, eNone, time, False, ""
			SSetLightColor kStack_Pri0, I86, modeRampColor, 2
			SSetLightColor kStack_Pri0, I107,modeRampColor, 1
			SSetLightColor kStack_Pri0, I62, modeRampColor, 1
		Case 2: 
			DMD "Start Mode", "SAME OLD", "", eNone, eNone, eNone, time, False, ""
			SSetLightColor kStack_Pri0, aRampLights(bRndModeShot),  modeRampColor, 2
		Case 3: ' sweet   4 eject holes
			DMD "Start Mode", "SWEET EMOTION", "", eNone, eNone, eNone, time, False, ""
			SSetLightColor kStack_Pri0, I35, modeRampColor, 2
			SSetLightColor kStack_Pri0, I81, modeRampColor, 2
			SSetLightColor kStack_Pri0, I91, modeRampColor, 2
			SSetLightColor kStack_Pri0, I97, modeRampColor, 2
		Case 4: 
			DMD "Start Mode", "DUDE", "", eNone, eNone, eNone, time, False, ""
			SSetLightColor kStack_Pri0, I86, modeRampColor, 1
			SSetLightColor kStack_Pri0, I91, modeRampColor, 1
		Case 5: 
			DMD "Start Mode", "BACK IN THE SADDLE", "", eNone, eNone, eNone, time, False, ""
			SSetLightColor kStack_Pri0, I107, modeRampColor, 1
		Case 6: ' Rats
			DMD "Start Mode", "RATS IN THE CELLAR", "", eNone, eNone, eNone, time, False, ""
			i=0
			RatsPos1 = Int(RND*4)  ' Lock in a random light for rats mode
			For each a in aRampLights
				If i = RatsPos1 Then
					SSetLightColor kStack_Pri0, a,  modeRampColor, 2
				Else
					SSetLightColor kStack_Pri0, a,  modeRampColor, 0
				End If
				i=i+1
			Next
	End Select

	StartPlayerModeVideo False

	If PlayerMode <> -1 then 
		ModeCountdownTimer.UserValue = ModeCountdown

		pDMDEvent(P+1)
		vpmtimer.addtimer cInt(defBallSaverTime/4), "ModeCountdownTimer.Enabled = True '" ' delay the countdown
	End If 

	ScorbitBuildGameModes()
	D "StartPlayerMode calling SetModeLights"
	SetModeLights()
End Sub


Sub StopPlayerMode()
	D "StopPlayerMode " & PlayerMode & " eMB:" & bElevMultiBall & " toyMB:" & bToyBoxMultiball
	Dim i
	ModeCountdownTimer.Enabled = False
    if not bGameInPlay then Exit Sub

	setCIULight(False)
	if not bElevMultiBall and not bToyBoxMultiball then ' Cant start a new mode while in MB mode
		For i = 0 to 6	' check if one of the modes is not completed then allow them to select
			if ModePercent(i) < 100 then
				D "Still another song available: ModePercent A i=" & i & " %" & ModePercent(i)
				setModeSelectLight(True)
				exit For
			end If
		Next
	End If

	If bUsePUPDMD then 
		' Turn off mode progress 
'		puPlayer.LabelSet pBackglass, "ModeProgress", "",0,""
		' Clear this in Case they finished the mode
		puPlayer.LabelSet pBackglass,"HighScore1", "" ,1,""
		puPlayer.LabelSet pBackglass,"HighScore2", "" ,1,""
		puPlayer.LabelSet pBackglass,"HighScore3", "" ,1,""
	End if

	dim a, debugVal
	debugVal = PlayerMode  ' This will show in the debugger on crash
	For each a in aRampLights				' Turn off all the ramp lights to reset the mode
		SSetLightColor kStack_Pri0, a, "white", 0
	Next

	Select Case PlayerMode
		Case -1:' No Mode Selected
		Case 0: ' last
        Case 1: ' walk
				ModeProgress(PlayerMode) = 10
				ModePercent(PlayerMode) = 100
				bBonusMode=True
        Case 2: ' same
			D "Checking if aerosmith lights need to be turned off"
			If bModeProgressUpgraded or NOT bHardMode Then 'If upgraded then aerosmithlights get enabled for this mode 
				D "restore the aerosmithlights"
				i=0
				for each a in aerosmithLights
					a.state=HoldAerosmithLights(i)
					i=i+1
				Next
			End If
		Case 3: ' sweet
			If bHardMode=True Then
				If ModePercent(PlayerMode) < 100 Then ' We didnt complete the mode timer expired, start over 
					ModeProgress(PlayerMode) = 0  
					ModePercent(PlayerMode) = 0
				End If
			End If  
        Case 4: ' dude
        Case 5: ' back
				ModeProgress(PlayerMode) = 8
				ModePercent(PlayerMode) = 100
				bBonusMode=True
        Case 6: ' rats
				ModeProgress(PlayerMode) = 8
				ModePercent(PlayerMode) = 100
				bBonusMode=True
	End Select
	
	
    D "PlayerMode: " & PlayerMode
	if PlayerMode >= 0 Then
		If ModeProgress(PlayerMode) > 0 Then ' save it in the Array
			for a = 0 to 8
				If ModeOrder(a)=-1 or ModeOrder(a)=PlayerMode Then
					ModeOrder(a)=PlayerMode
					D "Saving PlayerMode " & PlayerMode & " as #" & a & " mode played"
					Exit For
				End If
			Next
		End If
	End If

	if not bWizardMode Then
		' Turn off all the Mode Lights
		I15.State = 0:I16.State = 0:I17.State = 0:I18.State = 0
		I19.State = 0:I20.state = 0:I21.State = 0
		D "StopPlayerMode calling SetModeLights"
		SetModeLights()			' Set mode lights based on progress
	End If

	If PlayerMode <> -1 then 
		StackState(kStack_Pri0).Disable  ' Disable this stack level
	End If 

								
	If PlayerMode <> -1 Then	' Start 2nd Mode
		D "2nd Mode? " & ModePercent(PlayerMode) & " 2ndMode:" & bSecondMode & " BMode:" & bBonusMode
		If ModePercent(PlayerMode) >= 100 and bBonusMode then  
			PlayerMode2 = PlayerMode   ' +100 to show we are in ShotMode
			StartShotMode()  
		Else 
			' if in elevMB then dont allow them to start a new mode until 1 ball in play
			If not bElevMultiBall and not bToyBoxMultiball Then  ' Shot the last vid and allow them to select a new song
				StopPlayerModeVideo   ' todo is this condition needed?
				For i = 0 to 6	' check if one of the modes is not completed then allow them to select
					If ModePercent(i) < 100 then
						D "Still another song available: ModePercent B i=" & i & " %" & ModePercent(i)
						setModeSelectLight(True)
						exit For
					End If
				Next
				PuPlayer.LabelSet pBackglass, "ModeProgress", "",0,""  
			End If
		End if
	End If 
	PlayerMode = -1
	D "StopPlayerMode calling SetModeLights"

	CheckWizardModesReady		' See If we need to enable wizard modes
	SetModeLights

	ScorbitBuildGameModes()

End Sub


sub CheckWizardModesReady()
	If bWizardMode then exit sub  ' We are in a wizard mode no need to check to enable one

	D "Sub CheckWizardModesReady..Modes:" & modesStarted & " TourDone:" & bMedleyTourDone
	If modesStarted=9 and bMedleyTourDone=False and not bToyBoxMultiball Then	' Medley (all 7 > 0, toybox and elev multi ball atleast had 1 hit/started)
		D "Setup Medley MB"
		If bMedleyTourReady=False then
			bMedleyTourReady = True
			SceneClearMessage()
			setModeSelectLight(True)
		End If 
		PlayScoopLightSeq
	elseIf modesCompleted=9 and bMedleyTourDone Then	' Complete all modes 
		D "Setup Final Tour"
		bFinalTourReady=True 
		SceneClearMessage()
		setModeSelectLight(True)
		PlayScoopLightSeq
	End If 
End Sub 


Sub StartShotMode()
	D "StartShotMode Mode:" & PlayerMode & " Mode2:" & PlayerMode2
' Turn on All Shots
' Get One to go to Super mode
	Dim a
	StackState(kStack_Pri0).Enable(PlayerMode2)
	For each a in aRampLights
		SSetLightColor kStack_Pri0, a, modeRampColor, 2
	Next
End Sub

Sub StartPlayerMode2()
	D "StartPlayerMode2 : " & PlayerMode2 & " bBonusMode:" & bBonusMode
	Dim a
	If PlayerMode2 <> -1 Then
		'StackState(kStack_Pri0).Enable(PlayerMode2)
		For each a in aRampLights
			SSetLightColor kStack_Pri0, a, modeRampColor, 0
		Next

		Mode2Percent(PlayerMode2) = 0
		Mode2Total(PlayerMode2) = 0
		Mode2Progress(PlayerMode2) = 0
	End If
	
	ModePoints = 0
	StartPlayerModeVideo False

	Select Case PlayerMode2:
		Case -1:
		Case 0: ' last
			Mode2Value(PlayerMode2)=500000+(50*Coins)
			SetLightColor F151, "cyan", 2
			SetLightColor F150, "cyan", 2
			ShowPlayerMode2(PlayerMode2)
		Case 1: ' walk
			Mode2Value(PlayerMode2)=500000+(50*Coins)
			SetLightColor F143, "orange", 2
			SetLightColor F149, "orange", 2
			ShowPlayerMode2(PlayerMode2)
		Case 2: ' same
			Mode2Value(PlayerMode2)=50000+(50*Coins)
			FlashLevel(4) = 1 : Flasherflash4_Timer
			ShowPlayerMode2(PlayerMode2)
		Case 3: ' scoring
			Mode2Value(PlayerMode2)=500000+(50*Coins)
			FlashLevel(2) = 1 : Flasherflash2_Timer
			FlashLevel(3) = 1 : Flasherflash3_Timer
			ShowPlayerMode2(PlayerMode2)
		Case 4:
			Mode2Value(PlayerMode2)=500000+(50*Coins)
			setDudeLight(True)
			ShowPlayerMode2(PlayerMode2)
		Case 5: ' saddle
			Mode2Value(PlayerMode2)=250000+(50*Coins)
			SetLightColor F144, "purple", 2
			ShowPlayerMode2(PlayerMode2)
		Case 6: ' rats 
			Mode2Value(PlayerMode2)=5000+(50*Coins)
			SetLightColor F141, "red", 2
			ShowPlayerMode2(PlayerMode2)
	End Select
End Sub

Sub ShowPlayerMode2(mode)
	Select Case mode 
		Case 0: 
			If 10-Mode2Progress(mode) <=0 Then
				QueueScene "SceneBMessage """","""","""" '", 100,2
			Else
				QueueScene "SceneBMessage ""SUPER LOOPS"",""VALUE:" & Mode2Value(mode) & """, ""HITS LEFT: " & cStr(10-mode2progress(mode)) & """ '", 500,2
			End If
			If not bSecondMode Then QueueScene "SceneClearPlayMessage '", 0, 2
			If Mode2Progress(mode)>= 10 Then
				SetLightColor F151, "cyan", 0
				SetLightColor F150, "cyan", 0
			End If 
		Case 1:
			If 10-Mode2Progress(mode) <=0 Then
				QueueScene "SceneBMessage """","""","""" '", 100,2
			Else
				QueueScene "SceneBMessage ""SUPER RAMPS"",""VALUE:" & Mode2Value(mode) & """, ""HITS LEFT: " & cStr(10-mode2progress(mode)) & """ '", 500,2
			End If
			If not bSecondMode Then QueueScene "SceneClearPlayMessage '", 0, 2
			If Mode2Progress(mode)>= 10 Then
				SetLightColor F143, "orange", 0
				SetLightColor F149, "orange", 0
			End If
		Case 2:
			If 20-Mode2Progress(mode) <=0 Then
				QueueScene "SceneBMessage """","""","""" '", 100,2
			Else
				QueueScene "SceneBMessage ""SUPER TARGETS"",""VALUE:" & Mode2Value(mode) & """, ""HITS LEFT: " & cStr(20-mode2progress(mode)) & """ '", 500,2
			End If
			If not bSecondMode Then QueueScene "SceneClearPlayMessage '", 0, 2
			If Mode2Progress(mode)>= 20 Then
				FlashLevel(4) = 0: Flasherflash4_Timer
			End If
		Case 3:
			If 20-Mode2Progress(mode) <=0 Then
				QueueScene "SceneBMessage """","""","""" '", 100,2
			Else
				QueueScene "SceneBMessage ""SUPER SCORING"","""", ""HITS LEFT: "& cStr(20-mode2progress(mode)) & """ '", 100,2
			End If
			If not bSecondMode Then QueueScene "SceneClearPlayMessage '", 0, 2
			If Mode2Progress(mode)>= 100 Then
				FlashLevel(2) = 0 : Flasherflash2_Timer
				FlashLevel(3) = 0 : Flasherflash3_Timer
			End If
		Case 4:
			If 10-Mode2Progress(mode) <=0 Then
				QueueScene "SceneBMessage """","""","""" '", 100,2
			Else
				QueueScene "SceneBMessage ""SUPER LANES"",""VALUE:" & Mode2Value(mode) & """, ""HITS LEFT: " & cStr(10-mode2progress(mode)) & """ '", 500,2
			End If
			If not bSecondMode Then QueueScene "SceneClearPlayMessage '", 0, 2
			If Mode2Progress(mode)>= 10 Then
				setDudeLight(False)
			End If
		Case 5:
			If 50-Mode2Progress(mode) <=0 Then
				QueueScene "SceneBMessage """","""","""" '", 100,2
			Else
				QueueScene "SceneBMessage ""SUPER POPS"",""VALUE:" & Mode2Value(mode) & """, ""HITS LEFT: " & cStr(50-mode2progress(mode)) & """ '", 500,2
			End If
			If not bSecondMode Then QueueScene "SceneClearPlayMessage '", 0, 2
			If Mode2Progress(mode)>= 50 Then
				SetLightColor F144, "purple", 0
			End If
		Case 6:
			If 100-Mode2Progress(mode) <=0 Then
				QueueScene "SceneBMessage """","""","""" '", 100,2
			Else
				QueueScene "SceneBMessage ""SUPER SPINNERS"",""VALUE:" & Mode2Value(mode) & """, ""HITS LEFT: " & cStr(100-mode2progress(mode)) & """ '", 100,2
			End If
			If not bSecondMode Then QueueScene "SceneClearPlayMessage '", 0, 2
			If Mode2Progress(mode)>= 100 Then
				SetLightColor F141, "red", 0
			End If
	End Select
End Sub

Sub StopPlayerModeVideo()	' chooses the wait video 
	D "StopPlayerModeVideo. (Mix Music/video)"	
	If bGameInPlay = false then exit sub  

	If bToyBoxMultiball or bElevMultiBall Then
		D " Still in MB .. so let the MB video continue"
	else
		playclear pBackglass
		playmedia "Video-0x0000.mp4", "PupVideos", pBackglass, "", -1, "", 1, 1
	end If
End Sub 


Sub SelectPlayerMode(keycode)
	Dim cnt
	D "SelectPlayerMode " & (PlayerMode)
	cnt = 0

    If keycode = LeftFlipperKey Then
        PlaySoundVol "fx_Previous", VolDef
		Do
			cnt=cnt+1
			PlayerMode = (PlayerMode + 1) 
			If (PlayerMode > 6) Then PlayerMode=0
		Loop While ModePercent(PlayerMode) >= 100 and cnt<7  and ModePercent(PlayerMode) = -1  'ONLY SELECT NON STARTED MODES
        UpdatePlayerMode
    End If
    If keycode = RightFlipperKey Then
        PlaySoundVol "fx_Next", VolDef
		Do 
			cnt=cnt+1
			PlayerMode = (PlayerMode - 1)
			If (PlayerMode < 0) then PlayerMode=6
		Loop While ModePercent(PlayerMode) >= 100 and cnt<8
        UpdatePlayerMode
    End If
	D "SelectPlayerMode Done .." & PlayerMode
End Sub

Function GetModeIndex(lightArrowName)  
	GetModeIndex = -1
	Select Case lightArrowName
		Case "I35": 		 	' CIU 
			GetModeIndex = 0
        Case "I62":  			' left orbit
			GetModeIndex = 1
        Case "I81":  			' toy box
			GetModeIndex = 2	
        Case "I86": 			' center ramp
			GetModeIndex = 3 
        Case "I91": 	 		' Elevator
			GetModeIndex = 4
        Case "I97": 			' toybox kicker
			GetModeIndex = 5
        Case "I103": 			' right ramp
			GetModeIndex = 6 
        Case "I107": 			' right orbit
			GetModeIndex = 7
		Case "I94":
			GetModeIndex = 8 	' elevator lock
	End Select
end Function


Sub EndFinalTour()   '
	dim a, i
	D "sub EndFinalTour() Enabled:" & tmrFinalTour.Enabled
	if tmrFinalTour.Enabled Then
		tmrFinalTour.Enabled = False 
		bWizardMode = False
		StopPlayerModeVideo

		'PlaySound "YourTimeIsUp"

		For each a in aRampLights				' Turn off all the ramp lights to reset the mode
			SSetLightColor kStack_Pri2, a, "white", 0
		Next
		StackState(kStack_Pri2).Disable

		' Reset all the modes back to 0 and light the FinalTour light solid
		for i = 0 to 8 
			ModeProgress(i) = 0
			ModePercent(i) = -1
			Mode2Percent(i) = -1
			Mode2Progress(i) = 0
			Mode2Value(i) = 0
			Mode2Total(i) = 0
			ModeOrder(i) = -1
		Next
'**
		I15.State = 0
		I16.State = 0
		I17.State = 0
		I18.State = 0
		I19.State = 0
		I20.State = 0
		I21.State = 0
		I22.State = 0
		I23.State = 0

		bFinalTourDone = False
		FinalTourCount=0
 		bFinalTourReady=False

		SetLightColor I23, "pink", 0	' 
		SetLightColor I22, "green", 0	

		SetLightColor F147,"white", 2  ' Toys Light
		SetLightColor I110,"white", 2  ' Elevator Lights
		SetLightColor I65, "white", 2

		RatsStep=1 ' Go from 1L,0R to 0L,1R to 2L,0R, etc

		' Clear shot multipliers 
		I32.UserValue =0
		I67.UserValue =0
		I84.UserValue =0
		I95.UserValue =0
		I46.UserValue =0
		I112.UserValue=0
		I101.UserValue=0
		I89.UserValue =0

		modesCompleted=0
		setCIULight(False)
		setModeSelectLight(True)
		WizardModeStage=0
		WizardModesComplete=0
		ElevMultiBallCount=0

		ToyBoxMBJackpotHits = 0

		bMedleyTourCount=0
		bMedleyTourDone = False
		bMedleyTourReady = False

		For i = 0 to 8
			bMedleyTourProgress(i)=False
		Next

		ScorbitBuildGameModes()
		D "EndFinalTour() - Done. Verify Final Tour is over"
	end if
End Sub

'********************
' Music
'********************

Dim Song
Song = ""

Const Llast=1
Const Walk=2
Const Same=3
Const Sweet=4
Const Dude=5
Const Back=6
Const Rats=7
Const Toys=8
Const Love=9

Dim bPlayPaused
bPlayPaused = False

Sub PlaySong(name)
	Dim PlayLength



	PlayLength = -1
	bPlayPaused=False
	If name <> "" then Song = name
	If bUsePUPDMD then 			' Use Pup If we have it so we can pause the music
		StopSound Song	' Stop the old song
		playmedia Song, MusicDir, pMusic, "", -1, "", 1, 1   
	Else
		EndMusic
		D "Tring to play ...Aerosmith\" & Song
		PlayMusic "Aerosmith\" & Song
	End If
End Sub

Sub SongPause_Timer  ' If we ever need to pause the song
  SongPause.enabled=False
End Sub

Sub Table1_MusicDone
	PlayMusic "Aerosmith\" & Song
End Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  UTILITY - Video Manager & skipper
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 
'	Const pTopper=0			' show, 
'	Const pDMD=1			' ForceBack
'	Const pBackglass=2		' ForceBack
'	Const pPlayfield=3		' off
'	Const pMusic=4			' Music Only
'	Const pAudio=5			' Music Only
'	Const pCallouts=8		' Music Only
'	Const pOvervid=14		' ForcePopBack
'	Const pGame=15			' ForcePop - In Game mini game

	dim currentqueue
	dim cineon:cineon = 0
	dim skipped:skipped = 0
	dim bCancelNext:bCancelNext=False
	dim bMediaPaused(19)
	dim bMediaSet(19)
	bMediaPaused(0)=False:bMediaPaused(1)=False:bMediaPaused(2)=False:bMediaPaused(3)=False:bMediaPaused(4)=False:bMediaPaused(5)=False
	bMediaPaused(6)=False:bMediaPaused(7)=False:bMediaPaused(8)=False:bMediaPaused(9)=False:bMediaPaused(10)=False:bMediaPaused(11)=False
	bMediaPaused(12)=False:bMediaPaused(13)=False:bMediaPaused(14)=False:bMediaPaused(15)=False:bMediaPaused(16)=False:bMediaPaused(17)=False
	bMediaPaused(18)=False:bMediaPaused(19)=False
	bMediaSet(0)=False:bMediaSet(1)=False:bMediaSet(2)=False:bMediaSet(3)=False:bMediaSet(4)=False:bMediaSet(5)=False
	bMediaSet(6)=False:bMediaSet(7)=False:bMediaSet(8)=False:bMediaSet(9)=False:bMediaSet(10)=False:bMediaSet(11)=False
	bMediaSet(12)=False:bMediaSet(13)=False:bMediaSet(14)=False:bMediaSet(15)=False:bMediaSet(16)=False:bMediaSet(17)=False
	bMediaSet(18)=False:bMediaSet(19)=False
	sub pausemedia(channel) 
		If bUsePUPDMD=False then exit sub 

		D "pause media ch:" & channel & " current:" & bMediaPaused(channel)
		If bMediaSet(channel) = False then Exit Sub 
		If bMediaPaused(channel) = False then
			D "pause"
			PuPlayer.playpause channel
			bMediaPaused(channel)=True
		End If 
	End Sub

	sub resumemedia(channel) 
		If bUsePUPDMD=False then exit sub 
		'D "resume media ch:" & channel & " current:" & bMediaPaused(channel)
		If bMediaSet(channel) = False then Exit Sub 
		If bMediaPaused(channel) then
			D "resume"
			PuPlayer.playresume channel
			bMediaPaused(channel)=False
		End If 
	End Sub

'	Function PlayClearDelayed(chan)
'		D "PlayClearDelayed:" & lastPlayMS(chan)
'		PlayClearDelayed = (lastPlayMS(chan) < 400)
'	End Function
			
	sub playclear(chan)
		If bUsePUPDMD=False then exit sub 
		D2 "play clear'd " & chan
		bMediaSet(chan) = False
		bMediaPaused(chan) = False

		If chan = pOverVid then 
			PuPlayer.PlayStop pOverVid
		End If 

		If chan = pAudio Then
			PuPlayer.SetLoop pAudio, 0
			PuPlayer.playstop pAudio
		End If

		If chan = pBonusScreen then 
			PuPlayer.SetBackGround chan, 0
			PuPlayer.SetLoop chan, 0
			PuPlayer.playstop chan
		End If 

		If chan = pMusic Then
			PuPlayer.playstop pMusic
		End If

		If chan = pBackglass Then
			If currentqueue <> "" then 
				bCancelNext = True
				D "Clear is cancelling " & currentqueue
			End If 
			PuPlayer.SetBackGround pBackglass, 0
			PuPlayer.SetLoop pBackglass, 0
			PuPlayer.playstop pBackglass
		End If 
	end Sub


	dim lastvocall:lastvocall=""
	dim noskipper:noskipper=0

	'example playmedia "hs.mp3","audiomultiballs",pAudio,"cineon",10000,"",1,1  // (name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
'			 playmedia "player1-Sound-0x03E0.wav", MusicDir, pAudio, "", -1, "", 1, 1
'			playmedia "","audio-modes",pCallouts,"Sound-0x0458Walk.mp3",1500,"",1,1  ' 
'			playmedia "","audio-modes",pCallouts,"",1500,"",1,1  ' 
Sub playmedia(name,playlist,channel,cinematic,length,nextitem,audiolevel,priority)
	'D "playmedia " & name
		dim bPlayDelayed
		Dim DelayedCmd

		'D "Sub PlayMedia Name:" & name & " pl:" & playlist & " chnl:" & channel & "cine:" & cinematic & " len:" & length & " lvl:" & audiolevel
		If bUsePUPDMD=False then exit sub 
		bMediaSet(channel) = True
		If audiolevel = 1 Then
			If channel = pBackglass Then
				audiolevel = VolBGMusic
				currentqueue = ""
			ElseIf channel = pCallouts Then
				audiolevel = VolDef
			ElseIf channel = pMusic Then
				audiolevel = VolMusic
			ElseIf channel = pAudio Then
				audiolevel = VolBGMusic 
			ElseIf channel = pOverVid Then
				audiolevel = VolBGMusic  
			end If
			audiolevel = audiolevel * 100
		end If

		If channel = pCallouts Then
			If lastvocall <> "" and lastvocall = name  then exit sub
		end if
		
		If nextitem = "" Then
			If cinematic = "cineon" Then
				noskipper=1
				vpmtimer.addtimer length, "nextitems '"
			end If
		Else	
			currentqueue = "playclear " &channel& ":playmedia """ & nextitem &""","""&playlist&""","&channel&","""",-1,"""", "&audiolevel&",1 '"
			vpmtimer.addtimer length, "nextitems '"
			'vpmtimer.AddTimer length, "playclear " &channel& ":playmedia """ & nextitem &""","""&playlist&""","&channel&","""",-1,"""", "&audiolevel&",1 '"
			'vpmtimer.addtimer length, "nextitems '"
			'currentqueue = nextitem
		end If
		If cinematic = "cineon" and length <> -1 Then
			D "ChangeVol pMusic for Cineon"

			skipped=0
			'PuPlayer.playpause 4 ' stop then resume the music
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": " & pMusic &", ""FN"":11, ""VL"":20 }"
			vpmtimer.addtimer length, "turnitbackupcine '"
			GiOff
			cineon = 1
		end If
'   PuPlayer.playlistplayex pCallouts,"audioevents","player1-Sound-0x03E0.wav",100, 1
		'D "PlayMedia Chnl:" & channel & " PList:" & playlist & " Name:" & name & " AudioLevel:" & audiolevel & " Pri:" & priority & " Len:" & length
		
'		if PlayClearDelayed(channel) and (channel = pAudio or channel=pMusic or channel=pDMDFull) then  ' If the clear is delayed then the start needs to be delayed also so it doesnt clear 
Debug.print "Start Delayed"
'			DelayedCmd="PuPlayer.playlistplayex " & channel & ", """ & playlist & """,""" & name & """," & audiolevel & "," & priority
'			if channel = pAudio and length=-1 then DelayedCmd=DelayedCmd & ":PuPlayer.SetLoop " & channel & ", 1"
'			if channel = pDMDFull then DelayedCmd=DelayedCmd & ":PuPlayer.SetBackGround " & channel & ", 1"
'			DelayedCmd=DelayedCmd & " '"
'			VpmTimer.Addtimer kVLCDelay, DelayedCmd
'		Else

D "PuPlayer.playlistplayex channel=" & channel & " playlist:" & playlist & " " & name & " " & audiolevel
			PuPlayer.playlistplayex channel,playlist,name,audiolevel,priority
	
			If channel = pBackglass then PuPlayer.SetBackGround channel, 1
			If channel = pAudio and length=-1 then PuPlayer.SetLoop channel, 1
			If channel = pBonusScreen then PuPlayer.SetLoop channel, 1
'		End If

		If channel = pCallouts and length <> -1 Then
			'D "ChangeVol pBackglass Volume for Callouts"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pBackglass&", ""FN"":11, ""VL"":60 }"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pMusic&",     ""FN"":11, ""VL"":60 }"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pAudio&",     ""FN"":11, ""VL"":60 }"
			vpmtimer.addtimer length, "turnitbackup '"
		end If

		If channel = pOverVid and length <> -1 Then
			'D "ChangeVol pBackglass for OverVid" 
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pBackglass&", ""FN"":11, ""VL"":50 }"
			vpmtimer.addtimer length, "turnitbackupvid '"
		end If

		If channel = pCallouts Then
			lastvocall=name
		end if
	end sub

	Sub turnitbackup
		'D "Change Volume TURNITBACKUP BGMusic:" & VolBGMusic & " Music:" & VolMusic & " Sfx:" & VolSfx
		If bUsePUPDMD then
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pBackglass&", ""FN"":11, ""VL"":"&VolBGMusic*100&" }"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pMusic &",    ""FN"":11, ""VL"":"&VolMusic*100&" }"
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pAudio&",     ""FN"":11, ""VL"":"&VolSfx*100&" }"
		End If
	End Sub

	Sub turnitbackupvid
		D "Change Volume4 BGMusic:" & VolBGMusic
		If bUsePUPDMD Then PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pBackglass&", ""FN"":11, ""VL"":"&VolBGMusic*100&" }"
	End Sub

	Sub turnitbackupcine
		D "Change Volume3 Music" & VolMusic
		If bUsePUPDMD Then PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&pMusic&", ""FN"":11, ""VL"":"&VolMusic*100&" }"
		D "turnitbackupcine "
	End Sub

	sub nextitems
		If bCancelNext then 	' Cancel the last items that was sent to the queue
			D "Cancel Queue"
			bCancelNext=False
			Exit Sub
		End If 
		D "Executing " & currentqueue
		Execute currentqueue
		currentqueue = ""
		bCancelNext = False 	' Items in queue can trigger a cancel
	end Sub

	sub vidskipper_timer		' TBD: Make this work (flippers skip cinematics)
		If cineon = 1  and noskipper = 0 Then
			If ldown = 1 and rdown = 1 Then
				nextitems
				skipped = 1
			end If
		end If
	end Sub


DIM pCurAttractPos: pCurAttractPos=0


'********************** gets called auto each page next and timed already in DMD_Timer.  make sure you use pupDMDDisplay or it wont advance auto.
Sub pAttractNext
	pCurAttractPos=pCurAttractPos+1
	'D "sub pATTRACTNext:" & pCurAttractPos & " " & bGameInPlay
	If bGameInPlay Then PriorityReset=2000
	If bGameInPlay and pCurAttractPos=1 then pCurAttractPos=2		' During InstantInfo skip Intro
	If bGameInPlay and pCurAttractPos=7 then pCurAttractPos=9		' During InstantInfo skip credits 

	If bUsePUPDMD then 'wipe out the GAME OVER
		puPlayer.LabelSet pBackglass,"EnterHS1", " ",1,""
		puPlayer.LabelSet pBackglass,"EnterHS2", " ",1,""
		puPlayer.LabelSet pBackglass,"EnterHS3", " ",1,""
	End If 
  Select Case pCurAttractPos
'			      Name       Line1^Line2.. 	Video			 T  Flash	Priority
  Case 1:
	'D "Case 1"
	pupDMDDisplay "attract", "Insert Coin",	"@vidIntro.mp4" ,9, 1,		10
	PuPlayer.LabelShowPage pOverVid, 1,0,""
	PuPlayer.playlistplayex pOverVid,"PupBackgrounds","Video-0x004A.mp4", 1, 1
	PuPlayer.LabelSet pOverVid,"OverMessage1","LAST SCORE",1,"{'mt':2, 'size': 14 }"
	PuPlayer.LabelSet pOverVid,"OverMessage2","",1,""
	PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(Score(0)),1,"{'mt':2, 'size': 14 }"
  Case 2:
	'D "Case 2"
	if (Scorbit.bNeedsPairing) then 
		PlaySoundVol "scorbit_detected_unclaimed", VolSfx
		PuPlayer.LabelSet pOverVid, "ScorbitQR", "PuPOverlays\\QRcode.png",1,"{'mt':2,'width':32, 'height':64,'xalign':0,'yalign':0,'ypos':5,'xpos':5}"
		PuPlayer.LabelSet pOverVid, "ScorbitQRIcon", "PuPOverlays\\QRcodeS.png",1,"{'mt':2,'width':36, 'height':85,'xalign':0,'yalign':0,'ypos':3,'xpos':3,'zback':1}"
	End if 

	If bGameInPlay=False Then 
		pupDMDDisplay "attract", "Game Over",	"@vidIntro2.mp4",3, 1,		10
		PuPlayer.LabelSet pOverVid,"OverMessage1","",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2","",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage3","",1,""
	End If 
	'test playclear pOverVid
	PuPlayer.playlistplayex pOverVid,"PupBackgrounds","Video-0x0048.mp4", 1, 1

	If osbactive = 1 or osbactive = 2 Then
		PuPlayer.LabelSet pOverVid,"OverMessage1","OSB CHAMPION",1,"{'mt':2, 'size': 15 }"
		PuPlayer.LabelSet pOverVid,"OverMessage2",alltimevar(2),1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(alltimevar(3)),1,""
	Else 
		PuPlayer.LabelSet pOverVid,"OverMessage1","GRAND CHAMPION",1,"{'mt':2, 'size': 15 }"
		PuPlayer.LabelSet pOverVid,"OverMessage2",HighScoreName(0),1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(HighScore(0)),1,""
	End If
  Case 3:
	If bGameInPlay=False Then pupDMDDisplay "attract", "Insert Coin",	"@vidIntro.mp4" ,9, 1,		10
	PuPlayer.playlistplayex pOverVid,"PupBackgrounds","Video-0x004A.mp4", 1, 1
	If osbactive = 1 or osbactive = 2 Then
		PuPlayer.LabelSet pOverVid,"OverMessage1","OSB HIGHSCORE #1",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2",alltimevar(4),1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(alltimevar(5)),1,""
	Else 
		PuPlayer.LabelSet pOverVid,"OverMessage1","HIGH SCORE #1",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2",HighScoreName(1),1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(HighScore(1)),1,""
	End If 
  Case 4:
	PuPlayer.playlistplayex pOverVid,"PupBackgrounds","Video-0x004B.mp4", 1, 1
	If bGameInPlay=False Then pupDMDDisplay "attract", "Game Over",	"@vidIntro2.mp4",3, 1,		10
	If osbactive = 1 or osbactive = 2 Then
		PuPlayer.LabelSet pOverVid,"OverMessage1","OSB HIGHSCORE #2",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2",alltimevar(6),1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(alltimevar(7)),1,""
	Else 
		PuPlayer.LabelSet pOverVid,"OverMessage1","HIGH SCORE #2",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2",HighScoreName(2),1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(HighScore(2)),1,""
	End If 
  Case 5:
	If bGameInPlay=False Then pupDMDDisplay "attract", "Insert Coin",	"@ARS107-Scene-22.mp4" ,9, 1,		10
	PuPlayer.playlistplayex pOverVid,"PupBackgrounds","Video-0x0048.mp4", 1, 1
	If osbactive = 1 or osbactive = 2 Then
		PuPlayer.LabelSet pOverVid,"OverMessage1","OSB HIGHSCORE #3",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2",alltimevar(8),1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(alltimevar(9)),1,""
	Else 
		PuPlayer.LabelSet pOverVid,"OverMessage1","HIGH SCORE #3",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2",HighScoreName(3),1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(HighScore(3)),1,""
	End If 
  Case 6:
	If bGameInPlay=False Then pupDMDDisplay "attract", "Game Over",	"@vidIntro2.mp4",3, 1,		10
	PuPlayer.playlistplayex pOverVid,"PupVideos","Video-0x0048.mp4", 1, 1
	PuPlayer.LabelSet pOverVid,"OverMessage1","GAMES PLAYED",1,""
	PuPlayer.LabelSet pOverVid,"OverMessage2",FormatScore(TotalGamesPlayed),1,"{'mt':2}"
	PuPlayer.LabelSet pOverVid,"OverMessage3"," ",1,""
  Case 7:
	pupDMDDisplay "attract", "Insert Coin", "@vidIntro.mp4",3, 1,		10
	PuPlayer.playlistplayex pOverVid,"PupVideos","Video-0x0049.mp4", 1, 1
	PuPlayer.LabelSet pOverVid,"OverMessage1","CREDITS",1,""
	PuPlayer.LabelSet pOverVid,"OverMessage2","",1,"{'mt':2}"
	PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(Credits),1,""
  Case 8:
	If bGameInPlay=False Then pupDMDDisplay "attract", "Insert Coin",	"@ARS107-Scene-22.mp4" ,9, 1,		10
	PuPlayer.playlistplayex pOverVid,"PupBackgrounds","Video-0x0048.mp4", 1, 1
	PuPlayer.LabelSet pOverVid,"OverMessage1","MEDLEY CHAMPION",1,""
	PuPlayer.LabelSet pOverVid,"OverMessage2",HighScoreName(4),1,"{'mt':2}"
	PuPlayer.LabelSet pOverVid,"OverMessage3",FormatScore(HighScore(4)),1,""
  Case 9:
	If bGameInPlay then
		PuPlayer.playlistplayex pOverVid,"PupBackgrounds","Video-0x0047.mp4", 1, 1
		PuPlayer.LabelSet pOverVid,"OverMessage1","",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2","",1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3","",1,""
	else 
		pCurAttractPos=0
		pAttractNext 'reset to beginning	
	End If
  Case 10:
	If bGameInPlay then
		PuPlayer.playlistplayex pOverVid,"PupVideos","Video-0x004A.mp4", 1, 1
		PuPlayer.LabelSet pOverVid,"OverMessage1","Missles",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2", SmartButtonCount,1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3","",1,""
	else 
		pCurAttractPos=0
		pAttractNext 'reset to beginning	
	End If
  Case 11:
	If bGameInPlay then
		PuPlayer.playlistplayex pOverVid,"PupVideos","Video-0x004A.mp4", 1, 1
		PuPlayer.LabelSet pOverVid,"OverMessage1", "Multipliers", 1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2", "Play: x" & (Multiplier3x * PlayMultiplier),1,"{'mt':2}"
		PuPlayer.LabelSet pOverVid,"OverMessage3", "Bonus: x" & BonusMultiplier,1,""
	else 
		pCurAttractPos=0
		pAttractNext 'reset to beginning	
	End If
  Case Else
    pCurAttractPos=0
    pAttractNext 'reset to beginning
  end Select
End Sub

Sub I2(aStr)
	TextBox.Text=aStr
	'objFile.Write aStr & vbCrLf
End Sub

Sub D(aStr)
	debug.print aStr
End Sub

Sub D2(aStr)
	'debug.print aStr
	'TextBox.Text=aStr
End Sub

' ********************************
'   Attract Mode
' ********************************

Sub StartAttractMode(dummy)
	D "sub StartAttractMode()"
	If AutoAI or AutoQA Then AutoQAStartGame.interval=13000:AutoQAStartGame.Enabled = True
	StartLightSeq
	DMDFlush
End Sub


Sub StopAttractMode
    LightSeqAttract.StopPlay
	LightSeqBackFlashers.StopPlay
    DMDScoreNow
End Sub

Sub StartLightSeq()
    'lights sequences
	D "sub StartLightSeq()"
    LightSeqAttract.UpdateInterval = 25
	LightSeqBackFlashers.UpdateInterval = 150
	LightSeqBackFlashers.Play SeqRandom, 10, , 50000
    LightSeqAttract.Play SeqBlinking, , 5, 150
    LightSeqAttract.Play SeqRandom, 40, , 4000
    LightSeqAttract.Play SeqAllOff
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 40, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 40, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqRightOn, 30, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqLeftOn, 30, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqStripe1VertOn, 50, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
End Sub

Sub LightSeqAttract_PlayDone()
    StartLightSeq()
End Sub

Sub LightSeqTilt_PlayDone()
    LightSeqTilt.Play SeqAllOff
End Sub

Sub LightSeqSkillshot_PlayDone()
    LightSeqSkillshot.Play SeqAllOff
End Sub

Sub PlayScoopLightSeq()
	'	LightSeqScoopArrow.Play SeqDiagUpRightOn, 30, 1
End Sub 
Sub StopScoopLightSeq()
		LightSeqScoopArrow.StopPlay
End Sub 
Sub LightSeqScoopArrow_Playdone()
		PlayScoopLightSeq
End Sub


' ********************************
'   Lights Routines
' ********************************

Sub aRampLightsSave()
	Dim a, i
	D "Sub aRampLightsSave()"
	baRampLightsSaved = True
	i = 0
	For each a in aRampLights 
		RampLightsSaveColor(i, 0) = a.name
		RampLightsSaveColor(i, 1) = a.color
		RampLightsSaveColor(i, 2) = a.colorfull
		RampLightsSaveColor(i, 3) = a.state
		RampLightsSaveColor(i, 4) = a.uservalue
		i=i+1
	next
End Sub

Sub aRampLightsRestore()
	Dim a, i
	D "sub aRampLightsRestore()"
	if baRampLightsSaved Then
		i = 0
		for each a in aRampLights
			a.color = RampLightsSaveColor(i, 1)
			a.colorfull = RampLightsSaveColor(i, 2)
			a.state = RampLightsSaveColor(i, 3)
			a.uservalue = RampLightsSaveColor(i, 4)
			i=i+1
		Next
	End If
	baRampLightsSaved = False
End Sub

Sub aLightsSave()  ' not used
	Dim a, i
	D "sub aLightsSave"
	baLightsSaved = True
	i = 0
	For each a in aLights 
		LightsSaveColor(i, 0) = a.name
		LightsSaveColor(i, 1) = a.color
		LightsSaveColor(i, 2) = a.colorfull
		LightsSaveColor(i, 3) = a.state
		LightsSaveColor(i, 4) = a.uservalue
		i=i+1
	next
End Sub

Sub aSaveLights()  ' Prior to Medley Wizard Mode
	Dim a, i
	D "sub aSaveLights()"
	baSaveLightsSaved = True
	i = 0
	For each a in saveLights 
		SaveColor(i, 0) = a.name
		SaveColor(i, 1) = a.color
		SaveColor(i, 2) = a.colorfull
		SaveColor(i, 3) = a.state
		SaveColor(i, 4) = a.uservalue
		i=i+1
		'D "Save " & a.name & "=" & a.state
	next
End Sub

Sub aRestoreLights()   ' After Medley Wizard Mode
	Dim a, i
	D "sub aRestoreLights"
	If baSaveLightsSaved then 
		i = 0
		For each a in saveLights 
			a.color = SaveColor(i,1)
			a.colorfull = SaveColor(i,2)
			a.state = SaveColor(i,3)
			a.uservalue = SaveColor(i,4)
			i=i+1
			'D "Restore " & a.name & "=" & a.state
		next
	End If 
	baSaveLightsSaved = False
End Sub


Dim OldGiState
OldGiState = -1   'start witht the Gi off

Sub ChangeGi(col) 'changes the gi color
    Dim bulb
    For each bulb in aGILights
        SetLightColor bulb, col, -1
    Next
End Sub


Sub GiOn
    DOF 126, DOFOn
    Dim bulb
    For each bulb in GI
		SetLightColor bulb, "white", -1
        bulb.State = 1
    Next
End Sub


Sub GiOff
	DOF 126, DOFOff
	Dim bulb
	For each bulb in GI
		bulb.State = 0
	Next
End Sub


Sub GiAllOn
	DOF 126, DOFOn
	Dim bulb
	For each bulb in GI
		SetLightColor bulb, "white", -1
		bulb.State = 1
	Next
End Sub

' GI & light sequence effects

Sub GiEffect(n)
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 4
            LightSeqGi.Play SeqBlinking, , 5, 100
        Case 2 'random
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqRandom, 5, , 1000
        Case 3 'upon
            LightSeqGi.UpdateInterval = 4
            LightSeqGi.Play SeqUpOn, 5, 1
        Case 4 ' left-right-left
            LightSeqGi.UpdateInterval = 5
            LightSeqGi.Play SeqLeftOn, 10, 1
            LightSeqGi.UpdateInterval = 5
            LightSeqGi.Play SeqRightOn, 10, 1
    End Select
End Sub


Sub LightEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
        Case 1 'all blink
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqBlinking, , 5, 100
        Case 2 'random
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqRandom, 5, , 1000
        Case 3 'upon
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 10, 1
        Case 4 ' left-right-left
            LightSeqInserts.UpdateInterval = 5
            LightSeqInserts.Play SeqLeftOn, 10, 1
            LightSeqInserts.UpdateInterval = 5
            LightSeqInserts.Play SeqRightOn, 10, 1
    End Select
End Sub

	' Flasher Effects using lights

	Dim FEStep, FEffect
	FEStep = 0
	FEffect = 0

	Sub FlashEffect(n)
		Select case n
			Case 0 ' all off
				LightSeqBackFlashers.Play SeqAlloff
			Case 1 'all blink
				LightSeqBackFlashers.UpdateInterval = 4
				LightSeqBackFlashers.Play SeqBlinking, , 5, 100
			Case 2 'random
				LightSeqBackFlashers.UpdateInterval = 10
				LightSeqBackFlashers.Play SeqRandom, 5, , 1000
			Case 3 'upon
				LightSeqBackFlashers.UpdateInterval = 4
				LightSeqBackFlashers.Play SeqUpOn, 10, 1
			Case 4 ' left-right-left
				LightSeqBackFlashers.UpdateInterval = 5
				LightSeqBackFlashers.Play SeqLeftOn, 10, 1
				LightSeqBackFlashers.UpdateInterval = 5
				LightSeqBackFlashers.Play SeqRightOn, 10, 1
			Case 5 ' top flashers blink fast
				LightSeqBackFlashers.UpdateInterval = 4
				LightSeqBackFlashers.Play SeqBlinking, , 5, 100
		End Select
	End Sub

'===================
'  Flubber Flashers 
'===================	
Dim TestFlashers : TestFlashers = 0
Dim FlashLevel(20), objbase(20), objlit(20), objflasher(20), objlight(20)
Dim tablewidth, tableheight : tablewidth = Table1.width : tableheight = Table1.height

'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
InitFlasher 1, "green" 
InitFlasher 2, "green"
InitFlasher 3, "green"
InitFlasher 4, "purple"  
InitFlasher 5, "white"
InitFlasher 9, "red"  
InitFlasher 10, "red"

 

' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
RotateFlasher 1, 0
RotateFlasher 2, 0	
RotateFlasher 3, 90	
RotateFlasher 4, 0 
RotateFlasher 5, 0
RotateFlasher 9,0
RotateFlasher 10,0 

Sub InitFlasher(nr, col) 
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr)
	Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr)
	Set objlight(nr) = Eval("Flasherlight" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 80
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	' set the texture and color of all objects
	objbase(nr).BlendDisableLighting = 1

	select Case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
		Case Else
			msgbox  "Unknown objectbase Image:" & nr & " " & objbase(nr).image & "make sure flasherbase is set to white image"
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select Case col
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
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 500 *  FlashLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.03 * FlashLevel(nr)^3
	objbase(nr).BlendDisableLighting =  0.5 + 10 * FlashLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * FlashLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,FlashLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	FlashLevel(nr) = FlashLevel(nr) * 0.9 - 0.01
	If FlashLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objlit(nr).visible = 0 : End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub
Sub FlasherFlash9_Timer() : FlashFlasher(9) : End Sub
Sub FlasherFlash10_Timer() : FlashFlasher(10) : End Sub
Sub FlasherFlash11_Timer() : FlashFlasher(11) : End Sub
Sub FlasherFlash12_Timer() : FlashFlasher(12) : End Sub


' ***      script for demoing flashers 		        ***
' *** FlashLevel(xx) = 1 : FlasherFlashxx_Timer     ***
' *** for modulated flashers use 0-1 for FlashLevel ***

Sub Timer1_Timer
 FlashLevel(10) = .6 : FlasherFlash10_Timer
 FlashLevel(11) = .6 : FlasherFlash11_Timer
 FlashLevel(12) = 1 : FlasherFlash12_Timer
 FlashLevel(9) = .6 : FlasherFlash9_Timer
 FlashLevel(1) = .6 : FlasherFlash1_Timer
 FlashLevel(2) = .9 : FlasherFlash2_Timer
 FlashLevel(3) = .9 : FlasherFlash3_Timer
 FlashLevel(4) = .6 : FlasherFlash4_Timer
end Sub

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' droptargets, animations, etc
Sub VPObjects_Init
End Sub

Dim bAddedABall

Dim Cities: Cities = Array("","Sydney", "Tokyo", "Shanghai", "Hong Kong", "Moscow", "Dubai", "Tel Aviv", "Warsaw",  _
		 "Oslo", "London", "Berlin", "Paris", "Dublin", "Madrid", "Cape Town", "Buenos Aires", _
 		  "Rio de Janeiro", "Mexico City", "Los Angeles", "Las Vegas", "Chicago", "Toronto", "New York","**25**","**26**")

'****************************************

Sub ResetNewBallVariables()           'reset variables for a new ball or player
	VipValue=200000
	LaneBonus = 0
End Sub


Sub ResetNewBallLights() 'turn on or off the needed lights before a new ball is released
dim a
	for each a in ShotMultipliers
		SetLightColor a, "green", 0
		a.uservalue = 0
	Next
End Sub


Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

Sub SmartButtonFlash(color, enabled)  'the lockbar light
	D "SmartButton Flash: " & color & " " & enabled
	If enabled Then 
		tmrSmartButtonLightFlash.Enabled = False
		If color = "green" then 						' If color is blank just use what the color was before 
			if VR_Room Then
				Pincab_FireButtonLight.Colorfull = RGB(0,255,0) 
			Else
				Flasherflash5_5.Color = RGB(0,255,0)
				Flasherflash5.ImageA = "domegreenflash"
				Flasherbase5.Image = "dome2basegreen"
				Flasherlit5.Image = "dome2litgreen"
			End If
		elseIf color = "white" then
			if VR_Room Then
				Pincab_FireButtonLight.Colorfull = RGB(255,255,255)
			Else
				Flasherflash5_5.Color = RGB(255,255,255)
				Flasherflash5.ImageA = "domewhiteflash"
				Flasherbase5.Image = "dome2basewhite"
				Flasherlit5.Image = "dome2litwhite"
			End IF
		elseIf color = "cyan" then
			if VR_Room Then
				Pincab_FireButtonLight.Colorfull = RGB(0,255,255)
			Else
				Flasherflash5_5.Color = RGB(0,255,255)
				Flasherflash5.ImageA = "domeredflash"
				Flasherbase5.Image = "dome2basered"
				Flasherlit5.Image = "dome2litred"
			End If
		elseIf color = "orange" then
			if VR_Room Then
				Pincab_FireButtonLight.Colorfull = RGB(255,165,0) 
			Else
				Flasherflash5_5.Color = RGB(255,165,0)
				Flasherflash5.ImageA = "domeredflash"
				Flasherbase5.Image = "dome2basered"
				Flasherlit5.Image = "dome2litred"
			End If
		elseIf color = "orangedark" then
			if VR_Room Then
				Pincab_FireButtonLight.Colorfull = RGB(204,85,0)
			Else
				Flasherflash5_5.Color = RGB(204,85,0)
				Flasherflash5.ImageA = "domeredflash"
				Flasherbase5.Image = "dome2basered"
				Flasherlit5.Image = "dome2litred"
			End If
		elseIf color = "red" then 
			if VR_Room Then
				Pincab_FireButtonLight.Colorfull = RGB(255,0,0) 
			Else
				Flasherflash5_5.Color = RGB(255,0,0)
				Flasherflash5.ImageA = "domeredflash"
				Flasherbase5.Image = "dome2basered"
				Flasherlit5.Image = "dome2litred"
			End If
		elseIf color = "blue" then 
			if VR_Room Then
				Pincab_FireButtonLight.Colorfull = RGB(0,0,255)
			Else
				Flasherflash5_5.Color = RGB(0,0,255)
				Flasherflash5.ImageA = "domeblueflash"
				Flasherbase5.Image = "dome2baseblue"
				Flasherlit5.Image = "dome2litblue"
			End If
		elseIf color = "purple" then 
			if VR_Room Then
				Pincab_FireButtonLight.Colorfull = RGB(128,0,128) 
			Else
				Flasherflash5_5.Color = RGB(128,0,128)
				Flasherflash5.ImageA = "domepurpleflash"
				Flasherbase5.Image = "dome2basepurple"
				Flasherlit5.Image = "dome2litpurple"
			End If
		elseIf color = "yellow" then 
			if VR_Room Then
				Pincab_FireButtonLight.Colorfull = RGB(255,255,0)
			Else
				Flasherflash5_5.Color = RGB(255,255,0)
				Flasherflash5.ImageA = "domeyellowflash"
				Flasherbase5.Image = "dome2baseyellow"
				Flasherlit5.Image = "dome2lityellow"
			End If
		elseIf color = "white" then 
			if VR_Room Then
				Pincab_FireButtonLight.Colorfull = RGB(255,255,255) 
			Else
				Flasherflash5_5.Color = RGB(255,255,255)
				Flasherflash5.ImageA = "domewhiteflash"
				Flasherbase5.Image = "dome2basewhite"
				Flasherlit5.Image = "dome2litwhite"
			End If
		End If
		tmrSmartButtonLightFlash.Enabled = True
		DOF 141, DOFOn
	elseIf SmartButtonCount <= 0 or enabled=False then 
		tmrSmartButtonLightFlash.Enabled = False
		FlashLevel(5) = 0 : Flasherflash5_Timer
	End If
End Sub


Sub tmrSmartButtonLightFlash_Timer
	FlashLevel(5) = 1 : Flasherflash5_Timer
	If BallsOnPlayfield > 0 Then DOF 204, DOFPulse
End Sub 


Sub Flasherflash5_Timer
	dim flashx3, matdim
	If Flasherflash5.TimerEnabled = False Then 
		If VR_Room Then
			Pincab_FireButtonLight.State = 1
		Else
			Flasherflash5.visible = 1
			Flasherflash5_5.visible = 1
			Flasherlit5.visible = 1
		End if
		Flasherflash5.TimerEnabled = True
		Flasherflash5_5.TimerEnabled = True
	End If
	flashx3 = FlashLevel(5) * FlashLevel(5) * FlashLevel(5)
	Flasherflash5.opacity = 200 * flashx3
	Flasherflash5_5.opacity = 500 * flashx3
	Flasherlit5.BlendDisableLighting = 10 * flashx3
	Flasherbase5.BlendDisableLighting =  flashx3
	matdim = Round(10 * FlashLevel(5))
	Flasherlit5.material = "domelit" & matdim
	FlashLevel(5) = FlashLevel(5) * 0.9 - 0.01
	If FlashLevel(5) < 0.15 Then
		If VR_Room Then 
			Pincab_FireButtonLight.State = 0
		Else
			Flasherlit5.visible = 0

		End If 
	Else
		If VR_Room Then 
			Pincab_FireButtonLight.State = 1
		Else
			Flasherlit5.visible = 1

		End If 
	end If
	If FlashLevel(5) < 0 Then
		If VR_Room Then
			Pincab_FireButtonLight.State = 0
		Else		
			Flasherflash5.visible = 0
			Flasherflash5_5.visible = 0
		End If
		Flasherflash5.TimerEnabled = False
		Flasherflash5_5.TimerEnabled = False
	End If
End Sub

Sub CheckSmartButton(bUseIt)
'D "CheckSmartButton &" bUseIt
	Dim bUsed
	Dim a
	bUsed = False
	If smartButtonCount > 0 Then
		if bUseIt  then
			for each a in ArrowLights
				if a.state <> 0 then ' missile can be used
					bUsed = True
					Exit For
				end if
			Next
		End if
		If bUsed and bUseIt and NOT CreateMultiballTimer.Enabled Then  ' Trying to not trigger during toybox cancel
			SmartMissilePressed()
			If SmartButtonCount <= 0 Then
				SmartButtonFlash "", False
			End If
		ElseIf bUseIt = False then 	
			If SmartButtonCount <= 0 Then
				SmartButtonFlash "", False
			End If
		End If 
	End if
End Sub

'const kDMD_last		=521
'const kDMD_walk		=522
'const kDMD_same		=523
'const kDMD_sweet		=524
'const kDMD_dude		=525
'const kDMD_back		=526
'const kDMD_rats		=527
'const kDMD_toys		=528
'const kDMD_love		=529

Sub StartPlayerModeVideo(bSkipInitial)	' Play the GamePlay Video
	D "StartPlayerModevideo Mode:" & PlayerMode & " 2:" & PlayerMode2 & " 3:" & PlayerMode3
	Dim overVideo
	Dim bgVideo

	if bMedleyTourReady or bFinalTourReady then 
		StopPlayerModeVideo
	End If

	if tmrMedleyTour.Enabled or tmrFinalTour.Enabled then ' Wizard Mode
		playclear pBackglass 
		playclear pMusic
		Select Case PlayerMode3
			Case -1:' No Mode Selected  '

			Case 0: 
				pDMDEvent(kDMD_last+30)
			Case 1: 
				pDMDEvent(kDMD_walk+30)
			Case 2: 
				pDMDEvent(kDMD_same+30)
			Case 3: 
				pDMDEvent(kDMD_sweet+30)
			Case 4: 
				pDMDEvent(kDMD_dude+30)
			Case 5: 
				pDMDEvent(kDMD_back+30)
			Case 6: 
				pDMDEvent(kDMD_rats+30)
			Case 7: 
				pDMDEvent(kDMD_toys+30)
			Case 8: 
				pDMDEvent(kDMD_love+30)
		End Select
	elseif bSecondMode = False Then
		playclear pBackglass
		If PlayerMode <> -1 Then 
			If ModePercent(PlayerMode) < 0 then ModePercent(PlayerMode)=0
		End If
		Select Case PlayerMode
			Case -1:' No Mode Selected  '
				pDMDEvent(999)  
			Case 0: 
				If bSkipInitial = False then
					pDMDEvent(kDMD_last+10)
				End If 
				pDMDEvent(kDMD_last+30)
			Case 1: 
				If bSkipInitial = False then
					pDMDEvent(kDMD_walk+10)
				End If 
				pDMDEvent(kDMD_walk+30)
			Case 2: 
				If bSkipInitial = False then
					pDMDEvent(kDMD_same+10)
				End If 
				pDMDEvent(kDMD_same+30)
			Case 3: 
				If bSkipInitial = False then
					pDMDEvent(kDMD_sweet+10)
				End If 
				pDMDEvent(kDMD_sweet+30)
			Case 4: 
				If bSkipInitial = False then
					pDMDEvent(kDMD_dude+10)
				End If 
				pDMDEvent(kDMD_dude+30)
			Case 5: 
				If bSkipInitial = False then
					pDMDEvent(kDMD_back+10)
				End If 
				pDMDEvent(kDMD_back+30)
			Case 6: 
				If bSkipInitial = False then
					pDMDEvent(kDMD_rats+10)
				End If 
				pDMDEvent(kDMD_rats+30)
			Case 7: 
				If bSkipInitial = False then
					pDMDEvent(kDMD_toys+10)
				End If 
				pDMDEvent(kDMD_toys+30)
			Case 8: 
				If bSkipInitial = False then
					pDMDEvent(kDMD_love+10)
				End If 
				pDMDEvent(kDMD_love+30)   ' 529+30
		End Select
	ElseIf PlayerMode2 <> -1 Then
		playclear pBackglass
		Select Case PlayerMode2
			Case -1:
			Case 0:
				overVideo = "Loops-Video-0x0059.mp4"
				bgvideo = "Video-0x0000.mp4"
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x059C-superloops.wav",100, 1
			Case 1:
				overVideo = "Ramps-Video-0x005B.mp4"
				bgvideo = "Video-0x0000.mp4"
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x0346-superramps.wav",100, 1
			Case 2:
				overVideo = "Targets-Video-0x005E.mp4"
				bgvideo = "Video-0x0002.mp4"
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x053Bsupertargets.wav",100, 1
			Case 3:
				overVideo = "Scoring-Video-0x005C.mp4"
				bgvideo = "Video-0x0000.mp4"
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x0315doublescoring.wav",100, 1
			Case 4:
				overVideo = "Lanes-Video-0x0058.mp4"
				bgvideo = "Video-0x0000.mp4"
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x0598-superlanes.wav",100, 1
			Case 5:
				overVideo = "Pops-Video-0x005A.mp4"
				bgvideo = "Video-0x0002.mp4"
			Case 6:
				overVideo = "Spinners-Video-0x005D.mp4"
				bgvideo = "Video-0x0051.mp4"
				'PlaySoundVol "Sound-0x0598-superlanes", VolDef
		End Select
		If PlayerMode2 <> -1 then ' Just make sure  'TODO Put this to ADDScene so we block the Big Numbers
'			QueueScene "playmedia """&overVideo&""", ""PupVideos"", pOverVid , """", -1, """", 1, 1 '", 3540, 2	
			QueueScene "ScenePlayMessage """&overVideo&""", """","""","""" '", 3500, 2
			QueueScene "SceneClearPlayMessage '", 0, 2

			playmedia bgVideo, "PupVideos", pBackglass, "", -1, "", 1, 2
		End If 
	End If
End Sub


'************ PuPDMD
Sub pDMDStartGame
	Dim fsize
	fsize=8*FontScale
	If bUsePUPDMD then 
		D "pDMDStartGame"
		pInAttract=false
		If pDMDVideoPlaying Then 
			PuPlayer.playstop pDMD
			pDMDVideoPlaying=False
		End if
		' Clear the overvideo just in Case it is playing 
		playclear pOverVid
		PuPlayer.LabelSet pOverVid, "ScorbitQR", "PuPOverlays\\clear.png",0,""
		PuPlayer.LabelSet pOverVid, "ScorbitQRIcon", "PuPOverlays\\clear.png",0,""

		PuPlayer.LabelSet pOverVid,"OverMessage1"," ",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage2"," ",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage3"," ",1,"{'mt':2, 'size': "&fsize&",'ypos': 65}"
		pDMDSetPage(pScores)   'set blank text overlay page.
	end If
End Sub

Sub pDMDGameOver
	'D "pDMDGameOver"	
	'DisplayDMDText2 "GAME OVER", "", 20000, 5, 0
	DMD "GAME OVER", "", "", eNone, eBlink, eNone, 20000, True, "gameover"
	StartAttractMode 1

	If bUsePUPDMD then
		PuPlayer.PlayStop pOverVid
		PuPlayer.SetLoop pOverVid, 0

		playclear pBackglass
		playclear pMusic
		playclear pAudio

		PuPlayer.LabelShowPage pOverVid, 1,0,""
		PuPlayer.playlistplayex pOverVid,"PupBackgrounds","Video-0x0047.mp4", 1, 1
		PuPlayer.LabelSet pOverVid,"OverMessage1","GAME" ,1,"{'mt':2, 'size': 14 }"
		PuPlayer.LabelSet pOverVid,"OverMessage2","",1,""
		PuPlayer.LabelSet pOverVid,"OverMessage3","OVER",1,"{'mt':2, 'size': 14 }"
	End If 
End Sub


Sub pAttractStart
	pAttractStartIdx 0
end Sub

Sub pAttractStartIdx(idx)
	if (pInAttract = False) then 
		D "STARTING ATTRACT"
		pDMDSetPage(pDMDBlank)   'set blank text overlay page.
		pCurAttractPos=idx
		pDMDStartUP
	end if 
end Sub

Sub pDMDStartUP
	D "pDMDStartUp"
	pupDMDDisplay "attract","Welcome","@vidIntro.mp4",9,0,10
	pInAttract=true
End Sub

Sub pDMDEvent(id)
	If bUsePUPDMD then
		D2 "pDMDEvent " & id 
		PauseBigScore=True
		PuPlayer.LabelSet pBackglass,"BigScore"," ",0,""
		PuPEvent(id)  ' Send an event to the pup pack the E500 trigger
	End If 
End Sub

'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.

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

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel, ToyBoxSoundLevel

GateSoundLevel = 0.5/5													'volume level; range [0, 1]
SpinnerSoundLevel = 0.5/5												'volume level; range [0, 1]
TargetSoundFactor = 0.025 * 10											'volume multiplier; must not be zero
DTSoundLevel = 0.25														'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                              					'volume level; range [0, 1]
ToyBoxSoundLevel = 1.0                            					'volume level; range [0, 1]

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

Sub PlaySoundVol(soundname, Volume)
  PlaySound soundname, 1, Volume
End Sub


' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / tableheight-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / tablewidth-1
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
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound ("Nudge_1"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
		Case 2 : PlaySound ("Nudge_2"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
		Case 3 : PlaySound ("Nudge_3"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
	End Select
End Sub

Sub SoundNudgeRight()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound ("Nudge_1"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
		Case 2 : PlaySound ("Nudge_2"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
		Case 3 : PlaySound ("Nudge_3"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
	End Select
End Sub

Sub SoundNudgeCenter()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound ("Nudge_1"), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
		Case 2 : PlaySound ("Nudge_2"), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
		Case 3 : PlaySound ("Nudge_3"), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
	End Select
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
	PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, Plunger
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////
Sub RandomSoundDrain(drainswitch)
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic ("Drain_1"), DrainSoundLevel, drainswitch
		Case 2 : PlaySoundAtLevelStatic ("Drain_2"), DrainSoundLevel, drainswitch
		Case 3 : PlaySoundAtLevelStatic ("Drain_3"), DrainSoundLevel, drainswitch
		Case 4 : PlaySoundAtLevelStatic ("Drain_4"), DrainSoundLevel, drainswitch
		Case 5 : PlaySoundAtLevelStatic ("Drain_5"), DrainSoundLevel, drainswitch
		Case 6 : PlaySoundAtLevelStatic ("Drain_6"), DrainSoundLevel, drainswitch
		Case 7 : PlaySoundAtLevelStatic ("Drain_7"), DrainSoundLevel, drainswitch
		Case 8 : PlaySoundAtLevelStatic ("Drain_8"), DrainSoundLevel, drainswitch
		Case 9 : PlaySoundAtLevelStatic ("Drain_9"), DrainSoundLevel, drainswitch
		Case 10 : PlaySoundAtLevelStatic ("Drain_10"), DrainSoundLevel, drainswitch
		Case 11 : PlaySoundAtLevelStatic ("Drain_11"), DrainSoundLevel, drainswitch
	End Select
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBallRelease(drainswitch)
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("BallRelease1",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 2 : PlaySoundAtLevelStatic SoundFX("BallRelease2",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 3 : PlaySoundAtLevelStatic SoundFX("BallRelease3",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 4 : PlaySoundAtLevelStatic SoundFX("BallRelease4",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 5 : PlaySoundAtLevelStatic SoundFX("BallRelease5",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 6 : PlaySoundAtLevelStatic SoundFX("BallRelease6",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 7 : PlaySoundAtLevelStatic SoundFX("BallRelease7",DOFContactors), BallReleaseSoundLevel, drainswitch
	End Select
End Sub



'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft(sling)
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Sling_L1",DOFContactors), SlingshotSoundLevel, Sling
		Case 2 : PlaySoundAtLevelStatic SoundFX("Sling_L2",DOFContactors), SlingshotSoundLevel, Sling
		Case 3 : PlaySoundAtLevelStatic SoundFX("Sling_L3",DOFContactors), SlingshotSoundLevel, Sling
		Case 4 : PlaySoundAtLevelStatic SoundFX("Sling_L4",DOFContactors), SlingshotSoundLevel, Sling
		Case 5 : PlaySoundAtLevelStatic SoundFX("Sling_L5",DOFContactors), SlingshotSoundLevel, Sling
		Case 6 : PlaySoundAtLevelStatic SoundFX("Sling_L6",DOFContactors), SlingshotSoundLevel, Sling
		Case 7 : PlaySoundAtLevelStatic SoundFX("Sling_L7",DOFContactors), SlingshotSoundLevel, Sling
		Case 8 : PlaySoundAtLevelStatic SoundFX("Sling_L8",DOFContactors), SlingshotSoundLevel, Sling
		Case 9 : PlaySoundAtLevelStatic SoundFX("Sling_L9",DOFContactors), SlingshotSoundLevel, Sling
		Case 10 : PlaySoundAtLevelStatic SoundFX("Sling_L10",DOFContactors), SlingshotSoundLevel, Sling
	End Select
End Sub

Sub RandomSoundSlingshotRight(sling)
	Select Case Int(Rnd*8)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Sling_R1",DOFContactors), SlingshotSoundLevel, Sling
		Case 2 : PlaySoundAtLevelStatic SoundFX("Sling_R2",DOFContactors), SlingshotSoundLevel, Sling
		Case 3 : PlaySoundAtLevelStatic SoundFX("Sling_R3",DOFContactors), SlingshotSoundLevel, Sling
		Case 4 : PlaySoundAtLevelStatic SoundFX("Sling_R4",DOFContactors), SlingshotSoundLevel, Sling
		Case 5 : PlaySoundAtLevelStatic SoundFX("Sling_R5",DOFContactors), SlingshotSoundLevel, Sling
		Case 6 : PlaySoundAtLevelStatic SoundFX("Sling_R6",DOFContactors), SlingshotSoundLevel, Sling
		Case 7 : PlaySoundAtLevelStatic SoundFX("Sling_R7",DOFContactors), SlingshotSoundLevel, Sling
		Case 8 : PlaySoundAtLevelStatic SoundFX("Sling_R8",DOFContactors), SlingshotSoundLevel, Sling
	End Select
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperTop(Bump)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 2 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_2",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 3 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_3",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 4 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_4",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 5 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_5",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
	End Select
End Sub

Sub RandomSoundBumperMiddle(Bump)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 2 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_2",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 3 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_3",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 4 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_4",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 5 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_5",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
	End Select
End Sub

Sub RandomSoundBumperBottom(Bump)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 2 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_2",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 3 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_3",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 4 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_4",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 5 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_5",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
	End Select
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
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_L01",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_L02",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_L07",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_L08",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_L09",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_L10",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_L12",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_L14",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 9 : PlaySoundAtLevelStatic SoundFX("Flipper_L18",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 10 : PlaySoundAtLevelStatic SoundFX("Flipper_L20",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 11 : PlaySoundAtLevelStatic SoundFX("Flipper_L26",DOFFlippers), FlipperLeftHitParm, Flipper
	End Select
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_R01",DOFFlippers), FlipperRightHitParm, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_R02",DOFFlippers), FlipperRightHitParm, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_R03",DOFFlippers), FlipperRightHitParm, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_R04",DOFFlippers), FlipperRightHitParm, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_R05",DOFFlippers), FlipperRightHitParm, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_R06",DOFFlippers), FlipperRightHitParm, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_R07",DOFFlippers), FlipperRightHitParm, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_R08",DOFFlippers), FlipperRightHitParm, Flipper
		Case 9 : PlaySoundAtLevelStatic SoundFX("Flipper_R09",DOFFlippers), FlipperRightHitParm, Flipper
		Case 10 : PlaySoundAtLevelStatic SoundFX("Flipper_R10",DOFFlippers), FlipperRightHitParm, Flipper
		Case 11 : PlaySoundAtLevelStatic SoundFX("Flipper_R11",DOFFlippers), FlipperRightHitParm, Flipper
	End Select
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L01",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L02",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L03",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundReflipUpRight(flipper)
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R01",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R02",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R03",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_1",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_2",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_3",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_4",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_5",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_6",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_7",DOFFlippers), FlipperDownSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	Select Case Int(Rnd*8)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_1",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_2",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_3",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_4",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_5",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_6",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_7",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_8",DOFFlippers), FlipperDownSoundLevel, Flipper
	End Select
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
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_1"), parm  * RubberFlipperSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_2"), parm  * RubberFlipperSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_3"), parm  * RubberFlipperSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_4"), parm  * RubberFlipperSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_5"), parm  * RubberFlipperSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_6"), parm  * RubberFlipperSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_7"), parm  * RubberFlipperSoundFactor
	End Select
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rollover_1"), RolloverSoundLevel
		Case 2 : PlaySoundAtLevelActiveBall ("Rollover_2"), RolloverSoundLevel
		Case 3 : PlaySoundAtLevelActiveBall ("Rollover_3"), RolloverSoundLevel
		Case 4 : PlaySoundAtLevelActiveBall ("Rollover_4"), RolloverSoundLevel
	End Select
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
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_1"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_2"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_3"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_3"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_5"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_6"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_7"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_8"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_9"), Vol(ActiveBall) * RubberWeakSoundFactor
	End Select
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 5 then
 		RandomSoundRubberStrong 1 
	End if
	If finalspeed <= 5 then
 		RandomSoundRubberWeak()
 	End If	
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
	Select Case Int(Rnd*13)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Metal_Touch_1"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Metal_Touch_2"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Metal_Touch_3"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Metal_Touch_4"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Metal_Touch_5"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Metal_Touch_6"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Metal_Touch_7"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 8 : PlaySoundAtLevelActiveBall ("Metal_Touch_8"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 9 : PlaySoundAtLevelActiveBall ("Metal_Touch_9"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 10 : PlaySoundAtLevelActiveBall ("Metal_Touch_10"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 11 : PlaySoundAtLevelActiveBall ("Metal_Touch_11"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 12 : PlaySoundAtLevelActiveBall ("Metal_Touch_12"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 13 : PlaySoundAtLevelActiveBall ("Metal_Touch_13"), Vol(ActiveBall) * MetalImpactSoundFactor
	End Select
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
 		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_2"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
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
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_1"), BottomArchBallGuideSoundFactor * 0.25
		Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_2"), BottomArchBallGuideSoundFactor * 0.25
		Case 3 : PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_3"), BottomArchBallGuideSoundFactor * 0.25
	End Select
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
 		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Medium_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_2"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 3 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
		End Select
 	End If
	If finalspeed < 6 Then
 		Select Case Int(Rnd*7)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Soft_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Soft_2"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 3 : PlaySoundAtLevelActiveBall ("Apron_Soft_3"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 4 : PlaySoundAtLevelActiveBall ("Apron_Soft_4"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 5 : PlaySoundAtLevelActiveBall ("Apron_Soft_5"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 6 : PlaySoundAtLevelActiveBall ("Apron_Soft_6"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 7 : PlaySoundAtLevelActiveBall ("Apron_Soft_7"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  SPINNER  ////////////////////////////
Sub SoundSpinner()
	PlaySoundAtLevelStatic ("Spinner_12"), SpinnerSoundLevel, sw38
End Sub

Sub SoundMotor()
	PlaySoundAtLevelStatic ("fx_motor"), ToyBoxSoundLevel, ToyBoxKicker
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_5",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_6",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_7",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_8",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor		
	End Select
End Sub

Sub RandomSoundTargetHitWeak()
	Select Case Int(Rnd*4)+1		
		Case 1 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_1",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_2",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_3",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_4",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
	End Select
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
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_3"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_4"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_6"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
	End Select
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
	Select Case Int(Rnd*2)+1				
		Case 1 : PlaySoundAtLevelStatic ("Gate_FastTrigger_1"), GateSoundLevel, Activeball
		Case 2 : PlaySoundAtLevelStatic ("Gate_FastTrigger_2"), GateSoundLevel, Activeball
	End Select
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
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Arch_L1"), Vol(ActiveBall) * ArchSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Arch_L2"), Vol(ActiveBall) * ArchSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Arch_L3"), Vol(ActiveBall) * ArchSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Arch_L4"), Vol(ActiveBall) * ArchSoundFactor
	End Select
End Sub

Sub RandomSoundRightArch()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Arch_R1"), Vol(ActiveBall) * ArchSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Arch_R2"), Vol(ActiveBall) * ArchSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Arch_R3"), Vol(ActiveBall) * ArchSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Arch_R4"), Vol(ActiveBall) * ArchSoundFactor
	End Select
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
	Select Case Int(Rnd*2)+1	
		Case 1: PlaySoundAtLevelStatic ("Saucer_Enter_1"), SaucerLockSoundLevel, Activeball
		Case 2: PlaySoundAtLevelStatic ("Saucer_Enter_2"), SaucerLockSoundLevel, Activeball
	End Select
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

'******************************************************
'		TRACK ALL BALL VELOCITIES
' 		FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs

		for each b in allballs
			If b.id >= HighestID then highestID = b.id
		Next

		If uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
		If uBound(ballvelx) < highestID then redim ballvelx(highestID)	'set bounds
		If uBound(ballvely) < highestID then redim ballvely(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

Sub RDampen_Timer()
	Cor.Update
End Sub

' ***************************************************************************************************
' ******************************  Lampz from funhouse
Dim MaterialWhiteArray: MaterialWhiteArray = Array("BulbWhiteOff", "BulbWhiteOff","BulbWhiteOff","BulbWhiteOn")
Dim MaterialBlueArray: MaterialBlueArray = Array("BulbBlueOff", "BulbBlueOff","BulbBlueOff","BulbBlueOn")
Dim MaterialRedArray: MaterialRedArray = Array("BulbRedOff", "BulbRedOff","BulbRedOff","BulbRedOn")
Dim MaterialYellowArray: MaterialYellowArray = Array("BulbYellowOff", "BulbYellowOff","BulbYellowOff","BulbYellowOn")

Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
InitLampsNF              ' Setup lamp assignments
LampTimer.Interval = -1
LampTimer.Enabled = 1

Sub LampTimer_Timer()
	dim x, chglamp
	If b2son then chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
	Lampz.Update1	'update (fading logic only)
End Sub

dim FrameTime, InitFrameTime : InitFrameTime = 0
LampTimer2.Interval = -1
LampTimer2.Enabled = True
Sub LampTimer2_Timer()	'Stealing this random wall's timer for -1 updates
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
	Lampz.Update 'updates on frametime (Object updates only)
End Sub

Function FlashLevelToIndex(Input, MaxSize)
	FlashLevelToIndex = cInt(MaxSize * Input)
End Function

'***Material Swap***
'Fade material for green, red, yellow colored Bulb prims
Sub FadeMaterialColoredBulb(pri, group, ByVal aLvl)	'cp's script
	If Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select Case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
        Case 3:pri.Material = group(3) 'Full
    End Select
	pri.blenddisablelighting = aLvl * 1 'Intensity Adjustment
End Sub


'Fade material for red, yellow colored bulb Filiment prims
Sub FadeMaterialColoredFiliment(pri, group, ByVal aLvl)	'cp's script
	If Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select Case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
        Case 3:pri.Material = group(3) 'Full
    End Select
	'If tb.text <> pri.image then tb.text = pri.image : debug.print pri.image end If	'debug
	pri.blenddisablelighting = aLvl * 50  'Intensity Adjustment
End Sub


Sub InitLampsNF()
	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (1 / full MS fading time)
	dim x : for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 3/10 : Lampz.FadeSpeedDown(x) = 3/10 : next

	'Lamp Assignments
	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays
    Lampz.MassAssign(1)= l39
    Lampz.Callback(1) =         "FadeMaterialColoredBulb pFBase39, MaterialBlueArray, "
    Lampz.Callback(1) = "FadeMaterialColoredFiliment pFiliment39, MaterialBlueArray, "

    Lampz.MassAssign(2)= l40
    Lampz.Callback(2) =         "FadeMaterialColoredBulb pFBase40, MaterialYellowArray, "
    Lampz.Callback(2) = "FadeMaterialColoredFiliment pFiliment40, MaterialYellowArray, "

	'Turn off all lamps on startup
	Lampz.Init	'This just turns state of any lamps to 1

	'Immediate update to turn on GI, turn off lamps
	Lampz.update

End Sub


'====================
'Class jungle nf
'=============

'No-op object instead of adding more conditionals to the main loop
'It also prevents errors If empty lamp numbers are called, and it's only one object
'should be g2g?

Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
' Note: If using multiple 'LampFader' objects, set the 'name' variable to avoid conflicts with callbacks

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
			If IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property		'debug.print Lampz.Locked(100)	'debug
	Public Property Get state(idx) : state = OnOff(idx) : end Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : If UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function
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
		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		'If idx = 132 then msgbox out	'debug
		ExecuteGlobal Out

	End Property

	Public Property Let state(ByVal idx, input) 'Major update path
		If Input <> OnOff(idx) then  'discard redundant updates
			OnOff(idx) = input
			Lock(idx) = False
			Loaded(idx) = False
		End If
	End Property

	'Mass assign, Builds arrays where necessary
	'Sub MassAssign(aIdx, aInput)
	Public Property Let MassAssign(aIdx, aInput)
		If typename(obj(aIdx)) = "NullFadingObject" Then 'If empty, use Set
			If IsArray(aInput) then
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
			If IsArray(obj(idx)) then
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					If typename(tmp(x)) = "Light" then DisableState tmp(x)' : debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.001 ' this can prevent init stuttering
				Next
			Else
				If typename(obj(idx)) = "Light" then DisableState obj(idx)' : debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.001 ' this can prevent init stuttering
			end if
		Next
		'debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'Just runs TurnOnStates right now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all boolean numeric fading. If done fading, Lock(x) = True. Update on a '1' interval Timer!
		dim x : for x = 0 to uBound(OnOff)
			If not Lock(x) then 'and not Loaded(x) then
				If OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					If Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseIf Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					If Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(OnOff)
			If not Lock(x) then 'and not Loaded(x) then
				If OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					If Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseIf Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					If Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx : for x = 0 to uBound(OnOff)
			If not Loaded(x) then
				If IsArray(obj(x) ) Then	'If array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(Lvl(x)*Mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*Mult(x) : Next
					End If
				else						'If single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(Lvl(x)*Mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)
					End If
				end if
				If TypeName(lvl(x)) <> "Double" and typename(lvl(x)) <> "Integer" then msgbox  "uhh " & 2 & " = " & lvl(x)
				If UseCallBack(x) then Proc name & x,Lvl(x)*mult(x)	'Proc
				If Lock(x) Then
					If Lvl(x) = 1 or Lvl(x) = 0 then Loaded(x) = True	'finished fading
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
	If err.number = 13 then  msgbox  "Proc error! No such procedure: " & vbnewline & string
	If err.number = 424 then msgbox  "Proc error! No such Object"
End Sub

Function AppendArray(ByVal aArray, aInput)	'append one value, object, or Array onto the end of a 1 dimensional array
	If IsArray(aInput) then 'Input is an array...
		dim tmp : tmp = aArray
		If not IsArray(aArray) Then	'If not array, create an array
			tmp = aInput
		Else						'Append existing array with aInput array
			Redim Preserve tmp(uBound(aArray) + uBound(aInput)+1)	'If existing array, increase bounds by uBound of incoming array
			dim x : for x = 0 to uBound(aInput)
				If isObject(aInput(x)) then
					Set tmp(x+uBound(aArray)+1 ) = aInput(x)
				Else
					tmp(x+uBound(aArray)+1 ) = aInput(x)
				End If
			Next
		AppendArray = tmp	 'return new array
		End If
	Else 'Input is NOT an array...
		If not IsArray(aArray) Then	'If not array, create an array
			aArray = Array(aArray, aInput)
		Else
			Redim Preserve aArray(uBound(aArray)+1)	'If array, increase bounds by 1
			If isObject(aInput) then
				Set aArray(uBound(aArray)) = aInput
			Else
				aArray(uBound(aArray)) = aInput
			End If
		End If
		AppendArray = aArray 'return new array
	End If
End Function

' ******************************  END Lampz from funhouse
' ***************************************************************************************************


' *********************************************************************
'                        Table Object Hit Events
'
' Any target hit Sub will follow this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/Mode this trigger is a member of
' - set the "LastSwitchHit" variable in Case it is needed later
' *********************************************************************

' Slingshots has been hit

Dim LStep, RStep

Sub RightSlingShot_Slingshot
	RandomSoundSlingshotRight SLING1
	FlashLevel(9) = 1 : FlasherFlash9_Timer
	RSling.Visible = 0
	RSling1.Visible = 1
	sling1.TransZ = -20
	RStep = 0
	RightSlingShot.TimerEnabled = 1
	AddScore 30
End Sub


Sub RightSlingShot_Timer
	Select Case RStep
		Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
		Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
	End Select
	RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	RandomSoundSlingshotLeft SLING2
	FlashLevel(10) = 1 : FlasherFlash10_Timer
	LSling.Visible = 0
	LSling1.Visible = 1
	sling2.TransZ = -20
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
	AddScore 30
End Sub

Sub LeftSlingShot_Timer
	Select Case LStep
		Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
		Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
	End Select
	LStep = LStep + 1
End Sub

Sub tmrPops_Timer
	tmrPops.Enabled=False
	PopScore=PopScoreCurrent
	PopScoreCurrent=0
End Sub

Sub Bumper001_Hit
    If Tilted Then Exit Sub
	RandomSoundBumperTop Bumper001

	'BumperLight001.State = 2
	FlashForMs BumperLight001, 200, 50, 0

	'FlashForMs Bumper1Light, 200, 50, 0

	If PlayerMode=5 and not bModeProgressUpgraded and StackState(kStack_Pri0).GetArrowState(I107) = 0 then 'move the shot
		If StackState(kStack_Pri0).GetArrowState(I35) <> 0 Then
			SSetLightColor kStack_Pri0, I35, modeRampColor, 0
			SSetLightColor kStack_Pri0, I62, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I62) <> 0 Then
			SSetLightColor kStack_Pri0, I62, modeRampColor, 0
			SSetLightColor kStack_Pri0, I81, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I81) <> 0 Then
			SSetLightColor kStack_Pri0, I81, modeRampColor, 0
			SSetLightColor kStack_Pri0, I86, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I86) <> 0 Then
			SSetLightColor kStack_Pri0, I86, modeRampColor, 0
			SSetLightColor kStack_Pri0, I91, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I91) <> 0 Then
			SSetLightColor kStack_Pri0, I91, modeRampColor, 0
			SSetLightColor kStack_Pri0, I97, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I97) <> 0 Then
			SSetLightColor kStack_Pri0, I97, modeRampColor, 0
			SSetLightColor kStack_Pri0, I103, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I103) <> 0 Then
			SSetLightColor kStack_Pri0, I103, modeRampColor, 0
			SSetLightColor kStack_Pri0, I35, modeRampColor, 1
		End If
	End if

	If Not bWizardMode Then
		PopHits=PopHits-1
		BumperBonus=BumperBonus+1
		If PopHits <=0 Then
			PopValue=int(PopValue*1.15)
			BumperMultiplier=BumperMultiplier+1
			PopHits=25
			PopLevel=PopLevel+1
			SetPopColors()
		End If
	End If

	AddScore (PopValue * BumperMultiplier+(500*PopLevel))
	PopScoreCurrent=PopScoreCurrent+(PopValue * BumperMultiplier)
	tmrPops.Enabled=False
	tmrPops.Enabled=True
    CheckModeProgress("switch")
	CheckModeProgress("bumper")
	LastSwitchHit="bumper"
End Sub

Sub Bumper002_Hit
    If Tilted Then Exit Sub
	RandomSoundBumperMiddle Bumper002

	FlashForMs BumperLight002, 200, 50, 0


	If PlayerMode=5 and not bModeProgressUpgraded and StackState(kStack_Pri0).GetArrowState(I107) = 0 then 'move the shot
		If StackState(kStack_Pri0).GetArrowState(I35) <> 0 Then
			SSetLightColor kStack_Pri0, I35, modeRampColor, 0
			SSetLightColor kStack_Pri0, I62, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I62) <> 0 Then
			SSetLightColor kStack_Pri0, I62, modeRampColor, 0
			SSetLightColor kStack_Pri0, I81, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I81) <> 0 Then
			SSetLightColor kStack_Pri0, I81, modeRampColor, 0
			SSetLightColor kStack_Pri0, I86, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I86) <> 0 Then
			SSetLightColor kStack_Pri0, I86, modeRampColor, 0
			SSetLightColor kStack_Pri0, I91, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I91) <> 0 Then
			SSetLightColor kStack_Pri0, I91, modeRampColor, 0
			SSetLightColor kStack_Pri0, I97, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I97) <> 0 Then
			SSetLightColor kStack_Pri0, I97, modeRampColor, 0
			SSetLightColor kStack_Pri0, I103, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I103) <> 0 Then
			SSetLightColor kStack_Pri0, I103, modeRampColor, 0
			SSetLightColor kStack_Pri0, I35, modeRampColor, 1
		End If
	End if

	If Not bWizardMode Then
		PopHits=PopHits-1
		BumperBonus=BumperBonus+1
		If PopHits <=0 Then
			PopValue=int(PopValue*1.15)
			BumperMultiplier=BumperMultiplier+1
			PopHits=25
			PopLevel=PopLevel+1
			SetPopColors()
		End If
	End If

	AddScore (PopValue * BumperMultiplier)
	PopScoreCurrent=PopScoreCurrent+(PopValue * BumperMultiplier)
	tmrPops.Enabled=False
	tmrPops.Enabled=True
    CheckModeProgress("switch")
	CheckModeProgress("bumper")
	LastSwitchHit="bumper"
End Sub

Sub Bumper003_Hit
    If Tilted Then Exit Sub
	RandomSoundBumperBottom Bumper003

	FlashForMs BumperLight003, 200, 50, 0
	'BumperLight003.State = 2
    'FlashForMs Bumper3Light, 200, 50, 0

	If PlayerMode=5 and not bModeProgressUpgraded and StackState(kStack_Pri0).GetArrowState(I107) = 0 then 'move the shot
		If StackState(kStack_Pri0).GetArrowState(I35) <> 0 Then
			SSetLightColor kStack_Pri0, I35, modeRampColor, 0
			SSetLightColor kStack_Pri0, I62, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I62) <> 0 Then
			SSetLightColor kStack_Pri0, I62, modeRampColor, 0
			SSetLightColor kStack_Pri0, I81, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I81) <> 0 Then
			SSetLightColor kStack_Pri0, I81, modeRampColor, 0
			SSetLightColor kStack_Pri0, I86, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I86) <> 0 Then
			SSetLightColor kStack_Pri0, I86, modeRampColor, 0
			SSetLightColor kStack_Pri0, I91, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I91) <> 0 Then
			SSetLightColor kStack_Pri0, I91, modeRampColor, 0
			SSetLightColor kStack_Pri0, I97, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I97) <> 0 Then
			SSetLightColor kStack_Pri0, I97, modeRampColor, 0
			SSetLightColor kStack_Pri0, I103, modeRampColor, 1
		ElseIf StackState(kStack_Pri0).GetArrowState(I103) <> 0 Then
			SSetLightColor kStack_Pri0, I103, modeRampColor, 0
			SSetLightColor kStack_Pri0, I35, modeRampColor, 1
		End If
	End if

	If Not bWizardMode Then
		PopHits=PopHits-1
		BumperBonus=BumperBonus+1
		If PopHits <=0 Then
			PopValue=int(PopValue*1.15)
			BumperMultiplier=BumperMultiplier+1
			PopHits=25
			PopLevel=PopLevel+1
			SetPopColors()
		End If
	End If

	AddScore (PopValue * BumperMultiplier)
	PopScoreCurrent=PopScoreCurrent+(PopValue * BumperMultiplier)
	tmrPops.Enabled=False
	tmrPops.Enabled=True
    CheckModeProgress("switch")
	CheckModeProgress("bumper")
	LastSwitchHit="bumper"
End Sub

Sub SetPopColors()
	Select Case PopLevel:
		Case 0: SetLightColor BumperLight001, "white", 0:SetLightColor BumperLight002, "white", 0:SetLightColor BumperLight003, "white", 0
		Case 3: SetLightColor BumperLight001, "cyan", 0:SetLightColor BumperLight002, "cyan", 0:SetLightColor BumperLight003, "cyan", 0
		Case 5: SetLightColor BumperLight001, "red", 0:SetLightColor BumperLight002, "red", 0:SetLightColor BumperLight003, "red", 0
		Case 8: SetLightColor BumperLight001, "blue", 0:SetLightColor BumperLight002, "blue", 0:SetLightColor BumperLight003, "blue", 0
		Case 12: SetLightColor BumperLight001, "purple", 0:SetLightColor BumperLight002, "purple", 0:SetLightColor BumperLight003, "purple", 0
		Case 14: SetLightColor BumperLight001, "yellow", 0:SetLightColor BumperLight002, "yellow", 0:SetLightColor BumperLight003, "yellow", 0
		Case 16: SetLightColor BumperLight001, "green", 0:SetLightColor BumperLight002, "green", 0:SetLightColor BumperLight003, "green", 0
	End Select
End Sub

Sub sw61_hit 'saucer
	Dim delay, ShootToyBox
	SoundSaucerLock
 	delay=2000
	shootToyBox=false
	If BallsOnPlayfield = 1 then bPauseTimer=True

	If tmrSkillshot.Enabled and LastSwitchHit="ballsavestarttrigger" Then		
		PlayDMDScene "", "SKILL SHOT!", "+1 MISSILE", 2000
		AddScore 500000 ' todo track # of skill shots and use as a multiplier
		DOF 127, DOFPulse

		QueueScene "ScenePlayMessage ""Video-0x0006.mp4"", """",""SKILL SHOT"","""" '", 2000, 1
		QueueScene "SceneClearPlayMessage '", 0, 1
		SmartButtonCount=SmartButtonCount+1
		'todo add to timer
	Else
		AddScore 1200
	End If 
	'pJesterhead.Collidable=False
	ClearShotMultiplier(I101)
	CheckModeProgress("I97")
	Delay=2200
	If I100.state=2 and not bElevMultiBall Then ' lock is lit - should not be set during wizard modes
		SetLightColor I100, "green", 0
		bStartMB=False
		If I75.state=2 Then
				shootToyBox=True:I75.state=1
				If bToyBoxMultiball Then 
					D "ToyBoxMB 2x earned"
					QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", ""500,000"",""BALL LOCKED"",""ALL SCORES 2X FOR 20 SECONDS"" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
					bToyBoxBonus3x = False
					tmrToyBoxMBBonus.Enabled = True
					tmrToyBoxMBBonus.UserValue = 20
					AddScore 500000
					AddPlayMultiplier 2
					Delay=500
				Else
					QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""1 TOY"",""LOCKED"","""" '", 1000, 1
					QueueScene "SceneClearPlayMessage '", 0, 1
				End If
		ElseIf I76.state=2 Then	
				shootToyBox=True:I76.state=1
				If bToyBoxMultiball Then 
					QueueScene "ScenePlayMessage ""Video-0x0047.mp4"", ""550,000"",""BALL LOCKED"",""ALL SCORES 3X FOR 20 SECONDS"" '", 1500, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
					AddScore 550000
					If tmrToyBoxMBBonus.Enabled then
						tmrToyBoxMBBonus.Enabled = True
						tmrToyBoxMBBonus.UserValue = 20
						If bToyBoxBonus3x=False then 
							bToyBoxBonus3x = True
							AddPlayMultiplier 3/2					' Remove the 2x and add the 3x
						Else
							AddPlayMultiplier 2
						End If
D "Starting 3x"
						Delay=500
					end if
				Else
					QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""2 TOYS"",""LOCKED"","""" '", 1000, 1
					QueueScene "SceneClearPlayMessage '", 0, 1
				End If   'TODO DONT ALLOW ANYMORE LOCKS with 3x running.
		ElseIf I77.state=2 Then		
				shootToyBox=True:I77.state=1  
				bStartMB=True
				vpmTimer.addtimer 7500,"ToyBoxMB_Start '"
				ScenePlayMessage "Video-0x0044.mp4", "","",""
				PlayDMDScene "", "3-2-1..", "", 2000
		ElseIf I78.state=2 Then		
				shootToyBox=True:I78.state=1
				bStartMB=True
				vpmTimer.addtimer 7500,"ToyBoxMB_Start '"
				ScenePlayMessage "Video-0x0044.mp4", "","",""
				PlayDMDScene "", "3-2-1..", "", 2000
		ElseIf I79.state=2 Then		
				shootToyBox=True:I79.state=1
				bStartMB=True
				vpmTimer.addtimer 7500,"ToyBoxMB_Start '"
				ScenePlayMessage "Video-0x0044.mp4", "","",""
				PlayDMDScene "", "3-2-1..", "", 2000
		ElseIf I80.state=2 Then		
				shootToyBox=True:I80.state=1
				bStartMB=True
				vpmTimer.addtimer 7500,"ToyBoxMB_Start '"
				ScenePlayMessage "Video-0x0044.mp4", "","","" 
				PlayDMDScene "", "3-2-1..", "", 2000
			End If
	End If
	If BallsOnPlayfield = 1 then ' only 1 ball left, dont let it be locked
		If bToyBoxMultiball Then
			SetLightColor I100, "green",0
			ToyBox_CloseLid()
		end if
	End If

	MultiplierShot = 1		' Clear shot multiplier 
	vpmTimer.addtimer Delay, "RightEject " & shootToyBox & " '"
End Sub

dim bBallInBox, bStartMB
Sub RightEject(ShootToyBox)
	D "Right Eject " & ShootToyBox
	If ShootToyBox then
		SoundSaucerKick 1, sw61
		sw61.Kick -36, 37, 55 
		bBallInBox=False
	' No active locks for toybox
		If I76.state <> 2 AND I77.state <> 2 AND I78.state <> 2 AND I79.state <> 2 AND I80.state <> 2 then
			vpmTimer.addtimer 2000, "ToyBox_CloseLid '"
		End If
		vpmTimer.addtimer 3500, "ToyBox_Check '"
' check for success
' If success then release ball in shooter Lane
' either Case
' close lid
	Else
		SoundSaucerKick 1, sw61
		sw61.Kick -36, 7+RndNum(0,5)
		AddScore 1500
		bPauseTimer=False
	End If
' Relight LOCK If earned
	If not bElevMultiBall and not bWizardMode then
		If I80.state=2 or I79.state=2 or I78.state=2 or I77.state=2 or I76.state=2 Then
			D "Relight LOCK light"
			SetLightColor I100, "green", 2
		End If
	End If
End Sub

Sub ToyBoxKicker_Hit
	bBallInBox=True
	RandomSoundDrain ToyBoxKicker

	RealBallsInLock=RealBallsInLock+1
	BallsOnPlayfield=BallsOnPlayfield-1
	D "ToyboxKicker_Hit()  BOP: " & ballsOnPlayfield & " RealBall: " & RealBallsInLock

	ToyBoxKicker.DestroyBall
	' ToyBox_CloseLid() closed via timer in case it missed
End Sub

Dim SaveI65, SaveI94, SaveI110

Sub ToyBoxMB_Start
	Dim ballsLocked,a,b 
	D "sub ToyBoxMB_Start()"
	If bStartMB Then
		D "sub ToyBoxMB_Start() Too Late to Cancel"		
		bStartMB=False ' Too Late to Cancel
		bToyBoxMultiball=True
		ToyBox_CloseLid()

		ToyBoxMBAttempts=ToyBoxMBAttempts+1
		QueueScene "ScenePlayMessage ""Video-0x0065.mp4"", ""TOY BOX"",""MULTIBALL"","""" '", 7500, 1
		QueueScene "SceneClearPlayMessage '", 0, 1

		' determine number of balls to release, release the realballs first up to the number of balls in locks

		aRampLightsSave() 'Save the current lights 

		bToyBoxMultiBall=True
		ToyBoxMultiBallCount=0
		SetLightColor I22, "green", 2		' Flash ToyBox on lower playfield
		SetLightColor F147,"white", 2

' Turn off Elevator Lights and Lock Lights
		SaveI65=I65.state
		SaveI94=I94.state
		SaveI110=I110.state
		SetLightColor I94, "white", 0
		SetLightColor I65, "white", 0
		SetLightColor I110,"white", 0

		PuPlayer.playlistplayex pMusic,"audioclear","clear.mp3",100, 1
	
		SaveMode=PlayerMode
		PlayerMode=7
		vpmTimer.addtimer 2500, "PlaySong ""Song-7.mp3"" '"
		StartPlayerModeVideo False  
		PlayerMode=SaveMode
		pDMDEvent(P) 
		setModeSelectLight(False) ' Cant choose a new song once we start MB

		StackState(kStack_Pri1).Enable(8)
		SSetLightColor kStack_Pri1, I34, "white", 0		

		b=0
		For each a in aRampLights
			SSetLightColor kStack_Pri1, a, "green", 2
			shots(b)=2   ' takes 2 shots to turn it off
			shots(b)=2   ' takes 2 shots to turn it off
			b=b+1
		Next
		FlashEffect(2)

		ToyBoxMBJackpotBase=500000
		BallsLocked=3
		If I78.state=1 then BallsLocked=4:ToyBoxMBJackpotBase=600000
		If I79.state=1 then BallsLocked=5:ToyBoxMBJackpotBase=700000
		If I80.state=1 then BallsLocked=6:ToyBoxMBJackpotBase=1000000

		for each b in aLockLights
			SetLightColor b, "green", 0
			b.uservalue=2  ' todo higher each time
		Next
		SetLightColor F147, "white", 2

		D "ToyBoxMB: Locks: " & BallsLocked & " InToyBox: " & RealBallsInLock
		If RealBallsInLock > 0 Then
			ToyBox_OpenLid()
			If RealBallsInLock < ballsLocked then
				ballsLocked=ballsLocked-RealBallsInLock
				D "ToyBoxMB Adding ToyBox balls " & RealBallsInLock
				AddToyBoxMultiball RealBallsInLock
				RealBallsInLock=0
				D "TOYBOX MB Add BOP: " & ballsOnPlayfield & " RealBall: " & RealBallsInLock
			Else
				D "ToyBoxMB Adding ALL Real balls " & RealBallsInLock
				AddToyBoxMultiball RealBallsInLock
				RealBallsInLock=0
				ballsLocked=0
				D "TOYBOX2 MB Add BOP: " & ballsOnPlayfield & " RealBall: " & RealBallsInLock
			End If
		End if
		If ballsLocked > 0 then 
			D "Virtual Balls " & ballsLocked
			AddMultiball ballsLocked
		End if
		D "ToyBoxMB: After ...  " & ballsLocked & " Real:" & RealBallsInLock & " state:" & I34.state
		ScorbitBuildGameModes()
	Else
		D "ToyBoxMB - Cancelled"
	End If
	D "sub ToyBoxMB_Start() - Done"  & " state:" & I34.state
End Sub

Sub ToyBox_Cancel()
	D "ToyBox_Cancel " & bStartMB    ' last solid light is the last ball locked
	If I77.state=1 and I78.state<>1 Then
		I77.state=1:bStartMB=False
		PlayDMDScene "", "3 Toys Locked", "", 2000
		QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""3 TOYS"",""LOCKED"","""" '", 1000, 1
		QueueScene "SceneClearPlayMessage '", 0, 1
	ElseIf I78.state=1 and I79.state<>1 Then		
		I78.state=1:bStartMB=False
		PlayDMDScene "", "4 Toys Locked", "", 2000
		QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""4 TOYS"",""LOCKED"","""" '", 1000, 1
		QueueScene "SceneClearPlayMessage '", 0, 1
	ElseIf I79.state=1 and I80.state<>1 Then
		I79.state=1:bStartMB=False
		PlayDMDScene "", "5 Toys Locked", "", 2000
		QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""5 TOYS"",""LOCKED"","""" '", 1000, 1
		QueueScene "SceneClearPlayMessage '", 0, 1
	End If
	If I78.state=2 or I79.state=2 or I80.state=2 Then
		SetLightColor I100, "green", 2
		ToyBox_OpenLid
	End If
	D "ToyBox_Cancel ..Done. " & bStartMB
End Sub

Sub ToyBox_OpenLid
	D "===OPEN LID=="
	ToyBox_Direction = 2
	if tmrToyBox.Enabled=False Then tmrToyBox.Enabled = True:SoundMotor:FlashEffect(2)
End Sub


Sub ToyBox_CloseLid
	ToyBox_Direction = -2
	if tmrToyBox.Enabled=False Then tmrToyBox.Enabled = True:SoundMotor:FlashEffect(3)
End Sub

Dim Toybox_Direction, ToyBox_Angle
ToyBox_Angle = 1
ToyBox_Direction = 0

Sub tmrToyBox_Timer()      ' if down ... check if down off timer else move the lid down
	ToyBox_Angle = ToyBox_Angle + ToyBox_Direction
	if ToyBox_Direction < 0 then
		if ToyBox_Angle <= 1 then
			ToyBox_Angle = 1
			tmrToyBox.Enabled = False
			Exit Sub
		End If
	Else
		if ToyBox_Angle >= 30 Then
			ToyBox_Angle = 30
			tmrToyBox.Enabled = False
			Exit Sub
		End If
	End If
	pBoxLid.ObjRotx = ToyBox_Angle
	pBoxLid.ObjRoty = ToyBox_Angle*(-3/10)
	pBoxArms.ObjRotx = ToyBox_Angle
	pBoxArms.ObjRoty = ToyBox_Angle*(-3/10)
	pJesterHead.ObjRotx = ToyBox_Angle
	pJesterHead.ObjRoty = ToyBox_Angle*(-3/10)
End Sub

Sub ToyBox_Check
	Dim a
	D "Sub ToyBox_Check() Check If successful toybox shotbaBallInBox: " & bBallInBox & _ 
		" bStartMB:" & bStartMB & " bToyBoxMultiball:" & bToyBoxMultiball
	If bBallInBox and not bStartMB and not bToyBoxMultiball Then
		D "Ball locked in toybox successfully StartMB? " & bStartMB
		AddMultiball 1
		' bPauseTimer handled in the plunger lane
	Else	
		bPauseTimer=False
	End If
	If bToyBoxMultiball then ' you locked a ball during toybox MB_Multiplier
'If no playfield balls left then release the balls in the lock
		If BallsOnPlayfield=0 Then
			D "BallsOnPlayfield=0 - add 1"
			AddMultiball RealBallsInLock
			RealBallsInLock=0
			D "TOYBOX EMPTY: " & ballsOnPlayfield & " RealBall: " & RealBallsInLock
			for each a in aLockLights
				SetLightColor a, "green", 0
				a.uservalue=2 ' todo higher each time
			Next
		End If
	end if
	bBallInBox=False
End Sub

Sub tmrToyBoxMBBonus_Timer()
	Dim a
	D "ToyBoxMB Timer " & tmrToyBoxMBBonus.UserValue

	tmrToyBoxMBBonus.UserValue = tmrToyBoxMBBonus.UserValue - 1
	If bToyBoxBonus3x then 
		If bUsePUPDMD then PuPlayer.LabelSet pBackglass,"MTimer",	"3X FOR " & tmrToyBoxMBBonus.UserValue	,1,""
	Else
		If bUsePUPDMD then PuPlayer.LabelSet pBackglass,"MTimer",	"2X FOR " & tmrToyBoxMBBonus.UserValue	,1,""
	End If
	If tmrToyBoxMBBonus.UserValue = 0 then 
		If (bToyBoxBonus3x) Then 			' Clear 3x or 2x Play Multipliers 
			AddPlayMultiplier 1/3
			D "Add for the 2nd lock"
			ToyBox_OpenLid()
			AddToyBoxMultiball 2
			RealBallsInLock=0
			D "TOYBOX MB BONUS 3x: " & ballsOnPlayfield & " RealBall: " & RealBallsInLock
		Else
			AddPlayMultiplier 1/2	
			ToyBox_OpenLid()
			AddToyBoxMultiball 1
			RealBallsInLock=0
			D "TOYBOX MB BONUS 2x: " & ballsOnPlayfield & " RealBall: " & RealBallsInLock
		End If 
		D "Done 2x/3x releasing ball(s)"
		bToyBoxBonus3x = False
		tmrToyBoxMBBonus.Enabled = False
		If bUsePUPDMD then puPlayer.LabelSet pBackglass,"MTimer",	""	,1,""

		for each a in aLockLights
			SetLightColor a, "green", 0
			a.uservalue=2  ' todo higher each time
		Next
	End If 
End Sub 

Sub AddPlayMultiplier(n)
	PlayMultiplier = PlayMultiplier * n
	If PlayMultiplier < 1 then PlayMultiplier = 1	' Dont go less than 0
End Sub

Dim dBall, dZpos, ballInScoop

Sub ScoopEject_hit  ' 
	'D "ScoopEject_hit() " & ballInScoop
	If ballInScoop then SoundSaucerKick 1, ScoopEject:ScoopEject.Kick 178 + (5 * Rnd()), 4:exit sub
	ballInScoop=True
	If BallsOnPlayfield = 1 then bPauseTimer=True
	Set dBall = ActiveBall
	dZpos = dBall.Z
	ScoopDelay=200
	tmrLightFlash.Interval = 100
	bFlashFastEnabled = True

	ScoopEject.TimerInterval = 10
	ScoopEject.TimerEnabled = True
End Sub


Dim scoopdelay:scoopdelay=0

Sub ScoopEject_Timer
	Dim i
	dBall.Z = dZpos
	dZpos = dZpos-2
	If dZpos < -24 then me.timerinterval=1000 'slow down before you eject to buy some time
	If dZpos < -30 Then
		D "sub ScoopEject......Mode:" & PlayerMode & " MultiBallMode:" & bMultiBallMode & " I34=" & I34.state & " BonusMode:" & bBonusMode
		If Tilted Then
			ScoopEject.TimerEnabled = False
			vpmtimer.addtimer ScoopDelay, "ScoopKickOut '"
			Exit Sub
		End If

		ScoopEject.TimerEnabled = False
		If tmrSkillshot.Enabled and StackState(kStack_Pri3).GetArrowState(I35) = 2 and bSkillShotsReady(1)=True Then ' Hold Left, Loop around and hit CIU, Left Orbit or Center Ramp
			AddScore 1000000
			DOF 127, DOFPulse 
			bSkillshotsReady(1) = False 
			PlayDMDScene "", "SUPER SKILL SHOT!", "+2 MISSLES", 2000
			QueueScene "ScenePlayMessage ""Video-0x0056.mp4"", ""SUPER"",""SKILL SHOT"","""" '", 2000, 1
			QueueScene "SceneClearPlayMessage '", 0, 1
			SmartButtonCount=SmartButtonCount+2
		End If 

		If I33.state <> 0 Then
			setExtraBallLight(False)
			AwardExtraBall
		End If

		If I38.state <> 0 Then	' Mystery
			setMysteryLight(False)
			AwardMystery()
		End If

		ClearShotMultiplier(I32)
		CheckModeProgress "I35"   ' if I35/I34 is lit and ready for Medley or Tour ... set it and come back..

		D "Results: tmrMedley:" & tmrMedleyTour.Enabled & " tmrFinal:" & tmrFinalTour.Enabled
		if tmrMedleyTour.Enabled then 
			D "Scoop MedleyTour - skipping remainder of scoop checks"

			if ScoopDelay = 200 then ' Not being kicked from the MedleyTour Start process
				D "kickout 5"
				vpmtimer.addtimer ScoopDelay, "ScoopKickOut '" 
			End If
			Exit Sub
		elseif tmrFinalTour.Enabled then
			D "Scoop Final Tour - skipping remainder of scoop checks"
		elseif I34.state <> 0 and PlayerMode = -1 and bMultiBallMode=False and bFinalTourReady = False Then		' If we are in multiball dont go into player select
			D "Scoop - but we need a new song.. bBonusMode=" & bBonusMode & " 2nMode=" & bSecondMode & " Final:" & bFinalTourReady
			If bBonusMode Then ' Earn the Super and proceed to the song choice..???"  			
				CheckModeProgress "I34"
				D "kickout 6"
				vpmtimer.addtimer ScoopDelay, "ScoopKickOut '"
				Exit Sub
			ElseIf bSecondMode Then
				if bUsePupDMD then
					StopSound Song
				else
					EndMusic
				end if
				PauseBigScore=True
				StopPlayerMode2
			End If
			puPlayer.LabelSet pBackglass,"BigScore"," ",0,""

			EnablePlayerSelect  

			If AutoQA or AutoAI Then
				PlayerMode=0
				i=INT(7*RND())
				PlayerMode = (PlayerMode + i)
				If (PlayerMode > 6) Then PlayerMode=PlayerMode-6
				i=7
				D "Select Song " & PlayerMode
				DO
					i=i-1
					PlayerMode = (PlayerMode + 1)
					If (PlayerMode > 6) Then PlayerMode=0
					If i=0 then exit do
				Loop While ModePercent(PlayerMode) >= 100
			Else
				SelectPlayerMode LeftFlipperKey		' Force them to select a mode
			End If
			pClearMessageT()
			UpdatePlayerMode
			If autoAI then vpmtimer.addtimer 1200,"Table1_KeyDown(RightMagnaSave) '"
			If autoAI then vpmtimer.addtimer 1600,"Table1_KeyUp(RightMagnaSave) '"
			MultiplierShot = 1		' Clear shot multiplier 
			' Hold the ball until they select the next mode
			D "kickout XX NO KICK HERE"
			Exit Sub ' Bug 1.01
		Else
			MultiplierShot = 1		' Clear shot multiplier 
			If I34.state = 0 then ' Must be back in Final Tour Ready Mode and the ball was kicked during Bonus Timer
D "kickout 7"
				vpmtimer.addtimer ScoopDelay, "ScoopKickOut '" 
			End If
			Exit Sub
		End If

D "kickout 8"
		vpmtimer.addtimer ScoopDelay, "ScoopKickOut '"
	End If
End Sub

Sub ScoopKickOut
	D "Scoop - eject ball"
	ScoopEject.TimerEnabled = False
	dBall.Z = 35
	FlipperSkipCmd=""	' Clear it 
	bFlashFastEnabled = False
	tmrLightFlash.Interval = 200
	ScoopEject.Kick 178 + (5 * Rnd()), 4
	ballInScoop=False
	bPauseTimer=False
	SoundSaucerKick 1, ScoopEject
End Sub

Dim eBall, eZpos, ballInLock
Sub ElevatorEject_hit
	SoundSaucerLock
	If ballInLock then SoundSaucerKick 1, ElevatorEject:ElevatorEject.Kick 194, 14:Exit Sub	
	ballInLock=True
	AddScore 5000
	If BallsOnPlayfield = 1 then bPauseTimer=True
	Set eBall = ActiveBall
	eZpos = eBall.Z
	ElevatorEject.TimerInterval = 15
	ElevatorEject.TimerEnabled = True
End Sub

Sub ElevatorEject_Timer
	Dim ScoopDelay
	eBall.Z = eZpos
	eZpos = eZpos-2
	If eZpos < -24 then me.timerinterval=1000 'slow down before you eject to buy some time
	If eZpos < -30 Then
		ElevatorEject.TimerEnabled = False
		ScoopDelay=200
		ClearShotMultiplier(I95)
		CheckModeProgress("I91")
		If I94.state <> 0 Then ' lock is lit  
		FlashForMs I94, 200,50, 0
		If Mode2Progress(8) >= 100 Then
			ElevMultiBallCount=99
		Else
			ElevMultiBallCount=ElevMultiBallCount+1
		End If
		D "Elevator ball count: " & ElevMultiBallCount 
		Select Case ElevMultiBallCount:
			Case 1:
				QueueScene "ScenePlayMessage ""Elevator.mp4"", ""BALL 1"",""LOCKED"","""" '", 2000, 2
				QueueScene "ScenePlayMessage ""Video-0x003D.mp4"", """","""","""" '", 3000, 2
				QueueScene "SceneClearPlayMessage '", 0, 1
				SetLightColor I110,"white", 2   
				SetLightColor I65, "white", 2
				PlayDMDScene "Video-0x003D.mp4", "BALL 1", "LOCKED", 2000
			Case 2:
				QueueScene "ScenePlayMessage ""Elevator.mp4"", ""BALL 2"",""LOCKED"","""" '", 2000, 2
				QueueScene "ScenePlayMessage ""Video-0x0039.mp4"", """","""","""" '", 3000, 2
				QueueScene "SceneClearPlayMessage '", 0, 1
				SetLightColor I110,"white", 2   
				SetLightColor I65, "white", 2
				PlayDMDScene "Video-0x0039.mp4", "BALL 2", "LOCKED", 2000
			Case 3:
				QueueScene "ScenePlayMessage ""Elevator.mp4"", ""BALL 3"",""LOCKED"","""" '", 2000, 2
				QueueScene "ScenePlayMessage ""Video-0x003B.mp4"", """","""","""" '", 3000, 2
				vpmtimer.addtimer 2000, "StartElevMB '"
				QueueScene "ScenePlayMessage ""Video-0x0034.mp4"", """","""","""" '", 8000, 2
				QueueScene "SceneClearPlayMessage '", 0, 1
				ScoopDelay=6500
				PlayDMDScene "Video-0x0034.mp4", "ELEVATOR", "MULTIBALL", 2000
			Case 99:
				QueueScene "ScenePlayMessage ""Elevator.mp4"", ""MODE"",""COMPLETE"","""" '", 2000, 2
				QueueScene "SceneClearPlayMessage '", 0, 1
				SetLightColor I110,"white", 2   
				SetLightColor I65, "white", 2
		End Select

'show ball 1 Locked
'show animation
'If 3 balls then start MB 
' clear or reset the i94 lock
		End If
		MultiplierShot = 1		' Clear shot multiplier 
		vpmtimer.addtimer ScoopDelay, "ElevatorKickOut '" 
	End If
End Sub

Sub ElevatorKickOut
	D "Elevator - eject ball"
	eBall.Z = 35

	ElevatorEject.Kick 193, 20, 3.1415926/4   ' 194 off the left wall

	ballInLock=False
	bPauseTimer=False
	FlashforMs F148, 200, 50, 0
	FlashforMs F148B, 200, 50, 0
	SoundSaucerKick 1, ElevatorEject
End Sub

Sub StartElevMB 
	dim a
	I2 "Elevator Multiball!"
	D "sub StartElevMB()"
	SetLightColor I100, "green",0 ' toybox locks
	SetLightColor F147, "white",0 'toybox
	ToyBox_CloseLid()
	setModeSelectLight(False) ' Cant choose a new song once we start MB
	bElevMultiBall=True
	bAddedABall = False
	ElevMultiBallCount=0 
	ModeProgress(8)=0 
	ElevMultiBallAttempts=ElevMultiBallAttempts+1
	SetLightColor I23, "pink", 2		' Flash Elevator Multiball on lower playfield

	'If bUsePUPDMD then puPlayer.LabelSet pBackglass,"Timer",	"2000000"	,1,""
'
'	' store lights, light show, flash mode, start mode, end music, start music
'	' Mode is light all shots, get one, then light elevator, If elevator shot then score jackpot, and repeat for "each floor"
	PuPlayer.playlistplayex pMusic,"audioclear","clear.mp3",100, 1
	
	SaveMode=PlayerMode
	PlayerMode=8
	StartPlayerModeVideo False
	PlaySong "Song-" & PlayerMode & ".mp3"
	PlayerMode=SaveMode

	StackState(kStack_Pri1).Enable(8)
	For each a in aRampLights
		If a.name <> "I91" Then SSetLightColor kStack_Pri1, a, "pink", 2
	Next
	FlashEffect(5)
	vpmtimer.addtimer 2000, "AddMultiball 2 '"
End Sub

sub target001_hit
	target001.isDropped=True
end sub

Sub sw1_Timer
	sw1.TimerEnabled = False
End Sub

Sub sw1_Hit()
	If Tilted then Exit Sub
	If sw1.TimerEnabled then Exit Sub
	sw1.TimerInterval = 1000
    sw1.TimerEnabled = True

    LaneBonus = LaneBonus + 1
    AddScore 100000
    LastSwitchHit = "sw1"
	If I11.state <> 0 Then
		FlashForMs I11, 200, 50, 0
		AwardSpecial()
	End If
	PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x0350laughing.wav",100, 1
	If bBallSaverActive then BallSaverActiveBuffer=BallSaverActiveBuffer+1
End Sub

Sub sw2_Timer
	sw2.TimerEnabled = False
End Sub

Sub sw2_hit		' left inlane
	If Tilted then Exit Sub
	activeball.vely = 0.8* activeball.vely 
	activeball.angmomz = 0
	If sw2.TimerEnabled then Exit Sub
	sw2.TimerInterval = 1000
    sw2.TimerEnabled = True
	If bWizardMode Then
		If PlayerMode3 = 4 Then
			AddScore(5000)
			CheckModeProgress("sw2")
		End If
		exit Sub
	End If

    LaneBonus = LaneBonus + 1
	If F152.state = 0 Then
		FlashForMs F152, 200, 50, 1
		AddScore 5000
		CheckDude()

	Else
		AddScore 10000
	End If
	CheckModeProgress("sw2")
	LastSwitchHit="sw2"
End Sub

Sub sw3_Timer
	sw3.TimerEnabled = False
End Sub

Sub sw3_hit		' left inner lane
	If Tilted then Exit Sub
	activeball.vely = 0.8* activeball.vely 
	activeball.angmomz = 0
	If sw3.TimerEnabled then Exit Sub
	sw3.TimerInterval = 1000
    sw3.TimerEnabled = True

	If bWizardMode Then
		If PlayerMode3 = 4 Then
			AddScore(5000)
			CheckModeProgress("sw3")
		End If
		Exit Sub
	End If

    LaneBonus = LaneBonus + 1
	If F153.state = 0 Then
		FlashForMs F153, 200, 50, 1
		AddScore 5000
		CheckDude()

	Else
		AddScore 10000
	End If
	CheckModeProgress("sw3")
	LastSwitchHit="sw3"
End Sub

Sub sw5_Timer
	sw5.TimerEnabled = False
End Sub

Sub sw5_hit		' right inlane
	If Tilted then Exit Sub
	activeball.vely = 0.8* activeball.vely 
	activeball.angmomz = 0
	If sw5.TimerEnabled then Exit Sub
	sw5.TimerInterval = 1000
    sw5.TimerEnabled = True

	If bWizardMode Then
		If PlayerMode3 = 4 Then
			AddScore(5000)
			CheckModeProgress("sw5")
		End If
		exit Sub
	End If

    LaneBonus = LaneBonus + 1
	If F154.state = 0 Then
		FlashForMs F154, 200, 50, 1
		AddScore 5000
		CheckDude()
	Else
		AddScore 10000
	End If
	CheckModeProgress("sw5")
	LastSwitchHit="sw5"
End Sub

Sub sw6_Timer
	sw6.TimerEnabled = False
End Sub

Sub sw6_hit
	If Tilted then Exit Sub
	If sw6.TimerEnabled then Exit Sub
	sw6.TimerInterval = 1000
    sw6.TimerEnabled = True

	LaneBonus=LaneBonus+1
	AddScore 100000
	If I27.state <> 0 Then
		If bUsePUPDMD then 
			QueueScene "SceneBMessage """","""","""" '", 100,2
			QueueScene "ScenePlayMessage ""Video-0x0066.mp4"", """","""&FormatScore(VipValue)&""","""" '", 2000, 2
			QueueScene "SceneClearPlayMessage '", 0, 2
		Else 
			DisplayDMDText2 "VIP PASS",FormatScore(VipValue), 1000, 11, 0
		End If
		AddScore VipValue
		FlashForMs I27, 200, 50, 0
		bLaneSaverEnabled=False
		If BallsOnPlayfield=1 Then
			bPauseTimer=True
		End If
	Else
		PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x0350laughing.wav",100, 1
	End If
	If bBallSaverActive then BallSaverActiveBuffer=BallSaverActiveBuffer+1
	LastSwitchHit="sw6"
End Sub

Sub CheckAerosmith()
Dim astr, j, a

	If SpellAerosmith=9 Then
		D "CheckAerosmith() - complete"
		setMysteryLight(True)
		QueueScene "playmedia ""Video-0x0046.mp4"", ""PupMystery"", pOverVid , """", -1, """", 1, 1 '", 3000, 2

		for each a in ShotMultipliers
			if a.state = 0 then  ' If not already earned then make it available
				SetLightColor a, "green", 2
				a.uservalue = 0
			End If
		Next

		bShotMultiplierSelect=True  ' Next shot gets the multiplier
		AddScore 75000
		playmedia "ARS107-Scene-100.mp4", "PupVideos", pPopUp5, "", -1, "", 1, 1
		'QueueScene "ScenePlayMessage ""ARS107-Scene-100.mp4"", """","""",""2X"" '", 2000, 2
		'QueueScene "SceneClearPlayMessage '", 0, 2
		vpmtimer.addtimer 200, "ResetAerosmith '" 
	Else
		If Int(5*Rnd()) < 2 and SpellAerosmith < 6 Then
			playmedia "ARS107-Scene-99.mp4", "PupVideos", pPopUp5, "", -1, "", 1, 1
		End If
	End If
	PuPlayer.LabelSet pBackglass, "Aerosmith", "PuPOverlays\\aerosmith-"&SpellAerosmith&".png",1, _
							"{'mt':2,'color':111111,'width':51, 'height':18,'yalign':0,'ypos':0.0,'xpos':50.4}"
	astr = ""
	j=0
	For each a in aerosmithLights
		if a.State = 0 Then
			astr = astr + "-"
		Else
			astr = astr + mid("AEROSMITH",j+1,1)
		End If
		j=j+1
	Next
	PlayDMDScene "", CL(0,"COMPLETE"), CL(1,astr), 2000
End Sub

Sub ResetAerosmith()
	Dim a,i
	SpellAerosmith = 0
	i=0
	For each a in aerosmithLights
		a.State = 0
		HoldAerosmithLights(i)=a.state
		i=i+1
	Next
	PuPlayer.LabelSet pBackglass, "Aerosmith", "PuPOverlays\\aerosmith-"&SpellAerosmith&".png",1, _
							"{'mt':2,'color':111111,'width':51, 'height':18,'yalign':0,'ypos':0.0,'xpos':50.4}"
End Sub

Sub sw24_Timer
	sw24.TimerEnabled = False
End Sub

Sub sw24_hit  		' aerosmith A
	If Tilted then Exit Sub
	If sw24.TimerEnabled then Exit Sub
	sw24.TimerInterval = 1000
    sw24.TimerEnabled = True

	If I29.state=0 and not bSecondMode and not bMultiBallMode Then   ' debug do we still do this with SAME mode?
		If PlayerMode=2 and (bHardMode=True or bModeProgressUpgraded) Then
			AddScore 150
		Else
			AddScore(3000)
			SpellAerosmith=SpellAerosmith+1
			TargetBonus=TargetBonus+1
			SetLightColor I29, "orange", 1
			CheckAerosmith()
		End If
	Else	
		AddScore 150
	End If
	CheckModeProgress("I29")
	LastSwitchHit="sw24"
End Sub

Sub sw25_Timer
	sw25.TimerEnabled = False
End Sub

Sub sw25_hit  		' aerosmith E
	If Tilted then Exit Sub
	If sw25.TimerEnabled then Exit Sub
	sw25.TimerInterval = 1000
    sw25.TimerEnabled = True

	If I30.state=0 and not bSecondMode and not bMultiBallMode Then
		If PlayerMode=2 and (bHardMode=True or bModeProgressUpgraded) Then
			AddScore 150
		Else
			AddScore(3000)
			SpellAerosmith=SpellAerosmith+1
			TargetBonus=TargetBonus+1
			SetLightColor I30, "orange", 1
			CheckAerosmith()
		End If
	Else	
		AddScore 150
	End If
	CheckModeProgress("I30")
	LastSwitchHit="sw25"
End Sub

Sub sw26_Timer
	sw26.TimerEnabled = False
End Sub

Sub sw26_hit  		' aerosmith R
	If Tilted then Exit Sub
	If sw26.TimerEnabled then Exit Sub
	sw26.TimerInterval = 1000
    sw26.TimerEnabled = True

	If I31.state=0 and not bSecondMode and not bMultiBallMode Then
		If PlayerMode=2 and (bHardMode=True or bModeProgressUpgraded) Then
			AddScore 150
		Else
			AddScore(3000)
			SpellAerosmith=SpellAerosmith+1
			TargetBonus=TargetBonus+1
			SetLightColor I31, "orange", 1
			CheckAerosmith()
		End If
	Else	
		AddScore 150
	End If
	CheckModeProgress("I31")
	LastSwitchHit="sw26"
End Sub

Sub sw33_Timer
	sw33.TimerEnabled = False
End Sub

Sub sw33_hit  		' aerosmith H
	If Tilted then Exit Sub
	If sw33.TimerEnabled then Exit Sub
	sw33.TimerInterval = 1000
    sw33.TimerEnabled = True

	If I42.state=0 and not bSecondMode and not bMultiBallMode Then
		If PlayerMode=2 and (bHardMode=True or bModeProgressUpgraded) Then
			AddScore 150
		Else
			AddScore(3000)
			SpellAerosmith=SpellAerosmith+1
			TargetBonus=TargetBonus+1
			SetLightColor I42, "orange", 1
			CheckAerosmith()
		End If
	Else	
		AddScore 150
	End If
	CheckModeProgress("I42")
	LastSwitchHit="sw33"
End Sub

Sub sw34_Timer
	sw34.TimerEnabled = False
End Sub

Sub sw34_hit  		' aerosmith T
	If Tilted then Exit Sub
	If sw34.TimerEnabled then Exit Sub
	sw34.TimerInterval = 1000
    sw34.TimerEnabled = True

	If I43.state=0 and not bSecondMode and not bMultiBallMode Then
		If PlayerMode=2 and (bHardMode=True or bModeProgressUpgraded) Then
			AddScore 150
		Else
			AddScore(3000)
			SpellAerosmith=SpellAerosmith+1
			TargetBonus=TargetBonus+1
			SetLightColor I43, "orange", 1
			CheckAerosmith()
		End If
	Else	
		AddScore 150
	End If
	CheckModeProgress("I43")
	LastSwitchHit="sw34"
End Sub

Sub sw35_Timer
	sw35.TimerEnabled = False
End Sub

Sub sw35_hit  		' aerosmith I
	If Tilted then Exit Sub
	If sw35.TimerEnabled then Exit Sub
	sw35.TimerInterval = 1000
    sw35.TimerEnabled = True

	If I44.state=0 and not bSecondMode and not bMultiBallMode Then
		If PlayerMode=2 and (bHardMode=True or bModeProgressUpgraded) Then
			AddScore 150
		Else
			AddScore(3000)
			SpellAerosmith=SpellAerosmith+1
			TargetBonus=TargetBonus+1
			SetLightColor I44, "orange", 1
			CheckAerosmith()
		End If
	Else	
		AddScore 150
	End If
	CheckModeProgress("I44")
	LastSwitchHit="sw35"
End Sub

Sub sw36_Timer
	sw36.TimerEnabled = False
End Sub

Sub sw36_hit  		' aerosmith M
	If Tilted then Exit Sub
	If sw36.TimerEnabled then Exit Sub
	sw36.TimerInterval = 1000
    sw36.TimerEnabled = True

	If I45.state=0 and not bSecondMode and not bMultiBallMode Then
		If PlayerMode=2 and (bHardMode=True or bModeProgressUpgraded) Then
			AddScore 150
		Else
			AddScore(3000)
			SpellAerosmith=SpellAerosmith+1
			TargetBonus=TargetBonus+1
			SetLightColor I45, "orange", 1
			CheckAerosmith()
		End If
	Else	
		AddScore 150
	End If
	CheckModeProgress("I45")
	LastSwitchHit="sw36"
End Sub

Sub sw38_spin
	SoundSpinner()
	if NOT bWizardMode Then
		SpinHits=SpinHits - 1
		If SpinHits <=0 Then
			SpinValue=int(SpinValue*1.15)
			SpinHits=50
		End If
		SpinnerBonus=SpinnerBonus+1
	End If
	AddScore(SpinValue)
	SpinScoreCurrent=SpinScoreCurrent+SpinValue
	tmrSpin.Enabled=False
	tmrSpin.Enabled=True
	CheckModeProgress("sw38")
	' LastSwitchHit="sw38" caused problem as it continues to spinn
End Sub

Sub tmrSpin_Timer
	tmrSpin.Enabled=False
	SpinScore=SpinScoreCurrent
	SpinScoreCurrent=0
End Sub

Sub sw49_Timer
	sw49.TimerEnabled = False
End Sub

Sub sw49_hit  		' aerosmith O
	If Tilted then Exit Sub
	If sw49.TimerEnabled then Exit Sub
	sw49.TimerInterval = 1000
    sw49.TimerEnabled = True

	If I69.state=0 and not bSecondMode  and not bMultiBallMode Then
		If PlayerMode=2 and (bHardMode=True or bModeProgressUpgraded) Then
			AddScore 150
		Else
			AddScore(3000)
			SpellAerosmith=SpellAerosmith+1
			SetLightColor I69, "orange", 1
			CheckAerosmith()
		End If
	Else	
		AddScore 150
	End If
	CheckModeProgress("I69")
	LastSwitchHit="sw49"
End Sub

Sub sw50_Timer
	sw50.TimerEnabled = False
End Sub

Sub sw50_hit  		' aerosmith S		
	If Tilted then Exit Sub
	If sw50.TimerEnabled then Exit Sub
	sw50.TimerInterval = 1000
    sw50.TimerEnabled = True

	If I70.state=0 and not bSecondMode and not bMultiBallMode Then
		If PlayerMode=2 and (bHardMode=True or bModeProgressUpgraded) Then
			AddScore 150
		Else
			AddScore(3000)
			SpellAerosmith=SpellAerosmith+1
			SetLightColor I70, "orange", 1
			CheckAerosmith()
		End If
	Else	
		AddScore 150
	End If
	CheckModeProgress("I70")
	LastSwitchHit="sw50"
End Sub

Sub sw52_hit  		' left orbit
    If Tilted Then Exit Sub
	If sw52.TimerEnabled then Exit Sub
	sw52.TimerInterval = 1000
    sw52.TimerEnabled = True

	D "sw52_hit Last=" & LastSwitchHit

	If LastSwitchHit = "sw52pre" Then 	' We are going up the ramp
		If tmrSkillshot.Enabled and StackState(kStack_Pri3).GetArrowState(I62) = 2 and bSkillshotsReady(1)=True Then		' Hold Left, Loop around and hit CIU, Left Orbit or Center Ramp
			AddScore 1000000
			DOF 127, DOFPulse
			bSkillshotsReady(1) = False 
			PlayDMDScene "", "SUPER SKILL SHOT!", "+2 MISSLES", 2000
			QueueScene "ScenePlayMessage ""Video-0x0056.mp4"", ""SUPER"",""SKILL SHOT"","""" '", 2000, 1
			QueueScene "SceneClearPlayMessage '", 0, 1
			SmartButtonCount=SmartButtonCount+2
		End If 
	Else
		If tmrSkillshot.Enabled then 
			tmrSkillshot_Timer()
		End If
	End If 
	If LastSwitchHit="swlooper" Then ' full orbit
		ClearShotMultiplier(I112)
		CheckModeProgress "I107"
		AddScore 3000

		If I110.state <> 0 Then
			If ElevMultiBallAttempts = 0 then 
				SetLightColor I110, "white", 0
			Else
				if I110.state = 2 then
					SetLightColor I110, "white", 1
				Else
					SetLightColor I110, "white", 0
				End if
			End If
			Addscore 10000
			LoopBonus=LoopBonus+1
			If I65.state <> 0  or I110.state <> 0 Then ' need the 2nd elevator
					' If elevator light is on then 1/2 of elevator - If both are out then award elevator					
				QueueScene "ScenePlayMessage ""Elevator.mp4"", ""COMPLETE ORBITS"",""TO LIGHT"",""ELEVATOR LOCK"" '", 2000, 2
				QueueScene "SceneClearPlayMessage '", 0, 2
			Else
				QueueScene "ScenePlayMessage ""Elevator.mp4"", ""ELEVATOR"",""LIT"","""" '", 2000, 2
				QueueScene "SceneClearPlayMessage '", 0, 2
				'99999
				SetLightColor  I94, "white", 2
				PlayDMDScene "Elevator.mp4", "ELEVATOR LIT", "", 2000
			End If
		End If
		MultiplierShot = 1		' Clear shot multiplier 
	End If

	LastSwitchHit="sw52"
End Sub

Sub sw52_Timer
	sw52.TimerEnabled = False
End Sub

Sub sw52pre_hit
	LastSwitchHit="sw52pre"
End Sub


Sub sw53_hit  		' center ramp
    If Tilted Then Exit Sub
	D "sw53_hit"

	If tmrSkillshot.Enabled and StackState(kStack_Pri3).GetArrowState(I86) = 2 and bSkillShotsReady(1)=True Then		
									' Hold Left, Loop around and hit CIU, Left Orbit or Center Ramp
		AddScore 1000000
		DOF 127, DOFPulse
		bSkillshotsReady(1) = False 
		PlayDMDScene "", "SUPER SKILL SHOT!", "+2 MISSLE", 2000
		QueueScene "ScenePlayMessage ""Video-0x0056.mp4"", ""SUPER"",""SKILL SHOT"","""" '", 2000, 2
		QueueScene "SceneClearPlayMessage '", 0, 2
		SmartButtonCount=SmartButtonCount+2
	End If 

	ClearShotMultiplier(I89)
	CheckModeProgress "I86"
	AddScore 5000

	RampBonus=RampBonus+1

	If AutoAI and BallsOnPlayfield = 1 Then
		If F152.state<>0 Then ' try for a dude light 
			Table1_KeyDown(LeftFlipperKey)
			vpmtimer.addtimer 200,"Table1_KeyUp(LeftFlipperKey) '"
		End If
	End If
	MultiplierShot = 1		' Clear shot multiplier 
	LastSwitchHit="sw53"
End Sub

Sub sw56_Hit  ' toybox
	If Tilted Then Exit Sub
	If sw56.TimerEnabled then Exit Sub
	sw56.TimerInterval = 2000
    sw56.TimerEnabled = True
	D "sw56 Toy Box Medley:" & tmrMedleyTour.Enabled

	If bElevMultiBall then Exit Sub    ' no toy box during elevator mode
	If ModePercent(7) >= 100 then Exit Sub ' Already completed ToyBox

	if tmrMedleyTour.Enabled Then
		CheckModeProgress("I81")
		MultiplierShot = 1		' Clear shot multiplier 
		Exit Sub
	End If
	if tmrFinalTour.Enabled Then
		CheckModeProgress("I81")
		MultiplierShot = 1		' Clear shot multiplier 
		Exit Sub
	End If

	If I75.state=0 Then   ' Light the Locks
		I75.uservalue=I75.uservalue-1
		If I75.uservalue<=0 then
			SetLightColor I100, "green", 2
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""LOCK"",""IS LIT"","""" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
			I75.blinkinterval=125:I75.state=2
			PlayDMDScene "", "LOCK", "IS LIT", 2000
			ToyBox_OpenLid()
		Else
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""1 MORE"",""HIT"",""TO LIGHT LOCK"" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
			PlayDMDScene "", "1 MORE HIT", "TO LIGHT LOCK", 2000
		End If
	ElseIf I76.state=0 Then
		I76.uservalue=I76.uservalue-1
		If I76.uservalue<=0 then
'TODO		If bToyBoxMultiball and BallsOnPlayfield > 1 then
			SetLightColor I100, "green", 2
			ToyBox_OpenLid()
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""LOCK"",""IS LIT"","""" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
			I76.blinkinterval=125:I76.state=2
			ToyBox_OpenLid()
		Else
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""1 MORE"",""HIT"",""TO LIGHT LOCK"" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
		End if
	ElseIf I77.state=0 and not bToyBoxMultiball Then
		I77.uservalue=I77.uservalue-1
		If I77.uservalue<=0 then
			SetLightColor I100, "green", 2
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""LOCK"",""IS LIT"","""" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
			I77.blinkinterval=125:I77.state=2
			ToyBox_OpenLid()
		Else
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""1 MORE"",""HIT"",""TO LIGHT LOCK"" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
		End If
	ElseIf I78.state=0 and not bToyBoxMultiball Then
		I78.uservalue=I78.uservalue-1
		If I78.uservalue<=0 then
			SetLightColor I100, "green", 2
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""LOCK"",""IS LIT"","""" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
			I78.blinkinterval=125:I78.state=2
			ToyBox_OpenLid()
		Else
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""1 MORE"",""HIT"",""TO LIGHT LOCK"" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
		End If
	ElseIf I79.state=0 and not bToyBoxMultiball Then
		I79.uservalue=I79.uservalue-1
		If I79.uservalue<=0 then
			SetLightColor I100, "green", 2
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""LOCK"",""IS LIT"","""" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
			I79.blinkinterval=125:I79.state=2
			ToyBox_OpenLid()
		Else
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""1 MORE"",""HIT"",""TO LIGHT LOCK"" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
		End If
	ElseIf I80.state=0 and not bToyBoxMultiball Then
		I80.uservalue=I80.uservalue-1
		If I80.uservalue<=0 then
			SetLightColor I100, "green", 2
			SetLightColor F147, "white", 0
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""LOCK"",""IS LIT"","""" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
			I80.blinkinterval=125:I80.state=2
			SetLightColor F147, "white", 0
			ToyBox_OpenLid()
		Else
			QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", ""1 MORE"",""HIT"",""TO LIGHT LOCK"" '", 1500, 3
			QueueScene "SceneClearPlayMessage '", 0, 3
		End If
	End If
	if 10*Rnd() <2 then playmedia "","audio-jacky",pCallouts,"",1500,"",1,2

	ClearShotMultiplier(I84)
	CheckModeProgress("I81")
	AddScore 5000

	MultiplierShot = 1		' Clear shot multiplier 
End Sub

Sub sw56_Timer
	sw56.TimerEnabled = False
End Sub

Sub sw57_hit  		' toybox Left
	If Tilted Then Exit Sub
	If sw57.TimerEnabled then Exit Sub
	If ModePercent(7) >= 100 then Exit Sub ' Already completed ToyBox
	D "sw57"

	sw57.TimerInterval = 1000
    sw57.TimerEnabled = True
	If I72.state <> 0 Then  ' Todo .. perhaps make it harder , first hit goes to blink
		AddScore 1000
	Else ' light it up, If both are lit then show VIP Pass
		AddScore 2000
		If I73.state <> 0 Then 		'both are lit
			If I27.state=0 Then 	' no VIP yet
				SetLightColor I27, "white", 2
				If bUsePUPDMD then
					QueueScene "SceneBMessage """","""","""" '", 100,2 
					QueueScene "ScenePlayMessage ""Video-0x0066.mp4"", """",""LIT"","""" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
				Else 
					DisplayDMDText2 "VIP PASS","LIT", 1000, 11, 0
					if bBallSaverReady then VipValue=VipValue+50000 ' Added VIP if ballsave still on
				End If
 
			Else ' Increase value of VIP since it is lit already
				AddScore 5000
				VipValue=VipValue+50000
				If bUsePUPDMD then 
					QueueScene "SceneBMessage """","""","""" '", 100,2
					QueueScene "ScenePlayMessage ""Video-0x0066.mp4"", """","""&"AT  " & FormatScore(VipValue) & ""","""" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
				Else 
					DisplayDMDText2 "VIP VALUE","AT "&FormatScore(VipValue), 1000, 11, 0
				End If
			End If
			SetLightColor I72, "white", 0:SetLightColor I73, "white", 0
		Else
			SetLightColor I72, "white", 1
			If I27.state = 0 Then  ' already have VIP Pass
				If bUsePUPDMD then 
					QueueScene "SceneBMessage """","""","""" '", 100,2
					QueueScene "ScenePlayMessage ""Video-0x0067.mp4"", ""1 MORE FOR"",""VIP PASS"","""" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
				Else 
					DisplayDMDText2 "1 MORE FOR","VIP PASS", 1000, 11, 0
				End If
			Else
				If bUsePUPDMD then 
					QueueScene "SceneBMessage """","""","""" '", 100,2
					QueueScene "ScenePlayMessage ""Video-0x0067.mp4"", ""1 MORE FOR"",""VIP PASS"","""&"VIP PASS ="&FormatScore(VipValue)&""" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
				Else 
					DisplayDMDText2 "1 MORE FOR","VIP PASS", 1000, 11, 0
				End If
			End If
		End If
	End If
	LastSwitchHit="sw57"
End Sub

Sub sw57_Timer
	sw57.TimerEnabled = False
End Sub

Sub sw58_hit 		' toybox right
	If Tilted Then Exit Sub
	If sw58.TimerEnabled then Exit Sub

	sw58.TimerInterval = 1000
    sw58.TimerEnabled = True
	If I73.state <> 0 Then
		AddScore 1000
	Else ' light it up, If both are lit then show VIP Pass
		AddScore 2000
		If I72.state <> 0 Then 		'both are lit
			If I27.state=0 Then 	' no VIP yet
				SetLightColor I27, "white", 2
				If bUsePUPDMD then 
					QueueScene "SceneBMessage """","""","""" '", 100,2
					QueueScene "ScenePlayMessage ""Video-0x0066.mp4"", """",""LIT"","""" '", 3000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
				Else 
					DisplayDMDText2 "VIP PASS","LIT", 1000, 11, 0
					if bBallSaverReady then VipValue=VipValue+50000 ' Added VIP if ballsave still on
				End If
			Else ' Increase value of VIP since it is lit already
				AddScore 5000
				VipValue=VipValue+50000
				If bUsePUPDMD then 
					QueueScene "SceneBMessage """","""","""" '", 100,2
					QueueScene "ScenePlayMessage ""Video-0x0066.mp4"", """","""&"AT "&FormatScore(VipValue)&""","""" '", 3000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
				Else 
					DisplayDMDText2 "VIP VALUE AT","AT "&FormatScore(VipValue), 1000, 11, 0
				End If
			End If
			SetLightColor I72, "white", 0:SetLightColor I73, "white", 0
		Else
			SetLightColor I73, "white", 1
			If I27.state = 0 Then  ' already have VIP Pass
				If bUsePUPDMD then 
					QueueScene "SceneBMessage """","""","""" '", 100,2
					QueueScene "ScenePlayMessage ""Video-0x0067.mp4"", ""1 MORE FOR"",""VIP PASS"","""" '", 3000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
				Else 
					DisplayDMDText2 "1 MORE FOR","VIP PASS ", 1000, 11, 0
				End If
			Else
				If bUsePUPDMD then 
					QueueScene "SceneBMessage """","""","""" '", 100,2
					QueueScene "ScenePlayMessage ""Video-0x0067.mp4"", ""1 MORE FOR"",""VIP PASS"","""&"VIP PASS = "&FormatScore(VipValue)&""" '", 3000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
				Else 
					DisplayDMDText2 "1 MORE FOR","VIP PASS", 1000, 11, 0
				End If
			End If
		End If
	End If
	LastSwitchHit="sw58"
End Sub

Sub sw58_Timer
	sw58.TimerEnabled = False
End Sub


Sub sw62_hit		' right orbit
    If Tilted or bSkillshotsReady(1) or LastSwitchHit = "ballsavestarttrigger" Then Exit Sub
	If sw62.TimerEnabled then Exit Sub
	sw62.TimerInterval = 1000
    sw62.TimerEnabled = True

	If LastSwitchHit="swlooper" Then ' full orbit
		ClearShotMultiplier(I67)
		CheckModeProgress "I62"
		AddScore 3000

		If I65.state <> 0 Then
			If ElevMultiBallAttempts = 0 Then
				SetLightColor I65, "white", 0
			Else
				if I65.state = 2 Then
					SetLightColor I65, "white", 1
				Else
					SetLightColor I65, "white", 0
				End If
			End If
			Addscore 10000
			LoopBonus=LoopBonus+1
			If I65.state <> 0 or I110.state <> 0 Then ' need the 2nd elevator
					' If elevator light is on then 1/2 of elevator - If both are out then award elevator					
				QueueScene "ScenePlayMessage ""Elevator.mp4"", ""COMPLETE ORBITS"",""TO LIGHT"",""ELEVATOR LOCK"" '", 2000, 2
				QueueScene "SceneClearPlayMessage '", 0, 2
			Else
				QueueScene "ScenePlayMessage ""Elevator.mp4"", ""ELEVATOR"",""LIT"","""" '", 2000, 2
				QueueScene "SceneClearPlayMessage '", 0, 2
				'99999
				SetLightColor I94, "white", 2   
			End If
		End If
	End If

	MultiplierShot = 1		' Clear shot multiplier 
	LastSwitchHit="sw62"
End Sub

Sub sw62_Timer
	sw62.TimerEnabled = False
End Sub

Sub ballsavestarttrigger_hit
	LastSwitchHit = "ballsavestarttrigger"
End Sub

Sub sw62pre_hit
	LastSwitchHit="sw62pre"
End Sub

Sub sw63_hit ' Right Ramp001
	If Tilted Then Exit Sub
	D "sw63"
	activeball.vely = 0.8* activeball.vely 
	activeball.angmomz = 0
	If sw63.TimerEnabled then Exit Sub
	sw63.TimerInterval = 1000
    sw63.TimerEnabled = True

	ClearShotMultiplier(I46)
	CheckModeProgress "I103"
	AddScore 5000

	RampBonus=RampBonus+1

	MultiplierShot = 1		' Clear shot multiplier 

	If AutoAI and BallsOnPlayfield = 1 Then
		If F154.state<>0 Then ' try for a dude light 
			Table1_KeyDown(RightFlipperKey)
			vpmtimer.addtimer 200,"Table1_KeyUp(RightFlipperKey) '"
		End If
	End If

	MultiplierShot = 1		' Clear shot multiplier 
	LastSwitchHit="sw63"
End Sub

Sub sw63_Timer
	sw63.TimerEnabled = False
End Sub

Sub swlooper_hit
	D "swlooper: LastSwHit=" & LastSwitchHit
	If LastSwitchHit <> "ballsavestarttrigger" Then 
		LastSwitchHit="swlooper" ' dont credit the loop on a plunge
	End if
End Sub

Sub sw_shortplunge_hit
	If tmrSkillshot.Enabled and bSkillShotsReady(1)=False Then
		tmrSkillshot_Timer() 'no attempt at skillshot since left flipper not raised
	End If
End Sub

Sub CheckDude()
	If F152.state<>0 AND F153.state<>0 AND F154.state<>0 Then
		FlashForMs F152, 100, 20, 0
		FlashForMs F153, 100, 20, 0
		FlashForMs F154, 100, 20, 0
		AddBonusMultiplier 1
	End If
End Sub

Dim ShotMultiplierStrobe 
Sub ClearShotMultiplier(light)
	D "Sub ClearShotMultiplier " & light.name & " state=" & light.state & " tmr:" & tmrShotMultiplierStrobe.Enabled
	If light.State = 2 then ' blinking then light solid 
		If (tmrShotMultiplierStrobe.Enabled = False) Then 	' Hit a shot on the Multiplier
			light.UserValue = 1		' Mark we hit this one
			SetLightColor I32, "green", I32.UserValue
			SetLightColor I67, "green", I67.UserValue
			SetLightColor I84, "green", I84.UserValue
			SetLightColor I89, "green", I89.UserValue
			SetLightColor I95, "green", I95.UserValue
			SetLightColor I101,"green", I101.UserValue
			SetLightColor I46, "green", I46.UserValue
			SetLightColor I112, "green", I112.UserValue
						
			bShotMultiplierSelect = False
			'Start the moving spot shot if all Xs are complete
			if I32.userValue = 1 and I67.UserValue = 1 and _
			   I84.userValue = 1 and I89.UserValue = 1 and _
			   I95.userValue = 1 and I101.UserValue = 1 and _
			   I46.userValue = 1 and I112.UserValue = 1 then
				ShotMultiplierStrobe=0
				tmrShotMultiplierStrobe.Enabled = True
			End If
		Else												' We are in Strobing mode 
			tmrShotMultiplierStrobe.Enabled = False
			SetLightColor I32, "green", I32.UserValue
			SetLightColor I67, "green", I67.UserValue
			SetLightColor I84, "green", I84.UserValue
			SetLightColor I89, "green", I89.UserValue
			SetLightColor I95, "green", I95.UserValue
			SetLightColor I101,"green", I101.UserValue
			SetLightColor I46, "green", I46.UserValue
			SetLightColor I112,"green", I112.UserValue
			Multiplier3x = 3						' 3x is just for this shot
		End If
	ElseIf light.state = 1 then ' Solid lit setup 2x for the shot
		MultiplierShot = 2
	End If 
End Sub

' After getting all 2X lights, show a roving shot for 3x
Sub tmrShotMultiplierStrobe_Timer
	D "tmrShotMultiplierStrobe_Timer() Val=" & ShotMultiplierStrobe
	Select Case ShotMultiplierStrobe
		Case 0:
			SetLightColor I32, "green", I32.UserValue
			SetLightColor I67, "green", 2
		Case 1:
			SetLightColor I67, "green", I67.UserValue
			SetLightColor I84, "green", 2
		Case 2:
			SetLightColor I84, "green", I84.UserValue
			SetLightColor I89, "green", 2
		Case 3:
			SetLightColor I89, "green", I89.UserValue
			SetLightColor I95, "green", 2
		Case 4:
			SetLightColor I95, "green", I95.UserValue
			SetLightColor I101, "green", 2
		Case 5:
			SetLightColor I101, "green", I101.UserValue
			SetLightColor I46, "green", 2
		Case 6:
			SetLightColor I46, "green", I46.UserValue
			SetLightColor I112, "green", 2
		Case 7:
			SetLightColor I112, "green", I112.UserValue
			SetLightColor I32, "green", 2
	End Select
	ShotMultiplierStrobe=ShotMultiplierStrobe+1
	If ShotMultiplierStrobe>7 then ShotMultiplierStrobe=0
End Sub


Function addsuffix(num)
	Dim suff
    Select Case num
         Case 1 : suff = "ST"
         Case 2 : suff = "ND"
         Case 3 : suff = "RD"
         Case 4, 5, 6, 7, 8, 9: suff = "TH"
    End Select
    AddSuffix = cStr(num) + suff
End Function

sub DumpValues
dim a,b
b=0
for each a in aRampLights
	D "DumpValues " & a.name & " " & Shots(b)
	b=b+1
Next
D "DumpValues I34 State=" & I34.state
end sub

Sub CheckModeProgress(trigger_name)  ' rats is done after any attempt, same with back and walk -- get super and award
	dim bValidHit
	dim bFinalShot
	dim hitLight
	dim a, b, c, i 
	dim thisScore, vidToShow
	dim MB_Multiplier:MB_Multiplier=1
	Dim bModeComplete
	Dim bDoubleBonus, bGotLight
	Dim CountLeft, CountRight ' How many lights per side in Rats
	bDoubleBonus = False
	bModeComplete = False
	bValidHit = False

	D "sub CheckModeProgress() " & trigger_name & " PlayerMode:" & PlayerMode & " 2:" & PlayerMode2 & _
      " bSecondMode:" & bSecondMode & " bonusMode:" & bBonusMode & _
      " Wiz:" & bWizardMode & " state:" & I34.state & " Final:" & bFinalTourReady & _
	  " Medley: " & bMedleyTourReady

	If bDebounce Then		' we cant hit it twice or in between resets
		exit Sub
	End If

	If BallSearchCnt > 0 then exit sub  ' We are in an error mode skip doing anything
	ResetBallSearch

' Check For Any OTHER Remaining Super Awards  ie not the current PlayerMode2 one
	If not bWizardMode Then
		If (trigger_name = "I62" or trigger_name = "I107") and PlayerMode2 <> 0 Then
			If Mode2Percent(0) <> -1 and Mode2Percent(0) < 100 Then
				PlayProgress trigger_name

				Mode2Progress(0) = Mode2Progress(0)+1
				Mode2Percent(0) = CINT((Mode2Progress(0)  / 10) * 100)
				AddScore Mode2Value(0)			' 

				ShowPlayerMode2(0)
				If (Mode2Progress(0) >= 10) Then
					Mode2Percent(0) = 100
					QueueScene "SceneClearPlayMessage '", 0, 2
				End If
			End If
		End If

		If (trigger_name = "I86" or trigger_name = "I103") and PlayerMode2 <> 1 Then
			D "CheckModeProgress Step A " & Mode2Percent(1) & ":" & Mode2Percent(1)
			If Mode2Percent(1) <> -1 and Mode2Percent(1) < 100 Then
				PlayProgress trigger_name

				Mode2Progress(1) = Mode2Progress(1)+1
				Mode2Percent(1) = CINT((Mode2Progress(1)  / 10) * 100)
				AddScore Mode2Value(1)		' 

				ShowPlayerMode2(1)
				If (Mode2Progress(1) >= 10) Then
					Mode2Percent(1) = 100
					QueueScene "SceneClearPlayMessage '", 0, 2
				End If
			End If
		End If
	
		If Mode2Percent(2) <> -1 and Mode2Percent(2) < 100 and PlayerMode2 <> 2 Then
			For each a in aerosmithLights
				If a.name = trigger_name Then
					PlayProgress trigger_name

					Mode2Progress(2) = Mode2Progress(2)+1
					Mode2Percent(2) = CINT((Mode2Progress(2)  / 20) * 100)
					AddScore Mode2Value(2)		' 

					ShowPlayerMode2(2)
					If (Mode2Progress(2) >= 20) Then
						Mode2Percent(2) = 100
						QueueScene "SceneClearPlayMessage '", 0, 2
					End If
					exit for
				End If
			Next
		End If
' scoring  - handled in the Scoring Routine

		If (trigger_name = "sw2" or trigger_name = "sw3" or trigger_name = "sw5") and PlayerMode2 <> 4 Then
			If Mode2Percent(4) <> -1 and Mode2Percent(4) < 100 Then
				PlayProgress trigger_name

				Mode2Progress(4) = Mode2Progress(4)+1
				Mode2Percent(4) = CINT((Mode2Progress(4)  / 10) * 100)
				AddScore Mode2Value(4)	' 

				ShowPlayerMode2(4)
				If (Mode2Progress(4) >= 10) Then
					Mode2Percent(4) = 100
					QueueScene "SceneClearPlayMessage '", 0, 2
				End If
			End If
		End If

		If trigger_name = "bumper" Then
			If Mode2Percent(5) <> -1 and Mode2Percent(5) < 100 and PlayerMode2 <> 5 Then
				PlayProgress trigger_name

				Mode2Progress(5) = Mode2Progress(5)+1
				Mode2Percent(5) = CINT((Mode2Progress(5)  / 50) * 100)
				AddScore Mode2Value(5)

				ShowPlayerMode2(5) 
				If (Mode2Progress(5) >= 50) Then
					Mode2Percent(5) = 100
					QueueScene "SceneClearPlayMessage '", 0, 2
				End If
			End If
		End If

		If trigger_name = "sw38" Then
			If Mode2Percent(6) <> -1 and Mode2Percent(6) < 100 and PlayerMode2 <> 6 Then
				PlayProgress trigger_name

				Mode2Progress(6) = Mode2Progress(6)+1
				Mode2Percent(6) = CINT((Mode2Progress(6)  / 100) * 100)
				AddScore Mode2Value(6)			' 

				ShowPlayerMode2(6)
				If (Mode2Progress(6) >= 100) Then
					Mode2Percent(6) = 100
					QueueScene "SceneClearPlayMessage '", 0, 2
				End If
			End If
		End If
	End If


	If bToyBoxMultiball then ' check MB Lights
		D "Sub checkModeProgress - we are in bToyBoxMultiball"
		c=0
		for each a in aRampLights
			If a.name = trigger_name and StackState(kStack_Pri1).GetArrowState(a) <> 0 Then
				Shots(c)=Shots(c)-1
				ToyBoxMBJackpotHits=ToyBoxMBJackpotHits+1
				ModeProgress(7) = ModeProgress(7)+1
				ModePercent(7) = CINT((ModeProgress(7)  / 18) * 100)
				If (ModeProgress(7) >= 18) Then
					ModePercent(7) = 100
					SetModeLightComplete 7, I22
				End If

				'DumpValues
				D "ToyBoxMB: JackpotHits: " & ToyBoxMBJackpotHits & " " & a.name & " c=" & c
				If shots(c) <= 0 or ToyBoxMBJackpotHits >= 16 then 
					SSetLightColor kStack_Pri1, a, "white", 0 ' ScoreJackPot
				end if
				If ToyBoxMBJackpotHits <=7 then
					ToyBoxMBJackpot=ToyBoxMBJackpotBase
					QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", """","""&FormatScore(ToyBoxMBJackpot)&""","""" '", 1000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
					AddScore ToyBoxMBJackpot
				ElseIf ToyBoxMBJackpotHits=8 Then
					ToyBoxMBJackpot=4000000
					QueueScene "ScenePlayMessage ""Video-0x0091.mp4"", ""SUPER JACKPOT"","""&FormatScore(ToyBoxMBJackpot)&""","""" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
					AddScore ToyBoxMBJackpot
				ElseIf ToyBoxMBJackpotHits >8 and ToyBoxMBJackpotHits < 16 Then
					ToyBoxMBJackpot=ToyBoxMBJackpotBase
					QueueScene "ScenePlayMessage ""Video-0x0003.mp4"", """","""&FormatScore(ToyBoxMBJackpot)&""","""" '", 1000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
					AddScore ToyBoxMBJackpot
				ElseIf ToyBoxMBJackpotHits=16 then
					ToyBoxMBJackpot=4000000
					QueueScene "ScenePlayMessage ""Video-0x0090.mp4"", ""SUPER JACKPOT"","""&FormatScore(ToyBoxMBJackpot)&""","""" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
					AddScore ToyBoxMBJackpot
' Turn off all Shots and light one random shot
					SSetLightColor kStack_Pri1, aRampLights(bRndModeShot), "green", 2
					AddScore ToyBoxMBJackpot
				ElseIf ToyBoxMBJackpotHits=17 then
					ToyBoxMBJackpot=3500000
					QueueScene "ScenePlayMessage ""Video-0x008F.mp4"", ""SUPER JACKPOT"","""&FormatScore(ToyBoxMBJackpot)&""","""" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
' Light one last DSJ at Lock shot
					AddScore ToyBoxMBJackpot
					SSetLightColor kStack_Pri1, I97, "green", 2
				ElseIf ToyBoxMBJackpotHits=18 then
					ToyBoxMBJackpot=8000000
					QueueScene "ScenePlayMessage ""Video-0x0065.mp4"", ""DOUBLE"","""&FormatScore(ToyBoxMBJackpot)&""",""SUPER JACKPOT"" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
' reset - show all locks and start over
					i=0
					for each b in aRampLights
						SSetLightColor kStack_Pri1, b, "green", 2
						shots(i)=2
						i=i+1
					Next
					ToyBoxMBJackpotHits=0
					AddScore ToyBoxMBJackpot
				End If
				ToyBoxMBJackPotTotal=ToyBoxMBJackpotTotal+ToyBoxMBJackpot
				Exit For
			End If
			c=c+1
		Next
	elseIf bElevMultiBall Then ' If all lights then turn them off and award then light elevator again
		If trigger_name = "I91" and StackState(kStack_Pri1).GetArrowState(I91) <> 0 Then 'score Floor Jackpot and turn remaining lights on or Exit Mode
		D "##Love Elevator - Hit elevator lock"
			SSetLightColor kStack_Pri1, I91, "pink", 0
			ModeProgress(8)=ModeProgress(8)+1 
			ModePercent(8) = CINT((ModeProgress(8)  / 9) * 100)  ' 9th floor ie Elevator + Shot three times
			If ModeProgress(8) >= 9 Then 
				ModePercent(8) = 100
				SetModeLightComplete 8, I23
			    D "Elevator Complete - turn off lights %:" & ModePercent(8) & " #:" & ModeProgress(8)
				for each a in aRampLights
					SSetLightColor kStack_Pri1, a, "white", 0 ' no longer in ElevMode
				Next
			End If
' Any shot then Elev 1 FLOOR JACKPOT 1M  each floor, repeat till the 9th floor for Super Jackpot
			ElevMBJackpot=1000000
			if ModeProgress(8) = 9 then
				if ElevMultiBallAttempts = 1 then ' Super Jackpot is based on number of attempts thru ElevMb
					QueueScene "ScenePlayMessage ""Video-0x003E.mp4"", ""SUPER"",""JACKPOT"","""&FormatScore(ElevMBJackpot*10)&""" '", 3000, 1
					AddScore ElevMBJackpot * 10
					ElevMBJackpotTotal=ElevMBJackpotTotal+(ElevMBJackpot*10)
				ElseIf ElevMultiBallAttempts = 2 Then
					QueueScene "ScenePlayMessage ""Video-0x003E.mp4"", ""SUPER"",""JACKPOT"","""&FormatScore(ElevMBJackpot*7.5)&""" '", 3000, 1					
					AddScore ElevMBJackpot * 7.5
					ElevMBJackpotTotal=ElevMBJackpotTotal+(ElevMBJackpot*7.5)
				Else
					QueueScene "ScenePlayMessage ""Video-0x003E.mp4"", ""SUPER"",""JACKPOT"","""&FormatScore(ElevMBJackpot)&""" '", 3000, 1
					AddScore ElevMBJackpot * 5
					ElevMBJackpotTotal=ElevMBJackpotTotal+(ElevMBJackpot*5)
				End If
				QueueScene "ScenePlayMessage ""Video-0x0036.mp4"", """","""","""" '", 3000, 1
				QueueScene "SceneClearPlayMessage '", 0, 1
			Else
				QueueScene "ScenePlayMessage ""Video-0x003E.mp4"", """&addSuffix(ModeProgress(8))&" FLOOR"",""JACKPOT"","""&FormatScore(ElevMBJackpot)&""" '", 3000, 1
				QueueScene "ScenePlayMessage ""Video-0x0036.mp4"", """","""","""" '", 3000, 1
				QueueScene "SceneClearPlayMessage '", 0, 1		
				AddScore ElevMBJackpot 
				ElevMBJackpotTotal=ElevMBJackpotTotal+ElevMBJackpot
			End If
			For each a in aRampLights
				If a.name <> "I91" then 
					If StackState(kStack_Pri1).GetArrowColor(a) = "pink" Then ' check color here
						SSetLightColor kStack_Pri1, a, "pink", 2 ' Only relight the ones we need here ... 
					End If
				end if
			Next
		Else
			For each a in aRampLights
				If a.name = trigger_name and StackState(kStack_Pri1).GetArrowState(a) <> 0 then ' turn off lights and light Elev for jackpot
					D "##Love Elevator - hit shot " & a.name
					SSetLightColor kStack_Pri1, a, "pink", 0 ' ScoreJackPot
' Spinning mp4 and "JACKPOT 500000 + 25K for each floor - dont relight that mode
					QueueScene "ScenePlayMessage ""Video-0x0037.mp4"", ""JACKPOT"","""&FormatScore(int(500000+(25000*ModeProgress(8))))&""","""" '", 3000, 1
					QueueScene "SceneClearPlayMessage '", 0, 1
					bValidHit=True
					AddScore int(500000+(25000*ModeProgress(8)))
					Exit For
				end if
			Next
			If bValidHit Then
				D "##Love Elevator - reset to the Elevator shot"
				For each b in aRampLights
					If StackState(kStack_Pri1).GetArrowColor(b) = "pink" Then   ' Turn the remaining pink lights off
						SSetLightColor kStack_Pri1, b, "pink", 0
					End If
				Next
				D "##Love Elevator - turn elevator back on"
				SSetLightColor kStack_Pri1, I91, "pink", 2   ' elevator lock
			End If
		End If
	end If


	If CheckWizardModeStart(trigger_name) then Exit Sub				' Wizard Modes Stack so we check here, bail out when we Start 
	CheckWizardModeProgress(trigger_name)

	If bBonusMode Then 
		D "checkmodeprogress: Checking BONUSMODE"
		bValidHit=False
		for each a in aRampLights
			If a.name = trigger_name and StackState(kStack_Pri0).GetArrowState(a) <> 0 then
				D "Checking " & a.name &" and status: " & StackState(kStack_Pri0).GetArrowState(a)
				D ">>>>>>>START SUPER MODE :" & PlayerMode & " 2:" & PlayerMode2
				bValidHit=True
				exit for
			end if
		next
		If bValidHit Then
			for each a in aRampLights
				SSetLightColor kStack_Pri0, a, modeRampColor, 0
			next
			bBonusMode=False
			bSecondMode=True
			StartPlayerMode2
			exit sub
		end if
	End If

	D "Check progress 2nd: " & bSecondMode & " " & PlayerMode & " " & trigger_name
	If bWizardMode Then
		' Do Nothing
	ElseIf (bSecondMode) Then
		If trigger_name = "I35" and bMultiBallMode=False Then		' scoop hit
			StopPlayerMode2
		Else
			CheckModeProgress2nd(trigger_name)
		End If 
	Else
		If PlayerMode <> -1 Then
			D "Mode Progress Percent: " & ModePercent(PlayerMode) & " PlayerMode: " & PlayerMode
			If ModePercent(PlayerMode) >= 100 then 
				Exit Sub 
			End If 
		End If 

		' Randomly Award Jester for Non Lit Shot
			If int(Rnd()*100) < 15 Then
				For each a in aRampLights
					If a.name = trigger_name and StackState(kStack_Pri0).GetArrowState(a) = 0 then
						HiddenJesters=HiddenJesters-1
						If HiddenJesters <= 0 Then
							If I33.state = 0 Then
								setExtraBallLight(True)
								HiddenJesters=25
							Else ' dont try and award jester yet
								HiddenJesters=HiddenJesters+1
							End If
						End If
						Exit For
					End If
				Next
			End If


		D "Checking each of the playermodes :" & PlayerMode
		Select Case PlayerMode
		Case -1: 'No Mode Selected
        Case 0: '   last child    CIU lights F151  
				' left &right flash, center, right solid, make both ramps to relight them keep making orbits - 8 shots
			D "Looping thru the ramps trigger_name=" & trigger_name 
			For each a in aRampLights			
				If a.name = trigger_name then
				D "Processing " & a.name & " === " & StackState(kStack_Pri0).GetArrowState(a)
				if StackState(kStack_Pri0).GetArrowState(a) <> 0 then 
					bDebounce = True	
					D "Match trigger_name=" & trigger_name & " Name:" & a.name & " State:" & StackState(kStack_Pri0).GetArrowState(a)
					' So the ball doesnt roll over twice before we reset
					vpmtimer.addtimer 400, "debounceReset '"
						'PlaySound "gg_RampHit"					' Play sounds and reset the timer to reset the lights
					'LightSeqScore.Play SeqUpOn, 100, 0		 
					PlayProgress trigger_name
					
					If StackState(kStack_Pri0).GetArrowState(a) = 1 Then  ' If solid then turn off
						AddScore 15000
						FlashForMs a, 200, 50, 0
						SSetLightColor kStack_Pri0, a, modeRampColor, 0 
					End If 
					If StackState(kStack_Pri0).GetArrowState(I86)=0 and StackState(kStack_Pri0).GetArrowState(I103)=0 and not bModeProgressUpgraded then
						' relight them
						D "SSetLightColorName kStack_Pri0, ""I86"", """ & modeRampColor & """, 1 '"  ' 
						vpmtimer.addtimer 200, "SSetLightColorName kStack_Pri0, ""I86"", """ & modeRampColor & """, 1 '"  ' delay 
						vpmtimer.addtimer 200, "SSetLightColorName kStack_Pri0, ""I103"",""" & modeRampColor & """, 1 '"  ' delay
					End if
					If StackState(kStack_Pri0).GetArrowState(a) = 2 Then  ' If blink then quick blink and return to blink
						AddScore 25000
						If bModeProgressUpgraded Then
							FlashForMs a, 200, 50, 0
						Else
							vpmtimer.addtimer 100,  a.name & ".blinkinterval=20 '"
							vpmtimer.addtimer 1000, a.name & ".blinkinterval=" & BlinkIntDef & " '"
						End If
					End If
					
					ModeProgress(PlayerMode) = ModeProgress(PlayerMode)+1
					Coins=Coins+1:If bModeProgressUpgraded then Coins=Coins+1
					If NOT flshCIU.TimerEnabled and Not bModeProgressUpgraded Then 
						setCIULight(True)
					End If
					ModePercent(PlayerMode) = CINT((ModeProgress(PlayerMode)  / 8) * 100)  ' 8 shots per this mode
					RefreshPlayerMode 
					If (ModeProgress(PlayerMode) = 8) Then   ' 8 shots
						ModePercent(PlayerMode) = 100
						bModeComplete = True
						pDMDEvent(541)  ' Loops
						setCIULight(False)
					End If
				End if				
				End if
			Next
			If trigger_name = "I35" and flshCIU.TimerEnabled Then	' scoop hit - perform upgrade
				setCIULight(False)
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x04F2crankitup.wav",100, 1

				bModeProgressUpgraded=True
				If bHardMode Then ModeCountdownTimer.UserValue=ModeCountdownTimer.UserValue+20
				QueueScene "SceneMessage ""CRANK IT UP"","""&FormatScore(1000000)&""","""" '", 2000, 2
				QueueScene "SceneClearMessage '", 0, 2
				AddScore 1000000
				For each a in aRampLights				' Upgrade to solid
					If StackState(kStack_Pri0).GetArrowState(a) = 0 then
						SSetLightColor kStack_Pri0, a, modeRampColor, 1
					End If
				Next
			End If 
		Case 1:  ' walk   need both orbits for them to relight, after the center ramp the right ramp is lit, after right the center   
			If trigger_name = "I35" and flshCIU.TimerEnabled Then	' scoop hit - perform upgrade
				setCIULight(False)
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x04F2crankitup.wav",100, 1
				bModeProgressUpgraded=True
				If bHardMode Then ModeCountdownTimer.UserValue=ModeCountdownTimer.UserValue+20
				QueueScene "SceneMessage ""CRANK IT UP"","""&FormatScore(1000000)&""","""" '", 2000, 2
				QueueScene "SceneClearMessage '", 0, 2
				AddScore 1000000
				For each a in aRampLights				' Upgrade so All the arrows count now
					SSetLightColor kStack_Pri0, a, modeRampColor, 1
				Next
			else 
				D "Looping thru the ramps trigger_name=" & trigger_name & " for Walk"
				For each a in aRampLights
					If a.name = trigger_name and StackState(kStack_Pri0).GetArrowState(a) <> 0 then 
						D "Match trigger_name=" & trigger_name & "Name:" & a.name & " State:" & StackState(kStack_Pri0).GetArrowState(a)
						bDebounce = True							' So the ball doesnt roll over twice before we reset
						vpmtimer.addtimer 400, "debounceReset '"

						'PlaySound "gg_RampHit"					' Play sounds and reset the timer to reset the lights
						'LightSeqScore.Play SeqUpOn, 100, 0		 
						PlayProgress trigger_name
						AddScore 25000
						if NOT bModeProgressUpgraded then
							SSetLightColor kStack_Pri0, a, modeRampColor, 0
						Else
							FlashForMs a, 200, 50, 2
						End if
						If not bModeProgressUpgraded Then 'cycle the lights
							If a.name = "I86" Then
								SSetLightColor kStack_Pri0, I103, modeRampColor, 2
							elseIf a.name = "I103" Then
								SSetLightColor kStack_Pri0, I86, modeRampColor, 2
							elseIf a.name = "I107" Then
								If StackState(kStack_Pri0).GetArrowState(I62)=0 Then  ' both are are off so they relight
									SSetLightColor kStack_Pri0, I62, modeRampColor, 2
									SSetLightColor kStack_Pri0, I107,modeRampColor, 2
								End If
							elseIf a.name = "I62" Then
								If StackState(kStack_Pri0).GetArrowState(I107)=0 Then ' both are are off so they relight
									SSetLightColor kStack_Pri0, I62, modeRampColor, 2
									SSetLightColor kStack_Pri0, I107,modeRampColor, 2
								End If
							End If
						End If
						
						ModeProgress(PlayerMode) = ModeProgress(PlayerMode)+1
						Coins=Coins+1:If bModeProgressUpgraded then Coins=Coins+1
						If NOT flshCIU.TimerEnabled and Not bModeProgressUpgraded Then
							setCIULight(True)
						End If
						ModePercent(PlayerMode) = CINT((ModeProgress(PlayerMode)  / 10) * 100)
						RefreshPlayerMode 
						If (ModeProgress(PlayerMode) = 10) Then
							ModePercent(PlayerMode) = 100
							bModeComplete = True
							pDMDEvent(854) ' Ramps
							setCIULight(False)
						End If
					end if
				Next
			end if
		Case 2:  'same old song   ' random shots, CIU gets all aerosmith blinking for shots 
			If trigger_name = "I35" and flshCIU.TimerEnabled Then	' scoop hit - perform upgrade
				setCIULight(False)
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x04F2crankitup.wav",100, 1
				bModeProgressUpgraded=True
				If bHardMode Then ModeCountdownTimer.UserValue=ModeCountdownTimer.UserValue+20
				QueueScene "SceneMessage ""CRANK IT UP"","""&FormatScore(1000000)&""","""" '", 1000, 2
				QueueScene "SceneClearMessage '", 0, 2
				AddScore 1000000
				If bHardMode=True Then ' Now upgraded so make these available for the mode
					For each a in aerosmithLights
						SetLightColor a, "orange", 2
					Next
				End If
			End If
			D "Looping thru the ramps trigger_name=" & trigger_name & " for Same Old Song"
			b=0
			For each a in aRampLights
	'D "Checking trigger_name=" & trigger_name & " ramp:" & a.name & " state:" & StackState(kStack_Pri0).GetArrowState(a)
				If a.name = trigger_name and StackState(kStack_Pri0).GetArrowState(a) <> 0 then 
					bDebounce = True	
					D "Match trigger_name=" & trigger_name & " Name:" & a.name & " State:" & StackState(kStack_Pri0).GetArrowState(a)
					' So the ball doesnt roll over twice before we reset
					vpmtimer.addtimer 400, "debounceReset '"
					'PlaySound "gg_RampHit"					' Play sounds and reset the timer to reset the lights
					'LightSeqScore.Play SeqUpOn, 100, 0		 
					PlayProgress trigger_name
					AddScore 15000
					FlashForMs a, 200, 50, 0
					SSetLightColor kStack_Pri0, a, modeRampColor, 0 

					If bModeProgressUpgraded then AddScore 2500  ' some sort of bonus for it being upgraded
					c=getRndShot(b, kStack_Pri0)
					SSetLightColor kStack_Pri0, aRampLights(c), modeRampColor, 2
						
					ModeProgress(PlayerMode) = ModeProgress(PlayerMode)+1
					Coins=Coins+1:If bModeProgressUpgraded then Coins=Coins+1
					If NOT flshCIU.TimerEnabled and Not bModeProgressUpgraded Then 
						setCIULight(True)
					End If
					ModePercent(PlayerMode) = CINT((ModeProgress(PlayerMode)  / 10) * 100)  ' 8 shots per this mode
					RefreshPlayerMode 
					If (ModeProgress(PlayerMode) = 10) Then   ' 10 shots  Super Targets
						ModePercent(PlayerMode) = 100
						bModeComplete = True
						pDMDEvent(857)  ' Targets
						setCIULight(False)
						for each b in ShotMultipliers ' Code Update
							if b.state = 0 then  ' If not already earned then make it available
								SetLightColor b, "green", 2
								b.uservalue = 2
							End If
						Next
					End If
					Exit For
				End If
				b=b+1
			Next
			If bModeProgressUpgraded or bHardMode=False then ' check AEROSMITH Targets
				for each a in aerosmithLights
					If a.name = trigger_name and a.state=2 Then
						SetLightColor a, "orange", 1
						AddScore 10000
						ModeProgress(PlayerMode) = ModeProgress(PlayerMode)+1	
						ModePercent(PlayerMode) = CINT((ModeProgress(PlayerMode)  / 10) * 100)  ' 10 shots per this mode
						RefreshPlayerMode 
						If (ModeProgress(PlayerMode) = 10) Then   ' 10 shots  Super Targets
							ModePercent(PlayerMode) = 100
							bModeComplete = True
							pDMDEvent(857)  ' Targets
							for each b in ShotMultipliers ' Code Update
								if b.state = 0 then  ' If not already earned then make it available
									SetLightColor b, "green", 2
									b.uservalue = 2
								End If
							Next
						End If
					end If
				Next
			End If
		Case 3:  ' sweet
			If trigger_name = "I35" and flshCIU.TimerEnabled Then	' scoop hit - perform upgrade
				setCIULight(False)
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x04F2crankitup.wav",100, 1
				bModeProgressUpgraded=True
				If bHardMode Then  ModeCountdownTimer.UserValue=ModeCountdownTimer.UserValue+20
				QueueScene "SceneMessage ""CRANK IT UP"","""&FormatScore(1000000)&""","""" '", 3000, 2
				QueueScene "SceneClearMessage '", 0, 2
				AddScore 1000000
				For each a in aRampLights				' Upgrade to solid
					If StackState(kStack_Pri0).GetArrowState(a) = 0 then
						SSetLightColor kStack_Pri0, a, modeRampColor, 1
					End If
				Next
			Else ' Dont double process the CIU eject
				D "Looping thru the ramps trigger_name=" & trigger_name & " for Sweet Emotion"
				For each a in aRampLights
				If a.name = trigger_name and StackState(kStack_Pri0).GetArrowState(a) <> 0 then 
					bDebounce = True	
					D "Match trigger_name=" & trigger_name & " Name:" & a.name & " State:" & StackState(kStack_Pri0).GetArrowState(a)
					' So the ball doesnt roll over twice before we reset
					vpmtimer.addtimer 400, "debounceReset '"

					PlayProgress trigger_name
					If StackState(kStack_Pri0).GetArrowState(a) = 1 Then  ' If solid then turn off
						AddScore 200000+(15000*Coins)+(15000*ModeProgress(PlayerMode))
						SSetLightColor kStack_Pri0, a, modeRampColor, 0 
					End If 
					If StackState(kStack_Pri0).GetArrowState(a) = 2 Then  ' If blinking then turn to solid
						QueueScene "SceneMessage ""1 COIN"","""&FormatScore(500000+(25000*Coins)+(25000*ModeProgress(PlayerMode)))&""","""" '", 500, 2
						QueueScene "SceneClearMessage '", 0, 2
						AddScore 500000+(25000*Coins)+(25000*ModeProgress(PlayerMode))
						If bModeProgressUpgraded then ' prior to upgrade, light goes off
							SSetLightColor kStack_Pri0, a, modeRampColor, 1
						Else
							SSetLightColor kStack_Pri0, a, modeRampColor, 0
						End if
					End If 
' If initial lights are all out then proceed
					If StackState(kStack_Pri0).GetArrowState(I35)=0 and _  
                          StackState(kStack_Pri0).GetArrowState(I81)=0 and _
                          StackState(kStack_Pri0).GetArrowState(I86)=0 and _
						  StackState(kStack_Pri0).GetArrowState(I91)=0 and _
                          StackState(kStack_Pri0).GetArrowState(I97)=0 and _
                          StackState(kStack_Pri0).GetArrowState(I103)=0 and _
                          ModeProgress(PlayerMode) < 7 and _
										not bModeProgressUpgraded then
						' light all major shots
							SSetLightColor kStack_Pri0, I35,modeRampColor,2
							SSetLightColor kStack_Pri0, I62,modeRampColor,2
							SSetLightColor kStack_Pri0, I81,modeRampColor,2
							SSetLightColor kStack_Pri0, I86,modeRampColor,2
							SSetLightColor kStack_Pri0, I91,modeRampColor,2
							SSetLightColor kStack_Pri0, I97,modeRampColor,2
							SSetLightColor kStack_Pri0, I103,modeRampColor,2
							SSetLightColor kStack_Pri0, I107,modeRampColor,2
' if upgraded and all are out .. then relight them all.
					End if
				
					ModeProgress(PlayerMode) = ModeProgress(PlayerMode)+1
					D "Sweet ModeProgress:" & ModeProgress(PlayerMode)
					Coins=Coins+1:If bModeProgressUpgraded then Coins=Coins+1
					ModePercent(PlayerMode) = CINT((ModeProgress(PlayerMode)  / 12) * 100)  ' 12 shots per this mode
					If NOT flshCIU.TimerEnabled and Not bModeProgressUpgraded Then
						setCIULight(True)
					End If
					RefreshPlayerMode 
					If (ModeProgress(PlayerMode) = 12) Then   ' 12 shots
						ModePercent(PlayerMode) = 100
						bModeComplete = True
						pDMDEvent(855)  ' Scoring
						setCIULight(False)
					End If
					'D "jump out of the for loop"
					Exit For ' Jump out of the For Loop
				end if
				Next
			End If
			D "bModeComplete = " & bModeComplete
		Case 4: ' dude
			If trigger_name = "I35" and flshCIU.TimerEnabled Then	' scoop hit - perform upgrade - only increase point value
				D "Rats - Upgraded Crank It Up"
				setCIULight(False)
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x04F2crankitup.wav",100, 1
				bModeProgressUpgraded=True
				If bHardMode Then ModeCountdownTimer.UserValue=ModeCountdownTimer.UserValue+20
				QueueScene "SceneMessage ""CRANK IT UP"","""&FormatScore(1000000)&""","""" '", 3000, 2
				QueueScene "SceneClearMessage '", 0, 2
				AddScore 1000000
				SSetLightColor kStack_Pri0, I35, modeRampColor, 2
			Else ' Move Left shot over to the right, move right shot over to the left
				D "Looping thru the ramps trigger_name=" & trigger_name & " for Dude"
				For each a in aRampLights
					If a.name = trigger_name and StackState(kStack_Pri0).GetArrowState(a) <> 0 then 
						bDebounce = True	
						D "Match trigger_name=" & trigger_name & " Name:" & a.name & " State:" & StackState(kStack_Pri0).GetArrowState(a)
						' So the ball doesnt roll over twice before we reset
						vpmtimer.addtimer 400, "debounceReset '"

						PlayProgress trigger_name
						AddScore 15000
						If a.name <> "I35" then ' leave the scoop lit when lit in upgraded mode
							SSetLightColor kStack_Pri0, a, modeRampColor, 0 
						Else
							vpmtimer.addtimer 100,  a.name & ".blinkinterval=20 '"
							vpmtimer.addtimer 1500, a.name & ".blinkinterval=" & BlinkIntDef & " '"
						End If
						thisScore=200000+(50000*(ModeProgress(PlayerMode)+1))
						If bModeProgressUpgraded then thisScore=thisScore+500000
						Select Case Int(RND()*5)
							Case 0: vidToShow="Video-0x002E.mp4"
							Case 1: vidToShow="Video-0x002F.mp4"
							Case 2: vidToShow="Video-0x0030.mp4"
							Case 3: vidToShow="Video-0x0031.mp4"
							Case Else:   vidToShow="Video-0x000A.mp4"
						End Select
						QueueScene "ScenePlayMessage """&vidToShow&""","""&FormatScore(thisScore)&""","""","""" '", 3000, 2
						QueueScene "SceneClearMessage '", 0, 2
						AddScore thisScore
							If a.name = "I62" Then	
								If StackState(kStack_Pri0).GetArrowState(I81)<>0 Then 'Move Right
									SSetLightColor kStack_Pri0, I86, modeRampColor, 1
								Else	
									SSetLightColor kStack_Pri0, I103, modeRampColor, 1
								End If
							End If
							If a.name = "I81" Then	
								If StackState(kStack_Pri0).GetArrowState(I86)<>0 Then 'Move Right
									SSetLightColor kStack_Pri0, I91, modeRampColor, 1
								Else	
									SSetLightColor kStack_Pri0, I107, modeRampColor, 1
								End If
							End If
							If a.name = "I86" Then	
								If StackState(kStack_Pri0).GetArrowState(I91)<>0 Then 'Move Right
									SSetLightColor kStack_Pri0, I97, modeRampColor, 1
								Else	
									SSetLightColor kStack_Pri0, I62, modeRampColor, 1
								End If
							End If
							If a.name = "I91" Then	
								If StackState(kStack_Pri0).GetArrowState(I97)<>0 Then 'Move Right
									SSetLightColor kStack_Pri0, I103, modeRampColor, 1
								Else	
									SSetLightColor kStack_Pri0, I81, modeRampColor, 1
								End If
							End If
							If a.name = "I97" Then	
								If StackState(kStack_Pri0).GetArrowState(I103)<>0 Then 'Move Right
									SSetLightColor kStack_Pri0, I107, modeRampColor, 1
								Else	
									SSetLightColor kStack_Pri0, I86, modeRampColor, 1
								End If
							End If
							If a.name = "I103" Then	
								If StackState(kStack_Pri0).GetArrowState(I107)<>0 Then 'Move Right
									SSetLightColor kStack_Pri0, I62, modeRampColor, 1
								Else	
									SSetLightColor kStack_Pri0, I91, modeRampColor, 1
								End If
							End If
							If a.name = "I107" Then	
								If StackState(kStack_Pri0).GetArrowState(I62)<>0 Then 'Move Right
									SSetLightColor kStack_Pri0, I81, modeRampColor, 1
								Else	
									SSetLightColor kStack_Pri0, I97, modeRampColor, 1
								End If
							End If

						ModeProgress(PlayerMode) = ModeProgress(PlayerMode)+1
						Coins=Coins+1:If bModeProgressUpgraded then Coins=Coins+1
						If NOT flshCIU.TimerEnabled and Not bModeProgressUpgraded Then 
							setCIULight(True)
						End If
						ModePercent(PlayerMode) = CINT((ModeProgress(PlayerMode)  / 10) * 100)  ' 10 shots per this mode
						RefreshPlayerMode 
						If (ModeProgress(PlayerMode) = 10) Then   ' 10 shots
							ModePercent(PlayerMode) = 100
							bModeComplete = True
							pDMDEvent(858) ' Lanes
							setCIULight(False)
						End If
					end if
				Next
				D "Are we in Upgraded Status :" & bModeProgressUpgraded
				If bModeProgressUpgraded=True and (trigger_name = "sw2" or trigger_name = "sw3" or trigger_name = "sw5") then
					D "Award DUDE"
					ModeProgress(PlayerMode) = ModeProgress(PlayerMode)+1
					ModePercent(PlayerMode) = CINT((ModeProgress(PlayerMode)  / 10) * 100)  ' 10 shots per this mode
					RefreshPlayerMode 

					thisScore=200000+(50000*(ModeProgress(PlayerMode)))+500000
					Select Case Int(RND()*5)
						Case 0: vidToShow="Video-0x002E.mp4"
						Case 1: vidToShow="Video-0x002F.mp4"
						Case 2: vidToShow="Video-0x0030.mp4"
						Case 3: vidToShow="Video-0x0031.mp4"
						Case Else:   vidToShow="Video-0x000A.mp4"
					End Select
						QueueScene "ScenePlayMessage """&vidToShow&""","""&FormatScore(thisScore)&""","""","""" '", 3000, 2
						QueueScene "SceneClearMessage '", 0, 2
						AddScore thisScore


					If (ModeProgress(PlayerMode) = 10) Then   ' 10 shots
						ModePercent(PlayerMode) = 100
						bModeComplete = True
						pDMDEvent(858) ' Lanes
						setCIULight(False)
					End If 
				End If
			End If 
		Case 5:  ' back   Upgrade just raises bumper scoring, get Super Pops at end of time or mode
			If trigger_name = "I35" and flshCIU.TimerEnabled Then	' scoop hit - perform upgrade
				setCIULight(False)
				PopLevel=PopLevel+1
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x04F2crankitup.wav",100, 1
				bModeProgressUpgraded=True
				If bHardMode Then ModeCountdownTimer.UserValue=ModeCountdownTimer.UserValue+20
				QueueScene "SceneMessage ""CRANK IT UP"","""&FormatScore(1000000)&""","""" '", 3000, 2
				QueueScene "SceneClearMessage '", 0, 2
				AddScore 1000000
			Else ' after right orbit, light CIU, each bumper hit moves it one more .. need this to relight the right orbit
				D "Looping thru the ramps trigger_name=" & trigger_name & " for Back in the Saddle"
				For each a in aRampLights
					If a.name = trigger_name and StackState(kStack_Pri0).GetArrowState(a) <> 0 then 
						bDebounce = True	
						D "Match trigger_name=" & trigger_name & " Name:" & a.name & " State:" & StackState(kStack_Pri0).GetArrowState(a)
						' So the ball doesnt roll over twice before we reset
						vpmtimer.addtimer 400, "debounceReset '"

'back1						'PlaySound "gg_RampHit"					' Play sounds and reset the timer to reset the lights 
						PlayProgress trigger_name
						bValidHit=True
						AddScore 15000
						SSetLightColor kStack_Pri0, a, modeRampColor, 0 
						If a.name = "I107" and not bModeProgressUpgraded Then   ' light CIU shot then bumpers will move it
							SSetLightColor kStack_Pri0, I35, modeRampColor, 1
							Exit For
						else ' turn the orbit back on
							SSetLightColor kStack_Pri0, I107, modeRampColor, 1
							Exit For
						End If 
					End If
				Next
				If bValidHit Then
					ModeProgress(PlayerMode) = ModeProgress(PlayerMode)+1
					Coins=Coins+1:If bModeProgressUpgraded then Coins=Coins+1
					If NOT flshCIU.TimerEnabled and Not bModeProgressUpgraded Then 
						setCIULight(True)
					End If
					ModePercent(PlayerMode) = CINT((ModeProgress(PlayerMode)  / 8) * 100)  ' 8 shots per this mode
					RefreshPlayerMode 
					If (ModeProgress(PlayerMode) = 8) Then   ' 8 shots
						ModePercent(PlayerMode) = 100
						bModeComplete = True
						pDMDEvent(853)  ' Pops
						setCIULight(False)
					End If
				end if
			End If 
		Case 6: ' Rats 
			Dim NewUpgrade, a2
			NewUpgrade=False
			If trigger_name = "I35" and flshCIU.TimerEnabled Then	' scoop hit - perform upgrade 1xtra light`
				D "Rats Upgrade is underway"
				setCIULight(False)
				PuPlayer.playlistplayex pCallouts,"audioevents","Sound-0x04F2crankitup.wav",100, 1
				bModeProgressUpgraded=True  ' Go to next left
				NewUpgrade=True
				If bHardMode Then ModeCountdownTimer.UserValue=ModeCountdownTimer.UserValue+20
				QueueScene "SceneMessage ""CRANK IT UP"","""&FormatScore(1000000)&""","""" '", 3000, 2
				QueueScene "SceneClearMessage '", 0, 2
				AddScore 1000000
			End If

			' 1onL, then 1onR, then 2onL, 2onR, 3onL, 3onR
			D "Looping thru the ramps trigger_name=" & trigger_name  & " for Rats"
			For each a in aRampLights
				If a.name = trigger_name and StackState(kStack_Pri0).GetArrowState(a) <> 0 then 
					bDebounce = True	
					D "Match trigger_name=" & trigger_name & " Name:" & a.name & " State:" & StackState(kStack_Pri0).GetArrowState(a)

					' So the ball doesnt roll over twice before we reset
					vpmtimer.addtimer 400, "debounceReset '"

					'PlaySound "gg_RampHit"					' Play sounds and reset the timer to reset the lights
					'LightSeqScore.Play SeqUpOn, 100, 0		 
					PlayProgress trigger_name		

					If StackState(kStack_Pri0).GetArrowState(a) = 2 Then  ' If lit then turn off
						AddScore 15000
						FlashForMs a, 200, 50, 0
						SSetLightColor kStack_Pri0, a, modeRampColor, 0 
					End If 
					' If no lights left then go to next Rats Level
					i=0
					for each a2 in aRampLights
						if StackState(kStack_Pri0).GetArrowState(a2) <> 0 Then i=i+1
					Next
					D "Rats .. number of lights still lit = " & i
					If i=0 Then ' No lights left, so go to the next level
						RatsStep=RatsStep+1
						if NewUpgrade then RatsStep=RatsStep+1
						Select Case RatsStep:
							Case 1: CountLeft=1:CountRight=0
							Case 2: CountLeft=0:CountRight=1
							Case 3: CountLeft=2:CountRight=0
							Case 4: CountLeft=0:CountRight=2
							Case 5: CountLeft=3:CountRight=0
							Case 6: CountLeft=0:CountRight=3
							Case Else:CountLeft=3:CountRight=3
						End Select
					
						RatsPos1 = Int(RND*4)+1  ' Find upto 3 random arrows
						Do 
							RatsPos2 = INT(RND*4)+1
						Loop Until RatsPos1 <> RatsPos2
						Do 
							RatsPos3 = INT(RND*4)+1
						Loop Until RatsPos1 <> RatsPos3 and RatsPos2 <> RatsPos3
						D "Rats Positions: " & RatsPos1 & ":" & RatsPos2 & ":" & RatsPos3 & " Step:" & RatsStep
						i=1
						For each a2 in aRampLights
							If i = RatsPos1 and CountLeft > 0 Then SSetLightColor kStack_Pri0, a2, modeRampColor, 2
							If i = RatsPos2 and CountLeft > 1 Then SSetLightColor kStack_Pri0, a2, modeRampColor, 2
							If i = RatsPos3 and CountLeft > 2 Then SSetLightColor kStack_Pri0, a2, modeRampColor, 2				
							If i = RatsPos1+4 and CountRight > 0 Then SSetLightColor kStack_Pri0, a2, modeRampColor, 2
							If i = RatsPos2+4 and CountRight > 1 Then SSetLightColor kStack_Pri0, a2, modeRampColor, 2
							If i = RatsPos3+4 and CountRight > 2 Then SSetLightColor kStack_Pri0, a2, modeRampColor, 2
							i=i+1
						Next
					End if
					ModeProgress(PlayerMode) = ModeProgress(PlayerMode)+1
					Coins=Coins+1:If bModeProgressUpgraded then Coins=Coins+1
					If NOT flshCIU.TimerEnabled and Not bModeProgressUpgraded Then 
						D "Rats - light crank it up after the first hit"
						setCIULight(True)
					End If
					ModePercent(PlayerMode) = CINT((ModeProgress(PlayerMode)  / 8) * 100)  ' 8 shots per this mode
					RefreshPlayerMode 
					If (ModeProgress(PlayerMode) = 8) Then   ' 8 shots
						ModePercent(PlayerMode) = 100
						bModeComplete = True
						pDMDEvent(856) ' Spinners
						setCIULight(False)
					End If
				End If
			Next
		End Select
		If bModeComplete Then
			ModeCountdownTimer.Enabled = False	' Stop the countdown immediatly 
			'PuPlayer.LabelSet pBackglass,"Time", " ",0,""					' leave timer up as we go into mode2
			ShowPlayerModeComplete(-1)			' Show Mode total
			'Finished the level 
			bBonusMode = True
			'not till shot is obtained  setModeSelectLight(True)
			StopPlayerMode
			'''playmedia "Video-0x0000.mp4", "PupVideos", pBonusScreen, "", -1, "Mode Done", 1, 1
		End If
	end if
	CheckSmartButton False
	D "CheckModeProgress Done. " & trigger_name & " PlayerMode:" & PlayerMode & " 2:" & PlayerMode2 & _
      " bSecondMode:" & bSecondMode & " bonusMode:" & bBonusMode & _
      " Wiz:" & bWizardMode & " state:" & I34.state & " Final:" & bFinalTourReady & " tmrMed:" & tmrMedleyTour.Enabled 
End Sub

Sub CheckModeProgress2nd(trigger_name)
	dim bValidHit
	dim a, b, i 
	dim hitLight 
	Dim bModeComplete

	D "CheckModeProgress2nd " & trigger_name & " playermode:" & playermode & " 2:" & playermode2

	bModeComplete = False
	bValidHit = False

	If PlayerMode <> -1 Then		' Just in Case we get back in here we bail because we are already 100% progress
		D "Mode Progress: " & ModePercent(PlayerMode)
		If Mode2Percent(PlayerMode) >= 100 then 
			D "WE SHOULDNT GET HERE.  Skip Score: " & ModePercent(PlayerMode)
			Exit Sub 
		End If 
	End If

	If bWizardMode then Exit Sub ' Dont score and bonus during Wizard Mode

	Select Case PlayerMode2
		Case -1:' No Mode Selected
		Case 0: ' orbits
			If trigger_name = "I62" or trigger_name = "I107" Then
				PlayProgress2 trigger_name, False

				Mode2Progress(PlayerMode2) = Mode2Progress(PlayerMode2)+1
				Mode2Percent(PlayerMode2) = CINT((Mode2Progress(PlayerMode2)  / 10) * 100)
				AddScore Mode2Value(PlayerMode2)			' 

				ShowPlayerMode2(PlayerMode2)
				If (Mode2Progress(PlayerMode2) >= 10) Then
					Mode2Percent(PlayerMode2) = 100
					bModeComplete=True
				End If
			End If
		Case 1: ' ramps
			If trigger_name = "I86" or trigger_name = "I103" Then
				PlayProgress2 trigger_name, False

				Mode2Progress(PlayerMode2) = Mode2Progress(PlayerMode2)+1
				Mode2Percent(PlayerMode2) = CINT((Mode2Progress(PlayerMode2)  / 10) * 100)
				AddScore Mode2Value(PlayerMode2)		' 

				ShowPlayerMode2(PlayerMode2)
				If (Mode2Progress(PlayerMode2) >= 10) Then
					Mode2Percent(PlayerMode2) = 100
					bModeComplete=True
				End If
			End If
		Case 2: ' targets
			For each a in aerosmithLights
				If a.name = trigger_name Then
					SetLightColor a, "orange", 1
					PlayProgress2 trigger_name, False

					Mode2Progress(PlayerMode2) = Mode2Progress(PlayerMode2)+1
					Mode2Percent(PlayerMode2) = CINT((Mode2Progress(PlayerMode2)  / 20) * 100)
					AddScore Mode2Value(PlayerMode2)		' 

					ShowPlayerMode2(PlayerMode2)
					If (Mode2Progress(PlayerMode2) >= 20) Then
						Mode2Percent(PlayerMode2) = 100
						bModeComplete=True
					End If
					exit for
				End If
			Next
		Case 3: ' scoring
			If trigger_name = "switch" Then
				PlayProgress2 trigger_name, False

				Mode2Progress(PlayerMode2) = Mode2Progress(PlayerMode2)+1
				Mode2Percent(PlayerMode2) = CINT((Mode2Progress(PlayerMode2)  / 20) * 100)

				ShowPlayerMode2(PlayerMode2)
				If (Mode2Progress(PlayerMode2) >= 20) Then
					Mode2Percent(PlayerMode2) = 100
					bModeComplete=True
				End If
			End If
		Case 4: ' lanes
			If trigger_name = "sw2" or trigger_name = "sw3" or trigger_name = "sw5" Then
				PlayProgress2 trigger_name, False

				Mode2Progress(PlayerMode2) = Mode2Progress(PlayerMode2)+1
				Mode2Percent(PlayerMode2) = CINT((Mode2Progress(PlayerMode2)  / 10) * 100)
				AddScore Mode2Value(PlayerMode2)	' 

				ShowPlayerMode2(PlayerMode2)
				If (Mode2Progress(PlayerMode2) >= 10) Then
					Mode2Percent(PlayerMode2) = 100
					bModeComplete=True
				End If
			End If
		Case 5: ' pops
			If trigger_name = "bumper" Then
				PlayProgress2 trigger_name, False

				Mode2Progress(PlayerMode2) = Mode2Progress(PlayerMode2)+1
				Mode2Percent(PlayerMode2) = CINT((Mode2Progress(PlayerMode2)  / 50) * 100)
				AddScore Mode2Value(PlayerMode2)

				ShowPlayerMode2(PlayerMode2)
				If (Mode2Progress(PlayerMode2) >= 50) Then
					Mode2Percent(PlayerMode2) = 100
					bModeComplete=True
				End If
			End If
		Case 6: ' spinner
			If trigger_name = "sw38" Then
				PlayProgress2 trigger_name, False

				Mode2Progress(PlayerMode2) = Mode2Progress(PlayerMode2)+1
				Mode2Percent(PlayerMode2) = CINT((Mode2Progress(PlayerMode2)  / 100) * 100)
				AddScore Mode2Value(PlayerMode2)			' 

				ShowPlayerMode2(PlayerMode2)
				If (Mode2Progress(PlayerMode2) >= 100) Then
					Mode2Percent(PlayerMode2) = 100
					bModeComplete=True
				End If
			End If
	End Select

	If bModeComplete then
		D "2nd Mode complete" 
		'NOT NEEDED ShowPlayerModeComplete(-1)			' Show 2nd Mode total
		StopPlayerMode2						' Stop this mode.
	End If 
End Sub

Sub StopPlayerMode2
	Dim a,i
	D "StopPlayerMode2 " & PlayerMode & " 2:" & PlayerMode2
	If bSecondMode then
		StopPlayerModeVideo
		StackState(kStack_Pri0).Disable
	End If

	bSecondMode = False
	If playermode2 = 2 Then
		i=0
		for each a in aerosmithLights 'restore the lights to the settings prior to thie SAME mode
			SetLightColor a, "orange", HoldAerosmithLights(i)
			i=i+1
		next
	End If
	playermode2 = -1  

	' turn off pedals and bubble, light chg song
	PuPlayer.LabelSet pBackglass,"Time", " ",1,""
	' RefreshPlayerMode() asdf
	PuPlayer.LabelSet pBackglass, "ModeProgress", "PuPOverlays\\Progress8-0.png",0,"{'mt':2,'color':111111,'width':12, 'height':23, 'yalign':0,'ypos':4.0,'xpos':5,'pagenum':1}"
	
	pDMDEvent(P)
	D "StopPlayerMode2 calling SetModeLights"
	SetModeLights

	CheckWizardModesReady	' See of we need to enable wizard modes 
End Sub

Sub PlayProgress(name)
	PlayProgress2 name, True
End Sub

Sub PlayProgress2(name, bAudio)
	if bAudio then PlaySoundVol "Metal_Touch_1", VolDef

	LightSeqScore.UpdateInterval = 2
	LightSeqScore.Play SeqUpOn, 100, 0

	If name = "I35" or name = "I62" or name = "I81" or name = "I86" then 
		LeftLightFlash
	Else
		RightLightFlash
	End If 
End Sub

Dim FlashLevel1,FlashLevel2, FlashLevel3, FlashLevel4, FlashLevel5

tmrLightFlash.Interval = 200
Flasherflash1.TimerInterval = 30
' FlashLevel(10) = 1 : FlasherFlash10_Timer
Sub tmrLightFlash_Timer
	if bFlash1Enabled and Flasherflash1.TimerEnabled=False then FlashLevel1 = 1 : FlashLevel(1) = 1:Flasherflash1_Timer : If BallsOnPlayfield > 0 Then DOF 202, DOFPulse
	if bFlash2Enabled and Flasherflash2.TimerEnabled=False then FlashLevel2 = 1 : FlashLevel(2) = 1:Flasherflash2_Timer : If BallsOnPlayfield > 0 Then DOF 201, DOFPulse
	if bFlash3Enabled and Flasherflash3.TimerEnabled=False then FlashLevel3 = 1 : FlashLevel(3) = 1:Flasherflash3_Timer : If BallsOnPlayfield > 0 Then DOF 200, DOFPulse
	if bFlash4Enabled and Flasherflash4.TimerEnabled=False then FlashLevel4 = 1 : FlashLevel(4) = 1:Flasherflash4_Timer : If BallsOnPlayfield > 0 Then DOF 203, DOFPulse
	if bFlashFastEnabled then FlashLevel1 = 0.5 : FlashLevel(1) = 0.5: Flasherflash1_Timer : If BallsOnPlayfield > 0 Then DOF 203, DOFPulse

End Sub 

Sub LeftLightFlash()
	bFlash1Enabled = True
	vpmtimer.addtimer 200, "bFlash3Enabled = True'"
	vpmtimer.addtimer 400, "bFlash2Enabled = True'"
	vpmtimer.addtimer 600, "bFlash4Enabled = True'"
	vpmtimer.addtimer 1500, "bFlash1Enabled = False:bFlash2Enabled = False:bFlash3Enabled = False:bFlash4Enabled = False' "
End Sub

Sub RightLightFlash()
	bFlash3Enabled = True
	vpmtimer.addtimer 200, "bFlash1Enabled = True'"
	vpmtimer.addtimer 400, "bFlash2Enabled = True'"
	vpmtimer.addtimer 600, "bFlash4Enabled = True'"
	vpmtimer.addtimer 1500, "bFlash1Enabled = False:bFlash2Enabled = False:bFlash3Enabled = False:bFlash4Enabled = False' "
End Sub


Function getRndShot(a, Stack_Pri)
Dim b,c
	D "getRndShot a=" & a
	For b = 0 to 6
		c=Int(7*Rnd())  
		If c<>a and StackState(Stack_Pri).ArrowStates(c).ArrowState = 0 Then Exit For 
	Next
	D "Found shot " & c
	getRndShot=c
End Function


Sub SetModeLightComplete(Idx, Light)
	Dim bUpdated:bUpdated = False
	D "SetModeLightComplete Idx:" & Idx & " %:" & ModePercent(Idx) & " mode:" & PlayerMode

	If ModePercent(Idx) >= 0 Then 	' flashes slow
		bUpdated=True
		Light.State = 2
		If bHardMode=True or Idx=7 or Idx=8 Then
			If ModePercent(Idx) > 0 then  ' Need to have completed atleast 1 shot
				modesStarted=modesStarted+1
			End If
		Else
			modesStarted=modesStarted+1
		End If
		Light.BlinkInterval=BlinkIntSlow
		Light.BlinkPattern=BlinkPatternSlow
	End If 
	If ModePercent(Idx) >= 100 Then	' 100% Is Solid
		bUpdated=True
		Light.State = 1
		modesCompleted=modesCompleted+1
		Light.BlinkInterval=BlinkIntDef
		Light.BlinkPattern=BlinkPatternDef
	End If

	If Idx = PlayerMode then 	' Current mode flashes
		bUpdated=True
		Light.State = 2
		Light.BlinkInterval=BlinkIntDef
		Light.BlinkPattern=BlinkPatternDef
	End If 

	If bUpdated=False Then 
		Light.BlinkInterval=BlinkIntDef
		Light.BlinkPattern=BlinkPatternDef		
	End If 
End Sub

dim tmrFinishModeBlinkCnt
Dim tmrFinishModeIndex

Sub SceneFinishMode(Mode)
	dim i
	Dim visible
	If Mode = -1 then exit sub ' Just in Case 
	' Overlay the final onto a video for background
	playmedia "ARS107-Scene-72.mp4", "PupVideos", pOverVid, "", -1, "", 1, 1
	PuPlayer.LabelShowPage pOverVid, 3,0,""
	D "SceneFinishMode2"    	
	For i = 0 to 8
		If i = 7 then 
			If I22.state = 0 then visible = 0 else visible =1
		elseIf i = 8 then 
			If I23.state = 0 then visible = 0 else visible =1
		else
			If ModePercent(i) >= 100 then ' Only those that are COMPLETE
				visible = 1
			else
				visible = 0  
			End If 
		End If 
		PuPlayer.LabelSet pOverVid, "F" & i, "PuPOverlays\\CompleteLevel-"&i&".png", visible,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	Next 
'	tmrFinishMode.UserValue=-1
'	tmrFinishModeIndex=Mode
'	tmrFinishMode.Interval = 50
'	tmrFinishMode.enabled = true
End Sub 


Sub SceneClearFinishMode() ' 1
	tmrFinishMode.enabled = False
	D "Scene Clear"
	PuPlayer.LabelSet pOverVid, "F0", "PuPOverlays\\CompleteLevel-0.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F1", "PuPOverlays\\CompleteLevel-1.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F2", "PuPOverlays\\CompleteLevel-2.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F3", "PuPOverlays\\CompleteLevel-3.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F4", "PuPOverlays\\CompleteLevel-4.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F5", "PuPOverlays\\CompleteLevel-5.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F6", "PuPOverlays\\CompleteLevel-6.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F7", "PuPOverlays\\CompleteLevel-7.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"
	PuPlayer.LabelSet pOverVid, "F8", "PuPOverlays\\CompleteLevel-8.png",0,"{'mt':2,'color':111111,'width':100, 'height':100,'yalign':0,'ypos':0,'xpos':0}"

	PuPlayer.LabelShowPage pOverVid, 1,0,""
    playclear pOverVid
End Sub


'sub toyboxmb_end
Function ToyBoxMB_End(checkRelease) 
	Dim j,a
	D "Function ToyboxMB_End() check:" & checkRelease & " toyboxMB:" & bToyBoxMultiball & _
			" bop:" & BallsOnPlayfield & " PlayerMode:" & playermode & " tmr:" & tmrToyBoxMBBonus.enabled
	ToyBoxMB_End=False
	If bToyBoxMultiball then 
		D "check Release: " & checkRelease & " pfBalls:" & BallsOnPlayfield & " lock:" _
				&  RealBallsInLock & " MBBonus:" & tmrToyBoxMBBonus.Enabled
		' Release saved balls    
		If BallsOnPlayfield=0 and RealBallsInLock > 0 and tmrToyBoxMBBonus.Enabled then			
																		' During MB If we lock any balls we let them go once we drain (If we are still in MB)
			D "ToyBox MB Release " & RealBallsInLock
			bBallSaverReady = True		' Turn On Ball Saver 
			EnableBallSaver BallSaverTime
			ToyBoxMB_End=True
			If RealBallsInLock = 1 and checkRelease then	' Go ahead and ToyBoxMB_End but keep playing
				D "ToyBoxMB_End=False"
				ToyBoxMB_End = False
			End If

			for j = 0 to RealBallsInLock - 1
				RealBallsInLock = RealBallsInLock -1
				D "ToyBoxMB_End: Releasing 1 Remaining Ball" & RealBallsInLock
				vpmtimer.addtimer 1000 + (j*200), "CreateNewBallToyBox() '"
			Next
		elseIf BallsOnPlayfield=1 and RealBallsInLock > 0 then 	' We stay in MB because when the ball releases normally we can play MB
			D "ToyBoxMB_End=True"
			ToyBoxMB_End=True
		End If
		If checkRelease then Exit Function 				' Bail out because we are just checking If we continue 

		for each a in aRampLights
			SSetLightColor kStack_Pri1, a, "white", 0
		Next
		StackState(kStack_Pri1).Disable

		D "ToyBoxMB_End: Disable TOYBOX and restore the song playermode=" & Playermode
		if playerMode <> -1 Then 
			PlaySong "Song-" & SaveMode & ".mp3"
		Else
			PlaySong "clear.mp3"
			if BallsOnPlayfield < 2 then setModeSelectLight(True)
		End If

		'aRampLightsRestore				' Put light back to what they should before mode (Hopefully)
		D "Turn on Elevator Lights " & SaveI94 & SaveI65 & SaveI110
		
		SetLightColor I94, "white", SaveI94
		SetLightColor I65, "white", SaveI65
		SetLightColor I110,"white", SaveI110
		SetLightColor I100,"green", 0  ' reset toybox lock
		ShowPlayerModeComplete(0)	
		if ModeProgress(7) >= 100 Then SetLightColor I22, "green", 1		' Reset this solid if you earned it
		bToyBoxMultiball = False
		ToyBoxMBLocks = 0
		tmrToyBoxMBBonus.Enabled=False	' Disable the timer but leave whatever balls in there
		If bUsePUPDMD then puPlayer.LabelSet pBackglass,"MTimer",	""	,1,""
		bToyBoxBonus3x=False
		bAutoPlunger=False
		CheckWizardModesReady
	End If 
	D "Function ToybBoxMB_End return:" & ToyBoxMB_End
	ScorbitBuildGameModes()
End Function

Sub CreateNewBallToyBox  
    sw61.CreateSizedball BallSize / 2

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
	sw61.Kick -36, 7+RndNum(0,5)
' If there is 2 or more balls then set the multiball flag (remember to check for locked balls and other balls used for animations)
' set the bAutoPlunger flag to kick the ball in play automatically
    If BallsOnPlayfield-realballsinlock > 1 Then
		D "TOYBOX SETTING MULTIBALL MODE " & BallsOnPlayfield & "!!!!!!!!!!!!!!!!!!"
        bMultiBallMode = True
    End If
End Sub


Sub debounceReset() 		' Reset Debounce for those that dont have a timer
	bDebounce = False
End Sub

const kDMD_Myst_IsLit		=599		
const kDMD_Myst_AddABall	=600
const kDMD_Myst_AddTime 	=601
const kDMD_Myst_10Million 	=602
const kDMD_Myst_2Million	=603		
const kDMD_Myst_20Million	=604
const kDMD_Myst_BonusHold 	=605
const kDMD_Myst_BonusXHold	=606
const kDMD_Myst_DoubleScore =607
const kDMD_Myst_Lock1		=608
const kDMD_Myst_Lock2    	=609		
const kDMD_Myst_Lock3       =610
const kDMD_Myst_ExtraBallLit=611
const kDMD_Myst_HPhoneHurry =612
const kDMD_Myst_IncreasePop =613		
const kDMD_Myst_IncreaseSpin=614
const kDMD_Myst_ShotMult	=615
const kDMD_Myst_SmartMiss	=616
const kDMD_Myst_SmartMissLit=617		
const kDMD_Myst_SpecialLit	=618		
const kDMD_Myst_SuperScore	=619	
const kDMD_Myst_SmartMissQ  =620

const kDMD_Myst_MAX			=621

const kDMD_Myst_BonusMultiplier=621
const kDMD_Myst_Bonus2x		=622
const kDMD_Myst_Bonus3x		=623		
const kDMD_Myst_Bonus4x		=624		
const kDMD_Myst_Bonus5x		=625		
const kDMD_Myst_Bonus6x		=626		
const kDMD_Myst_Bonus7x		=627
const kDMD_Myst_Bonus8x		=628		
const kDMD_Myst_Bonus9x		=629		
const kDMD_Myst_Bonus10x	=630		

Dim bToyBoxMultiball:bToyBoxMultiball=False
Sub AwardMystery()
	dim award 
	award = INT((kDMD_Myst_MAX-600+1)*Rnd())+600
	D "Sub AwardMystery() #:" + cstr(award) & " Prior XtraBall:" & bExtraBallWonThisBall
	If (bElevMultiBall or bToyBoxMultiball) and bAddedABall=False Then ' If in MB then you get AddABall for Mystery
		award = kDMD_Myst_AddABall 
	ElseIf award = kDMD_Myst_ExtraBallLit and bExtraBallWonThisBall Then
			award = kDMD_Myst_2Million
	End If

	If (bSecondMode or PlayerMode = -1) and award = kDMD_Myst_AddABall Then
		award=kDMD_Myst_2Million
	End If

	If (bElevMultiBall or bToyBoxMultiball) Then
		If award = kDMD_Myst_Lock1 or award = kDMD_Myst_Lock2 or award = kDMD_Myst_Lock3 then
			award = kDMD_Myst_2Million
		End If
	End If

	If award = kDMD_Myst_AddTime and ModecountdownTimer.UserValue <=0 Then
		award = kDMD_Myst_2Million
	End If

	If award <> kDMD_Myst_BonusMultiplier then		' This one is based off the current multiplier 
		pDMDEvent(award)
	End If 
	D ">>>>>>>>>Mystery Award # being awarded is: (600isAdd) :" & award
	Select Case award:
		Case kDMD_Myst_BonusMultiplier:
			If BonusMultiplier < 10 then
				AddBonusMultiplier 1
			Else
				DisplayDMDText2 "MYSTERY","2 MILLION", 1000, 11, 0
				AddScore 2000000
			End If
		Case kDMD_Myst_AddTime:
			DisplayDMDText2 "MYSTERY","AWARD MORE TIME", 1000, 11, 0
			ModecountdownTimer.UserValue=ModecountdownTimer.UserValue+20
		Case kDMD_Myst_SuperScore:
			DisplayDMDText2 "MYSTERY","SUPER SCORING", 1000, 11, 0
		Case kDMD_Myst_BonusXHold:
			DisplayDMDText2 "MYSTERY","BONUS X HOLD", 1000, 11, 0
		Case kDMD_Myst_DoubleScore:
			DisplayDMDText2 "MYSTERY","DOUBLE SCORING", 1000, 11, 0
		Case kDMD_Myst_IncreasePop:
			DisplayDMDText2 "MYSTERY","INC POP VALUE", 1000, 11, 0
			PopValue=int(PopValue*1.15)
			BumperMultiplier=BumperMultiplier+1
			PopHits=25
			PopLevel=PopLevel+1
		Case kDMD_Myst_IncreaseSpin:
			DisplayDMDText2 "MYSTERY","INC SPIN VALUE", 1000, 11, 0
		Case kDMD_Myst_ShotMult:
			DisplayDMDText2 "MYSTERY","SHOT MULTIPLIER", 1000, 11, 0
		Case kDMD_Myst_SmartMiss:
			DisplayDMDText2 "MYSTERY","SMART MISSLE", 1000, 11, 0
			SmartButtonCount=SmartButtonCount+1  ' Give you one then call the routine
			SmartMissilePressed()
		Case kDMD_Myst_SmartMissLit:
			DisplayDMDText2 "MYSTERY","SMART MISSILE LIT", 1000, 11, 0
			SmartButtonCount=SmartButtonCount+1
		Case kDMD_Myst_SmartMissQ:
			DisplayDMDText2 "MYSTERY","SMART MISSILE QUALIFIED", 1000, 11, 0
			SmartButtonCount=SmartButtonCount+1  ' You get it but cant use it now anyways
		Case kDMD_Myst_SuperScore:
			DisplayDMDText2 "MYSTERY","SUPER SCORING", 1000, 11, 0
		Case kDMD_Myst_Lock1,kDMD_Myst_Lock2,kDMD_Myst_Lock3:
			ElevMultiBallCount = ElevMultiBallCount + 1
			'vpmtimer.addtimer 4000, "pDMDEvent(" & kDMD_ElevCollected + ElevMultiBallCount -1 & ") '"   
			DisplayDMDText2 "MYSTERY","LOCK AWARDED", 1000, 11, 0
			Select Case ElevMultiBallCount:
				Case 1:
					QueueScene "ScenePlayMessage ""Elevator.mp4"", ""BALL 1"",""LOCKED"","""" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
				Case 2:
					QueueScene "ScenePlayMessage ""Elevator.mp4"", ""BALL 2"",""LOCKED"","""" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
				Case 3:
					QueueScene "ScenePlayMessage ""Elevator.mp4"", ""BALL 3"",""LOCKED"","""" '", 2000, 2
					QueueScene "SceneClearPlayMessage '", 0, 2
			End Select
		Case kDMD_Myst_IsLit:
			DisplayDMDText2 "MYSTERY","VIP IS LIT", 1000, 11, 0
			SetLightColor I27, "white", 2
		Case kDMD_Myst_SpecialLit:
			DisplayDMDText2 "MYSTERY","SPECIAL IS LIT", 1000, 11, 0
			SetLightCOlor I11, "red", 2
		Case kDMD_Myst_AddABall:	' Add Multiball
			DisplayDMDText2 "MYSTERY","Add A Ball", 1000, 11, 0	
			D "Mystery addaball"
			bAddedABall=True
			AddMultiball 1
		Case kDMD_Myst_BonusHold:	
			DisplayDMDText2 "MYSTERY","BONUS HOLD", 1000, 11, 0
		Case kDMD_Myst_HPhoneHurry:
			DisplayDMDText2 "MYSTERY","HURRYUP", 1000, 11, 0
		Case kDMD_Myst_2Million:
			DisplayDMDText2 "MYSTERY","2 MILLION", 1000, 11, 0
			AddScore 2000000
		Case kDMD_Myst_10Million:
			DisplayDMDText2 "MYSTERY","10 MILLION", 1000, 11, 0
			AddScore 10000000
		Case kDMD_Myst_20Million:
			DisplayDMDText2 "MYSTERY","20 MILLION", 1000, 11, 0
			AddScore 20000000
		Case kDMD_Myst_ExtraBallLit:
			setExtraBallLight(True)
			DisplayDMDText2 "MYSTERY","EXTRA BALL LIT", 1000, 11, 0
		Case Else:
			DisplayDMDText2 "MYSTERY","2 MILLION", 1000, 11, 0
			AddScore 2000000
	End Select 
End Sub

Sub SmartMissilePressed ' Award the arrow and the shot multiplier if set
	D "Sub SmartMissilePressed()"

	SmartButtonCount = SmartButtonCount - 1
	dim a
	PlayDMDScene "", "SMART", "MISSILE!", 2000
	PlayDMDScene "", "SMART", "MISSILE!", 2000
	for each a in ArrowLights
		if a.state <> 0 then ' award this Hit
			Select case a.name:
				Case "I35": D a.name & " awarded .. to do" ' ScoopEject
					ClearShotMultiplier(I32)  ' the X multiplier
					CheckModeProgress "I35"
				Case "I62": D a.name & " awarded" ' sw62 loop
					ClearShotMultiplier(I67)
					CheckModeProgress "I62"
				Case "I81": D a.name & " awarded" ' sw56 toy box
					ClearShotMultiplier(I84)
					CheckModeProgress("I81")
				Case "I86": D a.name & " awarded" ' sw53 center loop
					ClearShotMultiplier(I89)
					CheckModeProgress "I86"
				Case "I91": D a.name & " awarded" ' elevatoreject
					ClearShotMultiplier(I95)
					CheckModeProgress("I91")
				Case "I97": D a.name & " awarded" ' sw61 lock
					ClearShotMultiplier(I101)
					CheckModeProgress("I97")
				Case "I103":D a.name & " awarded" ' sw63 right ramp
					ClearShotMultiplier(I46)
					CheckModeProgress "I103"
				Case "I107":D a.name & " awarded" ' sw52 loop
					ClearShotMultiplier(I112)
					CheckModeProgress "I107"
			End Select
			exit For
		end if
	Next
End Sub

sub debug_medleytour
dim a
	ModecountdownTimer.UserValue = 5
	for a = 0 to 8
		ModeProgress(a)=5
		ModePercent(a)=50
	Next 
	ModeOrder(0)=4:ModePercent(4)=50
	ModeOrder(1)=3:ModePercent(3)=50
	ModeOrder(2)=6:ModePercent(6)=50
	ModeOrder(3)=8:ModePercent(8)=100
	ModeOrder(4)=1:ModePercent(1)=100
	ModeOrder(5)=2:ModePercent(2)=100
	ModeOrder(6)=0:ModePercent(0)=100
	ModeOrder(7)=5:ModePercent(5)=50
	ModeOrder(8)=7:ModePercent(7)=100
	SetModeLights
D "1"
	bMedleyTourReady=True
	bFinalTourReady=False 
	SceneClearMessage()
	setModeSelectLight(True)
D "1"
	SetLightColor I34, "white", 2
D "2"
	PlayScoopLightSeq
D "3"
	Multiplier3x=1
	MultiplierShot=1
	PlayMultiplier=1
end sub

sub debug_finaltour
dim a
	D "sub debug_finaltour() currentplayer=" & CurrentPlayer
	ModecountdownTimer.UserValue = 5
	for a = 0 to 8
		ModeProgress(a)=5
		ModePercent(a)=50
		Mode2Progress(a)=100
	Next 
	ModeOrder(0)=4:ModePercent(4)=100
	ModeOrder(1)=3:ModePercent(3)=100
	ModeOrder(2)=6:ModePercent(6)=100
	ModeOrder(3)=8:ModePercent(8)=100
	ModeOrder(4)=1:ModePercent(1)=100
	ModeOrder(5)=2:ModePercent(2)=100
	ModeOrder(6)=0:ModePercent(0)=100
	ModeOrder(7)=5:ModePercent(5)=100
	ModeOrder(8)=7:ModePercent(7)=100

	PlayerState(CurrentPlayer).bExtraball_2 = True
	PlayerState(CurrentPlayer).bExtraball_3 = True

	SetModeLights
	bMedleyTourReady=True
	bFinalTourReady=True 
	SceneClearMessage()
	setModeSelectLight(True)
	SetLightColor I34, "white", 2
	PlayScoopLightSeq
	modeRampColor = "yellow"
	Multiplier3x=1
	MultiplierShot=1
	PlayMultiplier=1
end sub

Sub FlexDMDTimer_Timer
	Dim DMDp
	If FlexDMD.RenderMode = FlexDMD_RenderMode_DMD_RGB Then
		DMDp = FlexDMD.DmdColoredPixels
		If Not IsEmpty(DMDp) Then
			DMDWidth = FlexDMD.Width
			DMDHeight = FlexDMD.Height
			DMDColoredPixels = DMDp
		End If
	Else
		DMDp = FlexDMD.DmdPixels
		If Not IsEmpty(DMDp) Then
			DMDWidth = FlexDMD.Width
			DMDHeight = FlexDMD.Height
			DMDPixels = DMDp
		End If
	End If
End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  SCORBIT Interface
' To Use:
' 1) Define a timer tmrScorbit
' 2) Call DoInit at the end of PupInit or in Table Init if you are nto using pup with the appropriate parameters
'     Replace 2108 with your TableID from Scorbit 
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
	D "Scorbit PAIRED"
	PlaySoundVol "scorbit_login", VolSfx
	PuPlayer.LabelSet pOverVid, "ScorbitQR", "PuPOverlays\\clear.png",0,""
	PuPlayer.LabelSet pOverVid, "ScorbitQRIcon", "PuPOverlays\\clear.png",0,""
End Sub 

Sub Scorbit_PlayerClaimed(PlayerNum, PlayerName)	' Scorbit callback when QR Is Claimed 
	D "Scorbit LOGIN"
	PlaySoundVol "scorbit_login", VolSfx
	ScorbitClaimQR(False)
	puPlayer.LabelSet pDMD,"Player",	PlayerName	,1,""
	D "Scorbit_PlayerClaimed:" & PlayerNum & " " & PlayerName
End Sub 


Sub ScorbitClaimQR(bShow)						'  Show QRCode on first ball for users to claim this position
	if Scorbit.bSessionActive=False then Exit Sub 
	if ScorbitShowClaimQR=False then Exit Sub
	if Scorbit.bNeedsPairing then exit sub 

	if bShow and balls=1 and bGameInPlay and Scorbit.GetName(CurrentPlayer+1)="" then 
		PlaySoundVol "scorbit_detected_claimed", VolSfx
		if PupOption=0 or ScorbitClaimSmall=0 then ' Desktop Make it Larger
			PuPlayer.LabelSet pDMD, "ScorbitQR", "PuPOverlays\\QRclaim.png",1,"{'mt':2,'width':20, 'height':40,'xalign':0,'yalign':0,'ypos':40,'xpos':5}"
			PuPlayer.LabelSet pDMD, "ScorbitQRIcon", "PuPOverlays\\QRcodeB.png",1,"{'mt':2,'width':23, 'height':52,'xalign':0,'yalign':0,'ypos':38,'xpos':3.5,'zback':1}"
		else 
			PuPlayer.LabelSet pDMD, "ScorbitQR", "PuPOverlays\\QRclaim.png",1,"{'mt':2,'width':12, 'height':24,'xalign':0,'yalign':0,'ypos':60,'xpos':5}"
			PuPlayer.LabelSet pDMD, "ScorbitQRIcon", "PuPOverlays\\QRcodeB.png",1,"{'mt':2,'width':14, 'height':32.5,'xalign':0,'yalign':0,'ypos':58,'xpos':4,'zback':1}"
		End if 
	Else 
		PuPlayer.LabelSet pDMD, "ScorbitQR", "PuPOverlays\\clear.png",0,""
		PuPlayer.LabelSet pDMD, "ScorbitQRIcon", "PuPOverlays\\clear.png",0,""
	End if 
End Sub 

Sub ScorbitBuildGameModes()		' Custom function to build the game modes for better stats 
	dim GameModeStr
	if Scorbit.bSessionActive=False then Exit Sub 
	GameModeStr="NA{black}"

	Select Case PlayerMode 
		case -1:' No Mode Selected  
		Case 0: ' Last Child
			GameModeStr="NA{cyan}:Last Child"
        Case 1: ' Walk
			GameModeStr="NA{orange}:Walk This Way"
        Case 2: ' Same
			GameModeStr="NA{yellow}:Same Old Song and Dance"
        Case 3: ' Sweet 
			GameModeStr="NA{red}:Sweet Emotion"
        Case 4: ' Dide
			GameModeStr="NA{blue}:Dude Looks Like a Lady"
        Case 5: ' Back
			GameModeStr="NA{purple}:Back in the Saddle"
        Case 6: ' Rats
			GameModeStr="NA{red}:Rats in the Cellar"
        Case 7: ' Toys 
			GameModeStr="MB{green}:Toys in the Attic"
		Case 8: ' Love
			GameModeStr="MB{pink}:Love in an Elevator"
	End Select

	if bPlayerModeSelect=False then ' and bSecondMode then 
		Select Case PlayerMode2
			case -1:' No Mode Selected
			Case 0: ' Last Child
				GameModeStr="NA{cyan}:Last Child 2nd"
			Case 1: ' Walk
				GameModeStr="NA{orange}:Walk This Way 2nd"
			Case 2: ' Same
				GameModeStr="NA{red}:Same Old Song and Dance 2nd"
			Case 3: ' Sweet 
				GameModeStr="NA{blue}:Sweet Emotion 2nd"
			Case 4: ' Dide
				GameModeStr="NA{red}:Dude Looks Like a Lady 2nd"
			Case 5: ' Back
				GameModeStr="NA{purple}:Back in the Saddle 2nd"
			Case 6: ' Rats
				GameModeStr="NA{red}:Rats in the Cellar 2nd"
			Case 7: ' Toys 
				GameModeStr="NA{green}:Toys in the Attic 2nd"
			Case 8: ' Love
				GameModeStr="NA{pink}:Love in an Elevator 2nd"
		End Select
	End if 

	if bToyBoxMultiball then
		GameModeStr="MB{green}:Toy in the Attic"
	Elseif tmrMedleyTour.Enabled=True then
		GameModeStr="MB{white}:Medley Tour MB"
	Elseif tmrFinalTour.Enabled=True then
		GameModeStr="MB{white}:Final Tour MB"
	End If

D "Mode:" & GameModeStr & "Mode:" & PlayerMode & "Mode2:" & PlayerMode2 & " MB:" & tmrMedleyTour.Enabled
	Scorbit.SetGameMode(GameModeStr)

End Sub 

Sub Scorbit_LOGUpload(state)	' Callback during the log creation process.  0=Creating Log, 1=Uploading Log, 2=Done 
	Select Case state 
		case 0:
			Debug.print "CREATING LOG"
		case 1:
			Debug.print "Uploading LOG"
		case 2:
			Debug.print "LOG Complete"
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
		elseif bRunAsynch then ' Game in play
			Scorbit.SendUpdate Score(0), Score(1), Score(2), Score(3), Balls, CurrentPlayer+1, PlayersPlayingGame
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
				Debug.print "Using MAC Addresses:" & Nad.MACAddress & " From Adapter:" & Nad.description   
				MyMac=replace(Nad.MACAddress, ":", "")
				Exit For 
			End if 
		Next
        Serial=eval("&H" & mid(MyMac, 5))
        if Serial<0 then Serial=eval("&H" & mid(MyMac, 6))        ' Mac Address Overflow Special Case 
        if MyMachineID<>2108 then             ' GOTG did it wrong but MachineID should be added to serial number also
            Serial=Serial+MyMachineID
        End if 
'        Serial=123456
        Debug.print "Serial:" & Serial

		' Get System UUID
		set Nads = objService.ExecQuery("SELECT * FROM Win32_ComputerSystemProduct")
		for each Nad in Nads
			debug.print "Using UUID:" & Nad.UUID   
			MyUUID=Nad.UUID
			Exit For 
		Next

		if MyUUID="" then 
			debug.pring "SCORBIT - Can.t get UUID, Disabling."
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
		MyUUID=LCASE(Hex(eval("&h" & UUIDParts(0))+MyMachineID) & UUIDParts(1) &  UUIDParts(2) &  UUIDParts(3) & UUIDParts(4))         ' Add MachineID to UUID
		MyUUID=LPad(MyUUID, 32, "0")
'		MyUUID=Replace(MyUUID, "-",  "")
'		Debug.print "MyUUID:" & MyUUID 


' Debug
'		myUUID="adc12b19a3504453a7414e722f58737f"
'		Serial="123456778"

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
'			Debug.print "CALLBACK: " & objXmlHttpMain.Status & " " & objXmlHttpMain.readystate
			if objXmlHttpMain.Status=200 and objXmlHttpMain.readystate = 4 then 
				ResponseStr=objXmlHttpMain.responseText
				'Debug.print "RESPONSE: " & ResponseStr

				' Parse Name 
				if CachedPlayerNames(SaveCurrentPlayer-1)="" then  ' Player doesnt have a name
					if instr(1, ResponseStr, "cached_display_name") <> 0 Then	' There are names in the result
						Parts=Split(ResponseStr,",{")							' split it 
						if ubound(Parts)>=SaveCurrentPlayer-1 then 				' Make sure they are enough avail
							if instr(1, Parts(SaveCurrentPlayer-1), "cached_display_name")<>0 then 	' See if mine has a name 
								CachedPlayerNames(SaveCurrentPlayer-1)=GetJSONValue(Parts(SaveCurrentPlayer-1), "cached_display_name")		' Get my name
								CachedPlayerNames(SaveCurrentPlayer-1)=Replace(CachedPlayerNames(SaveCurrentPlayer-1), """", "")
								Scorbit_PlayerClaimed SaveCurrentPlayer, CachedPlayerNames(SaveCurrentPlayer-1)
'								Debug.print "Player Claim:" & SaveCurrentPlayer & " " & CachedPlayerNames(SaveCurrentPlayer-1)
							End if 
						End if
					End if 
				else												    ' Check for unclaim 
					if instr(1, ResponseStr, """player"":null")<>0 Then	' Someone doesnt have a name
						Parts=Split(ResponseStr,"[")						' split it 
'Debug.print "Parts:" & Parts(1)
						Parts2=Split(Parts(1),"}")							' split it 
						for i = 0 to Ubound(Parts2)
'Debug.print "Parts2:" & Parts2(i)
							if instr(1, Parts2(i), """player"":null")<>0 Then
								CachedPlayerNames(i)=""
							End if 
						Next 
					End if 
				End if
			End if 
			bWaitResp=False
		End if 
	End Sub

	Public Sub StartSession()
		if bEnabled=False then Exit Sub 
Debug.print "Scorbit Start Session" 
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

	Public Sub StopSession(P1Score, P2Score, P3Score, P4Score, NumberPlayers)
		StopSession2 P1Score, P2Score, P3Score, P4Score, NumberPlayers, False
	End Sub 

	Public Sub StopSession2(P1Score, P2Score, P3Score, P4Score, NumberPlayers, bCancel)
		Dim i
		dim objFile
		if bEnabled=False then Exit Sub 
		if bSessionActive=False then Exit Sub 
Debug.print "Scorbit Stop Session" 

		bRunAsynch=False 
		bActive="false" 
		SendUpdate P1Score, P2Score, P3Score, P4Score, -1, -1, NumberPlayers
		bSessionActive=False
'		SendHeartbeat

		if bUploadLog and LogIdx<>0 and bCancel=False then 
			Debug.print "Creating Scorbit Log: Size" & LogIdx
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

	Public Sub SendUpdate(P1Score, P2Score, P3Score, P4Score, CurrentBall, CurrentPlayer, NumberPlayers)
		SendUpdateAsynch P1Score, P2Score, P3Score, P4Score, CurrentBall, CurrentPlayer, NumberPlayers, bRunAsynch
	End Sub 

	Public Sub SendUpdateAsynch(P1Score, P2Score, P3Score, P4Score, CurrentBall, CurrentPlayer, NumberPlayers, bAsynch)
		dim i
		Dim PostData
		Dim resultStr
		dim LogScores(4)

		if bUploadLog then 
			if NumberPlayers>=1 then LogScores(0)=P1Score
			if NumberPlayers>=2 then LogScores(1)=P2Score
			if NumberPlayers>=3 then LogScores(2)=P3Score
			if NumberPlayers>=4 then LogScores(3)=P4Score
			LOGFILE(LogIdx)=DateDiff("S", "1/1/1970", Now()) & "," & LogScores(0) & "," & LogScores(1) & "," & LogScores(2) & "," & LogScores(3) & ",,," &  CurrentPlayer & "," & CurrentBall & ",""" & GameModeOrig & """"
			LogIdx=LogIdx+1
		End if
 
		if bSessionActive=False then Exit Sub
		if bEnabled=False then Exit Sub 
		if bWaitResp then exit sub ' Drop message until we get our next response 

		SaveCurrentPlayer=CurrentPlayer
'		PostData = "session_uuid=" & SessionUUID & "&session_time=" & DateDiff("S", "1/1/1970", Now()) & _
'					"&session_sequence=" & SessionSeq & "&active=" & bActive
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

			PostData = PostData & "&current_ball=" & CurrentBall & "&current_player=" & CurrentPlayer
			if GameMode<>"" then PostData=PostData & "&game_modes=" & GameMode
		End if 
		resultStr = PostMsg("https://" & domain, "/api/entry/", PostData, bAsynch)
		if resultStr<>"" then Debug.print "SendUpdate Resp:" & resultStr
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

'		Set wsh = CreateObject("WScript.Shell")
'		Set fso = CreateObject("Scripting.FileSystemObject")
'
'		dim rc
'		dim result
'		dim objFileToRead
'		Dim sessionID:sessionID=puplayer.getroot&"\" & cGameName & "\sessionID.txt"
'
'		on error resume next
'		fso.DeleteFile(sessionID)
'		On error goto 0 
'
'		rc = wsh.Run("powershell -Command ""(New-Guid).Guid"" | out-file -encoding ascii " & sessionID, 0, True)
'		if FileExists(sessionID) and rc=0 then
'			Set objFileToRead = fso.OpenTextFile(sessionID,1)
'			result = objFileToRead.ReadLine()
'			objFileToRead.Close
'			GUID=result
'		else 
'			MsgBox "Cant Create SessionUUID through powershell. Disabling Scorbit"
'			bEnabled=False 
'		End if

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
		dim TmpStr
		Dim Command
		Dim rc
		Dim QRFile:QRFile=puplayer.getroot&"\" & cGameName & "\" & dirQrCode
		if bEnabled=False then Exit Sub 
		resultStr = GetMsgHdr("https://" & domain, "/api/heartbeat/", "Authorization", "SToken " & sToken)
		'Debug.print "Heartbeat Resp:" & resultStr
		If VenueMachineID="" then 

			if resultStr<>"" and Instr(resultStr, """unpaired"":true")=0 then 	' We Paired
				bNeedsPairing=False
				Debug.print "Paired"
				Scorbit_Paired()
			else 
				bNeedsPairing=True
			End if 

			TmpStr=GetJSONValue(resultStr, "venuemachine_id")
			if TmpStr<>"" then 
				VenueMachineID=TmpStr
'Debug.print "VenueMachineID=" & VenueMachineID			
				Command = """" & puplayer.getroot&"\" & cGameName & "\sQRCode.exe"" " & VenueMachineID & " " & opdbID & " """ & QRFile & """"
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
		Dim QRFile:QRFile=puplayer.getroot&"\" & cGameName & "\" & dirQrCode
		Dim sTokenFile:sTokenFile=puplayer.getroot&"\" & cGameName & "\sToken.dat"

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

		Command = """" & puplayer.getroot&"\" & cGameName & "\sToken.exe"" " & tmpUUID & " " & tmpVendor & " " &  tmpSerial & " " & MachineID & " """ & QRFile & """ """ & sTokenFile & """ " & domain
		debug.print "RUNNING Command:" & Command
		rc = wsh.Run(Command, windowStyle, waitOnReturn)
		debug.print "Return:" & rc
		if FileExists(puplayer.getroot&"\" & cGameName & "\sToken.dat") and rc=0 then
			Set objFileToRead = fso.OpenTextFile(puplayer.getroot&"\" & cGameName & "\sToken.dat",1)
			result = objFileToRead.ReadLine()
			objFileToRead.Close
			Set objFileToRead = Nothing
			debug.print  result

			if Instr(1, result, "Invalid timestamp")<> 0 then 
				MsgBox "Scorbit Timestamp Error: Please make sure the time on your system is exact"
				getStoken=False
			elseif Instr(1, result, ":")<>0 then 
				results=split(result, ":")
				sToken=results(1)
				sToken=mid(sToken, 3, len(sToken)-4)
debug.print "Got TOKEN:" & sToken
				getStoken=True
			Else 
debug.print  "ERROR:" & result
				getStoken=False
			End if 
		else 
debug.print "ERROR No File:" & rc
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
'Debug.print "Url:" & Url  & "  Async=" & bRunAsynch
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
				debug.print "Server error: (" & err.number & ") " & Err.Description
			End if 
			if bRunAsynch=False then 
Debug.print "Status: " & objXmlHttpMain.status
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
'debug.print "PostMSg:" & Url & " " & PostData

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
				'debug.print "Multiplayer Server error (" & err.number & ") " & Err.Description
			End if 
			If objXmlHttpMain.status = 200 Then
				PostMsg = objXmlHttpMain.responseText
			else 
				PostMsg="ERROR: " & objXmlHttpMain.status & " >" & objXmlHttpMain.responseText & "<"
			End if 
		On error goto 0
	End Function

	Private Function pvPostFile(sUrl, sFileName, bAsync)
'Debug.print "Posting File " & sUrl & " " & sFileName & " " & bAsync & " File:" & Mid(sFileName, InStrRev(sFileName, "\") + 1)
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
'		fso.Open sFileName For Binary Access Read As nFile
'		If LOF(nFile) > 0 Then
'			ReDim baBuffer(0 To LOF(nFile) - 1) As Byte
'			Get nFile, , baBuffer
'			sPostData = StrConv(baBuffer, vbUnicode)
'		End If
'		Close nFile

		'--- prepare body
		sPostData = "--" & STR_BOUNDARY & vbCrLf & _
			"Content-Disposition: form-data; name=""uuid""" & vbCrLf & vbCrLf & _
			SessionUUID & vbcrlf & _
			"--" & STR_BOUNDARY & vbCrLf & _
			"Content-Disposition: form-data; name=""log_file""; filename=""" & SessionUUID & ".csv""" & vbCrLf & _
			"Content-Type: application/octet-stream" & vbCrLf & vbCrLf & _
			sPostData & vbCrLf & _
			"--" & STR_BOUNDARY & "--"

'Debug.print "POSTDATA: " & sPostData & vbcrlf

		'--- post
		With objXmlHttpMain
			.Open "POST", sUrl, bAsync
			.SetRequestHeader "Content-Type", "multipart/form-data; boundary=" & STR_BOUNDARY
			.SetRequestHeader "Authorization", "SToken " & sToken
			.Send sPostData ' pvToByteArray(sPostData)
			If Not bAsync Then
				Response= .ResponseText
				pvPostFile = Response
'Debug.print "Upload Response: " & Response
			End If
		End With

	End Function

	Private Function pvToByteArray(sText)
		pvToByteArray = StrConv(sText, 128)		' vbFromUnicode
	End Function

End Class 
'  END SCORBIT 
'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

'Extra VR Stuff
'***************************************************************************
'Beer Bubble Code - Rawd
'***************************************************************************
Sub BeerTimer_Timer()

	Randomize(21)
	BeerBubble1.z = BeerBubble1.z + Rnd(1)*0.5
	if BeerBubble1.z > -771 then BeerBubble1.z = -955
	BeerBubble2.z = BeerBubble2.z + Rnd(1)*1
	if BeerBubble2.z > -768 then BeerBubble2.z = -955
	BeerBubble3.z = BeerBubble3.z + Rnd(1)*1
	if BeerBubble3.z > -768 then BeerBubble3.z = -955
	BeerBubble4.z = BeerBubble4.z + Rnd(1)*0.75
	if BeerBubble4.z > -774 then BeerBubble4.z = -955
	BeerBubble5.z = BeerBubble5.z + Rnd(1)*1
	if BeerBubble5.z > -771 then BeerBubble5.z = -955
	BeerBubble6.z = BeerBubble6.z + Rnd(1)*1
	if BeerBubble6.z > -774 then BeerBubble6.z = -955
	BeerBubble7.z = BeerBubble7.z + Rnd(1)*0.8
	if BeerBubble7.z > -768 then BeerBubble7.z = -955
	BeerBubble8.z = BeerBubble8.z + Rnd(1)*1
	if BeerBubble8.z > -771 then BeerBubble8.z = -955
End Sub


'***************************************************************************
'VR Clock code below - THANKS RASCAL
'***************************************************************************


' VR Clock code below....
Sub ClockTimer_Timer()
	dim CurrentMinute: Currentminute = Minute(Now())
	VRClockMinutes.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6
	VRClockhours.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2)
    VRClockseconds.RotAndTra2 = (Second(Now()))*6
	CurrentMinute=Minute(Now())
End Sub

'***************************************************************************
' VR Plunger Animataion Code
'***************************************************************************

Sub TimerVRPlunger_Timer
	if Pincab_PLungerrod.Y < (Plunger.y - 1810) then Pincab_PLungerrod.Y = Pincab_PLungerrod.y +12
	if Pincab_PLungerKnob.Y < (Plunger.y - 1810) then Pincab_PLungerKnob.Y = Pincab_PLungerKnob.y +12
End Sub

Sub TimerVRPlunger2_Timer
	Pincab_PLungerrod.Y = Plunger.y -1900 + (5* Plunger.Position) 
	Pincab_PLungerKnob.Y = Plunger.y -1900 + (5* Plunger.Position) 
end sub


sub Pincab_TimserStartButton_timer
	if Credits =0 Then PinCab_Start_Button_Inner_Ring.DisableLighting=False : Pincab_TimserStartButton.enabled=False : exit sub
	' if Pincab_StartButton.DisableLighting=1 then  Pincab_StartButton.DisableLighting=0 else  Pincab_StartButton.DisableLighting=1
	if PinCab_Start_Button_Inner_Ring.DisableLighting=True then  PinCab_Start_Button_Inner_Ring.DisableLighting=False else  PinCab_Start_Button_Inner_Ring.DisableLighting=True
end Sub