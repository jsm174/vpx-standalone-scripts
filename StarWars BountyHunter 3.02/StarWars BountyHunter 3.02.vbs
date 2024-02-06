'*
'*	Star Wars BountyHunter 3.0 (2021-22) by tartzani
'*
'*	Optional Downloads if you want FlexDMD '***' Table works fine without it, but you wantz it i hear ? 
'*	VR-room must have FlexDMD !
'*
'*	Get the latest at: And Enable FlexDMD below in TableOptions
'*	https://github.com/vbousquet/flexdmd   atm 1.8.0.0 is the one you want
'*	https://github.com/freezy/dmd-extensions

'*	And move the BountyHunterDMD folder ->  /Visual Pinball/Tables/    ( along with b2s and vpx files )

'*	Music folder:	Move "BountyHunter" Folder -> /Visual Pinball/Music/  '	Add your own music to the list below from /Visual Pinball/Music/

'*	Errors in saved data ? Or if you want to reset Highscores ?
'*  --- Delete these files : "Visual Pinball/user/Bountyhunter.txt" and/or "Visual Pinball/user/BountyhunterTM.txt" for tournament play

Option Explicit
Randomize
Const BallSize = 50 : Const BallMass = 1.0
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0
Dim MiddleDisplay , FlexDMD , DTScore , ForceBackScreen

'**** Table Options **** 
Const VolumeDial = 0.5				' 0-1 : Mechanicalsounds Volume
Const MusicVolumeDial = 0.5	 		' 0-1 : Background Music Volume
Const BackGlassVolumeDial = 0.3 	' 0-1 : Callout Sound Volume
Const osbactive=			0		' Orbital Scoreboard ( google it ) - set to 0 for off, 1 for on. You must make your own "osb.vbs" for this to work !
Const TournamentMode=		False	' False/True : Enabled = NO extra balls, lower ballsave . lower centerpin, tournament mode disables Orbital Scoreboard !
Const VRroom=				0		' 1=on 0=off : Need flex DMD !! will override some other settings !
FlexDMD=					0		' 1=on 0=off : See instructions over for FLEX and Freezy
MiddleDisplay=				1		' 1=on 0=off ( 2=FlexDMD on Playfield: Will override FlexDMD setting )
ForceBackScreen=			0		' 1=on 0=off : dB2S
DTScore=					1		' 1=On 0=Auto: show desktop score reels 
'* Game too heavy ? You can turn off stuff here : 	
Const DynamicBallShadowsOn= 1		' 1=on 0=no dynamic ball shadow ("triangles" near slings and such)
Const MissionBase=			1		' 1=On 0=Off
Const SpaceStation=			1		' 1=On 0=Off
Const Slingshotfighters=	1		' 1=On 0=Off
Const LeftRampRazer=		1		' 1=On 0=Off
Const RightRampFighter=		1		' 1=On 0=Off
Const VRroomBlink=			True	' VR only. Set to False to disable blinking front of the room

'* Ball Config
Table1.BallPlayfieldReflectionScale=	20		' 0/alot  ! Default 20 ! More = it will glow more ! try 500 or 5000  !  50000+ is power to the ball !!
Const SelectBalls =						20		' 0/22  (22 = Metalball) 
'*	21=RANDOM2 same image on all balls on PF, new when last goes out
'*	20=RANDOM each and every Ball ( default, saves locked Balls )  
'*	0-19= ONLYONE 0fennec 1luke 2bobacoin 3razercrest 4helmet 5baby 6gina 7client 8mando 9kuiil 10vanth 11mythrol 12gideon 13ahsoka 14mayfield 15bokatan 16armorer 17greef 18ig11 19specialblinker
Const DesktopBackground=	1		' 1=On 0=Off Side flashers in Desktop mode.
Const DTbackscreenmoving=	1		' 1=On 0=Off Side flashers moving in Desktop mode.
'*******  MUSIC
Dim MusicFile(20)
Dim PlayVolume(20)
	MusicFile(1) ="BountyHunter/Mandalorian_Official_Trailer_2.mp3" : PlayVolume(1) =0.7
	MusicFile(2) ="BountyHunter/Razor_Crest_Crash_Landing.mp3"		: PlayVolume(2) =0.7
'*	Link Your Mp3's under here
	MusicFile(3) ="BountyHunter/Boba_Fett_Theme.mp3" 				: PlayVolume(4) =0.15
	MusicFile(4) ="BountyHunter/Duel_Of_The_Fates.mp3"				: PlayVolume(3) =0.15
	MusicFile(5) ="Empty"							 				: PlayVolume(5) =0.15
	MusicFile(6) ="Empty" 											: PlayVolume(6) =0.15
	MusicFile(7) ="Empty" 											: PlayVolume(7) =0.15
	MusicFile(8) ="Empty" 											: PlayVolume(8) =0.15
	MusicFile(9) ="Empty"											: PlayVolume(9) =0.15
'*	until here^^
	MusicFile(10)="BountyHunter/StarWarsIntro.mp3"					: PlayVolume(10)=0.7	
	MusicFile(11)="BountyHunter/startup.mp3"						: PlayVolume(11)=0.7	
Dim LastMusic 

'*	Put Your MP3 files in the "Visual Pinball/Music/BountyHunter/"  folder and and put path in the list above.  3-9 ingame
'*	Can be any mp3 file path that already be in "Visual Pinball/Music/"
'*	Leave unused as "Empty"
'* 
'*	Starwars music fan ?  visit  "https://www.youtube.com/c/samuelkimmusic/featured" and bookmark that one :)
'*
'*	Linked under is what i use in that order from 3-9
'*
'*	3-Star Wars: Boba Fett Theme | Epic Mandalorian Version
'*	https://www.youtube.com/watch?v=uq84RuBo2kg
'*
'*	4-Star Wars: Duel of The Fates x Imperial March | EPIC MANDALORIAN VERSION
'*	https://www.youtube.com/watch?v=_px0GkJEvcA
'*
'*	5-Star Wars: Jango Fett Theme | Epic Mandalorian Version (Bounty Hunter Theme)
'*	https://www.youtube.com/watch?v=8VGliugurEI
'*
'*	6-Star Wars: Jedi Temple March x Droid Army March | EPIC MANDALORIAN VERSION
'*	https://www.youtube.com/watch?v=D196H2g7oik
'*
'*	7-The Mandalorian: Moff Gideon Theme | EPIC IMPERIAL VERSION
'*	https://www.youtube.com/watch?v=gt8CLmRJSJk
'*
'*	8-Star Wars: The Dark Side March (Imperial March, Droid Army March, Jedi Temple March & MORE)
'*	https://www.youtube.com/watch?v=FKOA6315xBE
'*
'*	9-Star Wars: The Mandalorian Theme | EPIC VERSION'*
'*	https://www.youtube.com/watch?v=tt1EqFY_uqk
'*
'*	need help go google "youtube mp3"
'*
'*	All can be accessed anytime with left/right mangnasave 
'*

'**** DOF Events  :  SOMETHING MISSING ? Let me know and ill fix and update  
'*DOF method for non rom controller tables by Arngrim****************
'*******Use DOF 1**, 1 to activate a ledwiz output*******************
'*******Use DOF 1**, 0 to deactivate a ledwiz output*****************
'*******Use DOF 1**, 2 to pulse a ledwiz output**********************
'E101 0/1 LeftFlipper
'E102 0/1 RightFlipper
'E103 2 Leftslingshot
'E104 2 Rightslingshot
'E105 2 top bumper left ( blue )
'E106 2 top bumper center ( red )
'E107 2 top bumper right ( yellow )
'E111 2 Knocker
'E112 0/1 Shaker  ( fighters moving )
'E113 0/1 Gear Motor ( tie turning )
'E114 2 Red Flasher
'E115 2 Green Flasher
'E116 2 Blue Flasher
'E117 2 White Flashers  many
'E118 0/1 fan ( boss active)
'E119 2 strobes
'E120 0/1 Red undercab
'E121 0/1 Yellow undercab
'E122 0/1 Green undercab
'E123 0/1 Blue undercab
'E124 2 Bell ( yellow DT hit )
'E129 2 startgame
'E130 2 moving boss hit
'E131 2 moving boss down
'E132 2 smal redhit
'E133 2 blue DT hit
'E134 2 top middle droptarget x2 
'E135 2 blueflasher mid left
'E136 2 Redflasher mid left
'E137 2 Yellowflasher mid right
'E138 2 kicker blast Left
'E139 2 kicker blast Right
'E140 2 kicker blast Top
'E141 2 big boss Hit
'E142 2 big boss Down
'E143 2 finalboss Rampscore
'E144 2 kickback fire
'E145 2 ballrelease kicks out new ball - new
'E146 2 Big RED
'E202 2 plungerlane STARWARS starts
'E203 2 LeftRamp 
'E204 2 RightRamp 
'E205 2 Jackpot
'E208 0/1 Ball in front of plunger
'E209 2 Balllaunced
'E210 0/1 Multiball active
'E212 2 nocredits 
'E213 2 add credits
'E214 2 drain_hit
'E215 2 extraball awarded
'E217 2 awarded skillshot
'E218 2 Left Spinner 
'E221 2 Right spinner
'E219 2 ballsaved
'E220 2 quickmultiballstart
'E224 2 doublescoring
'E225 2 tripplescoring


'*****************
'*	Startup
'***************** 
Const cGameName = "starwarsbh" : Const cGameVersion = "V3002"
Const cHighscoreVersion = "HS09" ' scoring changed so need reset !
Dim OSDversion : OSDversion = "3.02"

	Dim Ballsave
	if osbactive = 1 or osbactive = 2 Then
		On Error Resume Next
		ExecuteGlobal GetTextFile("osb.vbs")
	end if
	If TournamentMode Then
		LiReplay.timerinterval = 4400
		Primitive060.visible = True
		Primitive013.visible=False
		Rubber005.visible=False
		Rubber005.collidable=False
		Rubber022.visible = True
		Rubber022.collidable = True
		Flasher011.visible = True		' apron tournament mode text
	End If
	If MiddleDisplay = 2 Then FlexDMD=1
	If VRroom Then
		FlexDMD=1
		ForceBackScreen = 0
		DTScore = 0
		If MiddleDisplay=2 Then MiddleDisplay=1
	End If

	Dim SHOWDTREELS
	Dim Chars(255),Digits2,CrazyBalls,Playlist
	Dim DTScoreReel,SelectBall,spinnerstatus
	DTScoreReel=DTScore
	If flexDMD=0 Then DTscoreReel=1
	DisplyInit
	Dim Controller

	If ForceBackScreen=1 Then HideDesktop=1 : startcontroller

	If VRroom = 1 Then
		For each i in vrcab : i.visible = True : Next 
		Dim X : X=2
		For each i in bg_letters : i.x=x : x=x+89.5 : i.y=228 : Next
		bg_letters013.x=23
		bg_letters013.y=233
		bg_letters014.x=926
		bg_letters014.y=233
		BG_letters015.visible=1
	Else
		For each i in vrcab : i.visible = False : Next 
		If Table1.ShowDT = true Then 
			HideDesktop=0
			If ForceBackScreen=1 Then HideDesktop=1 
			If DesktopBackground=1 Then
				Flasher004.visible=true
				Flasher005.visible=true
			End If
			Primary_LockDownBar.visible = True
		Else
			Primary_LockDownBar.visible = False
			HideDesktop=1
			If ForceBackScreen=0 Then startcontroller
			controller.B2SSetData 13,1
			controller.B2SSetData 40,1

			'hide lights and ball in play used in DT view
		 	'hide unused reels
			EMBallinPlay.visible=0
			DTScorereelOff

		End If
	End If

Sub startcontroller
	Set Controller = CreateObject("B2S.Server") 
	Controller.B2SName = "starwarsbh"
	Controller.Run
	B2SOn = True
End Sub





Sub Wall031_Hit : activeball.velx = activeball.velx/4 : activeball.vely = 0 : activeball.velz = activeball.velz/3 : End Sub
Sub Wall039_Hit : activeball.velx = activeball.velx=0 : activeball.vely = activeball.vely/5 : activeball.velz = activeball.velz/3 : End Sub
Sub Wall030_Hit : activeball.velx = activeball.velx/2 : activeball.vely = 1 : activeball.velz = 0 : debug.print "Wall030_hit" : End Sub


Dim StopthingsRestartmissions
Dim PlayersPlaying, CurrentPlayer
Dim PlayerSaved(4,80)
Dim TimeToAddPlayers ' realtimer 12ms
	RampsTotal=RampsTotal+1
Sub ResetPlayerSaved ' before gamestarts everything zero
		TimeToAddPlayers = 122 ' 1,5 seconds blocked adding players
		PlayersPlaying = 1
		CurrentPlayer = 1
		for i = 0 to 80
			PlayerSaved(1,i)=0
			PlayerSaved(1,i)=0
			PlayerSaved(1,i)=0
			PlayerSaved(1,i)=0
		Next
End Sub



Dim BigDT_ReloadPos: BigDT_ReloadPos=100
Sub LoadPlayerPos(player)

	ComboTotalRamps = PlayerSaved(player,57)
	RampsTotal = PlayerSaved(player,58)  
	DroptargetsCounter = PlayerSaved(player,59)
	MissionsDoneCounter = PlayerSaved(player,60)
	TotalTurned = PlayerSaved(player,61)
	WizardLevel = PlayerSaved(player,62)
	WizardBonusLevel = PlayerSaved(player,63)

	P1score			= PlayerSaved(player, 0)
	bonusmultiplyer	= PlayerSaved(player, 1)

	If bonusmultiplyer>0 Then BonusX001_Timer
	If bonusmultiplyer>1 Then BonusX002_Timer
	If bonusmultiplyer>2 Then BonusX003_Timer
	If bonusmultiplyer>3 Then BonusX004_Timer
	If bonusmultiplyer>4 Then BonusX005_Timer
	If bonusmultiplyer>5 Then BonusX006_Timer
	If bonusmultiplyer>6 Then BonusX007_Timer
	If bonusmultiplyer>7 Then BonusX008_Timer
	If bonusmultiplyer>8 Then BonusX009_Timer

	BossLevel		= PlayerSaved(player, 2)
	QuickMB			= PlayerSaved(player, 3)

	If BossLevel > 0 Then Boss1.state=1
	If bosslevel > 1 Then Boss2.state=1
	If bosslevel > 2 Then Boss3.state=1
	If bosslevel > 3 Then Boss4.state=1
	If bosslevel > 4 Then Boss5.state=1
	If bosslevel > 5 Then Boss6.state=1
	If bosslevel > 6 Then Boss7.state=1

	bossactive			= PlayerSaved(player, 5)
	BigDThitsCounter	= PlayerSaved(player, 6)
	EvasiveDestroyd	= PlayerSaved(player, 7)

	If bossactive = 1 Then									' reenable bosses for each player
		If bosslevel = 1 And BossActive=1 Then Boss1.state=2 : PlaySound "youhavesomething" ,0,0.7*BackGlassVolumeDial
		If bosslevel = 2 And BossActive=1 Then Boss2.state=2 : PlaySound "MudhornStart" ,0 ,0.7 * BackGlassVolumeDial
		If bosslevel = 3 And BossActive=1 Then Boss3.state=2 : PlaySound "youhavesomething" ,0,0.7*BackGlassVolumeDial
		If bosslevel = 4 And BossActive=1 Then Boss4.state=2 : PlaySound "SpiderStart"  ,0 ,0.7 * BackGlassVolumeDial
		If bosslevel = 5 And BossActive=1 Then Boss5.state=2 : PlaySound "youhavesomething" ,0,0.7*BackGlassVolumeDial
		If bosslevel = 6 And BossActive=1 Then Boss6.state=2 : PlaySound "DragonStart"  ,0 ,0.7 * BackGlassVolumeDial
'		If bosslevel = 7 And BossActive=1 Then Boss7.state=2

		bossdelayblinker.enabled=1
		If bosslevel=2 or BossLevel=4 Or BossLevel=5 Then BigDT_ReloadPos = PlayerSaved(player,64)



	End If




	Lockedballs 		= PlayerSaved(player, 8)
	If LockedBalls>0 Then LOCKED003.state=1
	If LockedBalls>1 Then LOCKED002.state=1

	LockedID1 			= PlayerSaved(player, 9)
	LockedID2 			= PlayerSaved(player,10)
	mission1after1onlyonce=PlayerSaved(player,11)
	Missions4Evasive	= PlayerSaved(player,12)
	TotalExtraBalls		= PlayerSaved(player,13)
	MissionsDoneCounter = PlayerSaved(player,14)
	MissionsDoneThisBall = PlayerSaved(player,15)
	extraballlight.state = PlayerSaved(player,16)
	knockeronce			= PlayerSaved(player,17)
	If PlayerSaved(player,18) > 0 Then JackPotScoring = PlayerSaved(player,18)
	If PlayerSaved(player,19) > 0 Then SkillShotScoring = PlayerSaved(player,19)
	If PlayerSaved(player,20) > 0 Then letterscoring = PlayerSaved(player,20)
	If PlayerSaved(player,21) > 0 Then MaxBallsScoring = PlayerSaved(player,21)
'	If PlayerSaved(player,22) > 0 Then ScoringRightRamp = PlayerSaved(player,22)
'	If PlayerSaved(player,23) > 0 Then ScoringLeftRamp = PlayerSaved(player,23)
'	If PlayerSaved(player,24) > 0 Then SecretScoring = PlayerSaved(player,24)
	If PlayerSaved(player,25) > 0 Then HunterScoring = PlayerSaved(player,25)
	If PlayerSaved(player,26) > 0 Then BountyScoring = PlayerSaved(player,26)
	If PlayerSaved(player,27) > 0 Then BountyCompleteScoring = PlayerSaved(player,27)
	If PlayerSaved(player,28) > 0 Then HunterCompleteScoring = PlayerSaved(player,27)
	If PlayerSaved(player,29) > 0 Then SpinnerScoring = PlayerSaved(player,29)
	If PlayerSaved(player,30) > 0 Then BumperScoring = PlayerSaved(player,30)
	If PlayerSaved(player,31) > 0 Then DroptargetsScoring = PlayerSaved(player,31)
	If PlayerSaved(player,32) > 0 Then enterlightscoring = PlayerSaved(player,32)
	If PlayerSaved(player,33) > 0 Then mysteryscoring = PlayerSaved(player,33)
	If PlayerSaved(player,34) > 0 Then mission6scoring = PlayerSaved(player,34)
	If PlayerSaved(player,35) > 0 Then mission5scoring = PlayerSaved(player,35)
	If PlayerSaved(player,36) > 0 Then mission4scoring = PlayerSaved(player,36)
	If PlayerSaved(player,37) > 0 Then mission3scoring = PlayerSaved(player,37)
	If PlayerSaved(player,38) > 0 Then mission2scoring = PlayerSaved(player,38)
	If PlayerSaved(player,39) > 0 Then mission1scoring = PlayerSaved(player,39)
	If PlayerSaved(player,40) > 0 Then mission8scoring = PlayerSaved(player,40)
	If PlayerSaved(player,41) > 0 Then mission7scoring = PlayerSaved(player,41)


	StopthingsRestartmissions = True
	If PlayerSaved(player,42) = 1 Then startmission1
	If PlayerSaved(player,43) = 1 Then startmission2
	If PlayerSaved(player,44) = 1 Then startmission3
	If PlayerSaved(player,45) = 1 Then startmission4
	If PlayerSaved(player,46) = 1 Then startmission5
	If PlayerSaved(player,47) = 1 Then startmission6
	If PlayerSaved(player,49) = 1 Then startmission8
	If PlayerSaved(player,48) = 1 Then startmission7


	If Mission(8)=1 Then
		If PlayerSaved(player,50)=0 Then : WallDT7.timerenabled=1 : Else : WallDT7.timerenabled=0 : bobaHIT1=1 : End If
		If PlayerSaved(player,51)=0 Then : WallDT6.timerenabled=1 : Else : WallDT6.timerenabled=0 : bobaHIT2=1 : End If
		If PlayerSaved(player,52)=0 Then : WallDT5.timerenabled=1 : Else : WallDT5.timerenabled=0 : bobaHIT3=1 : End If
		If PlayerSaved(player,53)=0 Then : WallDT4.timerenabled=1 : Else : WallDT4.timerenabled=0 : bobaHIT4=1 : End If
		If PlayerSaved(player,54)=0 Then : WallDT3.timerenabled=1 : Else : WallDT3.timerenabled=0 : bobaHIT5=1 : End If
	End If

	StopthingsRestartmissions = False

	LiSpecialRight.state = PlayerSaved(player,55)
	If PlayerSaved(player,56) = 2 Then
		Lilock2.state = 2
		LiLock1.state = 2
		LOCKED001.state=2
		LiRefuel001.state=2
	End If
	sw51.enabled=1
	sw001.enabled=0
	sw002.enabled=0


'	debug.print "Load : bosslevel" & bosslevel & "   evasivedestroyd" & EvasiveDestroyd
End Sub





Sub SavePlayerPos( player)

'debug.print "SAVE : bosslevel" & bosslevel & "   evasivedestroyd" & EvasiveDestroyd
	PlayerSaved(player,57) = ComboTotalRamps
	PlayerSaved(player,58) = RampsTotal
	PlayerSaved(player,59) = DroptargetsCounter
	PlayerSaved(player,60) = MissionsDoneCounter
	PlayerSaved(player,61) = TotalTurned
	PlayerSaved(player,62) = WizardLevel
	PlayerSaved(player,63) = WizardBonusLevel
	PlayerSaved(player,64) = Primitive059.z
	BigDT_ReloadPos=100

	PlayerSaved(player, 0) = P1score
	PlayerSaved(player, 1) = bonusmultiplyer

bonusx001.state=0
bonusx002.state=0
bonusx003.state=0
bonusx004.state=0
bonusx005.state=0
bonusx006.state=0
bonusx007.state=0
bonusx008.state=0
bonusx009.state=0


	PlayerSaved(player, 2) = BossLevel
	PlayerSaved(player, 3) = QuickMB ' displayletters to reset in load player
'	PlayerSaved(player, 4) = BossLevel
	PlayerSaved(player, 5) = bossactive

	PlayerSaved(player, 6) = BigDThitsCounter
	PlayerSaved(player, 7) = EvasiveDestroyd



BigDThitsCounter=0

Evasivestate=4
RaiseBigDT.enabled=2 : BigDTUD=2 : Wall025.collidable=False : Wall026.collidable=False : PlaySound SoundFX("elengine",DOFContactors), 0, 0.8*volumedial, AudioPan(Primitive059), 0.05,0,0,1,AudioFade(Primitive059)
Boss1ALLOFF
BOSS1.state=0
BOSS2.state=0
BOSS3.state=0
BOSS4.state=0
BOSS5.state=0
BOSS6.state=0
BOSS7.state=0



	PlayerSaved(player, 8) = Lockedballs
lockedballs=0
	PlayerSaved(player, 9) = LockedID1
	PlayerSaved(player,10) = LockedID2
	PlayerSaved(player,11) = mission1after1onlyonce
	PlayerSaved(player,12) = Missions4Evasive
	PlayerSaved(player,13) = TotalExtraBalls
	PlayerSaved(player,14) = MissionsDoneCounter
	PlayerSaved(player,15) = MissionsDoneThisBall
	PlayerSaved(player,16) = extraballlight.state
extraballlight.state=0
	PlayerSaved(player,17) = knockeronce
knockeronce=0


	PlayerSaved(player,18) = JackPotScoring
	PlayerSaved(player,19) = SkillShotScoring
	PlayerSaved(player,20) = letterscoring
	PlayerSaved(player,21) = MaxBallsScoring
'	PlayerSaved(player,22) = ScoringRightRamp
'	PlayerSaved(player,23) = ScoringLeftRamp
'	PlayerSaved(player,24) = SecretScoring
	PlayerSaved(player,25) = HunterScoring
	PlayerSaved(player,26) = BountyScoring
	PlayerSaved(player,27) = BountyCompleteScoring
	PlayerSaved(player,28) = HunterCompleteScoring
	PlayerSaved(player,29) = SpinnerScoring
	PlayerSaved(player,30) = BumperScoring
	PlayerSaved(player,31) = DroptargetsScoring
	PlayerSaved(player,32) = enterlightscoring
	PlayerSaved(player,33) = mysteryscoring
	PlayerSaved(player,34) = mission6scoring
	PlayerSaved(player,35) = mission5scoring
	PlayerSaved(player,36) = mission4scoring
	PlayerSaved(player,37) = mission3scoring
	PlayerSaved(player,38) = mission2scoring
	PlayerSaved(player,39) = mission1scoring
	PlayerSaved(player,40) = mission8scoring
	PlayerSaved(player,41) = mission7scoring
JackPotScoring=500000 ' reset to default ... chack saved if its 0 then dont overwrite these
SkillShotScoring=750000
letterscoring=28000
MaxBallsScoring=2000000
ScoringRightRamp=40000
ScoringLeftRamp=40000
SecretScoring=150000
HunterScoring=2500
BountyScoring=2500
BountyCompleteScoring=20000
HunterCompleteScoring=20000
SpinnerScoring=300
BumperScoring=800
DroptargetsScoring=490
enterlightscoring=14000
mysteryscoring=100000
mission6scoring=200000
mission5scoring=150000
mission4scoring=200000
mission3scoring=125000
mission2scoring=250000
mission1scoring=300000
mission8scoring=500000
mission7scoring=250000




	PlayerSaved(player,42) = Mission(1)
	PlayerSaved(player,43) = Mission(2)
	PlayerSaved(player,44) = Mission(3)
	PlayerSaved(player,45) = Mission(4)
	PlayerSaved(player,46) = Mission(5)
	PlayerSaved(player,47) = Mission(6)
	PlayerSaved(player,48) = Mission(7)
	Mission7resetAll
	PlayerSaved(player,49) = Mission(8)
Mission(1)=0
Mission(2)=0
Mission(3)=0
Mission(4)=0
Mission(5)=0
Mission(6)=0
Mission(7)=0
Mission(8)=0


	PlayerSaved(player,50) = bobaHIT1
	PlayerSaved(player,51) = bobaHIT2
	PlayerSaved(player,52) = bobaHIT3 
	PlayerSaved(player,53) = bobaHIT4 
	PlayerSaved(player,54) = bobaHIT5 
	bobaHIT1=0
	bobaHIT2=0
	bobaHIT3=0
	bobaHIT4=0
	bobaHIT5=0

	Drop_5bank

	Walldt7.timerenabled=0
	Walldt6.timerenabled=0
	Walldt5.timerenabled=0
	Walldt4.timerenabled=0
	Walldt3.timerenabled=0


	PlayerSaved(player,55) = LiSpecialRight.state
	PlayerSaved(player,56) = Lilock2.state
Sw51up.enabled=1 ' up
Sw51down.enabled=0
PlaySound SoundFX("springchange",DOFContactors), 0, .88*volumedial, AudioPan(sw51), 0.05,0,0,1,AudioFade(sw51)
LiRefuel001.state=0




	'Items reset all Balls

	mission1light.state=0
	mission1light001.state=0
	mission1light002.state=0
	mission1light003.state=0
	mission1light004.state=0
	mission1light005.state=0
	mission2light.state=0
	mission2light001.state=0
	mission2light002.state=0
	mission2light003.state=0
	mission2light004.state=0
	mission2light005.state=0
	mission3light.state=0
	mission3light001.state=0
	mission3light002.state=0
	mission3light003.state=0
	mission3light004.state=0
	mission3light005.state=0
	mission4light.state=0
	mission4light001.state=0
	mission4light002.state=0
	mission4light003.state=0
	mission4light004.state=0
	mission4light005.state=0
	mission5light.state=0
	mission5light001.state=0
	mission5light002.state=0
	mission5light003.state=0
	mission5light004.state=0
	mission5light005.state=0
	mission6light.state=0
	mission6light001.state=0
	mission6light002.state=0
	mission6light003.state=0
	mission6light004.state=0
	mission6light005.state=0
	mission7light.state=0
	mission7light001.state=0
	mission7light002.state=0
	mission7light003.state=0
	mission7light004.state=0
	mission7light005.state=0
	mission8light.state=0
	mission8light001.state=0
	mission8light002.state=0
	mission8light003.state=0
	mission8light004.state=0
	mission8light005.state=0


	LiRefuel1.state=0
	Mission7Light.timerenabled=0 



	hunter(1)=0
	hunter(2)=0
	hunter(3)=0
	hunter(4)=0
	hunter(5)=0
	hunter(6)=0
	hunter(7)=0
	hunter(8)=0
	hunter(9)=0
	hunter(10)=0
	hunter(11)=0
	hunter(12)=0
	LiBounty1.state=0
	LiBounty11.state=0
	LiBounty22.state=0
	LiBounty33.state=0
	LiBounty44.state=0
	LiBounty55.state=0
	LiBounty66.state=0
	LiBounty2.state=0
	LiBounty3.state=0
	LiBounty4.state=0
	LiBounty5.state=0
	LiBounty6.state=0
	LiHunter1.state=0
	LiHunter2.state=0
	LiHunter3.state=0
	LiHunter4.state=0
	LiHunter5.state=0
	LiHunter6.state=0

	bonus1=0
	bonus2=0
	bonus3=0
	LiLock2.state=0
	LiLock1.state=0
	boss1.state=0
	boss2.state=0
	boss3.state=0
	boss4.state=0
	boss5.state=0
	boss6.state=0
	boss7.state=0
	Libonus1.state=0
	Libonus2.state=0
	Libonus3.state=0
	Libonus01.state=0
	Libonus02.state=0
	Libonus03.state=0
	LiSpecialRight.state=0
	LiSpecialRight2.state=0
	LiSupply001.state=0
	LiSupply002.state=0
	LiSupply003.state=0
	LiSupply004.state=0
	LiSupply005.state=0
	LiSupply006.state=0
	LiSupply007.state=0
	LiSupply008.state=0
	LiSupply009.state=0
	LiSupply010.state=0
	Licenter001.state=0
	Licenter002.state=0
	Licenter003.state=0
	Licenter004.state=0
	Licenter005.state=0
	Licenter006.state=0
	Licenter007.state=0
	Licenter008.state=0
	Licenter009.state=0
	Licenter010.state=0
	Licenter011.state=0
	Licenter012.state=0
	Licenter015.state=0
	Licenter016.state=0
	Licenter017.state=0
	Licenter018.state=0
	Licenter019.state=0
	Licenter020.state=0
	Licenter021.state=0
	Licenter022.state=0
	Licenter023.state=0
	Licenter024.state=0
	Licenter025.state=0
	Licenter026.state=0
	lisecret2.state=0
	LiSecret3.state=0
	LiDouble.state=0
	LiDouble2.state=0
	LiDouble3.state=0
	LiTripple.state=0
	LiTripple2.state=0
	LiTripple3.state=0
			liTargetTop(1)=0
			liTargetTop(2)=0
			liTargetTop(3)=0
			liTargetTop(4)=0
			liTargetTop(5)=0
			liTargetTop(6)=0
			liTargetTop(7)=0
			liTargetTop(8)=0
			liTargetTop(9)=0
			liTargetTop(10)=0

	Raise_DT 1

	Raise_DT 2

	wormhole=0
	Dt01down=0
	Dt02down=0

	enterlight.timerenabled=0
	enterlight.state=0
	mysterylight.state=0
	extraballlight.state=0

	ClearAllStatus
	Missions4Evasive=0
	ButtonState=2
	mission1after1onlyonce=0
	LOCKED001.state=0
	LOCKED002.state=0
	LOCKED003.state=0
	TotalExtraBalls=0
	P1scorebonus=1


	bonusx001.timerenabled=0 : bonusx001.state=0 ': bonusx001.intensity=80
	bonusx002.timerenabled=0 : bonusx002.state=0 ': bonusx002.intensity=80
	bonusx003.timerenabled=0 : bonusx003.state=0 ': bonusx003.intensity=80
	bonusx004.timerenabled=0 : bonusx004.state=0 ': bonusx004.intensity=80
	bonusx005.timerenabled=0 : bonusx005.state=0 ': bonusx005.intensity=80
	bonusx006.timerenabled=0 : bonusx006.state=0 ': bonusx006.intensity=80
	bonusx007.timerenabled=0 : bonusx007.state=0 ': bonusx007.intensity=80
	bonusx008.timerenabled=0 : bonusx008.state=0 ': bonusx008.intensity=80
	bonusx009.timerenabled=0 : bonusx009.state=0 ': bonusx009.intensity=80

End Sub






Dim statisonturning,stationwobbly,stationwobblyonly,stationonlycounter
Sub TurningShip
	If stationonlycounter >0 Then
		stationonlycounter=stationonlycounter-1
		stationwobbly=stationwobbly+3
		If stationwobbly>719 Then stationwobbly=0
		If stationwobbly<360 Then
			Primitive047.rotZ=stationwobbly/14-13
		Else
			Primitive047.rotZ=(720-stationwobbly)/14-13
		End If
	Else
		If attractrefuel>0 Then attractrefuel=attractrefuel-1
		statisonturning=statisonturning+1
		stationwobbly=stationwobbly+3
		If stationwobbly>719 Then stationwobbly=0
		If statisonturning=720 Then statisonturning=0
		Primitive047.rotY=statisonturning/2
	
		If stationwobbly<360 Then
			Primitive047.rotZ=stationwobbly/14-13
		Else
			Primitive047.rotZ=(720-stationwobbly)/14-13
		End If
	End If
End Sub



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

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.001 * Csng(BallVel(ball) ^3)
End Function

Const RampRollVolume = 0.34 			'Level of ramp rolling volume. Value between 0 and 1
Dim RollingSoundFactor : RollingSoundFactor = 0.2


Sub RampRollUpdate()		'Timer update
	dim x : for x = 1 to uBound(RampBalls)
		if Not IsEmpty(RampBalls(x,1) ) then 
			if BallVel(RampBalls(x,0) ) > 1 then ' if ball is moving, play rolling sound
			If RampType(x) = 1 then 
					PlaySound("RampLoop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial*0.4, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0))/2, 1, 0, AudioFade(RampBalls(x,0))	
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial*8, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0))*12, 1, 0, AudioFade(RampBalls(x,0))
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

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7

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
	TargetBouncer activeball, 1.27
End Sub

Sub zCol_Rubber_Corner_002_hit
	TargetBouncer activeball, 1
End Sub
Sub zCol_Rubber_Corner_001_hit
	TargetBouncer activeball, 1
End Sub


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
'Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
'Dim tablewidth: tablewidth = Table1.width
'Dim tableheight: tableheight = Table1.height

' *** User Options - Uncomment here or move to top
'----- Shadow Options -----
'Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 0		'0 = Static shadow under ball ("flasher" image, like JP's)
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

'Dim PI: PI = 4*Atn(1)

'Function Atn2(dy, dx)
'	If dx > 0 Then
'		Atn2 = Atn(dy / dx)
'	ElseIf dx < 0 Then
'		If dy = 0 Then 
'			Atn2 = pi
'		Else
'			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
'		end if
'	ElseIf dx = 0 Then
'		if dy = 0 Then
'			Atn2 = 0
'		else
'			Atn2 = Sgn(dy) * pi / 2
'		end if
'	End If
'End Function
'
'Function AnglePP(ax,ay,bx,by)
'	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
'End Function

'****** End Part B:  Code and Functions ******


'****** Part C:  The Magic ******
Dim sourcenames, currentShadowCount, DSSources(30), numberofsources, numberofsources_hold
sourcenames = Array ("","","","","","","","","","","","")
currentShadowCount = Array (0,0,0,0,0,0,0,0,0,0,0,0)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!
dim objrtx1(12), objrtx2(12)
dim objBallShadow(12)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadowA5,BallShadowA6,BallShadowA7)

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
	Dim falloff:	falloff = 250			'Max distance to light sources, can be changed if you have a reason
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

'	Some extra magic ?==?
	Dim xyangle
'	If BOT(s).X < 217 Then   
'		If BOT(s).y > 355 Then
'			Shadowballs(s).visible=0
'		elseIf BOT(s).Y > 265 Then
'			Shadowballs(s).visible=1
'			Shadowballs(s).X = 13
'			shadowballs(s).y = BOT(s).y - (265-BOT(s).y)*0.4
'		Else
'			' left side ! X41-217(176)  y=49-292(243)
'			Shadowballs(s).visible=1
'			xyangle=( 217-BOT(s).x ) / 1.76
'			Shadowballs(s).X = (BOT(s).X) + ((BOT(s).X - 482)/9.75) 
'			If BOT(s).y>127 Then
'				shadowballs(s).y=25+xyangle - (127-BOT(s).y)*1.2
'			Else
'				shadowballs(s).y=25+xyangle
'			End If
'		End If
'	elseif
	If  BOT(s).Y<240 And BOT(s).Z < 40 Then
		If BOT(s).x>645 Then 
			xyangle=( BOT(s).x-645 ) / 2.8 
			Shadowballs(s).X = (BOT(s).X) - ((BOT(s).X - 482)/9.75) 
			Shadowballs(s).y = (Bot(s).Y) - 25+7 + xyangle/2 
			Shadowballs(s).size_Z = 95 : Shadowballs(s).size_X = 95 : Shadowballs(s).size_Y = 95
			Shadowballs(s).visible=1
		Elseif BOT(s).x > 575 Then
			Shadowballs(s).X = (BOT(s).X) + ((BOT(s).X - 482)/9.75) 
			Shadowballs(s).y=25+7
			Shadowballs(s).size_Z = 95 : Shadowballs(s).size_X = 95 : Shadowballs(s).size_Y = 95
			Shadowballs(s).visible=1
		Else
			Shadowballs(s).X = (BOT(s).X) - ((BOT(s).X - 482)/9.75) 
			i = 95 - (BOT(s).y - 48)*.4
			If i > 0 Then
				Shadowballs(s).size_Z = i
				Shadowballs(s).size_X = i
				Shadowballs(s).size_Y = i
				Shadowballs(s).y = 25
				Shadowballs(s).visible=1
			Else
				Shadowballs(s).visible=0
			End If
		End If
	Else
		Shadowballs(s).visible=0
	End If

'			Else
'				Shadowballs(maxTwo).visible=0
'
'			End If
'		End If
'	Else
'		Shadowballs(maxTwo).visible=0
'	End If

'	Elseif BOT(s).Y<250 Then
'		If BOT(s).x>645 Then
'			xyangle=( BOT(s).x-645 ) / 2.8 
'			Shadowballs(s).X = 930 + 25 - (250-BOT(s).y)*0.5
'			Shadowballs(s).y = (Bot(s).Y) - 25 + xyangle/2 '
'			Shadowballs(s).size_Z = 95 : Shadowballs(s).size_X = 95 : Shadowballs(s).size_Y = 95
'			Shadowballs(s).visible=1
'		Else
'			Shadowballs(s).visible=0
'		End If
'	Elseif BOT(s).Y<560 Then
'		If Not InRect(BOT(s).X,BOT(s).Y,  476,160,  740,161,  801,334,  522,542) then
'			If BOT(s).x>645 Then '
'				xyangle=( BOT(s).x-645 ) / 2.8 
'				Shadowballs(s).X = 930 + 25 
'				Shadowballs(s).y = (Bot(s).Y) - 25 + xyangle/2 '
'				If BOT(s).Y >277 Then Shadowballs(s).Z = 19 Else Shadowballs(s).Z = 23
'				Shadowballs(s).size_Z = 95 : Shadowballs(s).size_X = 95 : Shadowballs(s).size_Y = 95
'				Shadowballs(s).visible=1
'			Else
'				Shadowballs(s).visible=0
'			End If
'		Else
'			Shadowballs(s).visible=0
'		End If
'	Else 
'		Shadowballs(s).visible=0
'	End If

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
					If LSd < falloff And luttarget < 6 Then						    'If the ball is within the falloff range of a light and light is on (we will set numberofsources to 0 when GI is off)
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

'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************


'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************
' To add these slingshot corrections:
' 	- On the table, add the endpoint primitives that define the two ends of the Slingshot
'	- Initialize the SlingshotCorrection objects in InitSlingCorrection
' 	- Call the .VelocityCorrect methods from the respective _Slingshot event sub


dim LS : Set LS = New SlingshotCorrection
dim RS : Set RS = New SlingshotCorrection
'dim TS : Set TS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection

	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS

	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS

'	TS.Object = TestSlingshot
'	TS.EndPoint1 = EndPoint1TS
'	TS.EndPoint2 = EndPoint2TS

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

' This sub is needed, however it may exist somewhere else in the script. Uncomment below if needed
Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function


Function RotPoint(x,y,angle)
    dim rx, ry
    rx = x*dCos(angle) - y*dSin(angle)
    ry = x*dSin(angle) + y*dCos(angle)
    RotPoint = Array(rx,ry)
End Function


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



'Dim backpopping
'Sub backgroundpop_Timer
'	If Not backgroundpop.enabled Then
'		backgroundpop.enabled=1
'		backpopping=0
'	Else
'		backpopping=backpopping+1
'		If backpopping=14 Then
'			backgroundpop.enabled=0
'			Exit Sub
'		End If
'		If backpopping<8 Then
'			Flasher004.height=100-backpopping
'			Flasher005.height=100-backpopping
'		Else
'			Flasher004.height=100 - ( -14 + backpopping)
'			Flasher005.height=100 - ( -14 + backpopping)
'		End If
'	End If

Dim backpopping
Sub backgroundpop_Timer

	If DTbackscreenmoving=1 Then
		If backgroundpop.enabled=0 Then
			backgroundpop.enabled=1
			backpopping = 8
		Else
'			Flasher004.height= 100+ backpopping
'			Flasher005.height= 100+ backpopping
			Flasher004.rotY= backpopping/6
			Flasher005.rotY= backpopping/6

			If backpopping = 0 Then backgroundpop.Enabled = 0:Exit Sub
			If backpopping < 0 Then
				backpopping = ABS(backpopping) - 1
			Else
				backpopping = - backpopping + 1
			End If
		End If
	End If

End Sub
	

Dim baseshipz,baseshipdirection,turningshipon,ringturningpos
Sub turningship2
	If ringsonly=0 Then
		If attractwormhole>0 Then attractwormhole=attractwormhole-1
		If baseshipdirection=0 Then
			baseshipz=baseshipZ+1
			If baseshipz>190 Then baseshipdirection=1
		Else
			baseshipz=baseshipZ-1
			If baseshipz<1 Then baseshipdirection=0
		End If
		primitive050.objrotz=baseshipz/3-50
	End If

	ringturningpos=ringturningpos+1 
	If ringturningpos>359 Then ringturningpos=0
	Primitive049.rotZ=-ringturningpos
	Primitive051.rotZ=-ringturningpos
	Primitive052.rotZ=-ringturningpos

End Sub



'********************************************************************
Sub Table1_init

	boss1.uservalue=0
	Lostball.uservalue=0

	If DTscoreReel=0 Then DTScorereelOff
	For i = 4 to 9
		If MusicFile(i)="Empty" Then LastMusic = i-1 : Exit Sub
	Next
	LastMusic=9





'	If MissionBase=1 Then 
'		Primitive049.visible=1
'		Primitive050.visible=1
'		Primitive051.visible=1
'		Primitive052.visible=1
'		Primitive048.visible=1
'	End If
'	
'	If SpaceStation=1 Then
'		Primitive047.visible=1
'	End If
'
'	If Slingshotfighters=1 Then 
'		Primitive053.visible=1
'		Primitive054.visible=1
'	End If
'
'	If LeftRampRazer=1 Then
'		primitive001.visible=1
'	End If
'
'	If RightRampFighter=1 Then
'		primitive003.visible=1
'	End If
'
End Sub

Dim LastPlayd,tempstr,SongNr
LastPlayd=1
Sub PlayTune(tune)
	SongNr=tune
	If SongNr=11 And FlexDMD then FlexDMDTimer.enabled=0 '  turn off loop if song 11 starts
	If MusicFile(SongNr) = "Empty" Then SongNr=3
	Playmusic MusicFile(SongNr)

	MusicVolume = PlayVolume(SongNr) * MusicVolumeDial
	If LastPlayd<10 Then Playlist(LastPlayd-1).state=0
	If SongNr<10 Then
		Playlist(SongNr-1).state=2
		LSplaylist.StopPlay
		LSplaylist.Play SeqRandom,1,,500
	End If
	LastPlayd=SongNr
	If BossActive=1 And bosslevel=7 Then MusicVolume=0.01
	If MultiballActive Then MusicVolume=0.01
End Sub


Sub Table1_musicdone

		i=int(rnd(1)*(LastMusic-2))+3 
		If LastPlayd=2 Then
			i=1 
			gameovernolights=1
			If FlexDMD then FlexDMDTimer.enabled=1 : UmainDMD.cancelrendering()
			If salvagedjustonce=0 Then PlaySound "salvagedwhatremains",0,1*BackGlassVolumeDial : salvagedjustonce=1
		End If

		If LastPlayd=10 Then
			attractModeStatus=2
			gameovernolights=1
			i=1
			If FlexDMD then FlexDMDTimer.enabled=1 : UmainDMD.cancelrendering()
			If salvagedjustonce=0 Then PlaySound "salvagedwhatremains",0,1*BackGlassVolumeDial : salvagedjustonce=1
		End If
		PlayTune(i)
End Sub

'<Testing variables> no need to touch these

Const DoubleMission=0 

Dim testingstuff
Sub startuptest_Timer
	testingstuff=testingstuff+1
	Select Case testingstuff

		case 20 : dt010_Timer' : Wall027.visible=0': Primitive059.visible=0
		ButtonBlink=10

		Primitive071.blenddisablelighting=2.2



		case 21 : dt009_Timer 
		LiBumper1_Timer
		PlaySound SoundFX("bumpers"&int(rnd(1)*15)+1,DOFContactors), 0, .7*volumedial, AudioPan(Bumper1), 0.05,-35,0,1,AudioFade(Bumper1)
		ObjLevel(5) = 1 : FlasherFlash5_Timer
		PlaySound SoundFX("thud01",DOFContactors), 0, 0.05*volumedial, AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger)
		case 22 : dt001_Timer 
				RaiseBigDT.enabled=1 : BigDTUD=1 : PlaySound SoundFX("elengine",DOFContactors), 0, 0.8*volumedial, AudioPan(Primitive059), 0.05,0,0,1,AudioFade(Primitive059)
		case 23 : dt002_Timer 
		LiBumper2_Timer
		PlaySound SoundFX("bumpers"&int(rnd(1)*15)+1,DOFContactors), 0, .7*volumedial, AudioPan(Bumper1), 0.05,-35,0,1,AudioFade(Bumper1)
		ObjLevel(6) = 1 : FlasherFlash5_Timer
		PlaySound SoundFX("thud01",DOFContactors), 0, 0.05*volumedial, AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger)
		case 24 : dt003_Timer 
		case 25 : dt004_Timer 
		LiBumper3_Timer
		PlaySound SoundFX("bumpers"&int(rnd(1)*15)+1,DOFContactors), 0, .7*volumedial, AudioPan(Bumper1), 0.05,-35,0,1,AudioFade(Bumper1)
		ObjLevel(7) = 1 : FlasherFlash5_Timer
		PlaySound SoundFX("thud01",DOFContactors), 0, 0.05*volumedial, AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger)
		case 26 : dt005_Timer 
		case 27 : dt006_Timer 
		case 28 : dt007_Timer 
		case 29 : dt008_Timer 

		if HideDesktop = 1 Then
			Controller.B2SSetData 34,0 : Controller.B2SSetData 33,0
			Controller.B2SSetData 11,0
			Controller.B2SSetData 12,0
			Controller.B2SSetData 13,0
			Controller.B2SSetData 14,0
			Controller.B2SSetData 15,0
			Controller.B2SSetData 16,0
			Controller.B2SSetData 40,0
			Controller.B2SSetData 41,0
			Controller.B2SSetData 42,0
			Controller.B2SSetData 43,0
			Controller.B2SSetData 44,0
		End If

		case 41 : Raise_DT 1
		Raise_DT 2

		case 54
		RaiseBigDT.enabled=1 : BigDTUD=2 : PlaySound SoundFX("elengine",DOFContactors), 0, 0.8*volumedial, AudioPan(Primitive059), 0.05,0,0,1,AudioFade(Primitive059)

		Raise_DT 3 : Raise_DT 4 : Raise_DT 5 : Raise_DT 6 : Raise_DT 7

		case 75
		Drop_5Bank
		Case 77 : startuptest.enabled=0

	End Select
End Sub
	
Sub FlexDMDTimer_Timer 
	If NOT UMainDMD.IsRendering() Then
		If WaitforInitialsstatus=0 or WaitforInitialsstatus>10 Then
			UMainDMD.cancelrendering()
'			UMainDMD.Color = RGB(240,240,240)
			UMainDMD.DisplayScene00ExWithId "attract1",false,vid0, " ", 15, 0, " ", 15, 0, 14, 101000, 14
		End If
	End If
End Sub




Dim WingLState : WingLState=0 ' use normal light state 0 1 2
Dim WingRState : WingRState=0 ' use normal light state 0 1 2
Dim WingR_Pos : WingR_Pos=0
Dim WingL_Pos : WingL_Pos=0
Dim WingL_Blinks
Dim WingR_Blinks


Dim LazerState
Dim Lazer_Pos

Dim lutcounter, luttarget 
lutcounter=5 : luttarget=8
Sub testinglut_Timer


	If LazerState = 1 Then
		Lazer_Pos = Lazer_Pos + 2
		If Lazer_Pos > 10 Then Lazer_Pos = 10 : LazerState=0
	Else
		Lazer_Pos = Lazer_Pos - 1
		If Lazer_Pos < 0 Then
			Lazer_Pos = 0
		End If
	End If
	FlasherLazer.opacity = Lazer_Pos * 45
	FlasherLazer2.opacity = Lazer_Pos * 34
	LiLazer1.intensity = Lazer_Pos/4
	LiLazer2.intensity = Lazer_Pos/6
	bulbLitLazer.blenddisablelighting = Lazer_Pos/5
	bulbLitLazer001.blenddisablelighting = Lazer_Pos*50




	If WingLState > 3 Then WingL_Blinks = WingLState : WingLState = 2
	If WingLState = 1 Or WingLState = 2 Then
		WingL_Pos = WingL_Pos + 2
		If WingL_Pos > 10 Then WingL_Pos = 10 : If  WingLState > 1 Then WingLState = 3 Else WingLState = -1 ' finished going Up .. if blink then goto off next
	ElseIf WingLState <> -1 Then
		WingL_Pos = WingL_Pos - 1
		If WingL_Pos < 0 Then
			WingL_Pos = 0
			If WingLState > 1 Then WingLState = 2 Else WingLState = - 1			' finished going Down
			If WingL_Blinks > 0 Then
				WingL_Blinks = WingL_Blinks - 1 
				If WingL_Blinks = 0 Then WingLState = - 1						' finished blinking
			End If
		End If
	End If
	If WingL_Pos > 0 Then
		FlasherWingL.opacity = WingL_Pos * 70 + ( lutcounter - 1) * 4  
		LiWingL1.intensity = WingL_Pos + (lutcounter - 2) / 3
		LiWingL2.intensity = WingL_Pos + (lutcounter - 2) / 3
	Else 'off
		FlasherWingL.opacity = 0
		LiWingL1.intensity = 0
		LiWingL2.intensity = 0
	End If
	bulbLitWingL.blenddisablelighting = WingL_Pos


	If WingRState > 3 Then WingR_Blinks = WingRState : WingRState = 2
	If WingRState = 1 Or WingRState = 2 Then
		WingR_Pos = WingR_Pos + 2
		If WingR_Pos > 10 Then WingR_Pos = 10 : If  WingRState > 1 Then WingRState = 3 Else WingRState = -1 ' finished going Up .. if blink then goto off next
	ElseIf WingRState <> -1 Then
		WingR_Pos = WingR_Pos - 1
		If WingR_Pos < 0 Then
			WingR_Pos = 0
			If  WingRState > 1 Then WingRState = 2 Else WingRState = -1			' finished going Down
			If WingR_Blinks > 0 Then
				WingR_Blinks = WingR_Blinks - 1 
				If WingR_Blinks = 0 Then WingRState = - 1						' finished blinking
			End If
		End If
	End If
	If WingR_Pos > 0 Then
		FlasherWingR.opacity = WingR_Pos * 100 + ( lutcounter - 1) * 4  
		LiWingR1.intensity = WingR_Pos + (lutcounter - 2) / 3
		LiWingR2.intensity = WingR_Pos + (lutcounter - 2) / 3
	Else 'off
		FlasherWingR.opacity = 0
		LiWingR1.intensity = 0
		LiWingR2.intensity = 0
	End If
	bulbLitWingR.blenddisablelighting = WingR_Pos







	If lutcounter=luttarget Then Exit Sub
	Dim xy : xy=0
	If lutcounter>luttarget Then lutcounter=lutcounter-1 : xy=1

	If lutcounter<luttarget Then lutcounter=lutcounter+1 



	For Each i in DynamicSources
		i.state=xy
	Next

	Flasher008.opacity=140 + (lutcounter-2)*2
	table1.ColorGradeImage = "LUT"& lutcounter
	Flasher004.opacity=40+(lutcounter*30)
	Flasher005.opacity=40+(lutcounter*30)
	LightUnderDisp1.intensity = 0.02 + 0.22/lutcounter
	LightUnderDisp2.intensity = 0.02 + 0.22/lutcounter
	wall138.blenddisablelighting=9-lutcounter
	wall079.blenddisablelighting=9-lutcounter  ' spinnerbracket
	wall077.blenddisablelighting=(9-lutcounter)/5 ' ramp W TopFlasher1
	wall078.blenddisablelighting=(9-lutcounter)/5 ' ramp W TopFlasher1
	wall056.blenddisablelighting=(9-lutcounter)/3
	wall059.blenddisablelighting=(9-lutcounter)/3
	wall001.blenddisablelighting=(9-lutcounter)/3
	wall002.blenddisablelighting=(9-lutcounter)/3
	wall083.blenddisablelighting=(9-lutcounter)/3
	underlightfordark.intensity=(lutcounter-2)/10
	bulbLit001.blenddisablelighting = (8-lutcounter)/3
	bulbLit002.blenddisablelighting = (8-lutcounter)/3
	bulbLit003.blenddisablelighting = (8-lutcounter)/3
	bulbLit004.blenddisablelighting = (8-lutcounter)/3
	bulbLit005.blenddisablelighting = (8-lutcounter)/3
	bulbLit006.blenddisablelighting = (8-lutcounter)/3
	bulbLit007.blenddisablelighting = (8-lutcounter)/3
	bulbLit008.blenddisablelighting = (8-lutcounter)/3
	Primitive072.blenddisablelighting = 0.1+(8-lutcounter)/33 + bloomingVar/23 ' wings
	Primitive076.blenddisablelighting = 0.1+(8-lutcounter)/33 + bloomingVar/23


End Sub




Sub LightUnderDisp1_timer '750
	LightUnderDisp1.timerenabled=False
	LightUnderDisp1.state=1
	LightUnderDisp2.state=1
End Sub

Sub LightUnderDisp2_timer '250
	LightUnderDisp2.timerenabled=False
	LightUnderDisp1.state=1
	LightUnderDisp2.state=1
End Sub




Sub cutoutheoone_Timer
	cutoutheoone.enabled=0
	stopsound "aimingfortheotherone"
End Sub

dim talkingdelay,talkingnumber,onetimetalk
Sub talkingheads_timer
	if skillshot=0 And Bip=1 Then

		If Displaybuzy>0 And talkingdelay<8 Then
			talkingdelay=0
		Else
			talkingdelay=talkingdelay+1
		End If

		If talkingdelay=8 Then 
			talkingdelay=9
			i = int(rnd(1)*44)
			If i = talkingnumber Then 
				talkingnumber=talkingnumber+1
				If talkingnumber>43 Then talkingnumber=0
			Else
				talkingnumber=i
			End If
			If onetimetalk=0 Then onetimetalk=1 : talkingnumber=9
			talkingrandom(talkingnumber)

			End If

	End If

	if talkingdelay=38 Then talkingdelay=0

End Sub
Sub talkingrandom(toppers)
	If Tilted Then Exit Sub
				Select Case toppers
					Case 0 : Playsound ("aliegeancetonoone"),0,1*BackGlassVolumeDial
					Case 1 : Playsound ("wheretogo"),0,1*BackGlassVolumeDial
					Case 2 : Playsound ("darktroopers"),0,1*BackGlassVolumeDial
					Case 3 : Playsound ("droptheblaster"),0,1*BackGlassVolumeDial
					Case 4 : Playsound ("extremelygifted"),0,1*BackGlassVolumeDial
					Case 5 : Playsound ("fixtransponder"),0,1*BackGlassVolumeDial
					Case 6 : Playsound ("hereforthearmor"),0,1*BackGlassVolumeDial
					Case 7 : Playsound ("hyperspace"),0,1*BackGlassVolumeDial
					Case 8 : Playsound ("iwantmyarmor"),0,1*BackGlassVolumeDial
					Case 9 : Playsound ("legend"),0,1*BackGlassVolumeDial
					Case 10 : Playsound ("loweryourshields"),0,1*BackGlassVolumeDial
					Case 11 : Playsound ("madalorianarmour"),0,1*BackGlassVolumeDial
					Case 12 : Playsound ("manofhonor"),0,1*BackGlassVolumeDial
					Case 13 : Playsound ("mudhorn"),0,1*BackGlassVolumeDial
					Case 14 : Playsound ("nevermetamando"),0,1*BackGlassVolumeDial
					Case 15 : Playsound ("notbymyhand"),0,1*BackGlassVolumeDial
					Case 16 : Playsound ("offtheice"),0,1*BackGlassVolumeDial
					Case 17 : Playsound ("onlymandohere"),0,1*BackGlassVolumeDial
					Case 18 : Playsound ("onlytransport"),0,1*BackGlassVolumeDial
					Case 19 : Playsound ("piratehijack"),0,1*BackGlassVolumeDial
					Case 20 : Playsound ("robotdemand"),0,1*BackGlassVolumeDial
					Case 21 : Playsound ("simpleman"),0,1*BackGlassVolumeDial
					Case 22 : Playsound ("stillgotasset"),0,1*BackGlassVolumeDial
					Case 23 : Playsound ("targetpractice"),0,1*BackGlassVolumeDial
					Case 24 : Playsound ("trackingbeacon"),0,1*BackGlassVolumeDial
					Case 25 : Playsound ("pinky"),0,1*BackGlassVolumeDial
					Case 26 : Playsound ("completelysafe"),0,1*BackGlassVolumeDial
					Case 27 : Playsound ("empiregone"),0,1*BackGlassVolumeDial
					Case 28 : Playsound ("enourmousguns"),0,1*BackGlassVolumeDial
					Case 29 : Playsound ("getthekid"),0,1*BackGlassVolumeDial
					Case 30 : Playsound ("gungan"),0,1*BackGlassVolumeDial
					Case 31 : Playsound ("helpyouwiththat"),0,1*BackGlassVolumeDial
					Case 32 : Playsound ("kingsransom"),0,1*BackGlassVolumeDial
					Case 33 : Playsound ("lasthere"),0,1*BackGlassVolumeDial
					Case 34 : Playsound ("nodroids"),0,1*BackGlassVolumeDial
					Case 35 : Playsound ("notharmyou"),0,1*BackGlassVolumeDial
					Case 36 : Playsound ("offmyplanet"),0,1*BackGlassVolumeDial
					Case 37 : Playsound ("sorryremote"),0,1*BackGlassVolumeDial
					Case 38 : Playsound ("speederrunning"),0,1*BackGlassVolumeDial
					Case 39 : Playsound ("surrender"),0,1*BackGlassVolumeDial
					Case 40 : PlaySound ("salvagedwhatremains"),0,1*BackGlassVolumeDial
					Case 41 : PlaySound ("tabsoncrest"),0,1*BackGlassVolumeDial
					Case 42 : PlaySound ("ihavespoken"),0,1*BackGlassVolumeDial
					Case 43 : PlaySound ("cosyinthecockpit"),0,1*BackGlassVolumeDial

				End Select
End Sub
Sub Stoptalking

			Select Case talkingnumber
				Case 0 : StopSound ("aliegeancetonoone")
				Case 1 : StopSound ("wheretogo")
				Case 2 : StopSound ("darktroopers")
				Case 3 : StopSound ("droptheblaster")
				Case 4 : StopSound ("extremelygifted")
				Case 5 : StopSound ("fixtransponder")
				Case 6 : StopSound ("hereforthearmor")
				Case 7 : StopSound ("hyperspace")
				Case 8 : StopSound ("iwantmyarmor")
				Case 9 : StopSound ("legend")
				Case 10 : StopSound ("loweryourshields")
				Case 11 : StopSound ("madalorianarmour")
				Case 12 : StopSound ("manofhonor")
				Case 13 : StopSound ("mudhorn")
				Case 14 : StopSound ("nevermetamando")
				Case 15 : StopSound ("notbymyhand")
				Case 16 : StopSound ("offtheice")
				Case 17 : StopSound ("onlymandohere")
				Case 18 : StopSound ("onlytransport")
				Case 19 : StopSound ("piratehijack")
				Case 20 : StopSound ("robotdemand")
				Case 21 : StopSound ("simpleman")
				Case 22 : StopSound ("stillgotasset")
				Case 23 : StopSound ("targetpractice")
				Case 24 : StopSound ("trackingbeacon")
				Case 25 : StopSound ("pinky")
				Case 26 : StopSound ("completelysafe")
				Case 27 : StopSound ("empiregone")
				Case 28 : StopSound ("enourmousguns")
				Case 29 : StopSound ("getthekid")
				Case 30 : StopSound ("gungan")
				Case 31 : StopSound ("helpyouwiththat")
				Case 32 : StopSound ("kingsransom")
				Case 33 : StopSound ("lasthere")
				Case 34 : StopSound ("nodroids")
				Case 35 : StopSound ("notharmyou")
				Case 36 : StopSound ("offmyplanet")
				Case 37 : StopSound ("sorryremote")
				Case 38 : StopSound ("speederrunning")
				Case 39 : StopSound ("surrender")
				Case 40 : StopSound ("salvagedwhatremains")
				Case 41 : StopSound ("tabsoncrest")
				Case 42 : StopSound ("ihavespoken")
				Case 43 : StopSound ("cosyinthecockpit")

			End Select
End Sub
Dim trythisball,BALLSINPLAY	

Dim ShadowBalls : ShadowBalls = Array (ShadowBall0,ShadowBall1,ShadowBall2,ShadowBall3,ShadowBall4,ShadowBall5)
Sub ballblinker_Timer
	Dim BOT
    BOT = GetBalls
	BALLSINPLAY=uBound(BOT)
	If uBound(BOT) = -1 Then Exit Sub

	For i = 0 to uBound(BOT)
		If SelectBalls=20 Then
			If BOT(i).ID<100 Then
				If nextblackball=0 Then
					BOT(i).ID= 100+int(rnd(1)*20)
				Else
					BOT(i).id=nextblackball
					nextblackball=0
				End If
			End If
			If Bot(i).ID > 99 Then SelectBall = BOT(i).ID - 100
		End If
		If selectBalls<22 Then
			Select Case BorderColor
				case 1	 : BOT(i).FrontDecal="mandologo8c"' : ShadowBalls(i).image="mandologo8c"
				case 2	 : BOT(i).FrontDecal="mandologo8" ' : ShadowBalls(i).image="mandologo8"
				case 3	 : BOT(i).FrontDecal="mandologo8b"': ShadowBalls(i).image="mandologo8b"
				case 4,5 : BOT(i).FrontDecal="mandologo8d"' : ShadowBalls(i).image="mandologo8d"
			End Select
		End If
		If SelectBall=19 Then
			trythisball=trythisball+1
			Select Case trythisball
				Case 1:BOT(i).Image = "mandologo9a" : ShadowBalls(i).image="mandologo9a"
				Case 2:BOT(i).Image = "mandologo9b" : ShadowBalls(i).image="mandologo9b"
				Case 3:BOT(i).Image = "mandologo9c" : ShadowBalls(i).image="mandologo9c"
				Case 4:BOT(i).Image = "mandologo9d" : ShadowBalls(i).image="mandologo9d"
				Case 10:BOT(i).Image = "mandologo9c" : ShadowBalls(i).image="mandologo9c"
				Case 11:BOT(i).Image = "mandologo9b" : trythisball=0 : ShadowBalls(i).image="mandologo9b"
			End Select
		Else
			If Selectball<20 Then
				If SelectBall<0 Then SelectBall=0
				BOT(i).Image=CrazyBalls(SelectBall)  : ShadowBalls(i).image=CrazyBalls(SelectBall)
			End If
		End If
	Next
End Sub

Dim newbosshit,newbosses,NewBossStatus



Sub Wall025_Hit 
	newbosstimer.enabled=1
	
	If WizardLevel>0 Then
		GideonStatus = 2

		EvasiveOneScoring=EvasiveOneScoring+250000
		Scoring EvasiveOneScoring

		RaiseBigDT.enabled=1 : BigDTUD=2 : PlaySound SoundFX("elengine",DOFContactors), 0, 0.8*volumedial, AudioPan(Primitive059), 0.05,0,0,1,AudioFade(Primitive059)
		Wall025.collidable=false
		Wall026.collidable=false

		PlaySound SoundFX("target65",DOFContactors), 0, 1*VolumeDial, AudioPan(Primitive059), 0.05,1,0,1,AudioFade(Primitive059)
		PlaySound SoundFX("Ball_Collide_5",DOFContactors), 0, 1*VolumeDial, AudioPan(Primitive059), 0.05,1,0,1,AudioFade(Primitive059)

		Exit Sub
	End If

	If newbosshit=0 Then
		
		BigDThitsCounter=BigDThitsCounter+1
		UpdateBossLights
		NewBossStatus=BigDThitsCounter
		If BigDThitsCounter=8 then 	
			RaiseBigDT.enabled=1 : BigDTUD=2 : PlaySound SoundFX("elengine",DOFContactors), 0, 0.8*volumedial, AudioPan(Primitive059), 0.05,0,0,1,AudioFade(Primitive059)
			Wall025.collidable=false
			Wall026.collidable=false
			Bossactive=0 : DOF 118,0
			If bosslevel=2 Then boss2.state=1 
			If bosslevel=4 Then boss4.state=1 
			If bosslevel=6 Then boss6.state=1 
			DOF 142,2 : 'Debug.print "DOF 142, 2"  'apophis
		Else	
			DOF 141,2 : 'Debug.print "DOF 141, 2"  'apophis
		End If

		PlaySound SoundFX("target65",DOFContactors), 0, 1*VolumeDial, AudioPan(Primitive059), 0.05,1,0,1,AudioFade(Primitive059)
		PlaySound SoundFX("Ball_Collide_5",DOFContactors), 0, 1*VolumeDial, AudioPan(Primitive059), 0.05,1,0,1,AudioFade(Primitive059)
	End If
End Sub

Sub Newbosstimer_Timer
	newbosses=newbosses+1
	
	Select Case newbosses
		
		case 1      : Primitive059.image="bigDT" : Primitive059.Z=Primitive059.Z-1.3 : Primitive059.ObjRotX=6 : primitive059.blenddisablelighting=0
		case 4,7	: Primitive059.image="bigDT" : Primitive059.Z=Primitive059.Z-1.3 : Primitive059.blenddisablelighting=0
		Case 9  	: Primitive059.image="bigDT" : Primitive059.Z=Primitive059.Z-1.3
						PlaySound SoundFX("Metal_Touch_11",DOFContactors), 0, 0.7*volumedial, AudioPan(Primitive059), 0.05,0,0,1,AudioFade(Primitive059)
						primitive059.blenddisablelighting=0
		case 2,5,8  :	primitive059.blenddisablelighting=0.5

					If BossLevel=2 Or BossLevel=7 Then Primitive059.image="bigDT_Mudhorn"
					If BossLevel=4 Then Primitive059.image="bigDT_Spider"
					If BossLevel=6 Then Primitive059.image="bigDT_Dragon"
		case 3 		: newbosshit=0 : Primitive059.Z=Primitive059.Z-1.2 : Primitive059.ObjRotX=2
		case 6		: Primitive059.ObjRotX=0 
		case 10		: newbosses=0 : newbosstimer.enabled=0
					If bosslevel=7 Then playsound "theegg",0,1*BackGlassVolumeDial
					If bosslevel=2 Then
						If BigDThitsCounter=1 Or BigDThitsCounter=3 or BigDThitsCounter=5 Then
							Stopsound "theegg" : playsound "theegg",0,1*BackGlassVolumeDial
						Else
							playsound "mudhorn1" ,0,0.23*BackGlassVolumeDial,0,0,0,0,0,0
						End If
					End If
					If bosslevel=4 Then
						If BigDThitsCounter=1 Then
							playsound "backtotheship",0,1*BackGlassVolumeDial
						Elseif BigDThitsCounter=4 Then
							playsound "thisbetterwork",0,1*BackGlassVolumeDial
						Elseif BigDThitsCounter=6 Then
							playsound "bumpyride",0,1*BackGlassVolumeDial
						Else
							playsound "spider" & Int(rnd(1)*2)+1 ,0,0.23*BackGlassVolumeDial,0,0,0,0,0,0
						End If
					End If
					  If bosslevel=6 Then
						If BigDThitsCounter=2 or BigDThitsCounter=5 Then
							playsound "helpmekillit",0,1*BackGlassVolumeDial
						Else
							playsound "dragon" & Int(rnd(1)*2)+1 ,0,0.23*BackGlassVolumeDial,0,0,0,0,0,0
						End If
					  End If
	End Select

End Sub


Dim BossLightCounter, BossLightState
BossLightCounter=0
Sub BossLightBlinker_Timer
	Dim i2
	If EvasiveState=4 Then
		' blink up then down
		'50ms interval
		BossLightCounter=BossLightCounter+1
		If BossLightState>12 Then
			bossfights.Play SeqBlinking,,3,123
			BossLightState=BossLightState+1
		End If
		If BossLightState > 5 And BossLightState < 13 Then
			If BossLightCounter=1 Then
				for i = 0 to 4
					BossLights(i+15).state=1
					BossLights(i+35).state=1  ' the red ones
					BossLights(i).state=1
					BossLights(i+20).state=1

					BossLights(i+10).state=0
					BossLights(i+30).state=0  
					BossLights(i+5).state=0
					BossLights(i+25).state=0
				Next
			Else
				for i = 0 to 4
					BossLights(i+10).state=1
					BossLights(i+30).state=1  
					BossLights(i+5).state=1
					BossLights(i+25).state=1

					BossLights(i+15).state=0
					BossLights(i+35).state=0  
					BossLights(i).state=0
					BossLights(i+20).state=0
				Next
				BossLightState=BossLightState+1
				BossLightCounter=0
			End If

		End If

		If BossLightState<6	Then
			If BossLightCounter=1 Then
				For i = 0 to 9
					BossLights(i+10).state=1
					BossLights(i+30).state=1  ' the red ones
					BossLights(i).state=0
					BossLights(i+20).state=0
				Next
			Else
				For i = 0 to 9
					BossLights(i+10).state=0
					BossLights(i+30).state=0  
					BossLights(i).state=1
					BossLights(i+20).state=1
				Next
				BossLightState=BossLightState+1
				BossLightCounter=0
			End If
		End If
		iF BossLightState>13 Then Boss1ALLOFF : BossLightBlinker.Enabled=0 : BossLightCounter=0 : BossLightState=0 : ResetBossLights
	End If
End Sub
Dim Swaplights, Swapimage,Swapimage2   ', scratches
Dim shakeyshake(100)

Timer006.interval=60
Sub Timer006_Timer
	Dim ii,x
	swaplights=swaplights+1
	If swaplights=6 Then
		swaplights=0
		for ii = 0 to 67
			shakeyshake(ii)=0
		Next
		for ii = 1 to 9
			shakeyshake(Int(rnd(1)*67)) = 1
		Next
	End If

	Select Case swaplights
		case 0 : swapimage="round2on"  : Swapimage2="Insert_Rec_Star_ON_ap3"
		case 1 : swapimage="round2on5" : Swapimage2="Insert_Rec_Star_ON_2"
		case 2 : swapimage="round2on6" : Swapimage2="Insert_Rec_Star_ON_3"
		case 3 : swapimage="round2on7" : Swapimage2="Insert_Rec_Star_ON_4"
		case 4 : swapimage="round2on6" : Swapimage2="Insert_Rec_Star_ON_3"
		case 5 : swapimage="round2on5" : Swapimage2="Insert_Rec_Star_ON_2"
	End Select
	x=0
	For each ii in lightshaker
		If shakeyshake(x)=1 then
			ii.image=swapimage
		Else
			ii.image="round2on"
		End If
		x=x+1
	Next
	If RestartGame=1 Then 
		RestartCounter = RestartCounter + 1
		'debug.print "restartcounterr=" & restartcounter
		If RestartCounter = 30 Then
			ballsavedstatus = 0
			RestartGame = 2
			RestartCounter = 0
			Ballinplay = 3
			CurrentPlayer = PlayersPlaying
			Tiltstatus = 3
			Tilttimer.enabled=True
			Tilting
			flyagain1.state = 0 : pflyagain.blenddisablelighting=1
			flyagain2.state = 0
			flyagain3.state = 0
			extraextraball = 0

		End If
	End If


End Sub
Dim RestartCounter
	

Dim ButtonY, ButtonZ, ButtonDirection, ButtonState ,ButtonBlink
ButtonDirection=2
ButtonState=0 
ButtonBlink=0
Dim WormholeButton : WormholeButton = False
	'****
	'* middlebosslightning
Sub ButtonController
	
If Not WormholeButton Then
	If attract1Mode=1 Then
		If int(Rnd(1)*180)=4 Then ButtonBlink = Int(Rnd(1)*13)+2
	Else
		If Buttonstate>2 And superscoring=0 then ButtonState=2 : ButtonBlink=0
	End If
End If
	If ButtonZ=0 And ButtonBlink>0 Then ButtonState=3
	If ButtonZ<1 then ' only show lighs under when at 0 hight
		LiHelmet1.state=HelmetLightState
		LiHelmet2.state=HelmetLightState
		LiHelmet3.state=HelmetLightState
	Else
		LiHelmet1.state=0
		LiHelmet2.state=0
		LiHelmet3.state=0
	End If

	If ButtonZ>39 And ButtonState <4 Then
		If ButtonBlink>0 Then
			ButtonBlink=ButtonBlink-1
			LightSeq006.Play SeqBlinking,,1,33
			LiButtonRED.state=1
			DOF 146,2

			Playsound "Relay_off",0,volumedial

			LiButtonRED.timerenabled=1
			Flasherflash001.visible=1
			PlaySound SoundFX("fx_droptarget_solenoid",DOFContactors), 0, .17*VolumeDial, AudioPan(Light059), 0.05,0,0,1,AudioFade(Light059)
			If ButtonBlink=0 Then ButtonState=4 : WingLState = 2 : WingRState = 2 : WingL_Blinks = 1 : WingR_Blinks = 1
		End If
	End If

	If ButtonState=1  Then '  wiggle
		ButtonY=ButtonY+ButtonDirection 
		If ButtonZ>0 Then ButtonZ=ButtonZ-1
		If buttonY>80 then If ButtonDirection=2 Then ButtonDirection=-2 : else : ButtonDirection=2
		If ButtonY<-80 then If ButtonDirection=2 Then ButtonDirection=-2 : else : ButtonDirection=2
	End If


	If ButtonState=2 Then ' returntostart
		Primitive039.image="Droid3"
		If ButtonY>0 Then
			ButtonY=ButtonY - 4 : If ButtonY<0 Then ButtonY=0
		Else
			ButtonY=ButtonY + 4 : If ButtonY>359 Then ButtonY=0
		End If
		If ButtonZ>0 Then 
			ButtonZ=Buttonz-3 
			If ButtonZ<20 And GAMEISOVER=1 Then  luttarget=8 ' done
			If ButtonZ<0 Then
				ButtonZ=0
				If WormholeButton Then WormholeButton = False : luttarget=2
			End If
		End If

		If ButtonY=0 And ButtonZ=0 Then ButtonState=0 : PlaySound SoundFX("Flipper_Right_Down_3",DOFContactors), 0, .1*VolumeDial, AudioPan(Light059), 0.05,0,0,1,AudioFade(Light059)   ' all done
		If ButtonZ=0 Then If EvasiveState=6 Or EvasiveState=7 Then ButtonState=1 
			' return to wiggle ?
	End If


	If ButtonState=3 Then			' going up
		ButtonZ=ButtonZ+3
		If ButtonZ>40 Then ButtonZ=40 

		If ButtonZ>20 Then luttarget=2

		If ButtonZ = 40 Then ButtonState=0 : PlaySound SoundFX("Flipper_Right_Down_2",DOFContactors), 0, .1*VolumeDial, AudioPan(Light059), 0.05,0,0,1,AudioFade(Light059) ' done
		If ButtonZ > 25 Then Primitive039.image="Droid4" Else Primitive039.image="Droid3"
	End If

	
	If ButtonState>3 Then
		ButtonState=ButtonState+1
		If Buttonstate>20 Then Buttonstate=2
	End If

	Primitive039.RotY=ButtonY
	Primitive039.Z=ButtonZ-25

End Sub

Sub LiButtonRED_Timer
	Flasherflash001.visible=0
	LiButtonRED.timerenabled=0
	LiButtonRED.state=0

End Sub

Sub Light059_timer
	Light059.timerenabled=0
	primitive036.blenddisablelighting = 0.4
End Sub



Sub WallHit
	
	If EvasiveState=6 or EvasiveState=7  Then ' only hitable when moving
		PlaySound SoundFX("Ball_Collide_7",DOFContactors), 0, .47*VolumeDial, AudioPan(Light059), 0.05,0,0,1,AudioFade(Light059)
		PlaySound SoundFX("fx_droptarget",DOFContactors), 0, .23*VolumeDial, AudioPan(Light059), 0.05,0,0,1,AudioFade(Light059)
		PlaySound SoundFX("Expl2",DOFContactors), 0, .1*VolumeDial, AudioPan(Light059), 0.05,0,0,1,AudioFade(Light059)
		Primitive036.image="Droptarget3"
		Light059.state=2 : Primitive036.blenddisablelighting=3
		
		' 30 Long
		' evasiveone start at x=170
		i = int((EvasiveX-170)/30)
		If i<21 Then i=i+1
		movingwall(i+22).state=2 : movingwall(i+22).timerenabled=1 
		Evasivestate=8
		Wallupdate
	End If
End Sub


Dim EvasiveDestroydStatus , EvasiveOneScoring, bossDOF
Dim GideonStatus

Sub EvasiveOne_Timer


    ButtonController

	If EvasiveState>5 Then 
		Wallupdate
	End If

	If EvasiveState=4 Then
		If EvasiveZ >0 Then EvasiveZ = EvasiveZ-6 : p40off001.visible=1

	End If

	If EvasiveState=9 Then If EvasivePause>0 Then EvasivePause=EvasivePause-1 : else : EvasiveState=5
	If EvasiveState=8 Then 
		If EvasiveZ=0 Then
			EvasiveState=9 '  9 = pause down for 40ms x 30
			EvasivePause=44

			If bosslevel=7 Then
				GideonStatus = 1
				EvasiveDestroyd=0
				Evasivestate=4
				WallUpdate   
				For i = 0 to 21
					movingwall(i).collidable=0
				Next

				EvasiveOneScoring=EvasiveOneScoring+100000
				Scoring EvasiveOneScoring

				exit Sub

			End If






			EvasiveDestroyd=EvasiveDestroyd+1
			ButtonState=2
			
			If EvasiveDestroyd<10 then
				bossfights.Play SeqBlinking,,6,50
				If ManualLightSequencer.enabled=0 Then ManualLightSequencer.enabled=1 : MSeqCounter=160 : MSeqBigFL=20
				EvasiveDestroydStatus=EvasiveDestroyd : If Displaybuzy<2 Then Displaybuzy=0
			End If

			'UpdateBossLights
			ResetBossLights
			If EvasiveDestroyd = 6 Then
				EvasiveOneScoring=EvasiveOneScoring+1000000
				EvasiveDestroyd=0
				Evasivestate=4
				WallUpdate   
				Boss1ALLOFF
				DOF 118,0
				'Debug.print "DOF 118, 0"  'apophis
				bossdof=0
				DOF 131,2
				'Debug.print "DOF 131, 2"  'apophis
				If bosslevel=1 Then BOSS1.state=1
				If bosslevel=3 Then BOSS3.state=1
				If bosslevel=5 Then BOSS5.state=1

			Else
				DOF 130,2
				'Debug.print "DOF 131, 2"  'apophis
			End If
		Else
			EvasiveZ = EvasiveZ-6
			p40off001.visible=1

		End If 
	End If



	If EvasiveState=7 Then
		If EvasiveX>170 Then
			If EvasiveDestroyd=5 Then
				EvasiveX=EvasiveX-3
			Else
				EvasiveX=EvasiveX-2
			End If
		Else
			EvasiveState = 6
		End If
	End If

	If EvasiveState=6 Then
		If EvasiveX<770 Then
			If EvasiveDestroyd=5 Then
				EvasiveX=EvasiveX+3
			Else
				EvasiveX=EvasiveX+2
			End If
		Else
			EvasiveState = 7
		End If
	End If



	If EvasiveState=4 Or EvasiveState=6 Or EvasiveState=7 Or EvasiveState=9 Then Primitive036.image="Droptarget1" : Light059.state=0 : Primitive036.blenddisablelighting=0.4
	If Evasivestate=5 Then 
		Primitive036.image="Droptarget3"
		Light059.state=2 : Primitive036.blenddisablelighting=3
		If EvasiveZ = 60 then
			p40off001.visible=0
			ButtonState=1
			EvasiveState = 6+int(rnd(1)*2)
			ResetBossLights
		Else
			EvasiveZ = EvasiveZ + 6
			If EvasiveZ=12 Then PlaySound SoundFX("fx_resetdrop",DOFContactors), 0, .22*VolumeDial, AudioPan(Light059), 0.05,0,0,1,AudioFade(Light059)
		End If
	End If
	Primitive036.X=EvasiveX
	Primitive036.Z=EvasiveZ-39

End Sub

Dim bossylightson
Sub bossdelayblinker_Timer
	bossylightson=bossylightson+1
	Select Case bossylightson
		Case 1,7 ,14,20,26,32 : BossLights(10).state=1 : BossLights(0).state=1 : BossLights(9).state=1 : BossLights(19).state=1
		Case 2,8 ,15,21,27,33 : BossLights(11).state=1 : BossLights(1).state=1 : BossLights(8).state=1 : BossLights(18).state=1
		Case 3,9 ,16,22,28,34 : BossLights(12).state=1 : BossLights(2).state=1 : BossLights(7).state=1 : BossLights(17).state=1
		Case 4,10,17,23,29,35 : BossLights(13).state=1 : BossLights(3).state=1 : BossLights(6).state=1 : BossLights(16).state=1
		Case 5,11,18,24,30,36 : BossLights(14).state=1 : BossLights(4).state=1 : BossLights(5).state=1 : BossLights(15).state=1
		Case 6,13,19,25,31,37 : For i = 0 To 19 : BossLights(i).state=0 : Next
		Case 40 
		if bossactive=1 Then
			DOF 118,1
			'Debug.print "DOF 118, 1"  'apophis
		End If
				bossdelayblinker.enabled=0

				bossylightson=0
'				Light059.intensity=1000
				If BossLevel=1 Then EvasiveState = 5
				If BossLevel=2 Then RaiseBigDT.enabled=1 : BigDTUD=1 : Wall025.collidable=true : Wall026.collidable=true : PlaySound SoundFX("elengine",DOFContactors), 0, 0.8*VolumeDial, AudioPan(Primitive059), 0.05,0,0,1,AudioFade(Primitive059)
				If BossLevel=3 Then EvasiveState = 5
				If BossLevel=4 Then RaiseBigDT.enabled=1 : BigDTUD=1 : Wall025.collidable=true : Wall026.collidable=true : PlaySound SoundFX("elengine",DOFContactors), 0, 0.8*VolumeDial, AudioPan(Primitive059), 0.05,0,0,1,AudioFade(Primitive059)
				If BossLevel=5 Then EvasiveState = 5
				If BossLevel=6 Then RaiseBigDT.enabled=1 : BigDTUD=1 : Wall025.collidable=true : Wall026.collidable=true : PlaySound SoundFX("elengine",DOFContactors), 0, 0.8*VolumeDial, AudioPan(Primitive059), 0.05,0,0,1,AudioFade(Primitive059)
'				If BossLevel=7 Then 
				UpdateBossLights 
		End Select
' 
'1+3+5 = evasiveone
'2=mudhorn
'4=spider
'6=dragon
'7=Darkies

End Sub



Dim BigDTUD, BigDThitsCounter
Sub RaiseBigDT_Timer
	If BigDTUD=1 Then

		If Primitive059.Z >= BigDT_ReloadPos And BigDT_ReloadPos<100 Then
			Primitive059.Z=BigDT_ReloadPos
			PlaySound SoundFX("Metal_Touch_11",DOFContactors), 0, 0.7*VolumeDial, AudioPan(Primitive059), 0.05,0,0,1,AudioFade(Primitive059)
			RaiseBigDT.enabled=0
			Light069.state=0
			Exit Sub
		End If

		If Primitive059.Z>=30 Then
			Primitive059.Z=30
			PlaySound SoundFX("Metal_Touch_11",DOFContactors), 0, 0.7*VolumeDial, AudioPan(Primitive059), 0.05,0,0,1,AudioFade(Primitive059)
			RaiseBigDT.enabled=0
			Light069.state=0
			Exit Sub
		End If
		If Primitive059.Z>-58 Then
			Primitive059.Material="Plastic Amber1"
'			Wall027.visible=0
'			Wall089.visible=1
'			Wall089.sidevisible=1
		End If
		Primitive059.Z = Primitive059.Z + 2
	Else
		If Primitive059.Z<=-58 Then
			Primitive059.Z=-58
			PlaySound SoundFX("Metal_Touch_11",DOFContactors), 0, 0.7*VolumeDial, AudioPan(Primitive059), 0.05,0,0,1,AudioFade(Primitive059)
'			Primitive059.visible=0
			Primitive059.Material="Plastic Amber11"
			RaiseBigDT.enabled=0
'			Wall027.visible=1
'			Wall089.visible=0
'			Wall089.sidevisible=0
			Exit Sub
		End If
			Primitive059.Z = Primitive059.Z - 2
	End If
End Sub


Dim HelmetLightState
Sub UpdateBossLights

	If bosslevel=2 Or bosslevel=4 Or bosslevel=6 Then
		for i = 0 to 9
			If i+1>BigDThitsCounter+2 Then
				BossLights(i+10).state=2
				BossLights(i+30).state=2  ' the red ones
			Else
				BossLights(i+10).state=0
				BossLights(i+30).state=0
				BossLights(i).state=1
				BossLights(i+20).state=1 
			End If
		Next
		HelmetLightState=2		
	End If

	If bosslevel=1 or BossLevel=3 or BossLevel=5 Then
		If EvasiveState=5 Then
			for i = 0 to 9
				BossLights(i+10).state=2
				BossLights(i+30).state=2  ' the red ones
				HelmetLightState=2
			Next
		End If
		If EvasiveState=9 Then
			i=EvasiveDestroyd-1
			If i<5 Then
				BossLights(i).state=1
				BossLights(9-i).state=1
				BossLights(20+i).state=1
				BossLights(29-i).state=1
				BossLights(10+i).state=0
				BossLights(19-i).state=0
				BossLights(30+i).state=0
				BossLights(39-i).state=0
	
			Else
				HelmetLightState=0
			End If
		End If
	End If
End Sub

Sub ResetBossLights
	If EvasiveState>4 Then
	
		If EvasiveDestroyd<7 Then
			For i = 0 to 9
				BossLights(i+10).state=2
				BossLights(i+30).state=2  ' the red ones
				HelmetLightState=2
			Next
			If EvasiveDestroyd<6 Then
				For i=0 To EvasiveDestroyd-1
					BossLights(i).state=1
					BossLights(9-i).state=1
					BossLights(20+i).state=1
					BossLights(29-i).state=1
					BossLights(10+i).state=0
					BossLights(19-i).state=0
					BossLights(30+i).state=0
					BossLights(39-i).state=0
				Next
			Else
				HelmetLightState=0
			End If
		End If
	End If
End Sub


Sub Wallupdate
	dim i2
	' 30 Long
	' start at 170
	If EvasiveState<6 Or EvasiveState>7 Then
		for i = 0 to 21
			movingwall(i).collidable=0
		Next
	Else	
		i2 = int((EvasiveX-170)/30)
		for i = 0 to 21
			if i=i2 or i=i2+1 or i=i2+2 Then
				movingwall(i).collidable=1
			Else
				movingwall(i).collidable=0
			End If
		next
	End If

End Sub


MovingTarget_Init
Dim movingwall, BossLights
Sub MovingTarget_Init
	movingwall = Array (W1,W2,W3,W4,W5,W6,W7,W8,W9,W10,W11,W12,W13,W14,W15,W16,W17,W18,W19,W20,W21,W22,LW001,LW002,LW003,LW004,LW005,LW006,LW007,LW008,LW009,LW010,LW011,LW012,LW013,LW014,LW015,LW016,LW017,LW018,LW019,LW020,LW021,LW022)
	BossLights = Array (LiBoss001,LiBoss002,LiBoss003,LiBoss004,LiBoss005,LiBoss006,LiBoss007,LiBoss008,LiBoss009,LiBoss010,LiBoss011,LiBoss012,LiBoss013,LiBoss014,LiBoss015,LiBoss016,LiBoss017,LiBoss018,LiBoss019,LiBoss020,_
						LiBoss021,LiBoss022,LiBoss023,LiBoss024,LiBoss025,LiBoss026,LiBoss027,LiBoss028,LiBoss029,LiBoss030,LiBoss031,LiBoss032,LiBoss033,LiBoss034,LiBoss035,LiBoss036,LiBoss037,LiBoss038,LiBoss039,LiBoss040)
	Boss1ALLOFF
End Sub

Sub Boss1ALLOFF
	For i = 0 to 39
		BossLights(i).state=0
	Next
	For i = 0 to 21
		movingwall(i).collidable=0
	Next
End Sub


Dim EvasiveX, EvasiveZ, EvasiveState, EvasiveDestroyd, EvasivePause
Dim Droid1Y, Droid1Z, DroidInitState
Dim Droid2Y, Droid2Z, DroidFlash
EvasiveState=1 : EvasiveX=220 : EvasiveZ=0
Droid1Y=0 : Droid1Z=0
Sub EvasiveInit_Timer
		If EvasiveState = 3 Then If EvasiveZ =0 Then EvasiveState = 4 : Else : EvasiveZ=EvasiveZ-5: End If    
		If EvasiveState = 2 Then If EvasiveZ =60 then EvasiveState = 3 : Else : EvasiveZ = EvasiveZ +5 : End If
		If EvasiveState = 1 Then If EvasiveX > 170 Then EvasiveX=EvasiveX-2 : Else : EvasiveState=2 : End If
		Primitive036.X=EvasiveX
		Primitive036.Z=EvasiveZ-39

	If DroidInitState=1 Then
		Droid1Y=Droid1Y-9 : Droid1Z=Droid1Z-3
		Droid2Y=Droid2Y-9 : Droid2Z=Droid2Z-3
		If Droid1Y=0 Then EvasiveInit.enabled=0 : EvasiveOne.enabled=1 
	End If
	

	If DroidInitState=0 Then
		Droid1Y=Droid1Y+9 : Droid1Z=Droid1Z+3
		Droid2Y=Droid2Y+9 : Droid2Z=Droid2Z+3
		If Droid1Y=360 Then DroidInitState=1
	End If

End Sub

Sub W1_Hit  : LW001.state=2 : LW001.timerenabled=1 : WallHit : End Sub
Sub W2_Hit  : LW002.state=2 : LW002.timerenabled=1 : WallHit : End Sub
Sub W3_Hit  : LW003.state=2 : LW003.timerenabled=1 : WallHit : End Sub
Sub W4_Hit  : LW004.state=2 : LW004.timerenabled=1 : WallHit : End Sub
Sub W5_Hit  : LW005.state=2 : LW005.timerenabled=1 : WallHit : End Sub
Sub W6_Hit  : LW006.state=2 : LW006.timerenabled=1 : WallHit : End Sub
Sub W7_Hit  : LW007.state=2 : LW007.timerenabled=1 : WallHit : End Sub
Sub W8_Hit  : LW008.state=2 : LW008.timerenabled=1 : WallHit : End Sub
Sub W9_Hit  : LW009.state=2 : LW009.timerenabled=1 : WallHit : End Sub
Sub W10_Hit : LW010.state=2 : LW010.timerenabled=1 : WallHit : End Sub
Sub W11_Hit : LW011.state=2 : LW011.timerenabled=1 : WallHit : End Sub
Sub W12_Hit : LW012.state=2 : LW012.timerenabled=1 : WallHit : End Sub
Sub W13_Hit : LW013.state=2 : LW013.timerenabled=1 : WallHit : End Sub
Sub W14_Hit : LW014.state=2 : LW014.timerenabled=1 : WallHit : End Sub
Sub W15_Hit : LW015.state=2 : LW015.timerenabled=1 : WallHit : End Sub
Sub W16_Hit : LW016.state=2 : LW016.timerenabled=1 : WallHit : End Sub
Sub W17_Hit : LW017.state=2 : LW017.timerenabled=1 : WallHit : End Sub
Sub W18_Hit : LW018.state=2 : LW018.timerenabled=1 : WallHit : End Sub
Sub W19_Hit : LW019.state=2 : LW019.timerenabled=1 : WallHit : End Sub
Sub W20_Hit : LW020.state=2 : LW020.timerenabled=1 : WallHit : End Sub
Sub W21_Hit : LW021.state=2 : LW021.timerenabled=1 : WallHit : End Sub
Sub W22_Hit : LW022.state=2 : LW022.timerenabled=1 : WallHit : End Sub

Sub LW001_Timer : LW001.state=0 : LW001.timerenabled=0 : End Sub
Sub LW002_Timer : LW002.state=0 : LW002.timerenabled=0 : End Sub
Sub LW003_Timer : LW003.state=0 : LW003.timerenabled=0 : End Sub
Sub LW004_Timer : LW004.state=0 : LW004.timerenabled=0 : End Sub
Sub LW005_Timer : LW005.state=0 : LW005.timerenabled=0 : End Sub
Sub LW006_Timer : LW006.state=0 : LW006.timerenabled=0 : End Sub
Sub LW007_Timer : LW007.state=0 : LW007.timerenabled=0 : End Sub
Sub LW008_Timer : LW008.state=0 : LW008.timerenabled=0 : End Sub
Sub LW009_Timer : LW009.state=0 : LW009.timerenabled=0 : End Sub
Sub LW010_Timer : LW010.state=0 : LW010.timerenabled=0 : End Sub
Sub LW011_Timer : LW011.state=0 : LW011.timerenabled=0 : End Sub
Sub LW012_Timer : LW012.state=0 : LW012.timerenabled=0 : End Sub
Sub LW013_Timer : LW013.state=0 : LW013.timerenabled=0 : End Sub
Sub LW014_Timer : LW014.state=0 : LW014.timerenabled=0 : End Sub
Sub LW015_Timer : LW015.state=0 : LW015.timerenabled=0 : End Sub
Sub LW016_Timer : LW016.state=0 : LW016.timerenabled=0 : End Sub
Sub LW017_Timer : LW017.state=0 : LW017.timerenabled=0 : End Sub
Sub LW018_Timer : LW018.state=0 : LW018.timerenabled=0 : End Sub
Sub LW019_Timer : LW019.state=0 : LW019.timerenabled=0 : End Sub
Sub LW020_Timer : LW020.state=0 : LW020.timerenabled=0 : End Sub
Sub LW021_Timer : LW021.state=0 : LW021.timerenabled=0 : End Sub
Sub LW022_Timer : LW022.state=0 : LW022.timerenabled=0 : End Sub



Sub table1_Exit
	If TournamentMode Then
		SaveHighScore2
	else
		SaveHighScore 
	End If

	If FlexDMD And Not VRroom Then
		If Not UMainDMD is Nothing Then
			If UMainDMD.IsRendering Then
				UMainDMD.CancelRendering
			End If
			UMainDMD.Uninit
			UMainDMD = NULL
		End If
	end If
End Sub

Dim BGblink,ballsavedstatus, letterblink
Sub backglasstimer_Timer
	If BGBlink Then
		BGBlink=0
'		backglasstimer.interval = 300

		If HideDesktop=1 Then

			If WaitforInitialsstatus>0 And WaitforInitialsstatus<55 Then controller.B2SSetScorePlayer1 P1score 
			Controller.B2sSetData hsover,1   
			If GameOverStatus>0 Then controller.B2SSetData 11,1
			If BallSaved.enabled=1 Then  controller.B2SSetData 12,1
			If BallinPlay > 0 Then controller.B2SSetData 13,1
			If BallinPlay = 1 Then  controller.B2SSetData 14,1 
			If BallinPlay = 2 Then  controller.B2SSetData 15,1
			If BallinPlay = 3 Then  controller.B2SSetData 16,1 
			If Currentplayer > 0 Then controller.B2SSetData 40,1
			If Currentplayer = 1 Then controller.B2SSetData 41,1' Else controller.B2SSetData 41,0
			If Currentplayer = 2 Then controller.B2SSetData 42,1' Else controller.B2SSetData 42,0
			If Currentplayer = 3 Then controller.B2SSetData 43,1' Else controller.B2SSetData 43,0
			If Currentplayer = 4 Then controller.B2SSetData 44,1' Else controller.B2SSetData 44,0

		End If
		If letterblink=1 Then displayletters : letterblink = 0 
		If letterblink>1 Then
			Licenter001.state=1 : LiCenter015.state=1 : LiCenter015.timerenabled=1
			Licenter002.state=1 : LiCenter016.state=1 : LiCenter016.timerenabled=1
			Licenter003.state=1 : LiCenter017.state=1 : LiCenter017.timerenabled=1
			Licenter004.state=1 : LiCenter018.state=1 : LiCenter018.timerenabled=1
			Licenter005.state=1 : LiCenter019.state=1 : LiCenter019.timerenabled=1
			Licenter006.state=1 : LiCenter020.state=1 : LiCenter020.timerenabled=1
			Licenter007.state=1 : LiCenter021.state=1 : LiCenter021.timerenabled=1
			Licenter008.state=1 : LiCenter022.state=1 : LiCenter022.timerenabled=1
			Licenter009.state=1 : LiCenter023.state=1 : LiCenter023.timerenabled=1
			Licenter010.state=1 : LiCenter024.state=1 : LiCenter024.timerenabled=1
			Licenter011.state=1 : LiCenter025.state=1 : LiCenter025.timerenabled=1
			Licenter012.state=1 : LiCenter026.state=1 : LiCenter026.timerenabled=1
			If HideDesktop=1 Then
				Controller.B2sSetData 17,1
				Controller.B2sSetData 18,1
				Controller.B2sSetData 19,1
				Controller.B2sSetData 20,1
				Controller.B2sSetData 21,1
				Controller.B2sSetData 22,1
				Controller.B2sSetData 23,1
				Controller.B2sSetData 24,1
				Controller.B2sSetData 25,1
				Controller.B2sSetData 26,1
				Controller.B2sSetData 27,1
				Controller.B2sSetData 28,1
			End If
		End If
	Else
		If HideDesktop=1 And WaitforInitialsstatus>0 And WaitforInitialsstatus<55 Then 	controller.B2SSetScorePlayer1 -1
		If letterblink>1 Then
			letterblink=letterblink-1
			Licenter001.state=0 : LiCenter015.state=0
			Licenter002.state=0 : LiCenter016.state=0
			Licenter003.state=0 : LiCenter017.state=0 
			Licenter004.state=0 : LiCenter018.state=0 
			Licenter005.state=0 : LiCenter019.state=0 
			Licenter006.state=0 : LiCenter020.state=0 
			Licenter007.state=0 : LiCenter021.state=0 
			Licenter008.state=0 : LiCenter022.state=0 
			Licenter009.state=0 : LiCenter023.state=0 
			Licenter010.state=0 : LiCenter024.state=0 
			Licenter011.state=0 : LiCenter025.state=0
			Licenter012.state=0 : LiCenter026.state=0 
			If HideDesktop=1 Then
				Controller.B2sSetData 17,0
				Controller.B2sSetData 18,0
				Controller.B2sSetData 19,0
				Controller.B2sSetData 20,0
				Controller.B2sSetData 21,0
				Controller.B2sSetData 22,0
				Controller.B2sSetData 23,0
				Controller.B2sSetData 24,0
				Controller.B2sSetData 25,0
				Controller.B2sSetData 26,0
				Controller.B2sSetData 27,0
				Controller.B2sSetData 28,0
			End If

			If letterblink=1 Then bangtimer.enabled=1
		End If

		If HideDesktop=1 Then
			Controller.B2sSetData 1,0   
			Controller.B2sSetData 2,0  
			Controller.B2sSetData 3,0  
			Controller.B2sSetData 4,0  
			Controller.B2sSetData 5,0  
			Controller.B2sSetData 6,0  
			Controller.B2sSetData 7,0  
			Controller.B2sSetData 8,0  
			Controller.B2sSetData 9,0  
			Controller.B2sSetData 10,0  
			controller.B2SSetData 11,0
			controller.B2SSetData 12,0
			controller.B2SSetData 13,0
			controller.B2SSetData 14,0
			controller.B2SSetData 15,0
			controller.B2SSetData 16,0
			controller.B2SSetData 40,0
			controller.B2SSetData 41,0
			controller.B2SSetData 42,0
			controller.B2SSetData 43,0
			controller.B2SSetData 44,0
		End If
'		backglasstimer.interval = 150
		BGBLink=1
	End If
End Sub

dim bigbang
Sub bangtimer_Timer
	bigbang=bigbang+1


	If VRroom Then
		Select Case bigbang
			case 1 : BG_backglass.material = "_noXtraShading1"
					 If VRroomBlink Then Primitive061.blenddisablelighting= 1.0
			case 2 : BG_backglass.material = "_noXtraShading11" : If rnd(1)<.17 Then bigbang=5
					 If VRroomBlink Then Primitive061.blenddisablelighting= 1.5
			case 3 : BG_backglass.material = "_noXtraShading111" : If rnd(1)<.17 Then bigbang=5
					 If VRroomBlink Then Primitive061.blenddisablelighting= 2.0
			case 4 : BG_backglass.material = "_noXtraShading111"
					 If VRroomBlink Then Primitive061.blenddisablelighting= 1.5
			case 5 : BG_backglass.material = "_noXtraShading11"
					 If VRroomBlink Then Primitive061.blenddisablelighting= 1.0
			case 6 : BG_backglass.material = "_noXtraShading1"
					 If VRroomBlink Then Primitive061.blenddisablelighting= 0.5
			case 7 : BG_backglass.material = "_noXtraShading" : bigbang=0 : If rnd(1)<0.25 Then bangtimer.enabled=0 
					 If VRroomBlink Then Primitive061.blenddisablelighting= 0
		End Select
	Elseif HideDesktop=1 Then
		Select Case bigbang
			case 1 : controller.B2SSetData 32,1
			case 2 : controller.B2SSetData 31,1 : If rnd(1)<.17 Then bigbang=5
			case 3 : controller.B2SSetData 30,1 : If rnd(1)<.17 Then bigbang=5
			case 4 : controller.B2SSetData 29,1
			case 5 : controller.B2SSetData 29,0
			case 6 : controller.B2SSetData 30,0
			case 7 : controller.B2SSetData 31,0 : bigbang=0 : If rnd(1)<0.25 Then bangtimer.enabled=0 
		End Select
	End If



End Sub


Sub b2sBHtimer_Timer
'obsolete

'	controller.B2SSetData 17,Licenter001.state
'	controller.B2SSetData 18,Licenter002.state
'	controller.B2SSetData 19,Licenter003.state
'	controller.B2SSetData 20,Licenter004.state
'	controller.B2SSetData 21,Licenter005.state
'	controller.B2SSetData 22,Licenter006.state
'	controller.B2SSetData 23,Licenter007.state
'	controller.B2SSetData 24,Licenter008.state
'	controller.B2SSetData 25,Licenter009.state
'	controller.B2SSetData 26,Licenter010.state
'	controller.B2SSetData 27,Licenter011.state
'	controller.B2SSetData 28,Licenter012.state

	
'	If Licenter001.state=1 Then	controller.B2SSetData 17,1 : Else : controller.B2SSetData 17,0
'	If Licenter002.state=1 Then	controller.B2SSetData 18,1 : Else : controller.B2SSetData 18,0
'	If Licenter003.state=1 Then	controller.B2SSetData 19,1 : Else : controller.B2SSetData 19,0
'	If Licenter004.state=1 Then	controller.B2SSetData 20,1 : Else : controller.B2SSetData 20,0
'	If Licenter005.state=1 Then	controller.B2SSetData 21,1 : Else : controller.B2SSetData 21,0
'	If Licenter006.state=1 Then	controller.B2SSetData 22,1 : Else : controller.B2SSetData 22,0
'	If Licenter007.state=1 Then	controller.B2SSetData 23,1 : Else : controller.B2SSetData 23,0
'	If Licenter008.state=1 Then	controller.B2SSetData 24,1 : Else : controller.B2SSetData 24,0
'	If Licenter009.state=1 Then	controller.B2SSetData 25,1 : Else : controller.B2SSetData 25,0
'	If Licenter010.state=1 Then	controller.B2SSetData 26,1 : Else : controller.B2SSetData 26,0
'	If Licenter011.state=1 Then	controller.B2SSetData 27,1 : Else : controller.B2SSetData 27,0
'	If Licenter012.state=1 Then	controller.B2SSetData 28,1 : Else : controller.B2SSetData 28,0
End Sub


Dim tempX, tempY, P1score, P1scorebonus, skillshot, i, GamesPlayd, LastScore, TodaysTopScore, highscorename(5)
Dim JackPotScoring, JackPotStatus, JackPotScoringStatus
Dim BallinPlay, BIP, HideDesktop,startupstatus



Sub gamestartdelay_Timer
	If MissionBase=1 Then 
		Primitive049.visible=1
		Primitive050.visible=1
		Primitive051.visible=1
		Primitive052.visible=1
		Primitive048.visible=1
	End If
	
	If SpaceStation=1 Then
		Primitive047.visible=1
	End If

	If Slingshotfighters=1 Then 
		Primitive053.visible=1
		Primitive054.visible=1
	End If

	If LeftRampRazer=1 Then
		primitive001.visible=1
	End If

	If RightRampFighter=1 Then
		primitive003.visible=1
	End If


	StartupStatus=1
	talkingrandom(int(rnd(1)*43))
	If TournamentMode Then
		LoadHighScore2
	else
		LoadHighScore 
	End If

	P1Score=LastScore
	If HideDesktop=1 Then 
		controller.B2SSetScorePlayer1 P1score

	End If
	redeyes.enabled=1
	PlayTune(10)
	' just a delay on start Test
	Gameovertimer.interval=5500
	GameOverTimer.enabled=1 
	PlayersPlaying=0
'	Wall089.visible=0
'	wall089.sidevisible=0

	doubletripple=3
	attract1Mode = 1
	hunter(7)=2
	hunter(8)=2
	hunter(9)=2
	hunter(10)=2
	hunter(11)=2
	hunter(12)=2
	LeftFlipper.RotateToStart
	TopFlipper.RotateToStart
	RightFlipper.RotateToStart
	gamestartdelay.enabled=0
	WaitforInitialsstatus=55
	gameisover=1
	kicker001up.enabled=1
	sw51up.enabled=1
	PlaySound SoundFX("springchange",DOFContactors), 0, .88*VolumeDial, AudioPan(sw51), 0.05,0,0,1,AudioFade(sw51)

End Sub

Dim TurningSpeed, TurningSpeedUp, TotalTurned,turningDOF

turningspeed=0
turningspeedup=4


Sub turningtiefighter_Timer
	If turningspeedup>0.15 Then
		PlaySound SoundFX("elengine",DOFContactors), 0, 0.1*VolumeDial, AudioPan(LiShip002), 0.05,0,0,1,AudioFade(LiShip002)
		TurningSpeed=TurningSpeed+0.3
		If TurningSpeed > 9 Then TurningSpeed = 9
		turningspeedup=turningspeedup-0.15
		LiShip001.state=2
		LiShip002.state=0
	Else
		LiShip002.state=1
	End If

	if TurningSpeed>0.1 Then
		If turningDOF=0 Then turningDOF=1 : DOF 113,1 :'Debug.print "DOF 113, 1"  'apophis
		TurningSpeed=TurningSpeed-0.09
		i=tiefighter001.RotY
		i=i+TurningSpeed

		If i>360 Then 
			i=i-360
			TotalTurned=TotalTurned+1
			TieSoundTimer_Timer
		End If
		tiefighter001.RotY=i
	Else
		If turningDOF=1 Then turningDOF=0 : DOF 113,0 :'Debug.print "DOF 113, 0"  'apophis
		LiShip001.state=0
	End If
End Sub

Sub TieSoundTimer_Timer
	If TieSoundTimer.enabled=0 Then
		TieSoundTimer.enabled=1
		PlaySound "TIEflyby" & int(rnd(1)*6)+1 , 0,0.02*BackGlassVolumeDial,AudioPan(LiShip002),0.25,0,0,1,AudioFade(LiShip002)
	Else	
		TieSoundTimer.enabled=0
	End If
End Sub


'****************
'*ApronLights blinking
'****************

Sub Apro006_Timer
	If attract1Mode=0 Then
		LSAproradar.Play SeqRandom,1,,1000
	End If
End Sub

Sub Apro026_Timer
	If	attract1Mode=0 Then
		LSAproSmall.Play SeqRandom,2,,2200
		LSAproSmall.Play SeqBlinking,,2,133
		LSAproSmall.Play SeqRandom,2,,2200
	End If
	LSAproSmall.Play SeqBlinking,,5,33

End Sub


'**** TESTREEL  1st try
Dim RTScore(10),RTDisplayd(10),RTDelay

' spinning reels play catchup
Sub RealTimeScoring

	
'	If RTDelay=0 Then realtimeHighScores
	If HideDesktop=0 Or SHOWDTREELS=1 Then
		If RTdelay=1 Then
			RTdelay=0
			Dim i,i2,i3
			i2=P1Score
			i=Int(i2/1000000000) : RTScore(1)=i : i2=i2-(i*1000000000)
			i=Int(i2/100000000) : RTScore(2)=i : i2=i2-(i*100000000)
			i=Int(i2/10000000) : RTScore(3)=i : i2=i2-(i*10000000)
			i=Int(i2/1000000) : RTScore(4)=i : i2=i2-(i*1000000)
			i=Int(i2/100000) : RTScore(5)=i : i2=i2-(i*100000)
			i=Int(i2/10000) : RTScore(6)=i : i2=i2-(i*10000)
			i=Int(i2/1000) : RTScore(7)=i : i2=i2-(i*1000)
			i=Int(i2/100) : RTScore(8)=i : i2=i2-(i*100)
			i=Int(i2/10) : RTScore(9)=i : i2=i2-(i*10)

			RTScore(10)=i2
			For i3=1 to 10
				If RTScore(i3)*16<>RTDisplayd(i3) Then 
					RTDisplayd(i3)=RTDisplayd(i3)+1
					If RTDisplayd(i3)>159 Then RTDisplayd(i3)=0 
					Digits2(i3-1).SetValue RTDisplayd(i3)+2
				Else
					Digits2(i3-1).SetValue RTDisplayd(i3)

				End If
			Next
		Else
			RTdelay=RTdelay+1
		End If
	End If
End Sub





'*******************
'*** bonuslights ***
Dim bonus0,bonus1,bonus2,bonus3,bonusmultiplyer, FullbonusStatus
bonus0=0 : bonus1=0 : bonus2=0 : bonus3=0 : bonusmultiplyer=0
Sub bonustrigger1_Unhit : bonus1=1 : bonus01seq.play SeqBlinking,,5,30 : Scoring (410) : checkfortree : Light054.intensity=350 : Light054.timerenabled=1 : End Sub
Sub bonustrigger2_Unhit : bonus2=1 : bonus02seq.play SeqBlinking,,5,30 : Scoring (410) : checkfortree : Light045.intensity=350 : Light045.timerenabled=1 : End Sub
Sub bonustrigger3_Unhit : bonus3=1 : bonus03seq.play SeqBlinking,,5,30 : Scoring (410) : checkfortree : Light031.intensity=350 : Light031.timerenabled=1 : End Sub
Sub checkfortree
	PlaySound SoundFX("consolebeeps2",DOFContactors), 0, .67*VolumeDial, AudioPan(bonustrigger2), 0.05,0,0,1,AudioFade(bonustrigger2)
	If skillshot=1 And Not Tilted Then 
		Skillshot=0 
		Plungerispulled=0
		Plunger001.timerenabled=0
		shoottheballstatus=0 

		If bossactive=1 Then DOF 118,1
		rewardskillshot

			LiReplay.state=2 : LiReplay.timerenabled=1 : lireplay001.state = 2 : lireplay.blinkinterval=200 : lireplay001.blinkinterval=200
			pLireplay.blenddisablelighting=5

		Light003.state=0
		Light051.state=2

		LiSkillshot1.state=1 : LiSkillshot1.timerenabled=1 
		LiSkillshot2.state=1 

	End If
	If bonus1+bonus2+bonus3=3 Then
		bonus1=0 : bonus2=0 : bonus3=0 : Libonus3.Timerenabled=1
		Libonus1.state=0 : Libonus2.state=0 : Libonus3.state=0
		bonus01seq.stopplay : bonus01seq.play SeqBlinking,,8,30
		bonus02seq.stopplay : bonus02seq.play SeqBlinking,,8,30
		bonus03seq.stopplay : bonus03seq.play SeqBlinking,,8,30
		bonusmultiplyer=bonusmultiplyer+1 
		If bonusmultiplyer=10 Then 
		bonusmultiplyer=9
			FullbonusStatus=1
			scoring(1000000)
		Else
			Scoring (100000)
		End If
		PlaySound SoundFX("bonusup",DOFContactors), 0,.67*VolumeDial, AudioPan(bonustrigger2), 0.05,0,0,1,AudioFade(bonustrigger2) 
		ObjLevel(7) = 1 : FlasherFlash7_Timer
		'bonusmultiplyerlights
		If bonusmultiplyer=1 Then BonusX001.blinkinterval=25 : BonusX001_Timer
		If bonusmultiplyer=2 Then BonusX002.blinkinterval=25 : BonusX002_Timer
		If bonusmultiplyer=3 Then BonusX003.blinkinterval=25 : BonusX003_Timer
		If bonusmultiplyer=4 Then BonusX004.blinkinterval=25 : BonusX004_Timer
		If bonusmultiplyer=5 Then BonusX005.blinkinterval=25 : BonusX005_Timer
		If bonusmultiplyer=6 Then BonusX006.blinkinterval=25 : BonusX006_Timer
		If bonusmultiplyer=7 Then BonusX007.blinkinterval=25 : BonusX007_Timer
		If bonusmultiplyer=8 Then BonusX008.blinkinterval=25 : BonusX008_Timer
		If bonusmultiplyer=9 Then BonusX009.blinkinterval=25 : BonusX009_Timer
	End If
End Sub

Sub LiSkillshot1_Timer
	LiSkillshot1.state=0 : LiSkillshot1.timerenabled=0 
	LiSkillshot2.state=0 
End Sub

Sub Light054_Timer
	Light054.intensity=10
	Light054.timerenabled=0
End Sub

Sub Light045_Timer
	Light045.intensity=10
	Light045.timerenabled=0
End Sub

Sub Light031_Timer
	Light031.intensity=10
	Light031.timerenabled=0
End Sub

Sub Light042_Timer
	Light042.intensity=10
	Light042.timerenabled=0
End Sub

Sub Light048_Timer
	Light048.intensity=10
	Light048.timerenabled=0
End Sub

Dim SkillShotScoring,SkillShotStatus
Sub rewardskillshot

	If ManualLightSequencer.enabled=0 Then ManualLightSequencer.enabled=1 : MSeqCounter=180 : MSeqBigFL=40
	If enterlight.state=0 Then enterlight.state=1
	If mysterylight.state=0 Then mysterylight.state=1
	SkillShotScoring=SkillShotScoring+250000
	bossblinker.Play SeqBlinking,,12,33
	DOF 217,2 : 'Debug.print "DOF 217, 2"  'apophis
	addletter(1)

	If SSS Then
		SSS=False
		scoring(SkillShotScoring * 2 )
		SkillShotStatus=2
	Else
		scoring(SkillShotScoring)
		SkillShotStatus=1
	End If

	If Lidouble2.state>0 Then  doubleblinker.Play SeqBlinking,,6,80  
	If LiTripple2.state>0 Then trippleblinker.Play SeqBlinking,,6,80 



	Stoptalking : PlaySound "thisistheway0",0,1*BackGlassVolumeDial
	ObjLevel(1) = 1 : FlasherFlash1_Timer
	ObjLevel(2) = 1 : FlasherFlash2_Timer
	ObjLevel(3) = 1 : FlasherFlash3_Timer : 	DOF 135,2 : 'Debug.print "DOF 135, 2" ' blue left  'apophis
	ObjLevel(4) = 1 : FlasherFlash4_Timer : 	DOF 137,2 : 'Debug.print "DOF 137, 2" 'red left
	ObjLevel(5) = 1 : FlasherFlash5_Timer
	ObjLevel(6) = 1 : FlasherFlash6_Timer
	ObjLevel(7) = 1 : FlasherFlash7_Timer

End Sub

Sub BonusX001_Timer 
	If BonusX001.TimerEnabled=0 Then 
		BonusX001.TimerEnabled=1
'		BonusX001.intensity=200
		BonusX001.state=2
	Else
'		BonusX001.intensity=40
		BonusX001.blinkinterval=40
		BonusX001.state=1
		BonusX001.timerenabled=0
	End If
End Sub

Sub BonusX002_Timer 
	If BonusX002.TimerEnabled=0 Then 
		BonusX002.TimerEnabled=1
'		BonusX002.intensity=200
		BonusX002.state=2
	Else
'		BonusX002.intensity=40
		BonusX002.blinkinterval=40
		BonusX002.state=1
		BonusX002.timerenabled=0
	End If
End Sub

Sub BonusX003_Timer 
	If BonusX003.TimerEnabled=0 Then 
		BonusX003.TimerEnabled=1
'		BonusX003.intensity=200
		BonusX003.state=2
	Else
'		BonusX003.intensity=40
		BonusX003.blinkinterval=40
		BonusX003.state=1
		BonusX003.timerenabled=0
	End If
End Sub

Sub BonusX004_Timer 
	If BonusX004.TimerEnabled=0 Then 
		BonusX004.TimerEnabled=1
'		BonusX004.intensity=200
		BonusX004.state=2
	Else
'		BonusX004.intensity=40
		BonusX004.blinkinterval=40
		BonusX004.state=1
		BonusX004.timerenabled=0
	End If
End Sub

Sub BonusX005_Timer 
	If BonusX005.TimerEnabled=0 Then 
		BonusX005.TimerEnabled=1
'		BonusX005.intensity=200
		BonusX005.state=2
	Else
'		BonusX005.intensity=40
		BonusX005.blinkinterval=40
		BonusX005.state=1
		BonusX005.timerenabled=0
	End If
End Sub

Sub BonusX006_Timer 
	If BonusX006.TimerEnabled=0 Then 
		BonusX006.TimerEnabled=1
'		BonusX006.intensity=200
		BonusX006.state=2
	Else
'		BonusX006.intensity=40
		BonusX006.blinkinterval=40
		BonusX006.state=1
		BonusX006.timerenabled=0
	End If
End Sub

Sub BonusX007_Timer 
	If BonusX007.TimerEnabled=0 Then 
		BonusX007.TimerEnabled=1
'		BonusX007.intensity=200
		BonusX007.state=2
	Else
'		BonusX007.intensity=40
		BonusX007.blinkinterval=40
		BonusX007.state=1
		BonusX007.timerenabled=0
	End If
End Sub

Sub BonusX008_Timer 
	If BonusX008.TimerEnabled=0 Then 
		BonusX008.TimerEnabled=1
'		BonusX008.intensity=200
		BonusX008.state=2
	Else
'		BonusX008.intensity=40
		BonusX008.blinkinterval=40
		BonusX008.state=1
		BonusX008.timerenabled=0
	End If
End Sub

Sub BonusX009_Timer 
	If BonusX009.TimerEnabled=0 Then 
		BonusX009.TimerEnabled=1
'		BonusX009.intensity=200
		BonusX009.state=2
	Else
'		BonusX009.intensity=40
		BonusX009.blinkinterval=40
		BonusX009.state=1
		BonusX009.timerenabled=0
	End If
End Sub



Sub Libonus3_Timer : Libonus3.Timerenabled=0 : ObjLevel(7) = 1 : FlasherFlash7_Timer : End Sub


'**********************
'*** quickmultiball ***
Dim quickMB,quickMBblink,quickMBspark,quickMBactive,quickMBstatus
quickMB=0 : quickMBblink=0 : quickMBactive=0

quickMBblink=4 : LiCenter001.timerenabled=1
	
dim lettercount,letterpause
Sub addletter(temp)
	turningspeedup=turningspeedup+4 
	If turningspeedup > 12 Then turningspeedup = 12
	lettercount=lettercount+temp
End Sub

dim letterscoring,letterscoringstatus


Sub LiCenter013_Timer
	if letterpause>0 then 
		letterpause=letterpause-1 
	Else
		If lettercount>0 then 
			letterpause=5
			lettercount=lettercount-1
			If lilock1.state=2 Then locksequencer.Play seqblinking,,2,50
			If lilock2.state=2 Then multiballsequencer.Play seqblinking,,2,50
			If enterlight.state=>0 Then Enterseq.StopPlay : EnterSeq.Play Seqblinking,,2,35
			If mysterylight.state>0 Then Mysteryseq.StopPlay : MysterySeq.Play Seqblinking,,2,35
			If extraballlight.state>0 Then Extraseq.StopPlay : ExtraSeq.Play Seqblinking,,2,35
			If MultiballActive=1 Or BIP=2 Then
				letterscoring=letterscoring+2000
				scoring(letterscoring*P1scorebonus)
				letterscoringstatus=1
			Else
				If quickMB<12 Then 
					quickMB=quickMB+1
					Select Case quickMB
						Case 1  : nameflash001.Play SeqBlinking,,11,40
						Case 2  : nameflash002.Play SeqBlinking,,11,40
						Case 3  : nameflash003.Play SeqBlinking,,11,40
						Case 4  : nameflash004.Play SeqBlinking,,11,40
						Case 5  : nameflash005.Play SeqBlinking,,11,40
						Case 6  : nameflash006.Play SeqBlinking,,11,40
						Case 7  : nameflash007.Play SeqBlinking,,11,40
						Case 8  : nameflash008.Play SeqBlinking,,11,40
						Case 9  : nameflash009.Play SeqBlinking,,11,40
						Case 10 : nameflash010.Play SeqBlinking,,11,40
						Case 11 : nameflash011.Play SeqBlinking,,11,40
						Case 12 : nameflash012.Play SeqBlinking,,11,40
					End Select
					PlaySound SoundFX("blang2",DOFContactors), 0, 0.33*VolumeDial, AudioPan(opengate), 0.05,0,0,1,AudioFade(opengate)
					PlaySound SoundFX("guitarstring",DOFContactors), 0, .67*VolumeDial, AudioPan(closegate), 0.05,0,0,1,AudioFade(closegate)
				End If
			End If
			displayletters
		End If
	End If
end Sub

Dim MaxBallsScoring,MaxBallsStatus

Sub displayletters
	If quickMB>0 Then LiCenter001.state=1 :  LiCenter015.state=1 : LiCenter015.timerenabled=1 : Else : LiCenter001.state=0 : End If
	If quickMB>1 Then LiCenter002.state=1 :  LiCenter016.state=1 : LiCenter016.timerenabled=1 :Else : LiCenter002.state=0 : End If
	If quickMB>2 Then LiCenter003.state=1 :  LiCenter017.state=1 : LiCenter017.timerenabled=1 :Else : LiCenter003.state=0 : End If
	If quickMB>3 Then LiCenter004.state=1 :  LiCenter018.state=1 : LiCenter018.timerenabled=1 :Else : LiCenter004.state=0 : End If
	If quickMB>4 Then LiCenter005.state=1 :  LiCenter019.state=1 : LiCenter019.timerenabled=1 :Else : LiCenter005.state=0 : End If
	If quickMB>5 Then LiCenter006.state=1 :  LiCenter020.state=1 : LiCenter020.timerenabled=1 :Else : LiCenter006.state=0 : End If
	If quickMB>6 Then LiCenter007.state=1 :  LiCenter021.state=1 : LiCenter021.timerenabled=1 :Else : LiCenter007.state=0 : End If
	If quickMB>7 Then LiCenter008.state=1 :  LiCenter022.state=1 : LiCenter022.timerenabled=1 :Else : LiCenter008.state=0 : End If
	If quickMB>8 Then LiCenter009.state=1 :  LiCenter023.state=1 : LiCenter023.timerenabled=1 :Else : LiCenter009.state=0 : End If
	If quickMB>9 Then LiCenter010.state=1 :  LiCenter024.state=1 : LiCenter024.timerenabled=1 :Else : LiCenter010.state=0 : End If
	If quickMB>10 Then LiCenter011.state=1 :  LiCenter025.state=1 : LiCenter025.timerenabled=1 :Else : LiCenter011.state=0 : End If
	If quickMB>11 Then LiCenter012.state=1 :  LiCenter026.state=1 : LiCenter026.timerenabled=1 :Else : LiCenter012.state=0 : End If
	If HideDesktop=1 Then
		If quickMB>0 Then Controller.B2SSetData 17,1 : else : Controller.B2SSetData 17,0 : End If
		If quickMB>1 Then Controller.B2SSetData 18,1 : else : Controller.B2SSetData 18,0 : End If
		If quickMB>2 Then Controller.B2SSetData 19,1 : else : Controller.B2SSetData 19,0 : End If
		If quickMB>3 Then Controller.B2SSetData 20,1 : else : Controller.B2SSetData 20,0 : End If
		If quickMB>4 Then Controller.B2SSetData 21,1 : else : Controller.B2SSetData 21,0 : End If
		If quickMB>5 Then Controller.B2SSetData 22,1 : else : Controller.B2SSetData 22,0 : End If
		If quickMB>6 Then Controller.B2SSetData 23,1 : else : Controller.B2SSetData 23,0 : End If
		If quickMB>7 Then Controller.B2SSetData 24,1 : else : Controller.B2SSetData 24,0 : End If
		If quickMB>8 Then Controller.B2SSetData 25,1 : else : Controller.B2SSetData 25,0 : End If
		If quickMB>9 Then Controller.B2SSetData 26,1 : else : Controller.B2SSetData 26,0 : End If
		If quickMB>10 Then Controller.B2SSetData 27,1 : else : Controller.B2SSetData 27,0 : End If
		If quickMB>11 Then Controller.B2SSetData 28,1 : else : Controller.B2SSetData 28,0 : End If
	End If



	If quickMB=12 Then
		DOF 220,2
'Debug.print "DOF 220, 2"  'apophis
		quickMB=0
		YellowBorders
			nameflash012.StopPlay
			nameflash011.StopPlay
			nameflash010.StopPlay
			nameflash009.StopPlay
			nameflash008.StopPlay
			middleblinker.Play SeqBlinking,,10,40
			plungerlane.play SeqBlinking,,4,25
			DOF 202,2 : 'Debug.print "DOF 202, 2"  'apophis

			' maxballs reached =?
		If BIP > 1 Then 
			MaxBallsScoring=MaxBallsScoring+2000000
			MaxBallsStatus=1
			scoring(MaxBallsScoring)
			If Lidouble2.state>0 Then  doubleblinker.Play SeqBlinking,,5,50  
			If LiTripple2.state>0 Then trippleblinker.Play SeqBlinking,,5,50 
		Else
			quickMBactive=1
			quickMBstatus=1
			bangtimer.enabled=1
			redeyes.enabled=1
			PlaySound SoundFX("advancedwarp",DOFContactors), 0, 0.41*VolumeDial, AudioPan(opengate), 0.05,0,0,1,AudioFade(opengate)
			'Plunger.CreateBall
			If GAMEISOVER=0 Then
				BallRelease.CreateBall
				PlaySound SoundFX("BallRelease" & int(rnd(1)*7)+1 , DOFContactors), 0,.77*VolumeDial,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)
				BallRelease.Kick 90, 5 : DOF 145,2  : luttarget=2
				PlaySound SoundFX("fx_kicker",DOFContactors), 0,0.33*VolumeDial,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)
			End If
			autokicker.enabled=1
			li21.state=1
			KickbackFL.visible=1 : KickbackFL.timerenabled=1 : PlaySound SoundFX("fx_apron",DOFContactors), 0, 0.33*VolumeDial, AudioPan(li21), 0.05,0,0,1,AudioFade(li21)
			LiSpesialLeft.state=0
			LiSpesialLeftFL.visible=1 : LiSpesialLeftFL.timerenabled=1
			BIP = BIP + 1
			Gate002.twoway=false


			If WizardLevel < 3 Then
				LiReplay.state=0 : lireplay001.state = 0 : lireplay.blinkinterval=200 : lireplay001.blinkinterval=200
'				pLireplay.blenddisablelighting=1

				LiReplay2.state=0
				LiReplay2.timerenabled=0
				LiReplay.timerenabled=0
				'If Not TournamentMode Then
					LiReplay.state=2 : lireplay001.state = 2
					pLireplay.blenddisablelighting=5
					LiReplay.timerenabled=1 : lireplay.blinkinterval=200 : lireplay001.blinkinterval=200
				'End If
			End If

			Light003.state=0
			Light051.state=2
		End If
			Licenter001.state=2 : LiCenter015.state=1 : LiCenter015.timerenabled=1
			Licenter002.state=2 : LiCenter016.state=1 : LiCenter016.timerenabled=1
			Licenter003.state=2 : LiCenter017.state=1 : LiCenter017.timerenabled=1
			Licenter004.state=2 : LiCenter018.state=1 : LiCenter018.timerenabled=1
			Licenter005.state=2 : LiCenter019.state=1 : LiCenter019.timerenabled=1
			Licenter006.state=2 : LiCenter020.state=1 : LiCenter020.timerenabled=1
			Licenter007.state=2 : LiCenter021.state=1 : LiCenter021.timerenabled=1
			Licenter008.state=2 : LiCenter022.state=1 : LiCenter022.timerenabled=1
			Licenter009.state=2 : LiCenter023.state=1 : LiCenter023.timerenabled=1
			Licenter010.state=2 : LiCenter024.state=1 : LiCenter024.timerenabled=1
			Licenter011.state=2 : LiCenter025.state=1 : LiCenter025.timerenabled=1
			Licenter012.state=2 : LiCenter026.state=1 : LiCenter026.timerenabled=1
		LiCenter002.timerenabled=1

	End If
End Sub

Sub LiCenter002_Timer
			Licenter001.state=0 : LiCenter015.state=1 : LiCenter015.timerenabled=1
			Licenter002.state=0 : LiCenter016.state=1 : LiCenter016.timerenabled=1
			Licenter003.state=0 : LiCenter017.state=1 : LiCenter017.timerenabled=1
			Licenter004.state=0 : LiCenter018.state=1 : LiCenter018.timerenabled=1
			Licenter005.state=0 : LiCenter019.state=1 : LiCenter019.timerenabled=1
			Licenter006.state=0 : LiCenter020.state=1 : LiCenter020.timerenabled=1
			Licenter007.state=0 : LiCenter021.state=1 : LiCenter021.timerenabled=1
			Licenter008.state=0 : LiCenter022.state=1 : LiCenter022.timerenabled=1
			Licenter009.state=0 : LiCenter023.state=1 : LiCenter023.timerenabled=1
			Licenter010.state=0 : LiCenter024.state=1 : LiCenter024.timerenabled=1
			Licenter011.state=0 : LiCenter025.state=1 : LiCenter025.timerenabled=1
			Licenter012.state=0 : LiCenter026.state=1 : LiCenter026.timerenabled=1
	LiCenter002.timerenabled=0
End Sub

Sub LiCenter015_Timer : LiCenter015.state=0 : licenter015.timerenabled=0 : End Sub
Sub LiCenter016_Timer : LiCenter016.state=0 : licenter016.timerenabled=0 : End Sub
Sub LiCenter017_Timer : LiCenter017.state=0 : licenter017.timerenabled=0 : End Sub
Sub LiCenter018_Timer : LiCenter018.state=0 : licenter018.timerenabled=0 : End Sub
Sub LiCenter019_Timer : LiCenter019.state=0 : licenter019.timerenabled=0 : End Sub
Sub LiCenter020_Timer : LiCenter020.state=0 : licenter020.timerenabled=0 : End Sub
Sub LiCenter021_Timer : LiCenter021.state=0 : licenter021.timerenabled=0 : End Sub
Sub LiCenter022_Timer : LiCenter022.state=0 : licenter022.timerenabled=0 : End Sub
Sub LiCenter023_Timer : LiCenter023.state=0 : licenter023.timerenabled=0 : End Sub
Sub LiCenter024_Timer : LiCenter024.state=0 : licenter024.timerenabled=0 : End Sub
Sub LiCenter025_Timer : LiCenter025.state=0 : licenter025.timerenabled=0 : End Sub
Sub LiCenter026_Timer : LiCenter026.state=0 : licenter026.timerenabled=0 : End Sub



Sub autokicker_Hit
	kickertimer.enabled=1
	plunger001.pullspeed=0.33
	plunger001.pullback
	PlaySound "fx_plungerpull",0,.81*VolumeDial,AudioPan(Plunger001),0.25,0,0,1,AudioFade(Plunger001)
	skillshotrandom.play SeqRandom,3,,1300
	autokickerrandom.play SeqBlinking,,12,33

End Sub
Sub li84_Timer
	li84.timerenabled=0
'	plunger001.firespeed=115
	autokicker.Kick 0,70
End Sub
dim kickerReady
Sub kickertimer_Timer
		kickerReady=0
		plunger001.pullspeed=0.09
		li84.timerenabled=1
'		plunger001.firespeed=155
		plunger001.fire
		kickertimer.enabled=0
		luttarget=2
		PlaySound SoundFX("fx_Plunger",DOFContactors), 0, 0.61*VolumeDial, AudioPan(Plunger001), 0.05,0,0,1,AudioFade(Plunger001)
End Sub

Sub LiCenter014_Timer
	quickMBspark=quickMBspark+1
	If quickMBspark=2 Then
		LiCenter014.state=1 
	Else
		LiCenter014.state=0 : LiCenter014.timerenabled=0 : quickMBspark=0
	End If

End Sub

Sub LiCenter001_Timer
	quickMBblink=quickMBblink+1
	
	If quickMBblink=5 Then LiCenter001.state=0  : LiCenter002.state=0 :  LiCenter003.state=0 : LiCenter004.state=0 : LiCenter005.state=0 : LiCenter006.state=0 : LiCenter007.state=0 : LiCenter008.state=0 : LiCenter009.state=0 : LiCenter010.state=0 : LiCenter011.state=0 : LiCenter012.state=0 : End If
	If quickMBblink=6 Then LiCenter001.state=2  : LiCenter015.state=2 : LiCenter015.timerenabled=1 : End If
	If quickMBblink=8 Then LiCenter001.state=0  : LiCenter002.state=2 : LiCenter016.state=2 : LiCenter016.timerenabled=1 : End If
	If quickMBblink=10 Then LiCenter002.state=0 : LiCenter003.state=2 : LiCenter017.state=2 : LiCenter017.timerenabled=1 : End If
	If quickMBblink=12 Then LiCenter003.state=0 : LiCenter004.state=2 : LiCenter018.state=2 : LiCenter018.timerenabled=1 : End If
	If quickMBblink=14 Then LiCenter004.state=0 : LiCenter005.state=2 : LiCenter019.state=2 : LiCenter019.timerenabled=1 : End If
	If quickMBblink=16 Then LiCenter005.state=0 : LiCenter006.state=2 : LiCenter020.state=2 : LiCenter020.timerenabled=1 : End If
	If quickMBblink=18 Then LiCenter006.state=0 : LiCenter007.state=2 : LiCenter021.state=2 : LiCenter021.timerenabled=1 : End If
	If quickMBblink=20 Then LiCenter007.state=0 : LiCenter008.state=2 : LiCenter022.state=2 : LiCenter022.timerenabled=1 : End If
	If quickMBblink=22 Then LiCenter008.state=0 : LiCenter009.state=2 : LiCenter023.state=2 : LiCenter023.timerenabled=1 : End If
	If quickMBblink=24 Then LiCenter009.state=0 : LiCenter010.state=2 : LiCenter024.state=2 : LiCenter024.timerenabled=1 : End If
	If quickMBblink=26 Then LiCenter010.state=0 : LiCenter011.state=2 : LiCenter025.state=2 : LiCenter025.timerenabled=1 : End If
	If quickMBblink=28 Then LiCenter011.state=0 : LiCenter012.state=2 : LiCenter026.state=2 : LiCenter026.timerenabled=1 : End If
	If quickMBblink=30 Then LiCenter012.state=0  : End If
	If quickMBblink=32 Then displayletters : End If

	If quickMBblink=300 Then quickMBblink=4 : End If


End Sub



'**************************************************************
'*** right+lefit ramps and left/rightlanes + skillshot lane ***
Dim lasthit

Dim ScoringRightRamp,RightRampStatus


Dim ComboTotalRamps,RampsTotal,ComboStatus,ComboLastRamp

Sub ComboTimer_Timer
	ComboLastRamp=0
	ComboTimer.enabled=0
End Sub

Dim LockIsLitStatus,LockedBalls,RampsNeededForLockLight,RampsDoneForLockLight
RampsNeededForLockLight=1

Sub Lilock1_Timer
	locksequencer.play seqblinking,,3,50
	LiLock2.state=0
	multiballsequencer.play seqblinking,,3,50
	LiLock1.state=0
	LiLock1.timerenabled=0
End Sub



Dim LockoffCount : lockoffcount=0
Sub LiLock001_Timer ' stop locklite after 15 sec ( is restarted every ramp hit )
	
	LockoffCount=LockoffCount + 1
	lilock1.blinkinterval = 110-lockoffcount*5.5

	If lockoffcount >= 15 Then
		lockoffcount=0 : lilock1.blinkinterval = 110
		LiLock001.timerenabled=0  

		If MultiBallActive=0 And LockedBalls<2 Then ' really should not be needed but for safety ?
			If LockedBalls=0 Then LOCKED003.State=0
			PlaySound "saberoff", 0, .3*BackGlassVolumeDial, AudioPan(sw51), 0.05,0,0,1,AudioFade(sw51)
			If LockedBalls=1 Then LOCKED002.State=0
			LiLock1.state=0
			sw51.enabled=1
			sw001.enabled=0
			sw002.enabled=0
			Sw51up.enabled=1 ' up
			Sw51down.enabled=0
			PlaySound SoundFX("springchange",DOFContactors), 0, .88*VolumeDial, AudioPan(sw51), 0.05,0,0,1,AudioFade(sw51)
			LiRefuel001.state=0
		End If

	End If

End Sub



Sub LiteLock
	If MultiBallActive=0 Then
		RampsDoneForLockLight=RampsDoneForLockLight+1
		If RampsDoneForLockLight>=RampsNeededForLockLight Then 
			If LiLock1.state=0 Then
				LiLock001.timerenabled=0 : lockoffcount=0 : lilock1.blinkinterval = 110
				LiLock001.timerenabled=1 ' restart 

				RampsDoneForLockLight=0
				LiLock1.state=2   'sw51
				If LockedBalls=0 Then LOCKED003.State=2 :' LOCKED004.state=0
				If LockedBalls=1 Then LOCKED002.State=2 :' LOCKED005.state=0
				If LockedBalls=2 Then LOCKED001.State=2 :' LOCKED006.state=0
			
				locksequencer.play seqblinking,,3,50

				If lockedBalls=2 Then
					LiLock001.timerenabled=0  : lockoffcount=0 : lilock1.blinkinterval = 110
					sw51.enabled=1
					sw001.enabled=0
					sw002.enabled=0
					LockIsLitStatus=2
					LiLock2.state=2
					LiRefuel001.state=2
					multiballsequencer.play seqblinking,,3,50
				Else

					Sw51up.enabled=0
					Sw51down.enabled=1
					PlaySound SoundFX("springchange",DOFContactors), 0, .88*VolumeDial, AudioPan(sw51), 0.05,0,0,1,AudioFade(sw51)
					sw51downtarget = -30
					Primitive055.z=0
					sw51.enabled=0
					sw001.enabled=1
					sw002.enabled=1
					LockIsLitStatus=1
				End if
			End If
		End If
	Else
		LiLock1.state=0 'if light is still on turn it off here :
	End If
End Sub
Dim redblinks
Sub Redeyes_Timer
	redblinks=redblinks+1

	If VRroom Then
		Select Case redblinks
			Case 1,3,5,7,9,11,15,19 : bg_letters013.visible = 0 : bg_letters014.visible = 0
			Case 2,4,6,8,10,13,17,21 :bg_letters013.visible = 1 : bg_letters014.visible = 1
			Case 22 : redblinks=0 : redeyes.enabled=0
		End Select
	Elseif hidedesktop = 1 Then

		Select Case redblinks
			Case 1,3,5,7,9,11,15,19 : controller.B2SSetData 34,1 : Controller.B2SSetData 33,1
			Case 2,4,6,8,10,13,17,21 : controller.B2SSetData 34,0 : Controller.B2SSetData 33,0
			Case 22 : redblinks=0 : redeyes.enabled=0
		End Select
	End If
End Sub
Dim basementflash
Sub Flasher001_Timer
	If flasher001.timerenabled=0 Then
		flasher001.opacity=4000
		flasher001.visible=1
		flasher001.timerenabled=1
		
	Else
		i=flasher001.opacity
		i=i-750
		If i>0 Then
			flasher001.opacity=i
		Else
			flasher001.visible=0
			flasher001.timerenabled=0
		End If
	End If
End Sub

Sub LiMandoblink_Timer
	LiMandoblink.state=0
	LiMandoblink.Timerenabled=False
End Sub
Sub LiMandoblinkr_Timer
	LiMandoblinkr.state=0
	LiMandoblinkr.Timerenabled=False
End Sub

Sub leftrampend_hit : LiMandoblink.Timerenabled=True : End Sub
Sub Rightrampend_hit : LiMandoblinkR.Timerenabled=True : End Sub



Sub leftrampend_unhit : WireRampOff : PlaySound SoundFX("WireRamp_Stop",DOFContactors), 1, 0.5*VolumeDial, AudioPan(leftrampend), 0.05,0,0,0,AudioFade(leftrampend) : End Sub
Sub Rightrampend_unhit : WireRampOff : PlaySound SoundFX("WireRamp_Stop",DOFContactors), 1, 0.5*VolumeDial, AudioPan(rightrampend), 0.05,0,0,0,AudioFade(rightrampend) : End Sub


Sub Rightrampscore_UnHit
	WireRampOn False ' On Wire Ramp Pay Wire Ramp Sound
End Sub


Sub rightrampscore_Hit
	DTBLINK=10
	LiMandoblinkR.state=2

	LightSeq006.Play SeqBlinking,,2,40
	WireRampOff

	LightUnderDisp1.state=2
	LightUnderDisp2.state=2
	LightUnderDisp1.timerenabled=True

	Flasher001_Timer
	turningspeedup=turningspeedup+5
	plungerlane.play SeqBlinking,,2,25
	DOF 202,2: 'Debug.print "DOF 202, 2"  'apophis
	bossblinker.Play SeqBlinking,,3,40
	LightSeq002.Play SeqBlinking,,1,88
	If ManualLightSequencer.enabled=0 Then ManualLightSequencer.enabled=1 : MSeqCounter=180 : MSeqBigFL=10
	LightSeq005.Play SeqBlinking,,3,40

	RampsTotal=RampsTotal+1

	If ScoringRightRamp < 150000 Then
		ScoringRightRamp=ScoringRightRamp+10000
	Else
		ScoringRightRamp=ScoringRightRamp+5000
	End If

	Redcockpit.play Seqblinking,,2,100
	If mission(4)=1 Then 
		If missionstatus(4)=0 Then missionstatus(4)=2 : LiSupply004.state=0 : SupplySeq4.play seqblinking,,10,33  : End If
		If missionstatus(4)=1 Then missionstatus(4)=3 : LiSupply004.state=0 : SupplySeq4.play seqblinking,,10,33  : mission4update : End If
	End If
 


	If boss7countdown > 0 Then
		boss7ramps=boss7ramps+1
		boss7countdown=boss7countdown+3
		DOF 143,2 : 'Debug.print "DOF 143, 2"  'apophis
	Else
		DOF 114,2
		DOF 115,2
		DOF 116,2
		DOF 204,2
		'Debug.print "DOF 114, 2"  'apophis
		'Debug.print "DOF 115, 2"  'apophis
		'Debug.print "DOF 116, 2"  'apophis
		'Debug.print "DOF 204, 2"  'apophis
	End If

	backgroundpop_Timer

	If JP001.state=2 Then
		Redcockpit.play Seqblinking,,3,100
		Light061_Timer
		DOF 205,2
		'Debug.print "DOF 205, 2"  'apophis
		plungerlane.play SeqBlinking,,5,25
		DOF 202,2: 'Debug.print "DOF 202, 2"  'apophis
			bangtimer.enabled=1
			redeyes.enabled=1
		LightSeq002.Play SeqBlinking,,1,88
		DisplayBuzy=0
		Displaytext "            ","            "
		If boss7countdown > 0 Then
			scoring(JackPotScoring*3)
			Boss1_Timer
			boss7status=1
			Stoptalking : PlaySound "DarkWalking",0,0.5*BackGlassVolumeDial
		Else
			scoring(JackPotScoring)
			JackPotScoringStatus=1
			Stoptalking : PlaySound "jackpot2",0,BackGlassVolumeDial
		End If

		If Lidouble2.state>0 Then  doubleblinker.Play SeqBlinking,,10,40  
		If LiTripple2.state>0 Then trippleblinker.Play SeqBlinking,,10,40  
		JPsequencer.play seqblinking,,10,25

		If ComboLastRamp=2 Then
			ScoringRightRamp=ScoringRightRamp+5000
			ScoringLeftRamp=ScoringLeftRamp+5000
			ComboStatus=1
			ComboTotalRamps=ComboTotalRamps+1
		End If
	Else

		If ComboLastRamp=2 Then
			ScoringRightRamp=ScoringRightRamp+5000
			ScoringLeftRamp=ScoringLeftRamp+5000
			If boss7countdown > 0 Then
				Boss1_Timer
				scoring(JackPotScoring*2+ScoringLeftRamp+ScoringRightRamp)
				boss7status=4
			Else
				scoring(ScoringRightRamp+ScoringLeftRamp)
				ComboStatus=1
			End If

			ComboTotalRamps=ComboTotalRamps+1
		Else
			If boss7countdown > 0 Then
				Boss1_Timer
				scoring(JackPotScoring*2+ScoringRightRamp)
				boss7status=3
			Else
			scoring(ScoringRightRamp)
			RightRampStatus=1
			End If
		End If
	End If

	' lite lock
	LiteLock
	ComboTimer.enabled=0 
	ComboLastRamp=1
	ComboTimer.enabled=1 
	lasthit=0
	addletter(1)

	PlaySound SoundFX("clickclick",DOFContactors), 0, 0.6*VolumeDial, AudioPan(rightrampscore), 0.05,0,0,1,AudioFade(rightrampscore)
End Sub

Dim ScoringLeftRamp,LeftRampStatus,boss7status

Sub leftrampscore_UnHit
	WireRampOn False ' On Wire Ramp Pay Wire Ramp Sound
End Sub

Sub leftrampscore_Hit
	DTBLINK=10
	LiMandoblink.state=2

	LightSeq006.Play SeqBlinking,,2,40
	WireRampOff


	LightUnderDisp1.state=2
	LightUnderDisp2.state=2
	LightUnderDisp1.timerenabled=True
	Flasher001_Timer
	turningspeedup=turningspeedup+5
	LightSeq002.Play SeqBlinking,,1,88
	bossblinker.Play SeqBlinking,,3,40
	If ManualLightSequencer.enabled=0 Then ManualLightSequencer.enabled=1 : MSeqCounter=180 : MSeqBigFL=10
	LightSeq005.Play SeqBlinking,,3,40
	plungerlane.play SeqBlinking,,5,25
	DOF 202,2: 'Debug.print "DOF 202, 2"  'apophis
	RampsTotal=RampsTotal+1

	If ScoringLeftRamp < 150000 Then
		ScoringLeftRamp=ScoringLeftRamp+10000
	Else
		ScoringLeftRamp=ScoringLeftRamp+5000
	End If

	Redcockpit.play Seqblinking,,2,100
	If mission(4)=1 Then 
		If missionstatus(4)=0 Then missionstatus(4)=1 : LiSupply002.state=0 : SupplySeq2.play seqblinking,,10,33  : End If
		If missionstatus(4)=2 Then missionstatus(4)=3 : LiSupply002.state=0 : SupplySeq2.play seqblinking,,10,33  : mission4update : End If
	End If
	If boss7countdown > 0 Then
		boss7ramps=boss7ramps+1 
		boss7countdown=boss7countdown+3 
		DOF 143,2 : 'Debug.print "DOF 143, 2"  'apophis
	Else
		DOF 114,2
		DOF 115,2
		DOF 116,2	
		DOF 203,2
		'Debug.print "DOF 114, 2"  'apophis
		'Debug.print "DOF 115, 2"  'apophis
		'Debug.print "DOF 116, 2"  'apophis
		'Debug.print "DOF 203, 2"  'apophis


	End If

	backgroundpop_Timer

	If JP001.state=2 Then
		Redcockpit.play Seqblinking,,3,100
		Light061_Timer
		DOF 205,2
'		debug.print "DOF 205, 2"  'apophis
		plungerlane.play SeqBlinking,,2,25
	

		bangtimer.enabled=1
		redeyes.enabled=1


		LightSeq002.Play SeqBlinking,,1,88

		DisplayBuzy=0
		Displaytext "            ","            "

		If boss7countdown > 0 Then
			scoring(JackPotScoring*3)
			boss7status=1
			Boss1_Timer
		Else
			scoring(JackPotScoring)
			JackPotScoringStatus=1
		End If

		If Lidouble2.state>0 Then doubleblinker.Play SeqBlinking,,10,40  	
		If LiTripple2.state>0 Then trippleblinker.Play SeqBlinking,,10,40 
		JPsequencer.play seqblinking,,10,25

		PlaySound "spaceup", 2, .62*BackGlassVolumeDial, AudioPan(leftrampscore), 0.05,0,0,1,AudioFade(leftrampscore)
		Stoptalking : PlaySound "jackpot2", 2, .64*BackGlassVolumeDial, AudioPan(Drain), 0.05,0,0,1,AudioFade(Drain)
		If ComboLastRamp=1 Then
			ScoringRightRamp=ScoringRightRamp+5000
			ScoringLeftRamp=ScoringLeftRamp+5000

			ComboStatus=1
			ComboTotalRamps=ComboTotalRamps+1
		End If
	Else
		If ComboLastRamp=1 Then
			ScoringRightRamp=ScoringRightRamp+5000
			ScoringLeftRamp=ScoringLeftRamp+5000
			If boss7countdown > 0 Then
				Boss1_Timer
				scoring(JackPotScoring*2+ScoringLeftRamp+ScoringRightRamp)
				boss7status=4
			Else
				scoring(ScoringRightRamp+ScoringLeftRamp)
				ComboStatus=1
			End If
			ComboTotalRamps=ComboTotalRamps+1
		Else
			If boss7countdown > 0 Then
				Boss1_Timer
				scoring(JackPotScoring*2+ScoringLeftRamp)
				boss7status=1
			Else
				scoring(ScoringLeftRamp)
				LeftRampStatus=1
			End If

		End If
	End If
	LiteLock
	ComboTimer.enabled=0 
	ComboLastRamp=2
	ComboTimer.enabled=1
	lasthit=0
	addletter(1)

	PlaySound SoundFX("clickclick",DOFContactors), 0, 0.6*VolumeDial, AudioPan(leftrampscore), 0.05,0,0,1,AudioFade(leftrampscore)
End Sub


Sub leftrampenter_Hit 
	WingLState = 2 : WingL_Blinks = 1

	WireRampOn True 'Play Plastic Ramp Sound
	If lasthit=1 Then 
		lasthit=0
		PlaySound SoundFX("clickclick",DOFContactors), 0, 0.34*VolumeDial, AudioPan(leftrampscore), 0.15,0,0,1,AudioFade(leftrampscore)
	Else
		lasthit=1
		PlaySound "rampspeeder 2", 0, .67*BackGlassVolumeDial, AudioPan(leftrampscore), 0.15*VolumeDial,0,0,1,AudioFade(leftrampscore)
		ObjLevel(8) = 1 : FlasherFlash8_Timer
		ObjLevel(3) = 1 : FlasherFlash3_Timer : 	DOF 135,2 : 'Debug.print "DOF 135, 2" ' blue left  'apophis
		PlaySound SoundFX("fx_solenoidon",DOFContactors), 0, .67*VolumeDial, AudioPan(leftrampscore), 0.15,0,0,1,AudioFade(leftrampscore)
	End If

End Sub

Sub Plungerrampstart_hit
	WireRampOn True 'Play Plastic Ramp Sound
	lasthit=5 ' for skillshot start
End Sub

Sub Plungerrampend_unhit
	WireRampOff
End Sub

Sub rightrampenter_Hit
	WingRState = 2 : WingR_Blinks=1 :

	WireRampOn True 'Play Plastic Ramp Sound


	If lasthit=2 Then 
		lasthit=0 
		PlaySound SoundFX("clickclick",DOFContactors), 0, 0.33*VolumeDial, AudioPan(rightrampscore), 0.15,0,0,1,AudioFade(rightrampscore)
	Else
		'only on the way up... put here
		lasthit=2
		PlaySound "rampspeeder 2", 0, .72*BackGlassVolumeDial, AudioPan(rightrampscore), 0.15*VolumeDial,0,0,1,AudioFade(rightrampscore)

		ObjLevel(9) = 1 : FlasherFlash9_Timer
		ObjLevel(4) = 1 : FlasherFlash4_Timer : 	DOF 137,2 : 'Debug.print "DOF 137, 2" 'red left
		PlaySound SoundFX("fx_solenoidon",DOFContactors), 0, .67*VolumeDial, AudioPan(rightrampscore), 0.15,0,0,1,AudioFade(rightrampscore)
	End If

End Sub
	

Sub leftlane_Hit
	lasthit=3
	PlaySound "sauserflyby", 0, .67*BackGlassVolumeDial, AudioPan(leftrampscore), 0.15*VolumeDial,0,0,1,AudioFade(leftrampscore)
End Sub

Sub rightlane_Hit 
	Light048.intensity=350
	Light048.timerenabled=1 
	lasthit=4
	PlaySound "sauserflyby", 0, .67*BackGlassVolumeDial, AudioPan(rightrampscore), 0.15*VolumeDial,0,0,1,AudioFade(rightrampscore) 
	If skillshot=1 And not Tilted Then
		LiReplay.state=2 : lireplay.blinkinterval=200 : LiReplay.timerenabled=1
		lireplay001.state = 2 : lireplay001.blinkinterval=200 : pLireplay.blenddisablelighting=5
		Light003.state=0
		light051.state=2
		if bossactive=1 Then DOF 118,1
	End If
	skillshot=0
	Plungerispulled=0
	Plunger001.timerenabled=0
	shoottheballstatus=0 
	LiSkillshot1.state=0
	LiSkillshot2.state=0 

End Sub


Dim SecretScoring,SecretStatus,SSblast
DIM SSS : SSS=False
Sub skillshottrigger_Hit 
	If lasthit<5 Then
		SecretScoring=SecretScoring+25000

		If Lipassage.state=2 Then
			SSS=True
			rewardskillshot
			superskillshot.enabled=0
			LiPassage.state=0
		Else
			SecretStatus=1
		End If
		
		scoring(SecretScoring)
		addletter(3)
		PlaySound "mistery", 0, .71*BackGlassVolumeDial, AudioPan(skillshottrigger), 0.05*VolumeDial,0,0,1,AudioFade(skillshottrigger)
		Light049.Timerenabled=1
		LSPassage.Play SeqBlinking,,15,25
		bossblinker.Play SeqBlinking,,10,25
			redeyes.enabled=1


		SSBlast=2000
		PlaySound SoundFX("fx_solenoidon",DOFContactors), 0, .71*VolumeDial, AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger)

		ObjLevel(1) = 1 : FlasherFlash1_Timer
		ObjLevel(2) = 1 : FlasherFlash2_Timer
		ObjLevel(3) = 1 : FlasherFlash3_Timer :	DOF 135,2 : 'Debug.print "DOF 135, 2" ' blue left  'apophis
		ObjLevel(4) = 1 : FlasherFlash4_Timer : 	DOF 137,2 : 'Debug.print "DOF 137, 2" 'red left
		ObjLevel(5) = 1 : FlasherFlash5_Timer
		ObjLevel(6) = 1 : FlasherFlash6_Timer
		ObjLevel(7) = 1 : FlasherFlash7_Timer
		PlaySound SoundFX("fx_solenoidon",DOFContactors), 0, .71*VolumeDial, AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger)
	End If
    lasthit=5
End Sub

Sub Light049_Timer
	SSBlast=SSBlast-250
	Light049.intensity=SSBlast
	If SSBlast<300 Then Light049.timerenabled=0 : Light049.intensity=10
End Sub



dim lastframe
Sub Frametimer_Timer ' -1
'	If gametime-lastframe > 8 Then debug.print "framtime=" & gametime - lastframe
'	lastframe=gametime
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate
End Sub

'**************
' Game timer 
Dim BGletters(12,2)
Dim Colorswap


Dim BestinParcec
Dim Complicated

'dim bally1,bally2,bally3,bally4, ballydisp, wilvl1, wilvl2,wilvl3
Dim blinkerDT : blinkerDT=0
Dim blinkerDTdir : blinkerDTdir=0
Dim wings
Sub RealTime_Timer

	TargetTopBlink

	If blinkerDTdir=0 Then blinkerDT=blinkerDT+1 : If blinkerDT > 25 Then blinkerDTdir=1
	If blinkerDTdir=1 Then blinkerDT=blinkerDT-1 : If blinkerDT < 1 Then blinkerDTdir=0
	If blinkerDT < 16 Or pDT007.z < -88 Then pDT007.Visible=0 Else pDT007.blenddisablelighting=(blinkerDT-15)*81 : pDT007.Visible=1
	If blinkerDT < 16 Or pDT006.z < -88 Then pDT006.Visible=0 Else pDT006.blenddisablelighting=(blinkerDT-15)*81 : pDT006.Visible=1
	If blinkerDT < 16 Or pDT005.z < -88 Then pDT005.Visible=0 Else pDT005.blenddisablelighting=(blinkerDT-15)*81 : pDT005.Visible=1
	If blinkerDT < 16 Or pDT004.z < -88 Then pDT004.Visible=0 Else pDT004.blenddisablelighting=(blinkerDT-15)*81 : pDT004.Visible=1
	If blinkerDT < 16 Or pDT003.z < -88 Then pDT003.Visible=0 Else pDT003.blenddisablelighting=(blinkerDT-15)*81 : pDT003.Visible=1


'	wings=wings+1
'	If wings>20 then wings=0
'	dim wingcol
'	if wings<11 Then wingcol=wings Else wingcol = 20-wings
'	UpdateMaterial "Metal rampsWINGS" ,0,0.85,1,1,1,0,0.99,rgb(220-wingcol*2,176-wingcol*2,75-wingcol*2),rgb(0,0,0),rgb(0,0,0),False,True,0,0,0,0

	If TimeToAddPlayers > 0 Then TimeToAddPlayers = TimeToAddPlayers -1
	If BestinParcec>0 Then BestinParcec=BestinParcec+1 : If BestinParcec > 80 Then BestinParcec=0 : PlaySound "bestintheparcec",0,1*BackGlassVolumeDial
	If Complicated>0 Then Complicated=Complicated+1 : If Complicated > 80 Then Complicated=0 : PlaySound "Complicated2",0,1*BackGlassVolumeDial

'	ballydisp=0
'	If bally1 <> ballsinplay	then bally1=ballsinplay : ballydisp=1
'	If bally2 <> BIP 			then bally2=BIP 		: ballydisp=1
'	If bally3 <> LockedBalls	then bally3=LockedBalls : ballydisp=1
'	If bally4 <> ballswaiting	then bally4=ballswaiting: ballydisp=1
'	If ballydisp=1 Then debug.print gametime & "GetB=" & bally1 & "  BIP=" & bally2 & "  Lock=" & bally3 & "  BWaiting=" & bally4
'	ballydisp=0
'	If WiLvl1 <> WizardLevel Then WiLvl1= WizardLevel : ballydisp=1
'	If WiLvl2 <> WizardBonusLevel Then WiLvl2= WizardBonusLevel : ballydisp=1
'	If WiLvl3 <> Gameisover Then WiLvl3= Gameisover : ballydisp=1
'
'	If ballydisp=1 Then debug.print gametime & "WizL=" & WiLvl1 & "  BonusL=" & WiLvl2 & "  GiO=" & WiLvl3


	DisplayEnterDelay=DisplayEnterDelay+1

	bloom2

	Colorswap=Colorswap+0.5
	If colorswap > 30 Then Colorswap = 0
	dim xy
	if colorswap > 14 then xy = 30-Colorswap else xy = colorswap
	UpdateMaterial "Plastic Green"  ,0,0.85,1,1,1,0,0.99,rgb(0,xy*6,0),rgb(0,0,0),rgb(0,0,0),False,True,0,0,0,0
	UpdateMaterial "Plastic OrangeB",0,0.85,1,1,1,0,0.99,rgb(xy*5,xy*3,xy),rgb(0,0,0),rgb(0,0,0),False,True,0,0,0,0
	UpdateMaterial "Plastic YellowB",0,0.85,1,1,1,0,0.99,rgb(xy*6,xy*4.5,xy),rgb(0,0,0),rgb(0,0,0),False,True,0,0,0,0
	UpdateMaterial "Plastic RedB"   ,0,0.85,1,1,1,0,0.99,rgb(xy*6,2,2),rgb(0,0,0),rgb(0,0,0),False,True,0,0,0,0
	UpdateMaterial "Plastic BlueB"  ,0,0.85,1,1,1,0,0.99,rgb(2,2,xy*6),rgb(0,0,0),rgb(0,0,0),False,True,0,0,0,0
	xy=xy/20
	light040.intensityscale = xy
	light041.intensityscale = xy
	light036.intensityscale = xy
	light053.intensityscale = xy
	light035.intensityscale = xy
	light052.intensityscale = xy
	light056.intensityscale = xy
	light057.intensityscale = xy
	light058.intensityscale = xy
	If mission(3)=1 or attractrefuel or stationonlycounter>0 Then turningShip

	primitive045.rotx=90+gate005.currentangle
	primitive046.rotx=90+gate004.currentangle
	If primitive054.RotX<90 Then primitive054.RotX = primitive054.RotX+1
	If primitive053.RotX<90 Then primitive053.RotX = primitive053.RotX+1



	RealTimeScoring
	RollingUpdate
	FlipperLogoR.RotY=RightFlipper.CurrentAngle-268
	FlipperLogoL.RotY= LeftFlipper.CurrentAngle+93
	FlipperLogoL2.RotY= TopFlipper.CurrentAngle+93
	RightFlipperP.ObjRotZ=RightFlipper.CurrentAngle
	LeftFlipperP.ObjRotZ=LeftFlipper.CurrentAngle
	TopFlipperP.ObjRotZ=TopFlipper.CurrentAngle
	libonus1.state=bonus1
	libonus2.state=bonus2
	libonus3.state=bonus3
	BGletters(1,1)=LiCenter001.state
	BGletters(2,1)=LiCenter002.state
	BGletters(3,1)=LiCenter003.state
	BGletters(4,1)=LiCenter004.state
	BGletters(5,1)=LiCenter005.state
	BGletters(6,1)=LiCenter006.state
	BGletters(7,1)=LiCenter007.state
	BGletters(8,1)=LiCenter008.state
	BGletters(9,1)=LiCenter009.state
	BGletters(10,1)=LiCenter010.state
	BGletters(11,1)=LiCenter011.state
	BGletters(12,1)=LiCenter012.state
	Dim x : i=0
	for each x in bg_letters
		i=i+1
		If BGletters(i,1) = 0 Then
			BGletters(i,2)=BGletters(i,2)-1 : If BGletters(i,2)<0 Then BGletters(i,2)=0
		Else
			BGletters(i,2)=BGletters(i,2)+2 : If BGletters(i,2)>10 Then BGletters(i,2)=10
		End If
		x.Opacity = 25 + BGletters(i,2)*13
	Next


	If rnd(1)<0.004 Then
		if mission(8)=0 and mission(7)=0 then RLaser012.timerenabled=1 : RLaser011.timerenabled=1 
		'  starts turret for 1 sec from last position . stops under mission 7/8
	End If
	If leftflipperdown=1 or rightflipperdown=1 then
		If infostatus=0 Then infostatus=1
	End If

	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate

End Sub

Dim CardCounter, ScoreCard

Sub CardTimer_Timer
	If scorecard=1 Then
		CardCounter=CardCounter+2 : If CardCounter>50 Then CardCounter=50
	Else
		CardCounter=CardCounter-4 : If CardCounter<0 Then CardCounter=0
	End If
	InstructionCard.transX = CardCounter*8
	InstructionCard.transY = CardCounter*6
	InstructionCard.transZ = -cardcounter*3
	InstructionCard.objRoTX = -CardCounter/3
	InstructionCard.size_x = 1.1+CardCounter/23
	InstructionCard.size_y = 1.1+CardCounter/23
	If CardCounter=0 Then 
		InstructionCard.visible=0
		CardTimer.Enabled=0
	Else
		InstructionCard.visible=1
	End If 
End Sub


'**********
' Keys
'**********
Dim Tilted : Tilted = False
Sub Tilting	
	
	BallIsLockedStatus=0

	autokicker.enabled=True

	Tilted=True
	rightflipperdown=0
	leftflipperdown=0
	TopFlipper.timerenabled=0
	LeftFlipper.timerenabled=0
	infostatus=0


	stoptalking
	StopSound "warning1"
	StopSound "warning2"
	StopSound "warning3"
	StopSound "warning4"
	If RestartGame = 2 Then
		PlaySound "alosexpensive",0,1*BackGlassVolumeDial
	Else	
		PlaySound SoundFX("tilt", 0), 0, 1*VolumeDial, 0, 0.25 
	End If

''	LiReplay2.intensity=6
	LiReplay2.state=0 
	LiReplay2.timerenabled=0
	drainhelptimer.enabled=0
	doubletimeonmulti=0
	LiReplay.state=0 : lireplay001.state = 0
pLireplay.blenddisablelighting=1
	LiReplay.timerenabled=0
	Light003.state=2
	Light051.state=0
'	LiReplay2.intensity=2
	replayballs=0
	Multiballtimer.enabled=false
	ballswaiting=0
	luttarget=8


	leftflipper.eostorqueangle = EOSA
	leftflipper.eostorque = EOST

	rightflipper.eostorqueangle = EOSA
	rightflipper.eostorque = EOST
	LeftFlipper.RotateToStart
	RightFlipper.RotateToStart
	TopFlipper.RotateToStart
End Sub

Dim TiltValue
Dim Tiltstatus
Sub CheckTilt                 
	TopFlipper.timerenabled=0
	LeftFlipper.timerenabled=0
	infostatus=0
	leftflipperdown=0
	rightflipperdown=0

		TiltValue = TiltValue + 5
		If TiltValue > 15 Then 
			Tiltstatus = 2
			Tilttimer.enabled=True
			Tilting
		Elseif TiltValue > 5 Then
			Tiltstatus = 1
			StopSound "warning1"
			StopSound "warning2"
			StopSound "warning3"
			StopSound "warning4"
			Stoptalking : PlaySound "warning" & Int(rnd(1)*4)+1,0,1*BackGlassVolumeDial
		End If

End Sub

Sub Tilttimer_Timer ' 1500
	If Not Tilted Then Tilttimer.enabled=False : Exit Sub
	TopFlipper.timerenabled=0
	LeftFlipper.timerenabled=0
	infostatus=0
	leftflipperdown=0
	rightflipperdown=0
	Tiltstatus = 2

	If RestartGame = 2 Then
		Tiltstatus=3
		flyagain1.state=0 : pflyagain.blenddisablelighting=1
		flyagain2.state=0
		flyagain3.state=0
		extraextraball=0
	End If
	Playsound "tiltalarm",0,1*BackGlassVolumeDial

	LeftFlipper.RotateToStart
	RightFlipper.RotateToStart
	TopFlipper.RotateToStart


	luttarget=8

End Sub

Dim Plungerispulled

Const ReflipAngle = 20

Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm

FlipperUpAttackMinimumSoundLevel = 0.010           						'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635								'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                        						'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                      						'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel								'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel								'sound helper; not configurable

Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Function RndNum(min, max)
	RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

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
	PlaySoundAtLevelStatic SoundFX("Flipper_L" & Int(Rnd*11)+1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R" & Int(Rnd*11)+1,DOFFlippers), FlipperRightHitParm, Flipper
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
Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

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



Dim RestartGame : RestartGame = 0
Sub Table1_KeyDown(ByVal keycode)
	PlaySound ("Start_Button"), 0, 0.1*VolumeDial

	If keycode = PlungerKey Then
		PlaySound "fx_plungerpull",0,1*VolumeDial,AudioPan(swPlunger),0.25,0,0,1,AudioFade(swPlunger)
		If 	kickertimer.enabled=0 And plungerrelease.enabled=0 Then Plunger001.Pullback : Plungerispulled=1
	End If


	if keycode = 19 then ScoreCard=1 : CardTimer.enabled=1

	If attract1Mode=0 And Not Tilted And BIP>0 Then 
		If keycode = Startgamekey And RestartGame = 0 Then RestartGame = 1
		If keycode = LeftTiltKey    Then Nudge 90, 4 : PlaySound SoundFX("fx_nudge", 0), 0, 1*VolumeDial, -0.1, 0.25 : CheckTilt
		If keycode = RightTiltKey   Then Nudge 270, 4: PlaySound SoundFX("fx_nudge", 0), 0, 1*VolumeDial, 0.1, 0.25 : CheckTilt
		If keycode = CenterTiltKey  Then Nudge 0, 4  : PlaySound SoundFX("fx_nudge", 0), 0, 1*VolumeDial, 0, 0.25 : CheckTilt
		If keycode = MechanicalTilt Then PlaySound SoundFX("fx_nudge", 0), 0, 1*VolumeDial, 0, 0.25 : CheckTilt

		If keycode = LeftFlipperKey And SUPERSCORING=0 Then
			If 	kickertimer.enabled=0 And plungerrelease.enabled=0 And Plungerispulled=1 Then 
				plungerispulled=2
				Plunger001.Firespeed=10 : plunger001.fire
				plungerrelease.enabled=1
			End If
			DOF 101,1' : 'Debug.print "DOF 101, 1"  'apophis
			LeftFlipper.timerenabled=1
			bonus0=bonus1 : bonus1=bonus2 : bonus2=bonus3 :	bonus3=bonus0
			FlipperActivate LeftFlipper, LFPress
			SolLFlipper True
			TopFlipper.RotateToEnd

'			PlaySound SoundFX("Flipper_L"& int(rnd(1)*11)+1,DOFFlippers), 0, .42*VolumeDial, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
		End If

		If keycode = RightFlipperKey And SUPERSCORING=0 Then
			DOF 102,1' : 'Debug.print "DOF 102, 1"  'apophis
			TopFlipper.timerenabled=1
			bonus0=bonus3 : bonus3=bonus2 : bonus2=bonus1 :	bonus1=bonus0
			FlipperActivate RightFlipper, RFPress
			SolRFlipper True
'			PlaySound SoundFX("Flipper_R"& int(rnd(1)*11)+1,DOFFlippers), 0, .42*VolumeDial, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
		End If

	End If
End Sub
Sub plungerrelease_timer
	Plunger001.firespeed=150
	plungerrelease.enabled=0
	Plungerispulled=0
End Sub
Dim replaystatus
Sub trippleknocker_Timer
	DOF 111,2:'Debug.print "DOF 111, 2"  'apophis
	PlaySound SoundFX("fx_knocker",DOFContactors), 0, .42*VolumeDial, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)


	replaystatus=1
	trippleknocker.enabled=0
'just once afterall
End Sub




Dim Bshake1,Bshake2,Bshake3,BshakePos1,BshakePos2,BshakePos3
bshake1=5 : bshake2=5 : bshake3=5

Sub LiBumper1_Timer
	If LiBumper1.timerenabled=0 Then
		LiBumper1.timerenabled=1
		LiBumper1.intensity=700
		Primitive038.transY=3
		BumperRing1.Z=-30
	Else
		i=LiBumper1.intensity
		Select Case i
			Case 700 : BumperRing1.Z=-40
			Case 600 : BumperRing1.Z=-30
			Case 500 : BumperRing1.Z=-20
			Case 400 : BumperRing1.Z=-10
			Case 300 : BumperRing1.Z=0
			Case 200 : BumperRing1.Z=3
			Case 100 : BumperRing1.Z=0
		End Select
		If i>0 Then
			i=i-100
			Primitive038.transY=i/60
			LiBumper1.intensity=i
		Else
			Light038.state=1 ' razercrestblink
		End If
		BshakePos1=BshakePos1+int(rnd(1)*5)-2
		Primitive038.roty=Bshake1*2 + BshakePos1
		If Bshake1 = 0 Then LiBumper1.timerenabled=0 : LiBumper1.intensity=1 : Light038.state=1: Exit Sub
		If Bshake1 < 0 Then
			Bshake1 = ABS(Bshake1) - 1
		Else
			Bshake1 = - Bshake1 + 1
		End If
	End If
End Sub
Sub LiBumper2_Timer
	If LiBumper2.timerenabled=0 Then
		LiBumper2.timerenabled=1
		LiBumper2.intensity=700
		Primitive65.transY=3
		BumperRing2.Z=-30
	Else
		i=LiBumper2.intensity
		Select Case i
			Case 700 : BumperRing2.Z=-40
			Case 600 : BumperRing2.Z=-30
			Case 500 : BumperRing2.Z=-20
			Case 400 : BumperRing2.Z=-10
			Case 300 : BumperRing2.Z=0
			Case 200 : BumperRing2.Z=3
			Case 100 : BumperRing2.Z=0
		End Select
		If i>0 Then
			i=i-100
			Primitive65.transY=i/60
			LiBumper2.intensity=i
		Else
			Light038.state=1
		End If
		BshakePos2=BshakePos2+int(rnd(1)*5)-2
		Primitive65.roty=Bshake2*2 + BshakePos2
		If Bshake2 = 0 Then LiBumper2.timerenabled=0 : LiBumper2.intensity=1 : Light038.state=1: Exit Sub
		If Bshake2 < 0 Then
			Bshake2 = ABS(Bshake2) - 1
		Else
			Bshake2 = - Bshake2 + 1
		End If
	End If
End Sub
Sub LiBumper3_Timer
	If LiBumper3.timerenabled=0 Then
		LiBumper3.timerenabled=1
		LiBumper3.intensity=700
		Primitive037.transY=3
		BumperRing3.Z=-30
	Else
		i=LiBumper3.intensity
		Select Case i
			Case 700 : BumperRing3.Z=-40
			Case 600 : BumperRing3.Z=-30
			Case 500 : BumperRing3.Z=-20
			Case 400 : BumperRing3.Z=-10
			Case 300 : BumperRing3.Z=0
			Case 200 : BumperRing3.Z=3
			Case 100 : BumperRing3.Z=0
		End Select
		If i>0 Then
			i=i-100
			LiBumper3.intensity=i
			Primitive037.transY=i/60
		Else
			Light038.state=1
		End If
		BshakePos3=BshakePos3+int(rnd(1)*5)-2
		Primitive037.roty=Bshake3*2 + BshakePos1
		If Bshake3 = 0 Then LiBumper3.timerenabled=0 : LiBumper3.intensity=1 : Light038.state=1: Exit Sub
		If Bshake3 < 0 Then
			Bshake3 = ABS(Bshake3) - 1
		Else
			Bshake3 = - Bshake3 + 1
		End If
	End If
End Sub

Dim notenoughcredits
Dim DisplayEnterDelay
Dim AddplayerStatus

dim Credits,InsertCoinStatus, initialSelect, cancelbonus,addcreditsstatus
Sub Table1_KeyUp(ByVal keycode)
	PlaySound ("Start_Button2"), 0, 0.1*VolumeDial
		'nFozzy Begin'
	If keycode = LeftFlipperKey Then 
		If attract1Mode=0 Then DOF 101,0
		''Debug.print "DOF 101, 0"  'apophis
		FlipperDeActivate LeftFlipper, LFPress
		SolLFlipper False
	End If
	If keycode = RightFlipperKey Then 
		If attract1Mode=0 Then DOF 102,0
		'debug.print "DOF 102, 0"  'apophis
		FlipperDeActivate RightFlipper, RFPress
		SolRFlipper False
	End If
	'nFozzy End'

	'***  Change Music  ***
	if keycode = 19 then ScoreCard=0

	If keycode = LeftMagnaSave Then

		i=LastPlayd+1 
		If i=2 Then i=3
		If i<LastMusic+1 then
			PlayTune(i)
		else
			PlayTune(3)
		End If
	End If

	If keycode = RightMagnaSave Then 


	FlipperColor=FlipperColor+1 : If FlipperColor>4 Then FlipperColor=0
	Flippermaterial

'			i=LastPlayd-1 
'			If i>2 then
'				PlayTune(i)
'			else
'				PlayTune(LastMusic)
'			End If

	End If


	If keycode = StartGameKey Then

		If PlayersPlaying = 4 And TimeToAddPlayers < 1 And BallinPlay <3 Then
			TimeToAddPlayers=60
			PlaySound "NoDroids2", 0, .7*BackGlassVolumeDial
		End If
		If PlayersPlaying>0 And BallinPlay=2 And TimeToAddPlayers < 1 Then
			TimeToAddPlayers=100
			Playsound "shouldbeinteresting",0,1*BackGlassVolumeDial
		End If
		If PlayersPlaying>0 And BallinPlay=1 And PlayersPlaying<4 And TimeToAddPlayers < 1 Then
			
				If Credits > 0 Then
					Credits 			= Credits - 1
					PlayersPlaying		= PlayersPlaying + 1
					If PlayersPlaying	= 2 Then PlaySound "two"  ,0,BackGlassVolumeDial : AddplayerStatus=2 : PlaySound "two"  ,0,BackGlassVolumeDial
					If PlayersPlaying	= 3 Then PlaySound "three",0,BackGlassVolumeDial : AddplayerStatus=3 : PlaySound "three",0,BackGlassVolumeDial
					If PlayersPlaying	= 4 Then PlaySound "four" ,0,BackGlassVolumeDial : AddplayerStatus=4 : PlaySound "four" ,0,BackGlassVolumeDial
					TimeToAddPlayers	= 90 ' 1 second before next alowed

				Else

					addcreditsstatus=1
					notenoughcredits=notenoughcredits+1

					If notenoughcredits = 4 Then
						notenoughcredits=0
						PlaySound "notenoughcredits", 0, .8*BackGlassVolumeDial, AudioPan(BallRelease), 0.05,0,0,1,AudioFade(BallRelease)
					Elseif notenoughcredits = 3 Then
						PlaySound "Alright", 0, 1*BackGlassVolumeDial, AudioPan(BallRelease), 0.05,0,0,1,AudioFade(BallRelease)
					Else
						PlaySound "Bescar", 0, 1*BackGlassVolumeDial, AudioPan(BallRelease), 0.05,0,0,1,AudioFade(BallRelease)
					End If

					TimeToAddPlayers=90
				End If

		End If




		If keycode = Startgamekey And RestartGame < 2 Then
			RestartGame = 0
			RestartCounter = 0
		End If
		If Credits>0 Then
			If GameOverStatus>3 And startuptest.enabled=0 And ReleaseTimer.enabled=0 And EvasiveInit.enabled=0 And closebonus.enabled=0 Then
				DOF 129,2 ': debug.print "DOF 129, 2"  'apophis

				WingLState=5
				WingRState=5

				WormholeButton = True : buttonblink=15
				luttarget=2
				Credits=Credits-1
				Displaybuzy=0
				replayballs=0
				ballswaiting=0
				addcreditsstatus=0
				EndMusic
				GameOverStatus=5
				MultiballsDone=0
				WizardLevel=0
				WizardBonusLevel = 0
				DelayStartNewGame.enabled=1
				TimeToAddPlayers=90
			End If
		Else
			If TimeToAddPlayers < 1 Then
				If GameOverStatus>3 And startuptest.enabled=0 And ReleaseTimer.enabled=0 And EvasiveInit.enabled=0 And closebonus.enabled=0 Then
					DOF 212,2 '	debug.print "DOF 212, 2"  'apophis 
					addcreditsstatus=1
					notenoughcredits=notenoughcredits+1
					If notenoughcredits = 4 Then
						notenoughcredits=0
						PlaySound "notenoughcredits", 0, .8*BackGlassVolumeDial, AudioPan(BallRelease), 0.05,0,0,1,AudioFade(BallRelease)
					Elseif notenoughcredits = 3 Then
						PlaySound "Alright", 0, 1*BackGlassVolumeDial, AudioPan(BallRelease), 0.05,0,0,1,AudioFade(BallRelease)
					Else
						PlaySound "Bescar", 0, 1*BackGlassVolumeDial, AudioPan(BallRelease), 0.05,0,0,1,AudioFade(BallRelease)
					End If
					TimeToAddPlayers=90
				End If
			End If
		End If
	End If

	If keycode = AddCreditKey Then
		PlaySound SoundFX("fx_coin",DOFContactors), 0, .9*VolumeDial, AudioPan(BallRelease), 0.05,0,0,1,AudioFade(BallRelease)
		If Credits < 30 Then	
			If WingL_Blinks > 0 Then WingL_Blinks=WingL_Blinks+1 Else WingLState=2 : WingL_Blinks=1
			If WingR_Blinks > 0 Then WingR_Blinks=WingR_Blinks+1 Else WingRState=2 : WingR_Blinks=1
			DOF 213,2 : Credits=Credits+1 : InsertCoinStatus=1 ':debug.print "DOF 213, 2"  'apophis
		End If
	End If

	If keycode = AddCreditKey2 Then
		PlaySound SoundFX("fx_coin",DOFContactors), 0, .9*VolumeDial, AudioPan(BallRelease), 0.05,0,0,1,AudioFade(BallRelease)
		If Credits < 30 Then	
			If WingL_Blinks > 0 Then WingL_Blinks=WingL_Blinks+1 Else WingLState=2 : WingL_Blinks=1
			If WingR_Blinks > 0 Then WingR_Blinks=WingR_Blinks+1 Else WingRState=2 : WingR_Blinks=1
			DOF 213,2 : Credits=Credits+1 : InsertCoinStatus=1 ':debug.print "DOF 213, 2"  'apophis
		End If
	End If

	If keycode = PlungerKey Then
		PlaySound  SoundFX("fx_plunger",DOFContactors),0,1*VolumeDial,AudioPan(swPlunger),0.2,0,0,1,AudioFade(swPlunger)
		If kickertimer.enabled=0 Then
			Plungerispulled=0
			Plunger001.Fire
			If GetReadyToPlay>0 Then Displaytext "            ","            " : GetReadyToPlay=10 ' this is the way
			Plunger001.timerenabled=0
			shoottheballstatus=0 
		End If
		Plunger001.timerenabled=0
		shoottheballstatus=0 
	End If

	If attract1Mode=0 And Not Tilted Then
		If keycode = LeftFlipperKey Then
			if bonusstatus>0 And bonusstatus<8 Then DisplayString1="FLIPPER GIVE" : DisplayString2=" FAST BONUS " : Displaycounter=0 : DisplayEffect=8': If Displaybuzy<2 Then Displaybuzy=1
			leftflipperdown=0 : LeftFlipper.timerenabled=0 
			LeftFlipper.RotateToStart
			TopFlipper.RotateToStart
			PlaySound SoundFX("Flipper_Left_Down_" & int(rnd(1)*7)+1 , DOFFlippers), 0, .58*VolumeDial, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)

'			ObjLevel(10) = 1 : FlasherFlash10_Timer
		End If

		If keycode = RightFlipperKey Then
			if bonusstatus>0 And bonusstatus<8 Then Displaytext "            ","            " : If Displaybuzy<2 Then Displaybuzy=0
			RightFlipperdown=0 : TopFlipper.Timerenabled=0
			RightFlipper.RotateToStart
			PlaySound SoundFX("Flipper_Right_Down_" & int(rnd(1)*8)+1 , DOFFlippers), 0, .58*VolumeDial, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
		End If
		If rightflipperdown=0 and leftflipperdown=0 and infostatus>0 Then displaybuzy=0 : infostatus=0 : Displaytext "            ","            " : If FlexDMD Then UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex FlexBG, "PLAYER." & CurrentPlayer & "  Ball."& cstr (ballinplay), 0, 12, FlexScore , 15, 1, 14, 2, 14 
	End If


	If Displaybuzy=2 And DisplayEnterDelay>333 Then	 ' little over 3 seconds forced to wait
		If keycode = RightFlipperKey Then 
			initialletter=initialletter+1 
			PlaySound SoundFX("Bip2",DOFFlippers), 0, .88*VolumeDial, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
			if initialletter>90 Then initialletter=65
		End If
		If keycode = LeftFlipperKey Then 
			initialletter=initialletter-1
			PlaySound SoundFX("Bip2",DOFFlippers), 0, .88*VolumeDial, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
			if initialletter<65 Then initialletter=90
		End If
		If keycode = StartGameKey Then

			initialSelect=1
			PlaySound SoundFX("sonardown",DOFFlippers), 2, .71*VolumeDial, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
		End If
	End If

 End Sub
Sub Flippermaterial
		If FlipperColor=0 Then 
			RightFlipperP.material="Metal Yellow" 
			LeftFlipperP.material="Metal Yellow"
			TopFlipperP.material="Metal Yellow"
		End If
		If FlipperColor=1 Then
			RightFlipperP.material="Metal Green Dark" 
			LeftFlipperP.material="Metal Green Dark"
			TopFlipperP.material="Metal Green Dark"
		End If
		If FlipperColor=2 Then
			RightFlipperP.material="Metal Amber" 
			LeftFlipperP.material="Metal Amber"
			TopFlipperP.material="Metal Amber"
		End If
		If FlipperColor=3 Then
			RightFlipperP.material="Metal Red"
			LeftFlipperP.material="Metal Red"
			TopflipperP.material="Metal Red"
		End If
		If FlipperColor=4 Then
			RightFlipperP.material="Metal Blue2" 
			LeftFlipperP.material="Metal Blue2"
			TopFlipperP.material="Metal Blue2"
		End If
End Sub

' hold either lipper for 3sec for info, release to stop
Dim infostatus,leftflipperdown,rightflipperdown
Sub LeftFlipper_Timer    
	Leftflipperdown=1
	LeftFlipper.timerenabled=0
End Sub

Sub topflipper_Timer    
	rightflipperdown=1
	TopFlipper.timerenabled=0
End Sub

Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	FlipperLeftHitParm = parm/10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)

End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	FlipperRightHitParm = parm/10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

DIm RubberFlipperSoundFactor
RubberFlipperSoundFactor = 0.075/5
FlipperUpSoundLevel = 1.0                        						'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                      						'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel								'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel	
Sub RandomSoundRubberFlipper(parm)
	 PlaySound SoundFX("Flipper_Rubber_" & Int(Rnd*7)+1,DOFContactors) , 0, parm  * RubberFlipperSoundFactor*VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall) 
End Sub




'******************************************************
'				FLIPPER AND RUBBER CORRECTION
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		'safety coefficient (diminishes polarity correction only)
		'x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 0	'don't mess with these
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1

		x.enabled = True
		'x.DebugOn = True : stickL.visible = True : tbpl.visible = True : vpmSolFlipsTEMP.DebugOn = True
		x.TimeDelay = 60
	Next

	'rf.report "Velocity"
	addpt "Velocity", 0, 0, 	1
	addpt "Velocity", 1, 0.1, 	1.07
	addpt "Velocity", 2, 0.2, 	1.15
	addpt "Velocity", 3, 0.3, 	1.25
	addpt "Velocity", 4, 0.41, 1.05
	addpt "Velocity", 5, 0.65, 	1.0'0.982
	addpt "Velocity", 6, 0.702, 0.968
	addpt "Velocity", 7, 0.95,  0.968
	addpt "Velocity", 8, 1.03, 	0.945

	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.05, -5.5
	AddPt "Polarity", 2, 0.4, -5.5
	AddPt "Polarity", 3, 0.8, -5.5	
	AddPt "Polarity", 4, 0.85, -5.25
	AddPt "Polarity", 5, 0.9, -4.25
	AddPt "Polarity", 6, 0.95, -3.75
	AddPt "Polarity", 7, 1, -3.25
	AddPt "Polarity", 8, 1.05, -2.25
	AddPt "Polarity", 9, 1.1, -1.5
	AddPt "Polarity", 10, 1.15, -1
	AddPt "Polarity", 11, 1.2, -0.5
	AddPt "Polarity", 12, 1.25, 0
	AddPt "Polarity", 13, 1.3, 0

	LF.Object = LeftFlipper	
	LF.EndPoint = EndPointLp	'you can use just a coordinate, or an object with a .x property. Using a couple of simple primitive objects
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

'Trigger Hit - .AddBall activeball
'Trigger UnHit - .PolarityCorrect activeball
dim LFFlipBall, RFFlipBall : LFFlipBall = 0 : RFFlipBall = 0


Sub TriggerLF_Hit() : LF.Addball activeball : LFFlipBall = 1 : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : LFFlipBall = 0 : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : RFFlipBall = 1 : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : RFFlipBall = 0 : End Sub

'Methods:
'.TimeDelay - Delay before trigger shuts off automatically. Default = 80 (ms)
'.AddPoint - "Polarity", "Velocity", "Ycoef" coordinate points. Use one of these 3 strings, keep coordinates sequential. x = %position on the flipper, y = output
'.Object - set to flipper reference. Optional.
'.StartPoint - set start point coord. Unnecessary, if .object is used.

'Called with flipper - 
'ProcessBalls - catches ball data. 
' - OR - 
'.Fire - fires flipper.rotatetoend automatically + processballs. Requires .Object to be set to the flipper.

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
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

	Public Sub Report(aChooseArray) 	'debug, reports all coords in tbPL.text
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
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function	'Timer shutoff for polaritycorrect
	
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
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)						'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
	 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
	
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
				'debug.print BallPos & " " & AddX & " " & Ycoef & " "& PartialFlipcoef & " "& VelCoef
				'playsound "fx_knocker"
			End If
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
	redim aArray(aCount-1+offset)	'Resize original array
	for x = 0 to aCount-1		'set objects back into original array
		if IsObject(a(x)) then 
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



'******************** Slacky Flips **************************'

Const FlipY = 1831           'Default flip Y position
Const Dislocation = 2        'Amount of minimum slack, affect to recovery speed too
Const SlackHitLimit = 10    '6-16    Lower value will make slack happen with lower collision force
Const HitForceDivider = 50    '10-100    Lower value will add slack. Set to roughly half of the max hit force(parm) you see when playing
Const MaxSlack = 4
Dim SlackAmount

Sub CheckFlipperSlack(ball, Flipper, parm)
    If slackyFlips = 1 Then
        SlackAmount = Dislocation + parm / HitForceDivider
        if SlackAmount > MaxSlack then SlackAmount = MaxSlack
        if parm > SlackHitLimit Then
            Flipper.Y = FlipY + SlackAmount
           ' Logo.Y = FlipY + SlackAmount
            slacktimer.interval = 10
            slacktimer.Enabled = 1
            'debug.print parm & " parm and SlackAmount " & SlackAmount
        end If
    end If
end Sub

Sub slacktimer_timer()
    'msgbox "location change" & RightFlipper.Y & " and " & LeftFlipper.Y
    if RightFlipper.Y > FlipY + 2 Then
        RightFlipper.Y = RightFlipper.Y - 2
      '  RFLogo.Y = RFLogo.Y - 2
    else
        RightFlipper.Y = FlipY
       ' RFLogo.Y = FlipY
    End If
    if LeftFlipper.Y > FlipY + 2 Then
        LeftFlipper.Y = LeftFlipper.Y - 2
       ' LFLogo.Y = LFLogo.Y - 2
    else
        LeftFlipper.Y = FlipY
      '  LFLogo.Y = FlipY
    End If

    if LeftFlipper.Y = FlipY and RightFlipper.Y = FlipY Then slacktimer.Enabled = 0
end Sub

'******************************************************
'			FLIPPER TRICKS
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
	Dim b, BOT
	BOT = GetBalls

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
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
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then EOSNudge1 = 0
	End If
End Sub


'*************************************************
' Check ball distance from Flipper for Rem
'*************************************************
Const PI=3.1415927

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
' End - Check ball distance from Flipper for Rem
'*************************************************



dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 0.8
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
Const EOSReturn = 0.025

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
		Dim BOT, b
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


'*****************************************************************************************************
'*******************************************************************************************************
'END nFOZZY FLIPPERS'


Dim doubletimeonmulti

Sub LiReplay_Timer 
'	If JP003.state=2 Then
'		If doubletimeonmulti=0 Then
'			doubletimeonmulti=1
'		Else
'			LiReplay2.state=2 
'			LiReplay2.timerenabled=1 
'			Light003.state=2
'			Light051.state=2
'			drainhelptimer.enabled=1
'		End If
'	Else
	If Doubletimeonmulti>0 Then
		doubletimeonmulti=doubletimeonmulti-1
	Else

		lireplay.blinkinterval=60 : lireplay001.blinkinterval=60

		LiReplay2.timerenabled=1 
		Light003.state=2
		Light051.state=2
		drainhelptimer.enabled=1
	End If
End Sub

Lireplay2.timerinterval = 3200
Sub LiReplay2_Timer
		Lireplay.state=0 : lireplay001.state = 0 : lireplay.blinkinterval=200 : lireplay001.blinkinterval=200
		LiReplay2.state=2 
		Lireplay2.blinkinterval=60
		doubletimeonmulti=0
		LiReplay.timerenabled=0
		Light003.state=2
		Light051.state=0
pLireplay.blenddisablelighting=1
'		LiReplay2.intensity=1
End Sub

drainhelptimer.interval = 4700
Sub drainhelptimer_Timer  'extra time after light go our on autosave
'		LiReplay2.intensity=10
		LiReplay2.state=0 

		LiReplay2.timerenabled=0
		drainhelptimer.enabled=0
End Sub


Sub DelayStartNewGame_Timer
	Stoptalking : PlaySound "thisistheway1",0,1*BackGlassVolumeDial
	DelayStartNewGame.enabled=0
End Sub


Sub Ballsaved_Timer
	ballsavedstatus=0
	ballsaved.enabled=0
End Sub

Sub drainsoundtimer_Timer
	drainsoundtimer.enabled=0 ' only once pr 3 seconds
End Sub

Sub LSMissions_Timer
	LSMissions.StopPlay
	LSMissions.timerenabled=0
End Sub

dim finalsteps
Sub finalrestart_Timer
	Dim bot
	BOT = GetBalls
	If finalsteps=0 Then	
		If uBound(BOT) = -1 Then finalsteps=1 : ManualLightSequencer.enabled=1 : MSeqCounter=180 : MSeqBigFL=70 : ButtonBlink=15
	Else
		finalsteps=finalsteps+1

	End If

'	debug.print "finalsteps=" &finalsteps & "  getballs=" & uBound(BOT)+1  & "  bossstatus" & bossstatus

	

	if finalsteps>5 And bossstatus=13 Then

		BossStatus=0
		skillshot=1 
		LiSkillshot1.state=2 
		LiSkillshot2.state=2 
		Plunger001.Timerenabled=0
		autokicker.enabled=0 

		BallRelease.CreateBall
		BallRelease.Kick 90, 5 : DOF 145,2 : luttarget=2
		boss3_timer
		BIP=1
		li21.timerenabled=0
		li21.state=2
		li21.timerenabled=1
		LiSpesialLeft.state=0
		LiSpesialLeftFL.visible=1 : LiSpesialLeftFL.timerenabled=1
		PlaySound SoundFX("fx_kicker",DOFContactors), 0,.81*VolumeDial,AudioPan(BallRelease),0.2,0,0,1,AudioFade(BallRelease)
		Gate002.twoway=true
'		If Not TournamentMode Then
		LiReplay.state=2 : lireplay001.state = 2
		pLireplay.blenddisablelighting=5

		Light003.state=0
		Light051.state=2

		Superscoring=0
		MultiballActive=0
		GreenBorders
		finalsteps=0
		Finalflashers.enabled=0 : DOF 112,0 : DOF 118,0
		finalrestart.enabled=0
		Playsound "timetogo",0,1*BackGlassVolumeDial
	End If
End Sub

Dim Extragrace : Extragrace=0
Dim replayballs, ballswaiting,GAMEISOVER
DrainWaiting.interval = 800
Sub Drain_Hit()

	If Boss7Countdown>0 And BIP=1 Then Boss7Countdown=Boss7Countdown+3  ' give more time is wizard and just 1 ball active !

	PlaySound SoundFX("Metal_Touch_1",DOFContactors),0,VolumeDial,AudioPan(Drain),0.20,0,0,1,AudioFade(Drain)
	PlaySound SoundFX("fx_kicker_enter",DOFContactors), 0,.47*VolumeDial,AudioPan(Kicker002),0,0,0,1,AudioFade(Kicker002)



	If LockedReleseGO.enabled=1 Or ReleasedBalls = 1 Then Drain.DestroyBall : PlaySound SoundFX("Drain_" & int(rnd(1)*9)+1,DOFContactors),0,0.33*VolumeDial,AudioPan(Drain),0.20,0,0,1,AudioFade(Drain) : Exit Sub
'	If WaitforInitialsstatus > 0 And WaitforInitialsstatus<55 Then Drain.DestroyBall : PlaySound "Drain_" & int(rnd(1)*9)+1,0,0.33,AudioPan(Drain),0.20,0,0,1,AudioFade(Drain) : Exit Sub


	If BIP>1 And SUPERSCORING=1 And Not Tilted Then
		BIP = BIP-1 
		Drain.DestroyBall
		 PlaySound SoundFX("Drain_" & int(rnd(1)*9)+1,DOFContactors),0,0.33*VolumeDial,AudioPan(Drain),0.20,0,0,1,AudioFade(Drain) 
		StopSound "DarkTrooperMusic"
		Stopsound "cantina1" 
		JP001.state=0 
		JP002.state=0
		JP003.state=0
		JP004.state=0
		JP005.state=0
		JP006.state=0 
		quickMBactive=0
		MultiBallActive=0

		MusicVolume = PlayVolume (SongNr)* MusicVolumeDial' return to normal music playing
		Boss3_Timer
		boss7.timerenabled=0
		bossactive=0
		Boss7Countdown=0 
		DisplayBoss7Status=0
'		BossStatus=0
		boss7status=0
		boss7ramps=0
		Exit Sub
	End If

	If lireplay2.state=2 or LiReplay.State=2 Then Extragrace=Extragrace+1

	If Tilted Then DrainWaiting.interval = 3300 
	DrainWaiting.enabled=1 ' 1,3seconds wait before destroy
End Sub

Sub Drainwaiting_timer
	DrainWaiting.enabled=0
	DrainWaiting.interval = 800

	DOF 214,2 ' drain hit
	'Debug.print "DOF 214, 2"  'apophis
	redcockpit.play seqblinking,,2,40
	If ManualLightSequencer.enabled=0 Then ManualLightSequencer.enabled=1 : MSeqCounter=195 : MSeqBigFL=25

	Drain.DestroyBall

	luttarget=8

	LSCockpit.StopPlay
	LSCockpit.Play SeqBlinking,,10,40
	PlaySound SoundFX("Drain_" & int(rnd(1)*9)+1,DOFContactors),0,0.33*VolumeDial,AudioPan(Drain),0.20,0,0,1,AudioFade(Drain) 
	Stoptalking 

	If Tilted Then
		Extragrace=0
		replayballs=0
		ballswaiting=0
		Lileftinlane.state=0
		Lirightinlane.state=0
		If Lilock2.state<2 Then Lilock1.state=0 'mb not ready
		LiLock001.timerenabled=0 : lockoffcount=0 : lilock1.blinkinterval = 110

		bip=bip-1
		PlaySound "nooooooo",0,1*BackGlassVolumeDial
		If BiP=1 Then
			REDMISSION.StopPlay
			REDMISSION.Play SeqBlinking,,20,30
			LSMissions.Play SeqAllOff
			LSMissions.timerenabled=1
		Else
			REDMISSION.Play SeqBlinking,,5,35
		End If

	elseIf lireplay2.state=2 or LiReplay.State=2 or Extragrace > 0 Then
		Extragrace=Extragrace-1
		replayballs=replayballs+1 : ballsaved.enabled=1 : ballsavedstatus=1
		
		If drainsoundtimer.enabled=0 Then

			Stoptalking : talkingdelay=30
			If MultiballActive=0 Then
				i=int(rnd(1)*15)
				Select Case i
					Case 0  : PlaySound "anyupdateyet",0,1*BackGlassVolumeDial
					Case 1  : PlaySound "bringinghimback",0,1*BackGlassVolumeDial
					Case 2  : PlaySound "bringingyou",0,1*BackGlassVolumeDial
					Case 3  : PlaySound "crestonlyreason",0,1*BackGlassVolumeDial
					Case 4  : PlaySound "stillhave",0,1*BackGlassVolumeDial
					Case 5  : PlaySound "ifyoudoso",0,1*BackGlassVolumeDial
					Case 6  : PlaySound "keepbuzy",0,1*BackGlassVolumeDial
					Case 7  : PlaySound "imsorry",0,1*BackGlassVolumeDial
					Case 8  : PlaySound "magichand",0,1*BackGlassVolumeDial
					Case 9  : PlaySound "leavetheroom",0,1*BackGlassVolumeDial
					Case 10 : PlaySound "niceman",0,1*BackGlassVolumeDial
					Case 11 : PlaySound "stillwaiting",0,1*BackGlassVolumeDial
					Case 12 : PlaySound "onlyhope",0,1*BackGlassVolumeDial
					Case 13 : PlaySound "alivetoo",0,1*BackGlassVolumeDial
					Case 14 : PlaySound "startyourrun",0,1*BackGlassVolumeDial
				End Select
			Else
				REDMISSION.Play SeqBlinking,,5,35
				i=int(rnd(1)*6)
				Select Case i
					Case 0  : PlaySound "anyupdateyet",0,1*BackGlassVolumeDial
					Case 1  : PlaySound "bringingyou",0,1*BackGlassVolumeDial
					Case 2  : PlaySound "ifyoudoso",0,1*BackGlassVolumeDial
					Case 3  : PlaySound "imsorry",0,1*BackGlassVolumeDial
					Case 4  : PlaySound "nooooooo",0,1*BackGlassVolumeDial
					Case 5  : PlaySound "stillwaiting",0,1*BackGlassVolumeDial
				End Select
			End If
			drainsoundtimer.enabled=1
		End If
	Else
		bip=bip-1
		If BIP<1 Then
			If Ballinplay=3 And currentplayer=PlayersPlaying Then ' razer crashing start so just noooooo
				PlaySound "nooooooo",0,1*BackGlassVolumeDial			
			Else
			i=int(rnd(1)*4)
				Select Case i
					Case 0  : PlaySound "youhadyourshot",0,BackGlassVolumeDial
					Case 1  : PlaySound "nopucks",0,BackGlassVolumeDial
					Case 2  : PlaySound "notinterestedin",0,BackGlassVolumeDial
					Case 3  : PlaySound "everythingfromscratch",0,BackGlassVolumeDial


				End Select
			End If
		Else
			PlaySound "nooooooo",0,1*BackGlassVolumeDial
		End If

		If BiP=1 Then
			REDMISSION.StopPlay
			REDMISSION.Play SeqBlinking,,20,30
			LSMissions.Play SeqAllOff
			LSMissions.timerenabled=1
		Else
			REDMISSION.Play SeqBlinking,,5,35
		End If
	End If

'		If MultiballActive>0 and BIP>4 Then	Replayballs=0
'		If quickMBactive and BIP>2 Then Replayballs=0
	If replayballs>0 Then
		replayballs=replayballs-1
		If replayballs>0 then Multiballtimer.enabled=1
		ballswaiting=ballswaiting+1
		If BIP=1 And Selectballs=21 Then SelectBall=int(rnd(1)*20)
	End If
	
	If Bip=1 then 
		luttarget=2
		quickMBactive=0
		GreenBorders
		MultiBallActive=0
		If JP003.state>0 Then
			PlaySound "cantinaend",0, 0.45* MusicVolumeDial ,0,0,0,0,0,0
			DOF 210,0 ' multiball end
			'Debug.print "DOF 210, 0"  'apophis
		End If
		Stopsound "cantina1" 
		If BossActive=1 And bosslevel=7 Then
			MusicVolume = 0.01
		Else
			MusicVolume = PlayVolume (SongNr) * MusicVolumeDial 
		End If
		JP001.state=0 
		JP002.state=0
		JP003.state=0
		JP004.state=0
		JP005.state=0
		JP006.state=0 
		JPsequencer.play seqblinking,,3,30
		RampsDoneForLockLight=0
	End If

	If BIP < 1 Then

		Lileftinlane.state=0
		Lirightinlane.state=0
		If Lilock2.state<2 Then Lilock1.state=0 'mb not ready
		LiLock001.timerenabled=0 : lockoffcount=0 : lilock1.blinkinterval = 110

		ballsavedstatus=0
		StopSound "DarkTrooperMusic"
		Stopsound "cantina1" 
		JP001.state=0 
		JP002.state=0
		JP003.state=0
		JP004.state=0
		JP005.state=0
		JP006.state=0 
		quickMBactive=0
		MultiBallActive=0

		MusicVolume = PlayVolume (SongNr) * MusicVolumeDial ' return to normal music playing
		If Bosslevel=7 Then
			Boss3_Timer
			bossactive=0
		End If

		boss7.timerenabled=0
		Boss7Countdown=0 
		DisplayBoss7Status=0

		boss7status=0
		boss7ramps=0

		If Tilted Then finalrestart.enabled= False
		If finalrestart.enabled Then Exit Sub

		BossStatus=0


		If FlyAgain3.state=0 And BallinPlay = 3 And currentplayer=PlayersPlaying Then ' last play last ball play crashvideo ...
			GAMEISOVER=1
			PlayTune(2)
			If FlexDMD then UmainDMD.cancelrendering() : UMainDMD.DisplayScene00ExWithId "attract1",false,vid1, "END OF BALL", 15, 0, "BONUS ", 15, 0, 14, 44000, 14
		End If
		BonusStatus=1 ' give bonus	
		If Tilted Then BonusStatus=20 ' or not give bonus ! show things first
		Bonustime.enabled=1	


		Tilted = False
		TiltValue = 0

		'reset some scoring values for each ball'' mby 
		ScoringRightRamp = 40000
		ScoringLeftRamp = 40000
		SecretScoring = 150000

		DOF 118,0
		'Debug.print "DOF 118, 0"  'apophis
		letterblink=7
		LOSTBALL_Timer
		redcockpit.play seqblinking,,20,40
		If Selectballs<20 Then SelectBall=SelectBalls : else : If SelectBalls=21 Then SelectBall=int(rnd(1)*20) : End If
		BlueBorders

		Displaybuzy=0
		ClearAllStatus


		RestartGame=0 
		RestartCounter = 0

		kickertimer.enabled=0 
		plungerrelease.enabled=0 


	End If
	
End Sub

Sub Lostball_Timer
	If Lostball.enabled=0 Then
		Lostball.interval=950
		Lostball.enabled=1
		Lostball.uservalue=1
		Light038.state=2

	Else
		Lostball.interval=70
		i=Lostball.uservalue
'		If primitive001.material="metal gold dark" Then
'			primitive001.material="metal wire gold"
'		Else 
'			primitive001.material="metal gold dark"
'		End If
		Select Case i
			Case 15 : Bumper1_Hit
			Case 17 : Bumper2_Hit
			Case 19 : Bumper3_Hit
			Case 20 : Dt001.isdropped=True : Dt001.timerenabled=1 : PlaySound SoundFX("fx_droptarget",DOFContactors), 0, .1*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) 
			Case 21 : Dt002.isdropped=True : Dt002.timerenabled=1 : PlaySound SoundFX("fx_droptarget",DOFContactors), 0, .1*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) 
			Case 22 : Dt003.isdropped=True : Dt003.timerenabled=1 : PlaySound SoundFX("fx_droptarget",DOFContactors), 0, .1*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) 
			Case 23 : Dt004.isdropped=True : Dt004.timerenabled=1 : PlaySound SoundFX("fx_droptarget",DOFContactors), 0, .1*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) 
			Case 24 : Dt005.isdropped=True : Dt005.timerenabled=1 : PlaySound SoundFX("fx_droptarget",DOFContactors), 0, .1*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) 
			Case 25 : Dt006.isdropped=True : Dt006.timerenabled=1 : PlaySound SoundFX("fx_droptarget",DOFContactors), 0, .1*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) 
			Case 26 : Dt007.isdropped=True : Dt007.timerenabled=1 : PlaySound SoundFX("fx_droptarget",DOFContactors), 0, .1*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) 
			Case 27 : Dt008.isdropped=True : Dt008.timerenabled=1 : PlaySound SoundFX("fx_droptarget",DOFContactors), 0, .1*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) 
			Case 28 : Dt009.isdropped=True : Dt009.timerenabled=1 : PlaySound SoundFX("fx_droptarget",DOFContactors), 0, .1*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) 
			Case 29 : Dt010.isdropped=True : Dt010.timerenabled=1 : PlaySound SoundFX("fx_droptarget",DOFContactors), 0, .1*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) 
		End Select
		i=i+1
		Lostball.uservalue=i
		If i >40 Then 
			lostball.enabled=0
			Lostball.uservalue=0
			Light038.state=1 
'			primitive001.material="metal wire gold"
		End If
	End If
End Sub

Sub kickerhold_Timer
	If	kickerReady=0 And ballswaiting>0 Then
		If GAMEISOVER=0 then 
			kickerReady=1
			ballswaiting=ballswaiting-1
			autokicker.enabled=1 
			BallRelease.CreateBall
			PlaySound SoundFX("BallRelease" & int(rnd(1)*7)+1 , DOFContactors), 0,.71*VolumeDial,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)
			BallRelease.Kick 90, 5 : DOF 145,2  : luttarget=2
			PlaySound SoundFX("fx_kicker",DOFContactors), 0,0.4*VolumeDial,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)
		End If
	End If
End Sub


Dim hscore(11),temphs(11),hsover
Dim CounterB2s


Sub UpdateHighScores
	hsover=0
	For i = 10 to 1 step -1
		If P1score>hscore(i) then
		hsover=i
		hscore(i+1)=hscore(i)
		End If
	Next
	hscore(hsover)=P1score
end Sub

dim BonusStatus, FlyAgainStatus, attract1Mode, attractModeStatus, WaitforInitialsstatus, WaitforInitialsPlace
dim bonusover, completejustonce ,replaygoaldisplaystatus
dim stopdoubletrouble   ' found the bug at drain... this is not needed :i think

Sub Bonustime_Timer
	'bonus starts here
	LeftFlipper.timerenabled=0 
	TopFlipper.timerenabled=0
	infostatus=0
	leftflipperdown=0
	rightflipperdown=0

	If BonusStatus=10 Then
		
		luttarget=2

		Light069.state=0

		Bonustime.enabled=0
		If BallinPlay=2 And P1score<ReplayGoals And FlyAgain3.state=0 And knockeronce=0 Then replaygoaldisplaystatus=1
		If FlyAgain3.state=2 Then
			FlyagainSequencer.Play SeqBlinking,,20,30
			If BallinPlay=1 Then Stoptalking : PlaySound "yourreputation",0,1*BackGlassVolumeDial
			If BallinPlay=2 Then
				Stoptalking 
				If P1score>14999999 Then 
					If completejustonce=0 Then
						PlaySound "makeyoucomplete",0,1*BackGlassVolumeDial : completejustonce=1 
					Else
						PlaySound "morecredits",0,1*BackGlassVolumeDial
					End If
				Else
					PlaySound "morecredits",0,1*BackGlassVolumeDial
				End If
			End If

			If BallinPlay=3 Then
				Stoptalking 
				If P1score>14999999 Then 
					If completejustonce=0 Then
						PlaySound "makeyoucomplete",0,1*BackGlassVolumeDial : completejustonce=1 
					Else
						PlaySound "yourreputation",0,1*BackGlassVolumeDial
					End If
				Else
					PlaySound "yourreputation",0,1*BackGlassVolumeDial

				End If
			End If

			FlyAgain1.state=0 : pflyagain.blenddisablelighting=1
			FlyAgain2.state=0
			FlyAgain3.state=0
			If extraextraball>0 Then
				extraextraball=extraextraball-1
				FlyAgain1.state=2 : pflyagain.blenddisablelighting=5
				FlyAgain2.state=2
				FlyAgain3.state=2
			End If
			FlyAgainStatus=1
			If wormhole=2 And extraballlight.state=1 Then extraballlight.state=2 : ExtraSeq.Play Seqblinking,,3,50
			nextBallPlz
			If FlexDMD Then UMainDMD.cancelrendering() : tempstr=FormatScore(P1score) : UMainDMD.DisplayScene00Ex FlexBG, "PLAYER." & currentplayer & "  Ball."& cstr (ballinplay), 0, 12, FlexScore , 15, 1, 14, 700, 14 


		Else
		' no more extraballs from here

				
			' multiplayer action!
			' check highscores here before swapping players !


			If BallinPlay = 3 Then  ' check last ball on each player

				If osbactive Then SubmitOSBScore(p1score)

				WaitforInitialsstatus=4
				If P1score>hscore(5) Then WaitforInitialsstatus=1 : WaitforInitialsPlace=5 
				If P1score>hscore(4) Then WaitforInitialsstatus=1 : WaitforInitialsPlace=4
				If P1score>hscore(3) Then WaitforInitialsstatus=1 : WaitforInitialsPlace=3
				If P1score>hscore(2) Then WaitforInitialsstatus=1 : WaitforInitialsPlace=2
				If P1score>hscore(1) Then WaitforInitialsstatus=1 : WaitforInitialsPlace=1 : trippleknocker.enabled=1 : Credits=Credits+1
				If P1score<ReplayGoals And knockeronce=0 Then
					knockeronce=1
					ReplayGoalDown=ReplayGoalDown+1
					If ReplayGoalDown>5 Then
						ReplayGoalDown=0
						Replaygoals=Replaygoals-5000000
						If replaygoals<20000000 Then replaygoals=20000000
					End If
				End If
'				debug.print "WaitforInitialsstatus=" & WaitforInitialsstatus &  "1=HS else passHS"
				If WaitforInitialsstatus = 1 Then
					waitForHighScore.enabled=1
					Exit Sub
				End If
			End If
			SwapPlayers	
		End If	
	End If
End Sub


Sub WaitForHighScore_Timer ' then next player
'debug.print "WaitforHighscore_Timer   WaitforInitialsstatus=5 to skip"
	If WaitforInitialsstatus=5 Then
		WaitForHighScore.enabled=0
		LiDoTriR1.state=2 : lidotriR1.Timerenabled=1
		LiDoTriL1.state=2 : lidotriL1.Timerenabled=1
		LiDoTriR001.state=LiDoTriR1.state
		LiDoTriL001.state=LiDoTriL1.state
'		attractModeStatus=1
'		WaitforInitialsstatus=55
'		Luttarget=8
		LastScore=P1score
		If P1score>TodaysTopScore Then TodaysTopScore=P1score
		updateHighScores

		SwapPlayers

	End If
End Sub



Dim PlayerUpstatus

Sub SwapPlayers
	if ExtraBall=1 Then MissionsDoneThisBall=0
	Extraball=0

	If currentplayer=Playersplaying And ballinplay=3 Then
		SwapPlayerTimer.enabled=True
	Else
		SwapPlayerTimer.enabled=True
		SavePlayerPos CurrentPlayer
'debug.print "saveplayerpos:" & CurrentPlayer
	End If

End Sub

Sub SwapPlayerTimer_Timer
	SwapPlayerTimer.enabled=False


	CurrentPlayer = CurrentPlayer + 1

	If CurrentPlayer > PlayersPlaying Then
		CurrentPlayer = 1
		BallinPlay=BallinPlay+1
	End If

	If BallinPlay=4 Then
		CurrentPlayer = PlayersPlaying
		wormhole=0 ' close BlastDoor
	Else
		LoadPlayerPos CurrentPlayer
'debug.print "Loadplayerpos:" & CurrentPlayer & "   Ballinplay:" & BallinPlay
	End If

	EMPlayerNr.setvalue Currentplayer
	If HideDesktop=1 Then
		If CurrentPlayer=1 Then controller.B2SSetData 41,1 Else controller.B2SSetData 41,0
		If CurrentPlayer=1 Then controller.B2SSetData 42,1 Else controller.B2SSetData 42,0
		If CurrentPlayer=1 Then controller.B2SSetData 43,1 Else controller.B2SSetData 43,0
		If CurrentPlayer=1 Then controller.B2SSetData 44,1 Else controller.B2SSetData 44,0
		controller.B2SSetData 40,1
	End If



	IF BallinPlay < 4 And PlayersPlaying > 1 Then PlayerUpstatus=CurrentPlayer
	If HideDesktop=1 Then controller.B2SSetScorePlayer1 P1score

	Stoptalking


	spinnermission=0
	bumpermystery=0
	If BallinPlay= 1 Then
'		DelayStartNewGame.enabled=1
		GetReadyToPlay=1
		spinnermission=7
		bumpermystery=7
	End If
	If BallinPlay= 2 Then Complicated = 1
	If BallinPlay= 3 Then BestinParcec = 1


	' **** SET EB on right outlane if you do it poorly
	If Not TournamentMode Then
		If BallinPlay=2 And P1Score<3900000 Then LiSpecialRight.state=2 : LiSpecialRight2.state=0 : LiSpecialrightFL.visible=1 : LiSpecialrightFL.timerenabled=1
		If BallinPlay=3 And P1Score<6900000 Then LiSpecialRight.state=2 : LiSpecialRight2.state=0 : LiSpecialrightFL.visible=1 : LiSpecialrightFL.timerenabled=1
		If BallinPlay=3 And P1Score<9900000 Then
			If TotalExtraBalls<2 And MissionsDoneThisBall<2 Then LiSpecialRight.state=2 : LiSpecialRight2.state=0 : LiSpecialrightFL.visible=1 : LiSpecialrightFL.timerenabled=1
		End If
	End If
	If LiSpecialRight.state=0 Then LiSpecialRight2.state=1
	if BallinPlay<4 Then 
		spinnerstatus=1


		EMBallInPlay.setvalue BallinPlay
		If HideDesktop=1 Then
			If BallinPlay=1 Then  controller.B2SSetData 14,1 Else  controller.B2SSetData 14,0
			If BallinPlay=2 Then  controller.B2SSetData 15,1 Else  controller.B2SSetData 14,0
			If BallinPlay=3 Then  controller.B2SSetData 16,1  Else controller.B2SSetData 14,0
		End If

		If PlayersPlaying = 1 Then nextBallPlz
		If FlexDMD Then UMainDMD.cancelrendering() : tempstr=FormatScore(P1score) : UMainDMD.DisplayScene00Ex FlexBG, "PLAYER." & currentplayer & "  Ball."& cstr (ballinplay), 0, 12, FlexScore , 15, 1, 14, 700, 14  

	Else

		ShowScores(1)=PlayerSaved(1,0)
		ShowScores(2)=PlayerSaved(2,0)
		ShowScores(3)=PlayerSaved(3,0)
		ShowScores(4)=PlayerSaved(4,0)
		ShowScores(CurrentPlayer) = P1score
		ShowScores(0)=PlayersPlaying

'		IF WaitforInitialsstatus < 5 Then
'		WaitforInitialsstatus=5 ' tellit again for gameoverimter

		GAMEISOVER=1
		DOF 118,0
		'Debug.print "DOF 118, 0"  'apophis
		boss7.timerenabled=0
		MusicVolume = PlayVolume (SongNr) * MusicVolumeDial 
		bossactive=0
		bosslevel=0
		BiP=0
		Boss7Countdown=0 
		finalrestart.enabled=False
		OrangeBorders 
		hunter(7)=2
		hunter(8)=2
		hunter(9)=2
		hunter(10)=2
		hunter(11)=2
		hunter(12)=2

		mission7light.timerenabled=0
		shoottheballstatus=0
		Plunger001.TIMERENABLED=0
		LiDoTriL1.state=2 : LiDoTriL1.timerenabled=1
		LiDoTriR1.state=2 : LiDoTriR1.timerenabled=1 
		LiDoTriR001.state=LiDoTriR1.state
		LiDoTriL001.state=LiDoTriL1.state

		BossDelayBlinker.enabled=0

		ClearAllStatus
		BonusStatus = 11
		Bonusover=1
		If ManualLightSequencer.enabled=0 Then ManualLightSequencer.enabled=1 : MSeqCounter=100 : MSeqBigFL=40
		undertopflipper.state=2
		UNDERLEFTFLIPPER.state=2
		underrightflupper.state=2
		underleftflipper.timerenabled=1 
		luttarget=6

		closebonus_timer

'debug.print "gameover1"

		hunter(7)=2
		hunter(8)=2
		hunter(9)=2
		hunter(10)=2
		hunter(11)=2
		hunter(12)=2

		GameOverStatus=1
		attract1Mode=1
		ButtonBlink=22

		LeftFlipper.timerenabled=0 
		TopFlipper.timerenabled=0
		doubletripple=32

		LockedReleseGO.enabled=1 ' fixing ... let all balls out for all players :)
		LockedReleseGO_Timer

		Bumper1.TimerEnabled=0

		ApronRadar003.state=0 ' red dots = off
		LeftFlipper.RotateToStart
		TopFlipper.RotateToStart
		RightFlipper.RotateToStart
		If Mission(8)=0 Then Mission7ResetAll

		EMBallInPlay.setvalue 0  
		If HideDesktop=1 Then 
			controller.B2SSetData 14,0 
			controller.B2SSetData 15,0
			controller.B2SSetData 16,0 
		End If
	End If
End Sub
Dim ShowScores(4)


Dim bonusdown
Sub closebonus_timer
	If closebonus.enabled=0 Then
		closebonus.enabled=1 
		bonusdown=18
		PlaySound "caution",0,1*BackGlassVolumeDial
		Boss1ALLOFF
	Else
		If bonusdown>2 And bonusmultiplyer>bonusdown Then PlaySound SoundFX("ambiance" , DOFContactors), 0, 0.6*VolumeDial,AudioPan(BallRelease),0,0,0,1,AudioFade(BallRelease)
		For i = 1 To 12 : hunter(i)=0 : Next
		Select Case bonusdown
			case 12 : Licenter012.state=0 : LiCenter026.state=1 : LiCenter026.timerenabled=1
					  enterlight.state=0 : Libonus1.state=0
			case 11 : Licenter011.state=0 : LiCenter025.state=1 : LiCenter025.timerenabled=1
					  mysterylight.state=0 : Libonus2.state=0 : boss7.state=0
					  shoottheballstatus=0
			case 10 : Licenter010.state=0 : LiCenter024.state=1 : LiCenter024.timerenabled=1
					  extraballlight.state=0 : Libonus3.state=0
					  Mission1light.state=0 : Mission1light003.state=0 : Mission1light004.state=0 : Mission1light005.state=0 : mission(1)=0 ': Mission1FL1.visible=1 : Mission1FL1.timerenabled=1
					  LiSupply006.state=0 : boss6.state=0

						Raise_DT 1

						Raise_DT 2
						li21.state=0
						LiSpecialRight.state=0
						LiSpecialRight2.state=0
						LiSpesialLeft.state=0
						Lileftinlane.state=0
						Lirightinlane.state=0
						libonus1.state=0
						libonus2.state=0
						libonus3.state=0
						libonus01.state=0
						libonus02.state=0
						libonus03.state=0





			case 9  : bonusx009.state=2 : Licenter009.state=0 : LiCenter023.state=1 : LiCenter023.timerenabled=1 
					  Mission2light.state=0 : Mission2light003.state=0 : Mission2light004.state=0 : Mission2light005.state=0 : mission(2)=0 ': Mission2FL1.visible=1 : Mission2FL1.timerenabled=1
					  LiSupply003.state=0 : LiSupply010.state=0 : boss5.state=0
					  RaiseBigDT.enabled=1 : BigDTUD=2  : PlaySound SoundFX("elengine",DOFContactors), 0, 0.8*VolumeDial, AudioPan(Primitive059), 0.05,0,0,1,AudioFade(Primitive059)'big target must down

			case 8  : bonusx008.state=2 : BonusX009.state=0 : Licenter008.state=0 : LiCenter022.state=1 : LiCenter022.timerenabled=1
					  Mission3light.state=0 : Mission3light003.state=0 : Mission3light004.state=0 : Mission3light005.state=0 : mission(3)=0 ': Mission3FL1.visible=1 : Mission3FL1.timerenabled=1
					  LiSupply005.state=0 : LiRefuel1.state=0 : boss4.state=0
			case 7  : bonusx007.state=2 : BonusX008.state=0 : Licenter007.state=0 : LiCenter021.state=1 : LiCenter021.timerenabled=1
					  Mission4light.state=0 : Mission4light003.state=0 : Mission4light004.state=0 : Mission4light005.state=0 : mission(4)=0 ': Mission4FL1.visible=1 : Mission4FL1.timerenabled=1
					  LiSupply009.state=0 : LiSecret2.state=0 : LiSecret3.state=0 : boss3.state=0
			case 6  : bonusx006.state=2 : BonusX007.state=0 : Licenter006.state=0 : LiCenter020.state=1 : LiCenter020.timerenabled=1
					  Mission5light.state=0 : Mission5light003.state=0 : Mission5light004.state=0 : Mission5light005.state=0 : mission(5)=0 ': Mission5FL1.visible=1 : Mission5FL1.timerenabled=1
					  boss2.state=0
			case 5  : bonusx005.state=2 : BonusX006.state=0 : Licenter005.state=0 : LiCenter019.state=1 : LiCenter019.timerenabled=1
					  Mission6light.state=0 : Mission6light003.state=0 : Mission6light004.state=0 : Mission6light005.state=0 : mission(6)=0' : Mission6FL1.visible=1 : Mission6FL1.timerenabled=1
					  LiSupply002.state=0 : boss1.state=0
					  LiSupply004.state=0 : LiBounty1.state=0 : libounty11.state=0 : Lihunter1.state=0

			case 4  : Drop_DT 3 :  bonusx004.state=2 : BonusX005.state=0 : Licenter004.state=0 : LiCenter018.state=1 : LiCenter018.timerenabled=1
					  Mission7light.state=0 : Mission7light003.state=0 : Mission7light004.state=0 : Mission7light005.state=0 : mission(7)=0' : Mission7FL1.visible=1 : Mission7FL1.timerenabled=1
					  LiSupply001.state=0 : LiSupply007.state=0 : LiBounty2.state=0 : libounty22.state=0 : Lihunter2.state=0
			case 3  : Drop_DT 4 : bonusx003.state=2 : BonusX004.state=0 : Licenter003.state=0 : LiCenter017.state=1 : LiCenter017.timerenabled=1
					  Mission8light.state=0 : Mission8light003.state=0 : Mission8light004.state=0 : Mission8light005.state=0 : mission(8)=0 ': Mission8FL1.visible=1 : Mission8FL1.timerenabled=1
					  LiBounty3.state=0 : libounty33.state=0 : Lihunter3.state=0
			case 2  : Drop_DT 5 : bonusx002.state=2 : BonusX003.state=0 : Licenter002.state=0 : LiCenter016.state=1 : LiCenter016.timerenabled=1
					  LiBounty4.state=0 : libounty44.state=0 : Lihunter4.state=0
			case 1  : Drop_DT 6 : bonusx001.state=2 : BonusX002.state=0 : Licenter001.state=0 : LiCenter015.state=1 : LiCenter015.timerenabled=1
					  LiBounty5.state=0 : libounty55.state=0 : Lihunter5.state=0
			case 0  : Drop_DT 7 : bonusx001.state=0 : closebonus.enabled=0 : bonusdown=0 : bonusmultiplyer=0
					  LiBounty6.state=0 : libounty66.state=0 : Lihunter6.state=0
					  Sw51up.enabled=1
					  PlaySound SoundFX("springchange",DOFContactors), 0, .88*VolumeDial, AudioPan(sw51), 0.05,0,0,1,AudioFade(sw51)
					  Sw51down.enabled=0
					  kicker001up.enabled=1

		End Select
		bonusdown=bonusdown-1
		If quickMB>bonusdown Then quickmb=bonusdown
	End If
End Sub


Sub OverallFlasher001_Timer : OverallFlasher001.visible=0 : OverallFlasher001.timerenabled=0 : End Sub


Dim MSeqBigFL, MSeqCounter, MSeqDelay1

Sub ManualLightSequencer_Timer

	MSeqDelay1=MSeqDelay1+1
	If MSeqDelay1>2 Then 
		MSeqDelay1=0
		If MseqBigFL>0 Then
			MseqBigFL=MseqBigFL-1
			i=Int(Rnd(1)*5)+1
			If i = 1 Then OverallFlasher001.Color=RGB(64,0,0) : OverallFlasher001.timerenabled=1
			If i = 2 Then OverallFlasher001.Color=RGB(0,64,0) : OverallFlasher001.timerenabled=1
			If i = 3 Then OverallFlasher001.Color=RGB(128,255,255) : OverallFlasher001.timerenabled=1
			If i < 4 Then 
				If sidepanelstimer.enabled=0 Then
					Primitive026.blenddisablelighting=4
					Wall21.blenddisablelighting=6
					sidepanelstimer.enabled=1
					PlaySound "Relay_On",0,0.5*VolumeDial
					LiBulb001.state=1
					LiBulb002.state=1
					LiBulb003.state=1
					LiBulb004.state=1
					LiBulb005.state=1
					LiBulb006.state=1
					LiBulb007.state=1
					LiBulb008.state=1
					LiBulb009.state=1
					DOF 117,2
					DOF 114,2
					DOF 115,2
					DOF 116,2
					DOF 119,2
'Debug.print "DOF 117, 2"  'apophis
'Debug.print "DOF 114, 2"  'apophis
'Debug.print "DOF 115, 2"  'apophis
'Debug.print "DOF 116, 2"  'apophis
'Debug.print "DOF 119, 2"  'apophis
					If bonustime.enabled=0 And LockedReleseGO.enabled=0 And ReleaseTimer.enabled=0 And gameisover=1 Then luttarget=8 : lutcounter=6


					Primitive035.blenddisablelighting=5
					FlipperLogoL2.image="FlipperLogo1"
					FlipperLogoL.image="FlipperLogo1"
					FlipperLogoR.image="FlipperLogo1"
					Primitive036.image="Droptarget2"
					Flasher006.visible=1
				End If
			End If
		End If
	End if
	MseqCounter=MseqCounter+1
	If MseqCounter>222 Then MseqCounter=0 : ManualLightSequencer.enabled=0 : MseqBigFL=0 : WingLState=2 : WingRState=2 : wingL_blinks=1 : wingR_blinks=1
	If rnd(1)<0.40 Then
		i=int(rnd(1)*48)
		if i >44 Then PlaySound SoundFX("fx_solenoidon",DOFContactors), 0, 0.13*VolumeDial , AudioPan(skillshottrigger), 0.13,0,0,1,AudioFade(skillshottrigger)
'		If i = 32 Then Light034_2.state=1 : Light034_2.timerenabled=1
'		If i = 33 Then Light037_2.state=1 : Light037_2.timerenabled=1
		If i = 34 Then objLevel(1) = 1 : FlasherFlash1_Timer : PlaySound SoundFX("fx_solenoidon3",DOFContactors), 0, 0.04*VolumeDial , AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger)
		If i = 35 Then objLevel(2) = 1 : FlasherFlash2_Timer : PlaySound SoundFX("fx_solenoidon2",DOFContactors), 0, 0.04*VolumeDial , AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger)
		If i = 36 Then objLevel(3) = 1 : FlasherFlash3_Timer : PlaySound SoundFX("fx_solenoidon2",DOFContactors), 0, 0.04*VolumeDial, AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger) : 	DOF 135,2 : 'Debug.print "DOF 135, 2" ' blue left  'apophis
		If i = 37 Then objLevel(4) = 1 : FlasherFlash4_Timer : PlaySound SoundFX("fx_solenoidon2",DOFContactors), 0, 0.04*VolumeDial , AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger) : 	DOF 137,2 : 'Debug.print "DOF 137, 2" 'red left
		If i = 38 Then objLevel(5) = 1 : FlasherFlash5_Timer : PlaySound SoundFX("fx_solenoidon3",DOFContactors), 0, 0.04*VolumeDial , AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger)
		If i = 39 Then objLevel(6) = 1 : FlasherFlash6_Timer : PlaySound SoundFX("fx_solenoidon2",DOFContactors), 0, 0.04*VolumeDial , AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger)
		If i = 40 Then objLevel(7) = 1 : FlasherFlash7_Timer : PlaySound SoundFX("fx_solenoidon3",DOFContactors), 0, 0.04*VolumeDial , AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger)
		If i = 41 Then objLevel(8) = 1 : FlasherFlash8_Timer : PlaySound SoundFX("fx_solenoidon3",DOFContactors), 0, 0.04*VolumeDial , AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger)
		If i = 42 Then objLevel(9) = 1 : FlasherFlash9_Timer : PlaySound SoundFX("fx_solenoidon3",DOFContactors), 0, 0.04*VolumeDial , AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger)
		If i = 43 Then objLevel(10) = 1 : FlasherFlash10_Timer : PlaySound SoundFX("fx_solenoidon3",DOFContactors), 0, 0.04*VolumeDial, AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger) : 	DOF 136,2 : 'Debug.print "DOF 136, 2" ' yel right  'apophis
		If i = 44 Then objLevel(11) = 1 : FlasherFlash11_Timer : PlaySound SoundFX("fx_solenoidon3",DOFContactors), 0, 0.04*VolumeDial , AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger)

		If i = 45 Then Secretblast.state=1 : Secretblast.timerenabled=1 : DOF 139,2 : 'Debug.print "DOF 139, 2"  'apophis
		If i = 46 Then lowbumperblast.state=1 : lowbumperblast.timerenabled=1
		If i = 47 Then lockholeblast.state=1 : lockholeblast.timerenabled=1 : 	DOF 138,2 : 'Debug.print "DOF 138, 2"  'apophis
	End If
End Sub

Dim bloomingDir,bloomingVar
Sub bloom2
	If LiBulb001.state=0 Then
		bloomingDir=-1
	Else
		bloomingDir=3
	End If
	bloomingVar=bloomingVar+bloomingDir
	If bloomingVar<0 Then bloomingVar=0
	If bloomingVar>9 Then bloomingVar=9
	Wall035.blenddisablelighting = bloomingVar/22 'topwalls
	Wall034.blenddisablelighting = bloomingVar/22

	primitive078.blenddisablelighting = bloomingVar/20+1
	primitive079.blenddisablelighting = bloomingVar/10+2
	whitebloom2.opacity = bloomingVar*3

	primitive063.blenddisablelighting = bloomingVar/20
	Primitive074.blenddisablelighting = bloomingVar/17 +.5 + WingL_Pos/13'wings
	Primitive075.blenddisablelighting = bloomingVar/17 +.5 + WingR_Pos/13
	Primitive001.blenddisablelighting = bloomingVar/13
	Primitive003.blenddisablelighting = bloomingVar/13
	Primitive047.blenddisablelighting = bloomingVar/13
	Primitive050.blenddisablelighting = bloomingVar/13 + 0.25
	tiefighter001.blenddisablelighting = bloomingVar/13
	Primitive053.blenddisablelighting = bloomingVar/13
	Primitive054.blenddisablelighting = bloomingVar/13
	Primitive059.blenddisablelighting = bloomingVar/13
	Primitive046.blenddisablelighting = bloomingVar/16
	Primitive045.blenddisablelighting = bloomingVar/16
End Sub


Sub sidepanelstimer_Timer
	If LiBulb001.state=1 Then
		Primitive026.blenddisablelighting=.9
		Wall21.blenddisablelighting=.9

		PlaySound "Relay_Off",0,0.4*VolumeDial
		LiBulb001.state=0
		LiBulb002.state=0
		LiBulb003.state=0
		LiBulb004.state=0
		LiBulb005.state=0
		LiBulb006.state=0
		LiBulb007.state=0
		LiBulb008.state=0
		LiBulb009.state=0
		If bonustime.enabled=0 And LockedReleseGO.enabled=0 And ReleaseTimer.enabled=0 And Gameisover=0 Then luttarget=2 : lutcounter=5

		Primitive035.blenddisablelighting=.5
		FlipperLogoL2.image="FlipperLogo2"
		FlipperLogoL.image="FlipperLogo2"
		FlipperLogoR.image="FlipperLogo2"	
		Primitive036.image="Droptarget1"
		Flasher006.visible=0
	Else
		sidepanelstimer.enabled=0
	End If
End Sub


Sub LightAllMissionlights
	Mission1Light.state=1 : Mission1Light003.state=1 : Mission1Light004.state=1 : Mission1Light005.state=1 ': Mission1FL1.visible=1 : Mission1FL1.timerenabled=1
	Mission2Light.state=1 : Mission2Light003.state=1 : Mission2Light004.state=1 : Mission2Light005.state=1 ': Mission2FL1.visible=1 : Mission2FL1.timerenabled=1
	Mission3Light.state=1 : Mission3Light003.state=1 : Mission3Light004.state=1 : Mission3Light005.state=1 ': Mission3FL1.visible=1 : Mission3FL1.timerenabled=1
	Mission4Light.state=1 : Mission4Light003.state=1 : Mission4Light004.state=1 : Mission4Light005.state=1 ': Mission4FL1.visible=1 : Mission4FL1.timerenabled=1
	Mission5Light.state=1 : Mission5Light003.state=1 : Mission5Light004.state=1 : Mission5Light005.state=1 ': Mission5FL1.visible=1 : Mission5FL1.timerenabled=1
	Mission6Light.state=1 : Mission6Light003.state=1 : Mission6Light004.state=1 : Mission6Light005.state=1 ': Mission6FL1.visible=1 : Mission6FL1.timerenabled=1
	Mission7Light.state=1 : Mission7Light003.state=1 : Mission7Light004.state=1 : Mission7Light005.state=1 ': Mission7FL1.visible=1 : Mission7FL1.timerenabled=1
	Mission8Light.state=1 : Mission8Light003.state=1 : Mission8Light004.state=1 : Mission8Light005.state=1 ': Mission8FL1.visible=1 : Mission8FL1.timerenabled=1
End Sub
Sub StopAllMissionlights
	Mission1Light.state=0 : Mission1Light003.state=0 : Mission1Light004.state=1 : Mission1Light005.state=1 ': Mission1FL1.visible=1 : Mission1FL1.timerenabled=1
	Mission2Light.state=0 : Mission2Light003.state=0 : Mission2Light004.state=1 : Mission2Light005.state=1 ': Mission2FL1.visible=1 : Mission2FL1.timerenabled=1
	Mission3Light.state=0 : Mission3Light003.state=0 : Mission3Light004.state=1 : Mission3Light005.state=1 ': Mission3FL1.visible=1 : Mission3FL1.timerenabled=1
	Mission4Light.state=0 : Mission4Light003.state=0 : Mission4Light004.state=1 : Mission4Light005.state=1 ': Mission4FL1.visible=1 : Mission4FL1.timerenabled=1
	Mission5Light.state=0 : Mission5Light003.state=0 : Mission5Light004.state=1 : Mission5Light005.state=1 ': Mission5FL1.visible=1 : Mission5FL1.timerenabled=1
	Mission6Light.state=0 : Mission6Light003.state=0 : Mission6Light004.state=1 : Mission6Light005.state=1 ': Mission6FL1.visible=1 : Mission6FL1.timerenabled=1
	Mission7Light.state=0 : Mission7Light003.state=0 : Mission7Light004.state=1 : Mission7Light005.state=1 ': Mission7FL1.visible=1 : Mission7FL1.timerenabled=1
	Mission8Light.state=0 : Mission8Light003.state=0 : Mission8Light004.state=1 : Mission8Light005.state=1 ': Mission8FL1.visible=1 : Mission8FL1.timerenabled=1
End Sub

dim gameovernolights, attractwormhole, attractrefuel
Sub LightSeq002_Timer

	If gameovernolights=1 Then
		plungerlane.stopplay

		Select Case BorderColor
			Case 1: YellowBorders
					plungerlane.play seqrandom,2,,2700
DOF 202,2: 'Debug.print "DOF 202, 2"  'apophis
			Case 2: OrangeBorders
					plungerlane.play SeqUpOn,13,3,50
DOF 202,2: 'Debug.print "DOF 202, 2"  'apophis
			Case 3: RedBorders
					plungerlane.play SeqDownOn,13,3,50
DOF 202,2: 'Debug.print "DOF 202, 2"  'apophis
			Case 4: BlueBorders
					plungerlane.play Seqblinking,,10,33 
DOF 202,2: 'Debug.print "DOF 202, 2"  'apophis
			Case 5: GreenBorders
					plungerlane.play Seqblinking,,1,40
DOF 202,2: 'Debug.print "DOF 202, 2"  'apophis
		End Select

		letterblink =12
		LSPassage.Play SeqBlinking,,1,50
		MSeqBigFL   = Int(Rnd(1)*120)
		MSeqCounter = Int(Rnd(1)*70)
		ManualLightSequencer.enabled=1
		' attract move 1 toy
		i=int(rnd(1)*6)
		If i=0 Then shiptrigger_Hit
		If i=1 Then shiptrigger2_Hit
		If i=2 Then turningspeedup=15
		If i=3 Then attractwormhole=400
		If i=4 Then	attractrefuel=700

		LSMissions.StopPlay
		LSMissions.Play SeqRightOn, 20, 1
		LSMissions.Play SeqLeftOn, 20, 1
		LSMissions.Play SeqBlinking,,2,70
		If BossLightBlinker.enabled=0 Then
			If Int(rnd(1)*2)=1 Then	
				bossfights.Play SeqBlinking,,7,150
			Else
				BossLightBlinker.enabled=1 
			End If
		End If
		
		If WaitforInitialsstatus=0 Or WaitforInitialsstatus=55 Then
			LSpathpassage.Play SeqBlinking,,4,101
			LSpathpassage.Play SeqLeftOn,20,5,70

			LSMissions.StopPlay
			i=Int(Rnd(1)*7)
			If i=1 Or i=3 Then
				LSMissions.Play SeqBlinking,,2,110
				LSMissions.Play SeqBlinking,,1,70
				LSMissions.Play SeqBlinking,,2,140
			End If
			If i=2 Then
				LSMissions.Play SeqBlinking,,12,65
				REDMISSION.Play SeqBlinking,,6,130
			End If
			If i=4 Then
				REDMISSION.Play SeqBlinking,,6,130
			End If
			If i=0 Or i=5 Then
				REDMISSION.Play SeqRandom,10,,1250
			End If
			LightSeq002.StopPlay
			LightSeq002.Play SeqRandom,10,,1250
			LightSeq002.Play SeqBlinking,,3,110
			LightSeq002.play SeqDownOn,20,1
			LightSeq002.play SeqAllOn,1,1
	
			If int(rnd(1)*2)=1 Then
				bossblinker.Play SeqBlinking,,12,40
			End If

		End If

	End If
End Sub

Sub undertopflipper_Timer
	undertopflipper.state=1
	UNDERLEFTFLIPPER.state=1
	underrightflupper.state=1
	undertopflipper.timerenabled=0
End	Sub

Sub underleftflipper_Timer
	undertopflipper.state=0
	UNDERLEFTFLIPPER.state=0
	underrightflupper.state=0
	underleftflipper.timerenabled=0

End	Sub




Dim GameOverStatus, P1Level,salvagedjustonce

Sub GameOverTimer_Timer


		If GameOverStatus < 1 Then GameOverStatus=4

		GameOverTimer.interval=100

		ringsonly=0
		turningshipon=0 
		Plunger001.timerenabled=0
		shoottheballstatus=0
		LOCKED001.state=0
		LOCKED002.state=0
		LOCKED003.state=0
		LiLock1.state=0
		LiLock2.state=0
		If LightSeq002.timerenabled=0 Then LightSeq002.timerenabled=1 
		If WaitforInitialsstatus=5 Then '  only after entering highscore name
			WaitforInitialsstatus=4
			If FlexDMD then UmainDMD.cancelrendering() : UMainDMD.DisplayScene00ExWithId "attract1",false,"grogu2.wmv", "    ", 15, 0, "PLAY AGAIN ! ", 15, 0, 14, 44000, 14

		End If
		If WaitforInitialsstatus=4 Then
			
			EMPlayerNr.setvalue 0
			If HideDesktop=1 Then
				 controller.B2SSetData 41,0
				 controller.B2SSetData 42,0
				 controller.B2SSetData 43,0
				 controller.B2SSetData 44,0
				 controller.B2SSetData 40,0

			End If

			LiDoTriR1.state=2 : lidotriR1.Timerenabled=1
			LiDoTriL1.state=2 : lidotriL1.Timerenabled=1
			LiDoTriR001.state=LiDoTriR1.state
			LiDoTriL001.state=LiDoTriL1.state
			attractModeStatus=1
			WaitforInitialsstatus=55
			Luttarget=8
'			LastScore=P1score
'			If P1score>TodaysTopScore Then TodaysTopScore=P1score
'			updateHighScores
			If HideDesktop=1 Then controller.B2SSetScorePlayer1 P1score
		End If

'		If GameOverStatus>3 And ReleaseTimer.enabled=0 And EvasiveInit.enabled=0 And waitforInitialsstatus=55 Then luttarget=2

		
		If HideDesktop=1 Then controller.B2SSetData 34,0 : Controller.B2SSetData 33,0

			
	If Gameoverstatus=5 Then

		EMPlayerNr.setvalue 1
		If HideDesktop=1 Then
			 controller.B2SSetData 41,1
			 controller.B2SSetData 40,1
		End If

		ResetPlayerSaved

		LiSpecialRight2.state=2
		LiSpecialright2FL.visible=1 : LiSpecialright2FL.timerenabled=1
		LiSpecialRight2.timerenabled=1
		PlaySound SoundFX("off",DOFContactors), 0, .7*VolumeDial, AudioPan(rightoutlane), 0.05,0,0,1,AudioFade(rightoutlane)

		luttarget=2 : lutcounter=5

		BigDT_ReloadPos=100
		finalsteps=0
		mission1after1onlyonce=0
		bossactive=0
		Boss7Countdown=0
		bosslevel=0
		boss7.timerenabled=0
		boss7ramps=0
		boss7status=0
		bossstatus=0
		Sw51up.enabled=1
		PlaySound SoundFX("springchange",DOFContactors), 0, .88*VolumeDial, AudioPan(sw51), 0.05,0,0,1,AudioFade(sw51)
		Sw51down.enabled=0
		kicker001up.enabled=1
		redcockpit.play Seqblinking,,12,100
		spinnerstatus=1
		Wall025.collidable=false
		Wall026.collidable=false
		StartupStatus=0
		displaybuzy=0
		GAMEISOVER=0
		SelectBall=SelectBalls
		If Selectballs=21 Then SelectBall=int(rnd(1)*20)

		StopSound "salvagedwhatremains"
		ClearAllStatus
		CRAPRESTARTS=0
		ButtonState=2
		bonusover=0
		Missions4Evasive=0

 
			redeyes.enabled=1

		EvasiveOneScoring=0
		P1Level=1
		EvasiveDestroyd=0
		MovingTarget_Init
		Boss1ALLOFF

		EndMusic : Playtune(11) 
		If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex FlexBG, " ", 15, 3, " ", 15, 3, 14, 5, 14
		FlexScore="0"
		BumperFlasher1.visible=0 
		ManualLightSequencer.enabled=1

		AGrantedonlyonce=0
		Lileftinlane.uservalue=0
		LiRightinlane.uservalue=0
		LiSpecialRight.state=0
		TotalExtraBalls=0
		undertopflipper.state=2
		UNDERLEFTFLIPPER.state=2
		underrightflupper.state=2
		undertopflipper.timerenabled=1
		scoring(0)
		onetimetalk=0
		GameOverTimer.enabled=0
		completejustonce=0
		shoottheballstatus=1
		attract1Mode=0
		ApronRadar003.state=2
		Bumper1.TimerEnabled=1
		LightSeq002.timerenabled=0
		LightSeq002.StopPlay
		LSMissions.StopPlay
		BonusStatus = 0
		attractModeStatus=0
		GetReadyToPlay=1
		totalturned=0
		'Plunger.CreateBall at startup
		BallRelease.CreateBall 
		PlaySound SoundFX("BallRelease" & int(rnd(1)*7)+1 , DOFContactors), 0,.71*VolumeDial,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)
		stopdoubletrouble=0
		skillshot=1
		LiSkillshot1.state=2 
		LiSkillshot2.state=2 
		BallRelease.Kick 90, 5 : DOF 145,2  : luttarget=2
		PlaySound SoundFX("fx_kicker",DOFContactors), 0,.81*VolumeDial,AudioPan(BallRelease),0.22,0,0,1,AudioFade(BallRelease)
		Plunger1.Pullback
		PlaySound SoundFX("fx_plungerpull",DOFContactors),0,.81*VolumeDial,AudioPan(Plunger1),0.25,0,0,1,AudioFade(Plunger1)
		BIP = 1
		P1score=0
		GamesPlayd=GamesPlayd+1
		knockeronce=0
		GameOverStatus=0
		P1scorebonus=1
		gameovernolights=0
		li21.state=1
		KickbackFL.visible=1 : KickbackFL.timerenabled=1 : PlaySound SoundFX("fx_apron",DOFContactors), 0, 0.33*VolumeDial, AudioPan(li21), 0.05,0,0,1,AudioFade(li21)
		LiSpesialLeft.state=0
		LiSpesialLeftFL.visible=1 : LiSpesialLeftFL.timerenabled=1
'		li1wh.state=2 : li1wh001.state=2
'		li2wh.state=2 : li2wh001.state=2
'		If Not TournamentMode Then
		LiReplay.state=2 : lireplay001.state = 2
pLireplay.blenddisablelighting=5
		Light003.state=0
		Light051.state=2
		BallinPLay=1
		PlaySound SoundFX("fx_motor",DOFContactors), 5, 0.4*VolumeDial, AudioPan(LiReplay2), 0.04,0,0,1,AudioFade(LiReplay2)
		If HideDesktop=1 then  controller.B2SSetData 14,1 
		EMBallInPlay.setvalue BallinPLay
		ObjLevel(8) = 1 : FlasherFlash8_Timer
		PlaySound SoundFX("fx_solenoidon",DOFContactors), 0, .81*VolumeDial, AudioPan(skillshottrigger), 0.12,0,0,1,AudioFade(skillshottrigger)
		ObjLevel(9) = 1 : FlasherFlash9_Timer
		bonus0=0 : bonus1=0 : bonus2=0 : bonus3=0 : bonusmultiplyer=0
		autokicker.enabled=0 
		plunger001.fire
		salvagedjustonce=0

		PoppedBoba=0

		Drop_5Bank

		if HideDesktop=1 Then
			For i = 1 to 10
				Controller.B2sSetData i,0'
			Next
		end If
		enterflashing=0

		JackPotScoring=500000
		SkillShotScoring=750000
		letterscoring=28000
		MaxBallsScoring=2000000
		ScoringRightRamp=40000
		ScoringLeftRamp=40000
		SecretScoring=150000

		HunterScoring=2500
		BountyScoring=2500
		BountyCompleteScoring=20000
		HunterCompleteScoring=20000
		SpinnerScoring=300
		BumperScoring=800
		DroptargetsScoring=490
		enterlightscoring=14000
		mysteryscoring=100000

		mission6scoring=200000
		mission5scoring=150000
		mission4scoring=200000
		mission3scoring=125000
		mission2scoring=250000
		mission1scoring=300000
		mission8scoring=500000
		mission7scoring=250000
		mission7status=0
		mission8status=0

		RampsTotal=0
		ComboTotalRamps=0
		DroptargetsCounter=0
		MissionsDoneCounter=0
		MissionsDoneThisBall=0
		ExtraBallIsLitStatus=0
		quickMB=0
		quickMBactive=0
		MultiballActive=0
		huntercount=0
		doubletripple=0
				LiDoTriL1.state=2 : LiDoTriL1.timerenabled=1
				LiDoTriR1.state=2 : LiDoTriR1.timerenabled=1 
				LiDoTriR001.state=LiDoTriR1.state
				LiDoTriL001.state=LiDoTriL1.state
		wormhole=0
		DT01down=0
		DT02down=0
		spinnermission=7
		bumpermystery=7

		Mission1Light.state=0
		Mission1light003.state=0
		Mission1light004.state=0
		Mission1light005.state=0
		Mission2Light.state=0
		Mission2Light003.state=0
		Mission2Light004.state=0
		Mission2Light005.state=0
		Mission3Light.state=0
		Mission3Light003.state=0
		Mission3Light004.state=0
		Mission3Light005.state=0
		Mission4Light.state=0
		Mission4Light003.state=0
		Mission4Light004.state=0
		Mission4Light005.state=0
		Mission5Light.state=0
		Mission5Light003.state=0
		Mission5Light004.state=0
		Mission5Light005.state=0
		Mission6Light.state=0
		Mission6Light003.state=0
		Mission6Light004.state=0
		Mission6Light005.state=0
		Mission7Light.state=0
		Mission7Light003.state=0
		Mission7Light004.state=0
		Mission7Light005.state=0
		Mission8Light.state=0
		Mission8Light003.state=0
		Mission8Light004.state=0
		Mission8Light005.state=0
	
		Lileftinlane.state=0
		Lirightinlane.state=0
		LiSpecialRight.state=0
		LiSpesialLeft.state=0

		light003.state=0
		light051.state=2


		licenter001.state=0
		licenter002.state=0
		licenter003.state=0
		licenter004.state=0
		licenter005.state=0
		licenter006.state=0
		licenter007.state=0
		licenter008.state=0
		licenter009.state=0
		licenter010.state=0
		licenter011.state=0
		licenter012.state=0

		bonusx001.timerenabled=0 : bonusx001.state=0 ': bonusx001.intensity=80
		bonusx002.timerenabled=0 : bonusx002.state=0 ': bonusx002.intensity=80
		bonusx003.timerenabled=0 : bonusx003.state=0 ': bonusx003.intensity=80
		bonusx004.timerenabled=0 : bonusx004.state=0 ': bonusx004.intensity=80
		bonusx005.timerenabled=0 : bonusx005.state=0 ': bonusx005.intensity=80
		bonusx006.timerenabled=0 : bonusx006.state=0 ': bonusx006.intensity=80
		bonusx007.timerenabled=0 : bonusx007.state=0 ': bonusx007.intensity=80
		bonusx008.timerenabled=0 : bonusx008.state=0 ': bonusx008.intensity=80
		bonusx009.timerenabled=0 : bonusx009.state=0 ': bonusx009.intensity=80
				

		
		libonus1.state=0
		libonus2.state=0
		libonus3.state=0

		lilock1.state=0
		lilock2.state=0
		LOCKED001.State=0
		LOCKED002.State=0
		LOCKED003.State=0
		LOCKED004.state=1
		LOCKED005.state=1
		LOCKED006.state=1
		lockedBalls=0

		lihunter1.state=0
		Libounty1.state=0 : LiBounty11.state=0
		lihunter2.state=0
		Libounty2.state=0 : LiBounty22.state=0
		lihunter3.state=0
		Libounty3.state=0 : LiBounty33.state=0
		lihunter4.state=0
		Libounty4.state=0 : LiBounty44.state=0
		lihunter5.state=0
		Libounty5.state=0 : LiBounty55.state=0
		lihunter6.state=0
		Libounty6.state=0 : LiBounty66.state=0

		hunter(1)=0
		Hunter(2)=0
		hunter(3)=0
		hunter(4)=0
		Hunter(5)=0
		hunter(6)=0
		hunter(7)=0
		Hunter(8)=0
		hunter(9)=0
		hunter(10)=0
		Hunter(11)=0
		hunter(12)=0



		enterlight.timerenabled=0
		enterlight.state=0
		mysterylight.state=0
		extraballlight.state=0

		mission(1)=0
		mission(2)=0
		mission(3)=0
		mission(4)=0
		mission(5)=0
		mission(6)=0
		mission(7)=0
		mission(8)=0

		missionstatus(1)=0
		missionstatus(2)=0
		missionstatus(3)=0
		missionstatus(4)=0
		missionstatus(5)=0
		missionstatus(6)=0
		missionstatus(7)=0
		missionstatus(8)=0

		GreenBorders
		lightHQmissionOFF 


		'li2wh.state=0 : li2wh001.state=0
		'li1wh.state=0 : li1wh001.state=0
		'li2wh.state=2 : li2wh001.state=2
		'li1wh.state=2 : li1wh001.state=2

		Raise_DT 1

		Raise_DT 2
				liTargetTop(1)=0
			liTargetTop(2)=0
			liTargetTop(3)=0
			liTargetTop(4)=0
			liTargetTop(5)=0
			liTargetTop(6)=0
			liTargetTop(7)=0
			liTargetTop(8)=0
			liTargetTop(9)=0
			liTargetTop(10)=0

		LiDouble.state=0
		LiDouble2.state=0
		LiDouble3.state=0
		LiTripple.state=0
		LiTripple2.state=0
		LiTripple3.state=0


		lisecret2.state=0
		LiSecret3.state=0
		Lisupply009.state=0
		Lisupply006.state=0
		Lisupply002.state=0
		Lisupply001.state=0
		LiSupply007.state=0
		Lisupply004.state=0
		Lisupply005.state=0
		Lisupply003.state=0
		LiSupply010.state=0
		Lisupply008.state=0

		LiRefuel1.state=0
		doubletripple=0

		LiDoTriR1.state=2 : lidotriR1.Timerenabled=1
		LiDoTriL1.state=2 : lidotriL1.Timerenabled=1
		LiDoTriR001.state=LiDoTriR1.state
		LiDoTriL001.state=LiDoTriL1.state

		If HideDesktop=1 Then controller.B2SSetScorePlayer1 0
	End If

End Sub

Sub LiDoTriL1_Timer 
	LiDoTriL1.timerenabled=0
	If doubletripple<2 Then
		LiDoTriL1.state=1
	Else
		LiDoTriL1.state=0
	End If
				LiDoTriL001.state=LiDoTriL1.state
End Sub


Sub LiDoTriR1_Timer 
	LiDoTriR1.timerenabled=0 
	If doubletripple=2 or doubletripple=0 Then
		LiDoTriR1.state=1
	Else
		LiDoTriR1.state=0
	End If
	LiDoTriR001.state=LiDoTriR1.state
End Sub


	

Sub nextBallPlz
		P1scorebonus=1 
		doubletripple=0
		LiDoTriL1.state=2 : LiDoTriL1.timerenabled=1
		LiDoTriR1.state=2 : LiDoTriR1.timerenabled=1 
				LiDoTriR001.state=LiDoTriR1.state
				LiDoTriL001.state=LiDoTriL1.state

		LiDouble.state=0
		LiDouble2.state=0
		LiDouble3.state=0
		LiTripple.state=0
		LiTripple2.state=0
		LiTripple3.state=0
		BonusStatus=0
		skillshot=1 
		LiSkillshot1.state=2 
		LiSkillshot2.state=2 
		Plunger001.Timerenabled=1
		autokicker.enabled=0 
'		plunger001.fire
If GAMEISOVER=0 Then

		BallRelease.CreateBall
		BallRelease.Kick 90, 5 : DOF 145,2  : luttarget=2
		li21.timerenabled=0
		li21.state=2
		li21.timerenabled=1
		LiSpesialLeft.state=0
		LiSpesialLeftFL.visible=1 : LiSpesialLeftFL.timerenabled=1

		PlaySound SoundFX("fx_kicker",DOFContactors), 0,.81*VolumeDial,AudioPan(BallRelease),0.2,0,0,1,AudioFade(BallRelease)
		BIP = BIP + 1
End If
		Gate002.twoway=true
		'If Not TournamentMode Then
		LiReplay.state=2 : lireplay001.state = 2
		pLireplay.blenddisablelighting=5
		Light003.state=0
		Light051.state=2
		GreenBorders
End Sub

Dim starwarscount
Sub plungerlane_PlayDone()
' The Light Sequencer has finished all the effects in the attract sequence
	If starwarscount>0 then
		starwarscount=starwarscount-1
		plungerlane.play SeqUpOn,13,1,50
		DOF 202,2: 'Debug.print "DOF 202, 2"  'apophis
	End If	
End Sub


'plungerball light
Sub SWPlunger_Hit
	luttarget=2
	DOF 208,1
'Debug.print "DOF 208, 1"  'apophis
	Light061.state=2
	Light050.state=2
	Li84.state=2
	Light034.state=2
	plungerlane.play SeqBlinking,,5,25
	starwarscount=500
	DOF 202,2: 'Debug.print "DOF 202, 2"  'apophis
		L001.state=2	
		L002.state=2
		L003.state=2
		L004.state=2
		L005.state=2
		L006.state=2
		L007.state=2
		L008.state=2
		L009.state=2
		L010.state=2

End Sub

Sub SWPlunger_UnHit
	DOF 208,0 ' ball launched
	DOF 209,2
'Debug.print "DOF 208, 0"  'apophis
'Debug.print "DOF 209, 2"  'apophis
	Light050.state=0
	Light061.state=0
	Li84.state=0
	Light034.state=0
	plungerlane.stopplay
	plungerlane.play SeqUpOn,13,1,0
DOF 202,2: 'Debug.print "DOF 202, 2"  'apophis
starwarscount=0

		L001.state=0
		L002.state=0
		L003.state=0
		L004.state=0
		L005.state=0
		L006.state=0
		L007.state=0
		L008.state=0
		L009.state=0
		L010.state=0

End Sub

Sub	Light061_timer
	If light061.timerenabled=0 Then
		light061.timerenabled=1
		light061.state=2
	Else
		light061.timerenabled=0
		light061.state=0
	End If


End Sub
Sub Sw51BLinker_Timer
	PlaySound SoundFX("fx_solenoidon3",DOFContactors), 0, 0.4*VolumeDial, AudioPan(sw51), 0.04,0,0,1,AudioFade(sw51)
	Lockholeblast.state=1 : Lockholeblast.timerenabled=1
	DOF 138,2 : 'Debug.print "DOF 138, 2"  'apophis
	ObjLevel(10) = 1 : FlasherFlash10_Timer : 	DOF 136,2 : 'Debug.print "DOF 136, 2" ' yel right  'apophis
End Sub

dim nextblackball
Sub Multiballtimer_Timer
	Sw51BLinker.enabled=0
	If BallWaitsw51=1 Then
		sw51.timerenabled=0
 
			bangtimer.enabled=1
			redeyes.enabled=1

		LSMissions.Play SeqBlinking,,3,110
		letterblink=8
		BallWaitsw51=0
		sw51.kick 160,3
		Primitive055.Z = -20
		Primitive056.Y = 970 
		sw51arm.enabled=1
		If Lockedballs=1 Then LOCKED002.state=0 
		If LockedBalls=0 Then
			LOCKED003.state=0
			Sw51up.enabled=1
			Sw51down.enabled=0
		else
			sw51up.enabled=0
			Sw51down.enabled=1
		End If
		PlaySound SoundFX("springchange",DOFContactors), 0, .88*VolumeDial, AudioPan(sw51), 0.05,0,0,1,AudioFade(sw51)

		LOCKED001.state=0 : LOCKED006.state=1
		PlaySound SoundFX("fx_kicker",DOFContactors), 0,.81*VolumeDial,AudioPan(sw51),0.2,0,0,1,AudioFade(sw51)
		'If Not TournamentMode Then
		If WizardLevel < 3 Then
				LiReplay.state=2  : lireplay001.state = 2
				LiReplay.timerenabled=0 
				'If Not TournamentMode Then 
				LiReplay.timerenabled=1 : lireplay.blinkinterval=200 : lireplay001.blinkinterval=200 : pLireplay.blenddisablelighting=5
		End If
		Light003.state=0
		Light051.state=2
		JPsequencer.play seqblinking,,3,40
		locksequencer.play seqblinking,,3,40
		multiballsequencer.play seqblinking,,3,40

	Else
		If MultiBallActive=1 Then
				
			If LockedBalls=0 Then Multiballtimer.enabled=0  
			If LockedBalls=1 And boss7countdown = 0 Then PlaySound "cantina1",-1, 0.1 * MusicVolumeDial ,0, 0,0,0,0,0 : MusicVolume = 0.01
				
			If ballwaitsw51=0 then
				If LockedBalls>0 Then

					LockedBalls=LockedBalls-1
					autokicker.enabled=1 
					Primitive055.Z = -18 
					If GAMEISOVER=0 Then
						Sw51.Createball
						If LockedBalls=0 Then nextblackball=LockedID1
						If LockedBalls=1 Then nextblackball=LockedID2
					
						BallWaitsw51=1
						Sw51BLinker.enabled=1
						PlaySound SoundFX("BallRelease" & int(rnd(1)*7)+1 , DOFContactors), 0,.71*VolumeDial,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)

	'					BIP = BIP + 1
					End If
				Else

					Multiballtimer.enabled=0
				End If
			End If
		End If
	End If
	If BallWaitsw51=2 Then BallWaitsw51=1
End Sub

Dim shoottheballstatus
Sub Plunger001_Timer
	shoottheballstatus=int(rnd(1)*4)+1
End Sub


Dim ReleasedBalls : ReleasedBalls=0
Dim lockedcount : lockedcount=0
Dim PewPewBalls(10)
LockedReleseGO.interval=600

Sub LockedReleseGO_Timer
'	DEBUG.PRINT "LockedReleseGO_Timer" 

	If ReleasedBalls = 0 Then
		ReleasedBalls = 1

		lockedcount=0 

		If LockedID2 > 0 Then PewPewBalls(lockedcount) = LockedID2 : lockedcount = lockedcount + 1
		If LockedID1 > 0 Then PewPewBalls(lockedcount) = LockedID1 : lockedcount = lockedcount + 1

		If CurrentPlayer <> 1 Then
			If PlayerSaved(1, 9) > 0 Then PewPewBalls(lockedcount) = PlayerSaved(1, 9) : lockedcount = lockedcount + 1
			If PlayerSaved(1,10) > 0 Then PewPewBalls(lockedcount) = PlayerSaved(1,10) : lockedcount = lockedcount + 1
		End If

		If CurrentPlayer <> 2 Then
			If PlayerSaved(2, 9) > 0 Then PewPewBalls(lockedcount) = PlayerSaved(2, 9) : lockedcount = lockedcount + 1
			If PlayerSaved(2,10) > 0 Then PewPewBalls(lockedcount) = PlayerSaved(2,10) : lockedcount = lockedcount + 1
		End If

		If CurrentPlayer <> 3 Then
			If PlayerSaved(3, 9) > 0 Then PewPewBalls(lockedcount) = PlayerSaved(3, 9) : lockedcount = lockedcount + 1
			If PlayerSaved(3,10) > 0 Then PewPewBalls(lockedcount) = PlayerSaved(3,10) : lockedcount = lockedcount + 1
		End If

		If CurrentPlayer <> 4 Then
			If PlayerSaved(4, 9) > 0 Then PewPewBalls(lockedcount) = PlayerSaved(4, 9) : lockedcount = lockedcount + 1
			If PlayerSaved(4,10) > 0 Then PewPewBalls(lockedcount) = PlayerSaved(4,10) : lockedcount = lockedcount + 1
		End If

	End If
	
'	DEBUG.PRINT "Lockedballs to release =" & lockedcount

	If lockedcount>0 Then

		If lockedcount = 1 Then
			nextblackball=PewPewBalls(lockedcount)
			sw51down.enabled=0
			Sw51up.enabled=1
			Primitive055.Z = -80 
			Primitive056.Y = 970 
			sw51arm.enabled=1
		Else
			nextblackball=PewPewBalls(lockedcount)

			sw51down.enabled=1
			Sw51up.enabled=0
			Primitive055.Z = -20 
			Primitive056.Y = 970 
			sw51arm.enabled=1
		End If

		lockedcount=lockedcount-1


		PlaySound SoundFX("BallRelease" & int(rnd(1)*7)+1 , DOFContactors), 0,.71*VolumeDial,AudioPan(sw51),0.25,0,0,1,AudioFade(sw51)		
		PlaySound SoundFX("fx_kicker",DOFContactors), 0,0.34*VolumeDial,AudioPan(sw51),0.25,0,0,1,AudioFade(sw51)
		Sw51.Createball 
		sw51.kick 160,3
		LOCKED001.state=0
		LOCKED002.state=0
		LOCKED003.state=0
	Else
		EvasiveState=4
		ReleaseTimer.enabled=1
'		DEBUG.PRINT "ALL Balls out " 
	End If
End Sub

Sub ReleaseTimer_Timer   '300
'	DEBUG.PRINT "Waiting for all to drain    BIP=" & BALLSINPLAY 
	If BALLSINPLAY=-1 Then
'		DEBUG.PRINT "ALL DONE READY FOR NEW GAME " 
		ReleasedBalls = 0
		LiDoTriL1.state=2 : LiDoTriL1.timerenabled=1
		LiDoTriR1.state=2 : LiDoTriR1.timerenabled=1  
		LiDoTriR001.state=LiDoTriR1.state
		LiDoTriL001.state=LiDoTriL1.state
		LiDouble.state=0
		LiDouble2.state=0
		LiDouble3.state=0
		LiTripple.state=0
		LiTripple2.state=0
		LiTripple3.state=0
		LockedReleseGO.enabled=0
		ReleaseTimer.enabled=0
		luttarget=2
		GameOverTimer.enabled=1
		PlayersPlaying=0

	End If
End Sub



'************************************
' Targets

Dim hunter(12),huntercount
Dim HunterScoring,HunterStatus
Dim BountyScoring,BountyStatus


Sub TargetHunter1_Hit
	LSHunters.StopPlay 
	LSHunters.play SeqBlinking,,4,50

	LiHunter1.state=2 : LiHunter1.timerenabled=1 : huntercount=9
	if hunter(1)=0 then 
		hunter(1)=1 
		PlaySound SoundFX("scrdisplay2",DOFContactors), 0, .7*VolumeDial, AudioPan(TargetHunter1), 0.05,0,0,1,AudioFade(TargetHunter1)
		scoring(hunterscoring) : huntershit
	Else
		scoring(hunterscoring/2)
	End If
	PlaySound SoundFX ("Target_Hit_" & int(rnd(1)*9)+1 ,DOFContactors), 0, .7*VolumeDial, AudioPan(TargetHunter1), 0.05,0,0,1,AudioFade(TargetHunter1)
End Sub

Sub TargetHunter12_Hit
	TargetHunter1_Hit
	TargetHunter2_Hit
End Sub

Sub TargetHunter2_Hit
	LSHunters.StopPlay 
	LSHunters.play SeqBlinking,,4,50

	LiHunter2.state=2 : LiHunter2.timerenabled=1 : huntercount=9
	if hunter(2)=0 then 
		hunter(2)=1
		PlaySound SoundFX("scrdisplay2",DOFContactors), 0, 0.6*VolumeDial, AudioPan(TargetHunter1), 0.05,0,0,1,AudioFade(TargetHunter1)
		scoring(hunterscoring) : huntershit
	Else
		scoring(hunterscoring/2)
	End If
	PlaySound SoundFX ("Target_Hit_" & int(rnd(1)*9)+1 ,DOFContactors), 0, .7*VolumeDial, AudioPan(TargetHunter1), 0.05,0,0,1,AudioFade(TargetHunter1)
End Sub

Sub TargetHunter23_Hit
	TargetHunter2_Hit
	TargetHunter3_Hit
End Sub

Sub TargetHunter3_Hit
	LSHunters.StopPlay 
	LSHunters.play SeqBlinking,,4,50


	LiHunter3.state=2 : LiHunter3.timerenabled=1 : huntercount=9
	if hunter(3)=0 then 
		hunter(3)=1 
		PlaySound SoundFX("scrdisplay2",DOFContactors), 0, 0.6*VolumeDial, AudioPan(TargetHunter1), 0.05,0,0,1,AudioFade(TargetHunter1)
		scoring(hunterscoring) : huntershit
	Else
		scoring(hunterscoring/2)
	PlaySound SoundFX ("Target_Hit_" & int(rnd(1)*9)+1 ,DOFContactors), 0, .7*VolumeDial, AudioPan(TargetHunter1), 0.05,0,0,1,AudioFade(TargetHunter1)
	End If
End Sub

Sub TargetHunter34_Hit
	TargetHunter3_Hit
	TargetHunter4_Hit
End Sub

Sub TargetHunter4_Hit
	LSHunters.StopPlay 
	LSHunters.play SeqBlinking,,4,50

	LiHunter4.state=2 : LiHunter4.timerenabled=1 : huntercount=9
	if hunter(4)=0 then 
		hunter(4)=1
		PlaySound SoundFX("scrdisplay2",DOFContactors), 0, 0.6*VolumeDial, AudioPan(TargetHunter1), 0.05,0,0,1,AudioFade(TargetHunter1)
		scoring(hunterscoring) : huntershit
	Else
		scoring(hunterscoring/2)
	PlaySound SoundFX ("Target_Hit_" & int(rnd(1)*9)+1 ,DOFContactors), 0, .7*VolumeDial, AudioPan(TargetHunter1), 0.05,0,0,1,AudioFade(TargetHunter1)
	End If
End Sub

Sub TargetHunter45_Hit
	TargetHunter4_Hit
	TargetHunter5_Hit
End Sub

Sub TargetHunter5_Hit 
	LSHunters.StopPlay 
	LSHunters.play SeqBlinking,,4,50

	LiHunter5.state=2 : LiHunter5.timerenabled=1 : huntercount=9
	if hunter(5)=0 then 
		hunter(5)=1
		PlaySound SoundFX("scrdisplay2",DOFContactors), 0, 0.6*VolumeDial, AudioPan(TargetHunter1), 0.05,0,0,1,AudioFade(TargetHunter1)
		scoring(hunterscoring) : huntershit
	Else
		scoring(hunterscoring/2)
	End If
	PlaySound SoundFX ("Target_Hit_" & int(rnd(1)*9)+1 ,DOFContactors), 0, .7*VolumeDial, AudioPan(TargetHunter1), 0.05,0,0,1,AudioFade(TargetHunter1)
End Sub

Sub TargetHunter56_Hit
	TargetHunter5_Hit
	TargetHunter6_Hit
End Sub

Sub TargetHunter6_Hit
	LSHunters.StopPlay 
	LSHunters.play SeqBlinking,,4,50
	LiHunter6.state=2 : LiHunter6.timerenabled=1 : huntercount=9
	if hunter(6)=0 then 
		hunter(6)=1
		PlaySound SoundFX("scrdisplay2",DOFContactors), 0, 0.6*VolumeDial, AudioPan(TargetHunter1), 0.05,0,0,1,AudioFade(TargetHunter1)
		scoring(hunterscoring) : huntershit
	Else
		scoring(hunterscoring/2)
	End If
	PlaySound SoundFX ("Target_Hit_" & int(rnd(1)*9)+1 ,DOFContactors), 0, .7*VolumeDial, AudioPan(TargetHunter1), 0.05,0,0,1,AudioFade(TargetHunter1)
End Sub


'bounty targets

Sub TargetBounty1_Hit 
	DOF 132,2
	Bountyblinker.enabled=1
	LiBounty1.state=2 : LiBounty1.timerenabled=1 : huntercount=9 : LiBounty11.state=2
	if hunter(7)=0 then 
		hunter(7)=1
		PlaySound SoundFX("scrdisplay2",DOFContactors), 0, 0.6*VolumeDial, AudioPan(TargetBounty1), 0.05,0,0,1,AudioFade(TargetBounty1)
		scoring(BountyScoring) : bountyshit
	Else
		scoring(BountyScoring/2)
	End If
	PlaySound SoundFX ("Target_Hit_" & int(rnd(1)*9)+1 ,DOFContactors), 0, .7*VolumeDial, AudioPan(TargetBounty1), 0.05,0,0,1,AudioFade(TargetBounty1)

End Sub

Sub TargetBounty2_Hit 
	DOF 132,2
	Bountyblinker.enabled=1
	LiBounty2.state=2 : LiBounty2.timerenabled=1 : huntercount=9: LiBounty22.state=2
	if hunter(8)=0 then 
		hunter(8)=1
		PlaySound SoundFX("scrdisplay2",DOFContactors), 0, 0.6*VolumeDial, AudioPan(TargetBounty1), 0.05,0,0,1,AudioFade(TargetBounty1)
		scoring(BountyScoring) : bountyshit
	Else
		scoring(BountyScoring/2)
	End If
	PlaySound SoundFX ("Target_Hit_" & int(rnd(1)*9)+1 ,DOFContactors), 0, .7*VolumeDial, AudioPan(TargetBounty1), 0.05,0,0,1,AudioFade(TargetBounty1)

End Sub

Sub TargetBounty3_Hit 
	DOF 132,2
	Bountyblinker.enabled=1
	LiBounty3.state=2 : LiBounty3.timerenabled=1 : huntercount=9: LiBounty33.state=2
	if hunter(9)=0 then 
		hunter(9)=1
		PlaySound SoundFX("scrdisplay2",DOFContactors), 0, 0.6*VolumeDial, AudioPan(TargetBounty1), 0.05,0,0,1,AudioFade(TargetBounty1)
		scoring(BountyScoring) : bountyshit
	Else
		scoring(BountyScoring/2)
	End If
	PlaySound SoundFX ("Target_Hit_" & int(rnd(1)*9)+1 ,DOFContactors), 0, .7*VolumeDial, AudioPan(TargetBounty1), 0.05,0,0,1,AudioFade(TargetBounty1)

End Sub

Sub TargetBounty4_Hit
	DOF 132,2
	Bountyblinker.enabled=1
	LiBounty4.state=2 : LiBounty4.timerenabled=1 : huntercount=9: LiBounty44.state=2
	if hunter(10)=0 then 
		hunter(10)=1 
		PlaySound SoundFX("scrdisplay2",DOFContactors), 0, 0.6*VolumeDial, AudioPan(TargetBounty4), 0.05,0,0,1,AudioFade(TargetBounty4)
		scoring(BountyScoring) : bountyshit
	Else
		scoring(BountyScoring/2)
	End If
	PlaySound SoundFX ("Target_Hit_" & int(rnd(1)*9)+1 ,DOFContactors), 0, .7*VolumeDial, AudioPan(TargetBounty4), 0.05,0,0,1,AudioFade(TargetBounty4)
End Sub

Sub TargetBounty5_Hit
	DOF 132,2
	Bountyblinker.enabled=1
	LiBounty5.state=2 : LiBounty5.timerenabled=1 : huntercount=9: LiBounty55.state=2
	if hunter(11)=0 then 
		hunter(11)=1 
		PlaySound SoundFX("scrdisplay2",DOFContactors), 0, 0.6*VolumeDial, AudioPan(TargetBounty4), 0.05,0,0,1,AudioFade(TargetBounty4)
		scoring(BountyScoring) : bountyshit
	Else
		scoring(BountyScoring/2)
	End If
	PlaySound SoundFX ("Target_Hit_" & int(rnd(1)*9)+1 ,DOFContactors), 0, .7*VolumeDial, AudioPan(TargetBounty4), 0.05,0,0,1,AudioFade(TargetBounty4)
End Sub

Sub TargetBounty6_Hit
	DOF 132,2
	Bountyblinker.enabled=1
	LiBounty6.state=2 : LiBounty6.timerenabled=1 : huntercount=9: LiBounty66.state=2
	if hunter(12)=0 then 
		hunter(12)=1
		PlaySound SoundFX("scrdisplay2",DOFContactors), 0, 0.6*VolumeDial, AudioPan(TargetBounty4), 0.05,0,0,1,AudioFade(TargetBounty4)
		scoring(BountyScoring) : bountyshit
	Else
		scoring(BountyScoring/2)
	End If
	PlaySound SoundFX ("Target_Hit_" & int(rnd(1)*9)+1 ,DOFContactors), 0, .7*VolumeDial, AudioPan(TargetBounty4), 0.05,0,0,1,AudioFade(TargetBounty4)
End Sub

Dim BBCounter
Sub Bountyblinker_Timer
	If LiBounty1.uservalue=0 Then LiBounty1.uservalue=1 else LiBounty1.uservalue=0
	i=LiBounty1.uservalue
	LiBounty1.state=i : LiBounty11.state=i
	LiBounty2.state=i : LiBounty22.state=i
	LiBounty3.state=i : LiBounty22.state=i
	LiBounty4.state=i : LiBounty22.state=i
	LiBounty5.state=i : LiBounty22.state=i
	LiBounty6.state=i : LiBounty22.state=i
	BBCounter=BBCounter+1 
	If BBCounter=8 Then
		BBCounter=0 
		Bountyblinker.enabled=0
		BountyTimer1_Timer
	End If
End Sub

Dim BountyCompleteScoring, BountyCompleteStatus
Dim HunterCompleteScoring, HunterCompleteStatus , Doubleinfo


Dim doubletripple
Sub Bountyshit
	LiDoTriL1.timerenabled=0
	LiDoTriL1.state=2 : LiDoTriL1.timerenabled=1
				LiDoTriL001.state=LiDoTriL1.state
	checkhunters
End Sub
Sub Huntershit
	LiDoTriR1.timerenabled=0
	LiDoTriR1.state=2 : LiDoTriR1.timerenabled=1
				LiDoTriR001.state=LiDoTriR1.state
	checkhunters
End Sub


Sub checkhunters

	tempX=hunter(1)+hunter(2)+hunter(3)+hunter(4)+hunter(5)+hunter(6)
	If tempX=6 Then
		hunter(1)=0 : hunter(2)=0 : hunter(3)=0 : hunter(4)=0 : hunter(5)=0 : hunter(6)=0 : 
		addletter(1)
		If MultiballActive=0 Then	
			Stoptalking 
			i=int(rnd(1)*7)
			Select Case i
				Case 0 : PlaySound "goodastheysay",0,1*BackGlassVolumeDial
				Case 1 : PlaySound "wiseass",0,1*BackGlassVolumeDial
				Case 2 : PlaySound "knowthepolicy",0,1*BackGlassVolumeDial
				Case 3 : PlaySound "butfirst",0,1*BackGlassVolumeDial
				Case 4 : PlaySound "lookingforwork",0,1*BackGlassVolumeDial
				Case 5 : PlaySound "beginoffload",0,1*BackGlassVolumeDial
				Case 6 : PlaySound "leedyou",0,1*BackGlassVolumeDial
			End Select
		End If

		HunterCompleteScoring=HunterCompleteScoring+10000
		scoring(HunterCompleteScoring)
		HunterCompleteStatus=1 
		HunterScoring=HunterScoring+1000
		HunterStatus=1
		LiHunter1.state=2
		LiHunter2.state=2
		LiHunter3.state=2
		LiHunter4.state=2
		LiHunter5.state=2
		LiHunter6.state=2 
		HunterTimer1.enabled=1

		If doubletripple=0 Then
			DOF 224,2
'Debug.print "DOF 224, 2"  'apophis
			doubleinfo=1
			doubletripple=1 
			LiDouble.state=2 
			LiDouble2.state=2
		End If
		If doubletripple=2 Then
			doubleinfo=2
			doubletripple=3
			DOF 225,2
'Debug.print "DOF 225, 2"  'apophis
			LiTripple.state=2
			LiTripple2.state=2 
			LiDouble.state=0
			Lidouble2.state=0 
		End If
	End If

	tempY=hunter(7)+hunter(8)+hunter(9)+hunter(10)+hunter(11)+hunter(12)
	If tempY=6 Then
		hunter(7)=0 : hunter(8)=0 : hunter(9)=0 : hunter(10)=0 : hunter(11)=0 : hunter(12)=0 : 
		addletter(1)

		If MultiballActive=0 Then
			Stoptalking
			i=int(rnd(1)*7)
			Select Case i
				Case 0 : PlaySound "goodastheysay",0,1*BackGlassVolumeDial
				Case 1 : PlaySound "wiseass",0,1*BackGlassVolumeDial
				Case 2 : PlaySound "knowthepolicy",0,1*BackGlassVolumeDial
				Case 3 : PlaySound "butfirst",0,1*BackGlassVolumeDial
				Case 4 : PlaySound "lookingforwork",0,1*BackGlassVolumeDial
				Case 5 : PlaySound "beginoffload",0,1*BackGlassVolumeDial
				Case 6 : PlaySound "leedyou",0,1*BackGlassVolumeDial
			End Select
		End If

		BountyCompleteScoring=BountyCompleteScoring+10000
		scoring(BountyCompleteScoring)
		BountyCompleteStatus=1 
		BountyScoring=BountyScoring+1000
		BountyStatus=1

		LiBounty1.state=2 : LiBounty2.state=2 : LiBounty3.state=2 : LiBounty4.state=2 : LiBounty5.state=2 : LiBounty6.state=2 
		LiBounty11.state=2 : LiBounty22.state=2 : LiBounty33.state=2 : LiBounty44.state=2 : LiBounty55.state=2 : LiBounty66.state=2
		BountyTimer1.enabled=1

		If doubletripple=0 Then 
			DOF 224,2
'Debug.print "DOF 224, 2"  'apophis
			doubleinfo=1
			doubletripple=2
			LiDouble.state=2
			LiDouble2.state=2

		End If
		If doubletripple=1 Then
			doubleinfo=2
			doubletripple=3
			DOF 225,2
'Debug.print "DOF 225, 2"  'apophis
			LiTripple.state=2
			LiTripple2.state=2
			LiDouble.state=0
			Lidouble2.state=0
		End If

	End If
End Sub

DIM kickbacktree
Sub KickbackFL_Timer
	If li21.state=0 Then
		Select Case kickbacktree
			Case 0,2,4,6 : KickbackFL.visible=0
			Case 1,3,5,7 : KickbackFL.visible=1 : PlaySound SoundFX("fx_apron",DOFContactors), 0, 0.33*VolumeDial, AudioPan(li21), 0.05,0,0,1,AudioFade(li21)
			Case 8 : KickbackFL.visible=0 : KickbackFL.timerenabled=0 : kickbacktree=0 
		End Select
		kickbacktree=kickbacktree+1
	Else
		KickbackFL.visible=0
		KickbackFL.timerenabled=0
	End If
End Sub

Sub LiSpesialLeftFL_Timer : LiSpesialLeftFL.visible=0 : LiSpesialLeftFL.timerenabled=0 : End Sub

Sub LiSpecialRightFL_Timer : LiSpecialRightFL.visible=0 : LiSpecialRightFL.timerenabled=0 : End Sub

Sub LiSpecialRight2FL_Timer : LiSpecialRight2FL.visible=0 : LiSpecialRight2FL.timerenabled=0 : End Sub


Sub HunterTimer1_Timer
	LiHunter1.state=hunter(1)
	LiHunter2.state=hunter(2)
	LiHunter3.state=hunter(3)
	LiHunter4.state=hunter(4)
	LiHunter5.state=hunter(5)
	LiHunter6.state=hunter(6) 
	HunterTimer1.enabled=0
End Sub
Sub BountyTimer1_Timer
	LiBounty1.state=hunter(7) : LiBounty11.state=hunter(7)
	LiBounty2.state=hunter(8) : LiBounty22.state=hunter(8)
	LiBounty3.state=hunter(9) : LiBounty33.state=hunter(9)
	LiBounty4.state=hunter(10) : LiBounty44.state=hunter(10)
	LiBounty5.state=hunter(11) : LiBounty55.state=hunter(11)
	LiBounty6.state=hunter(12)  : LiBounty66.state=hunter(12)
	BountyTimer1.enabled=0
End Sub
Sub LiHunter1_Timer : LiHunter1.state=hunter(1) : LiHunter1.timerenabled=0 : End Sub
Sub LiHunter2_Timer : LiHunter2.state=hunter(2) : LiHunter2.timerenabled=0 : End Sub
Sub LiHunter3_Timer : LiHunter3.state=hunter(3) : LiHunter3.timerenabled=0 : End Sub
Sub LiHunter4_Timer : LiHunter4.state=hunter(4) : LiHunter4.timerenabled=0 : End Sub
Sub LiHunter5_Timer : LiHunter5.state=hunter(5) : LiHunter5.timerenabled=0 : End Sub
Sub LiHunter6_Timer : LiHunter6.state=hunter(6) : LiHunter6.timerenabled=0 : End Sub
Sub LiBounty1_Timer : LiBounty1.state=hunter(7) : LiBounty11.state=hunter(7) : LiBounty1.timerenabled=0 : End Sub
Sub LiBounty2_Timer : LiBounty2.state=hunter(8) : LiBounty22.state=hunter(8) : LiBounty2.timerenabled=0 : End Sub
Sub LiBounty3_Timer : LiBounty3.state=hunter(9) : LiBounty33.state=hunter(9) : LiBounty3.timerenabled=0 : End Sub
Sub LiBounty4_Timer : LiBounty4.state=hunter(10) :LiBounty44.state=hunter(10) : LiBounty4.timerenabled=0 : End Sub
Sub LiBounty5_Timer : LiBounty5.state=hunter(11) :LiBounty55.state=hunter(11) : LiBounty5.timerenabled=0 : End Sub
Sub LiBounty6_Timer : LiBounty6.state=hunter(12) :LiBounty66.state=hunter(12) : LiBounty6.timerenabled=0 : End Sub




'***** ALL HUNTERS LIT BLINKS ...
Sub HunterTimer2_Timer
	huntercount=huntercount+1
	If huntercount>52 Then huntercount=1 : End If
	If huntercount=9 Then
		LiHunter1.state=1:LiHunter2.state=1:LiHunter3.state=1
		LiHunter4.state=1:LiHunter5.state=1:LiHunter6.state=1
		LiBounty1.state=1:LiBounty2.state=1:LiBounty3.state=1
		LiBounty11.state=1 : LiBounty22.state=1 : LiBounty33.state=1
		LiBounty4.state=1:LiBounty5.state=1:LiBounty6.state=1
		LiBounty44.state=1:LiBounty55.state=1:LiBounty66.state=1

	End If
	If huntercount=10 Then 
		LiHunter1.state=hunter(1) : LiHunter2.state=hunter(2) : LiHunter3.state=hunter(3)
		LiHunter4.state=hunter(4) : LiHunter5.state=hunter(5) : LiHunter6.state=hunter(6) 
		LiBounty1.state=hunter(7) : LiBounty2.state=hunter(8) : LiBounty3.state=hunter(9)
		LiBounty11.state=hunter(7) : LiBounty22.state=hunter(8) : LiBounty33.state=hunter(9)
		LiBounty4.state=hunter(10) : LiBounty5.state=hunter(11) : LiBounty6.state=hunter(12) 
		LiBounty44.state=hunter(10) : LiBounty55.state=hunter(11) : LiBounty66.state=hunter(12)
	End If
End Sub

Sub Sw001_hit 
	LiLock001.timerenabled=0 : lockoffcount=0 : lilock1.blinkinterval = 110
	Sw001.enabled=0

End Sub


Sub sw002_hit
	LiLock001.timerenabled=0 : lockoffcount=0 : lilock1.blinkinterval = 110
	LSMissions.Play SeqBlinking,,3,100
	letterblink=8
	Lockholeblast.state=1 : Lockholeblast.timerenabled=1
	DOF 138,2 : 'Debug.print "DOF 138, 2"  'apophis
	LiLock1.state=1
	LiRefuel001.state=0
	locksequencer.play seqblinking,,3,50
	LiLock1.timerenabled=1
	' ball is locked 
	BallIsLockedStatus=1
	If LockedBalls=0 Then LOCKED003.State=1
	If LockedBalls=1 Then LOCKED002.State=1
	LockedBalls=LockedBalls+1

	LockedBallWait.enabled=1
	If LockedBalls=1 Then LockedID1=ActiveBall.ID
	If LockedBalls=2 Then LockedID2=ActiveBall.ID
	lockedballremove.enabled=1
	Sw51BLinker.enabled=1

	RandomLockedBallVoice

End Sub

Sub RandomLockedBallVoice

	i = int(rnd(1)*7)
	Select Case i
		Case 0 : PlaySound "andwewillbeready",0,1*BackGlassVolumeDial
		Case 1 : PlaySound "sneekuponus",0,1*BackGlassVolumeDial
		Case 2 : PlaySound "CobbVanthMarshall",0,1*BackGlassVolumeDial
		Case 3 : PlaySound "WillHelpYou",0,1*BackGlassVolumeDial
		Case 4 : PlaySound "bendalotofrules",0,1*BackGlassVolumeDial
		Case 5 : PlaySound "YouAreBountyhunter",0,1*BackGlassVolumeDial
		Case 6 : PlaySound "guildmember",0,1*BackGlassVolumeDial
	End Select

End Sub

Dim multiballsdone
Dim BallIsLockedStatus,BallWaitsw51,MultiBallStatus,MultiballActive
Dim LockedID1,LockedID2		
sub sw51_Hit
	LiLock001.timerenabled=0 : lockoffcount=0 : lilock1.blinkinterval = 110
	PlaySound SoundFX("fx_kicker_enter",DOFContactors), 0,.47*VolumeDial,AudioPan(sw51),0.75,0,0,1,AudioFade(sw51)
	If GAMEISOVER = 1 Then
		PlaySound SoundFX("fx_kicker",DOFContactors), 0,.7*VolumeDial,AudioPan(sw51),0.25,0,0,1,AudioFade(sw51)
		sw51.kick 160,3
		Exit Sub
	End If

	ManualLightSequencer.enabled=1 : MSeqCounter=170 : MSeqBigFL=20 

	If MultiballActive=1 Or Tilted Then
		LiLock1.state=0
		LiLock2.state=0 
	End If 
	If LiLock2.state=2 Then
			RestartCounter=0
			Light061_Timer
			DOF 210,1 ' multiball start
			'Debug.print "DOF 210, 1"  'apophis
			middleblinker.Play SeqBlinking,,20,30
			JP001.state=2
			JP002.state=2
			JP003.state=2 
			JP004.state=2
			JP005.state=2
			JP006.state=2 
			BallWaitsw51=2

			MultiballsDone=MultiballsDone+1
			If multiballsdone < 4 Then doubletimeonmulti=0
			If multiballsdone < 3 Then doubletimeonmulti=1
			If WizardLevel > 1 Then doubletimeonmulti=0

			JPsequencer.play seqblinking,,6,40
			plungerlane.play SeqBlinking,,10,50
			starwarscount=1
			DOF 202,2: 'Debug.print "DOF 202, 2"  'apophis
			i=Int(rnd(1)*3)
			If i = 1 Then Stoptalking : PlaySound "DeathAndChaos",0,1*BackGlassVolumeDial
			If i = 2 Then Stoptalking : PlaySound "lookoutside",0,1*BackGlassVolumeDial
			If i = 0 Then Stoptalking : PlaySound "warmorcold",0,1*BackGlassVolumeDial

			sw51.timerenabled=1
			multiballtimer.enabled=1
			Sw51BLinker.enabled=1
			BIP=BIP+2
			RedBorders
			PlaySound SoundFX("ambiance",DOFContactors), 0,.7*VolumeDial,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)
			MultiBallStatus=1
			MultiBallActive=1
			autokicker.enabled=1
			PlaySound SoundFX("bellvic",DOFContactors), 0,.7*VolumeDial,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)
			PlaySound SoundFX("bellvic",DOFContactors), 0,.7*VolumeDial,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)
			LiLock1.state=1
			LOCKED001.State=1
			locksequencer.play seqblinking,,3,50
			LiLock1.timerenabled=1
			LiLock2.state=1
			LiRefuel001.state=0
			multiballsequencer.play seqblinking,,3,50
	Else
		EmptyLock.enabled=1
		Sw51BLinker.enabled=1
		PlaySound SoundFX("fx_solenoidon3",DOFContactors), 0, 0.34*VolumeDial, AudioPan(sw51), 0.05,0,0,1,AudioFade(sw51)
		ObjLevel(10) = 1 : FlasherFlash10_Timer : 	DOF 136,2 : 'Debug.print "DOF 136, 2" ' yel right  'apophis
		If MultiballActive=0 Then PlaySound SoundFX ("walker",DOFContactors), 0,0.4*VolumeDial,0,0.25,0,0,1,0
	End If
End Sub
Sub	lockedballremove_timer
	sw002.DestroyBall
	sw51.enabled=1
	sw001.enabled=0
	sw002.enabled=0
	Primitive055.Z = -80
	sw51down.enabled=0
	Sw51up.enabled=1
	PlaySound SoundFX("springchange",DOFContactors), 0, .88*VolumeDial, AudioPan(sw51), 0.05,0,0,1,AudioFade(sw51)

	PlaySound "Drain_" & int(rnd(1)*9)+1,0,0.4*VolumeDial,AudioPan(sw51),0.50,0,0,1,AudioFade(sw51)
	lockedballremove.enabled=0
End Sub

Sub LockedBallWait_Timer
	Sw51BLinker.enabled=0
	LockedBallWait.enabled=0

	If BIP>1 then
		ballswaiting=ballswaiting+1
	Else
		If GAMEISOVER=0 Then
			BallRelease.CreateBall
			BallRelease.Kick 90, 5 : DOF 145,2  : luttarget=2
			PlaySound SoundFX("BallRelease" & int(rnd(1)*7)+1 , DOFContactors), 0,.7*VolumeDial,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)
			PlaySound SoundFX("fx_kicker",DOFContactors), 0,0.5*VolumeDial,AudioPan(BallRelease),0.22,0,0,1,AudioFade(BallRelease)
		End If
		skillshot=1
		LiSkillshot1.state=2 : 
		LiSkillshot2.state=2 
		Plunger001.Timerenabled=1
		autokicker.enabled=0 
		plunger001.fire
		li21.state=1
		KickbackFL.visible=1 : KickbackFL.timerenabled=1
		LiSpesialLeft.state=0
		LiSpesialLeftFL.visible=1 : LiSpesialLeftFL.timerenabled=1
		Gate002.twoway=true
	End If

	If WizardLevel < 3 And multiballsdone < 5 Then 
		LiReplay.timerenabled=0
		Lireplay2.state=0
		LiReplay2.timerenabled=0
		'If Not TournamentMode Then
		LiReplay.state=2 : LiReplay.timerenabled=1 : lireplay001.state = 2 : lireplay.blinkinterval=200 : lireplay001.blinkinterval=200 : pLireplay.blenddisablelighting=5
'pLireplay.blenddisablelighting=25
	End If

	Light003.state=0
	Light051.state=2
	RampsDoneForLockLight=0
	Stoptalking : PlaySound "conclusion",0,1*BackGlassVolumeDial

End Sub

Dim sw51downtarget : sw51downtarget=-70
Sub Sw51down_Timer
	If Primitive055.Z < sw51downtarget Then Sw51down.enabled=0 : sw51downtarget=-70 : stopsound "springchange" :  Exit Sub
		Primitive055.Z=Primitive055.Z-1
End Sub

Sub Sw51up_Timer
	If Primitive055.Z > -5 Then Sw51up.enabled=0 : stopsound "springchange" : Exit Sub
		Primitive055.Z=Primitive055.Z+1

End Sub



Sub EmptyLock_Timer
	PlaySound SoundFX("fx_solenoidon3",DOFContactors), 0, 0.34*VolumeDial, AudioPan(sw51), 0.05,0,0,1,AudioFade(sw51)
	ObjLevel(10) = 1 : FlasherFlash10_Timer : 	DOF 136,2 : 'Debug.print "DOF 136, 2" ' yel right  'apophis
	PlaySound SoundFX("rebound1",DOFContactors), 0, .7*VolumeDial, AudioPan(sw51), 0.05,0,0,1,AudioFade(sw51)
	PlaySound SoundFX("fx_kicker",DOFContactors), 0,.7*VolumeDial,AudioPan(sw51),0.25,0,0,1,AudioFade(sw51)
	sw51.kick 160,3
	sw51down.enabled=0
	Sw51up.enabled=1
	Primitive055.Z = -20 
	Primitive056.Y = 970 
	sw51arm.enabled=1
	scoring(510)
	Sw51BLinker.enabled=0
	EmptyLock.enabled=0
	sw001.timerenabled=1
End Sub

Sub sw51arm_timer
	If Primitive056.Y<919 Then sw51arm.enabled=0
	Primitive056.Y=Primitive056.Y-1
End Sub

Sub sw51_Timer
	PlaySound SoundFX("fx_solenoidon3",DOFContactors), 0, 0.34*VolumeDial, AudioPan(sw51), 0.05,0,0,1,AudioFade(sw51)
	ObjLevel(10) = 1 : FlasherFlash10_Timer : 	DOF 136,2 : 'Debug.print "DOF 136, 2" ' yel right  'apophis
End Sub


'*** SPINNERS ****
Dim spinnermission,spinnerScoring

'left spinner
Sub Spinner001_Spin()
	DOF 218,2
	'Debug.print "DOF 218, 2"  'apophis
	If mission(5)=1 Then 
		If missionstatus(5)=0 Then missionstatus(5)=1 : LiSupply006.state=0 : SupplySeq6.play seqblinking,,10,33  : End If
		If missionstatus(5)=2 Then missionstatus(5)=3 : LiSupply006.state=0 : SupplySeq6.play seqblinking,,10,33  : mission5update : End If
	End If
	If spinnermission<8 Then spinnermission=spinnermission+1 : Else : spinnerScoring=spinnerScoring+10 : spinnermission=0 : lightHQmissionON : End If
	PlaySound SoundFX("fx_spinner",DOFContactors), 0,.71*VolumeDial,AudioPan(Spinner001),0.25,0,0,1,AudioFade(Spinner001)
	scoring(spinnerScoring)


End Sub


'rightspinner
Sub Spinner002_Spin()
	DOF 221,2
	'Debug.print "DOF 218, 2"  'apophis
	If mission(5)=1 Then 
		If missionstatus(5)=0 Then missionstatus(5)=2 : LiSupply008.state=0 : SupplySeq8.play seqblinking,,10,33  :End If
		If missionstatus(5)=1 Then missionstatus(5)=3 : LiSupply008.state=0 : SupplySeq8.play seqblinking,,10,33  : mission5update : End If
	End If
	If spinnermission<8 Then spinnermission=spinnermission+1 : Else : spinnerScoring=spinnerScoring+10 : spinnermission=0 : lightHQmissionON : End If
	PlaySound SoundFX("fx_spinner",DOFContactors), 0,.7*VolumeDial,AudioPan(Spinner002),0.25,0,0,1,AudioFade(Spinner002)
	scoring(spinnerScoring)


End Sub


Sub lightHQmissionON : If enterlight.state=0 Then : enterlight.state=1 : EnterSeq.Play Seqblinking,,3,50 : LiStation2.state=0 : LiStation3.state=2 : End if : End Sub
Sub lightHQmissionOFF : enterlight.state=0 : EnterSeq.Play Seqblinking,,3,50 : LiStation2.state=2 : LiStation3.state=0 : End Sub


	
'************************************
' BUMPERS
Dim bumpermystery, BumperScoring , BumperStatus


Sub Bumper1_Timer    
	LSMissions.Play SeqBlinking,,3,60
End Sub


Sub Bumper1_Hit
	Bshake1=6
	LiBumper1_Timer

	If lostball.uservalue=0 Then
		Light038.state=0
		DOF 105, 2
		'	Debug.print "DOF 105, 2"  'apophis
		BumperScoring=BumperScoring+30
		If mission(6)=1 Then 
			If missionstatus(6)<15 Then missionstatus(6)=missionstatus(6)+1 : Else : mission6update : End If
		End If
		If bumpermystery<8 Then bumpermystery=bumpermystery+1 : Else : if mysterylight.state=0 then bumpermystery=0 : mysterylight.state=1 : MysterySeq.Play Seqblinking,,3,50 : End if : End If
		scoring(BumperScoring)
	End If
	PlaySound SoundFX("bumpers"&int(rnd(1)*15)+1,DOFContactors), 0, .7*VolumeDial, AudioPan(Bumper1), 0.05,-35,0,1,AudioFade(Bumper1)
	ObjLevel(5) = 1 : FlasherFlash5_Timer
	PlaySound SoundFX("thud01",DOFContactors), 0, 0.05*VolumeDial, AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger)
End Sub

Sub Bumper2_Hit

	Bshake2=6
	LiBumper2_Timer
	If lostball.uservalue=0 Then
		Light038.state=0
		DOF 106, 2
'Debug.print "DOF 106, 2"  'apophis
		BumperScoring=BumperScoring+30
		lowbumperblast.state=1 : lowbumperblast.timerenabled=1
		If mission(6)=1 Then 
			If missionstatus(6)<15 Then missionstatus(6)=missionstatus(6)+1 : Else : mission6update : End If
		End If
		If bumpermystery<8 Then bumpermystery=bumpermystery+1 : Else : if mysterylight.state=0 then bumpermystery=0 : mysterylight.state=1 : MysterySeq.Play Seqblinking,,3,50 : End if : End If
		scoring(BumperScoring)
	End If
	PlaySound SoundFX("bumpers"&int(rnd(1)*15)+1,DOFContactors), 0, .7*VolumeDial, AudioPan(Bumper1),  0.05,-35,0,1,AudioFade(Bumper1)
	ObjLevel(6) = 1 : FlasherFlash6_Timer
	PlaySound SoundFX("thud01",DOFContactors), 0, 0.05*VolumeDial, AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger)
End Sub

Sub Bumper3_Hit

	Bshake3=6
	LiBumper3_Timer
	If lostball.uservalue=0 Then
		Light038.state=0
		DOF 107, 2
'Debug.print "DOF 107, 2"  'apophis
	BumperScoring=BumperScoring+30
		If mission(6)=1 Then 
			If missionstatus(6)<15 Then missionstatus(6)=missionstatus(6)+1 : Else : mission6update : End If
		End If
		If bumpermystery<8 Then bumpermystery=bumpermystery+1 : Else : if mysterylight.state=0 then bumpermystery=0 : mysterylight.state=1 : MysterySeq.Play Seqblinking,,3,50 : End if : End If
		scoring(BumperScoring)
	End If
	PlaySound SoundFX("bumpers"&int(rnd(1)*15)+1,DOFContactors), 0, .7*VolumeDial, AudioPan(Bumper1), 0.05,-35,0,1,AudioFade(Bumper1)
	ObjLevel(7) = 1 : FlasherFlash7_Timer
	PlaySound SoundFX("thud01",DOFContactors), 0, 0.05*VolumeDial, AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger)
End Sub


'************************************
' lights entrance 5 X lights blinkers change for mini flashers ?
Dim light6,light7,light1,light2
Dim testlight
Sub Timer003_Timer
	If turningshipon=1 or attractwormhole Or ringsonly=1 Then TurningShip2
	If testlight=1 Then Timer001.enabled=1 
	If testlight=11 Then Timer002.enabled=1 
	If testlight=5 Then Timer004.enabled=1
	If testlight=15 Then Timer005.enabled=1 
	
	testlight=testlight+1
	If testlight>20 Then testlight=0
End Sub
Sub Timer001_Timer
	If light7=1 Then Light010.state=1 
	If light7=3 Then Light011.state=1
	If light7=5 Then Light012.state=1 
	If light7=6 Then Light012.state=0 : Light013.state=1 
	If light7=7 Then Light013.state=0 
	If light7=8 Then Light011.state=0 
	If light7=7 Then : Light010.state=0 
	light7=light7+1
	If light7>10 Then Timer001.Enabled=0 : light7=0
End Sub
Sub Timer002_Timer
	If light6=1 Then Light006.state=1 
	If light6=3 Then Light005.state=1 
	If light6=5 Then Light007.state=1 
	If light6=6 Then Light007.state=0 : Light008.state=1 
	If light6=7 Then Light008.state=0 
	If light6=8 Then Light005.state=0 
	If light6=10 Then Light006.state=0 
	light6=light6+1
	If light6>10 Then Timer002.enabled=0 : light6=0 
End Sub
Sub Timer004_Timer
	If light1=1 Then Light021.state=1 
	If light1=3 Then Light022.state=1 
	If light1=5 Then Light023.state=1 
	If light1=6 Then Light023.state=0 : Light024.state=1 
	If light1=7 Then Light024.state=0 
	If light1=8 Then Light022.state=0 
	If light1=10 Then Light021.state=0 
	light1=light1+1
	If light1>10 Then Timer004.enabled=0 : light1=0 
End Sub
Sub Timer005_Timer
	If light2=1 Then Light017.state=1 
	If light2=3 Then Light019.state=1 
	If light2=5 Then Light020.state=1 
	If light2=6 Then Light020.state=0 : Light025.state=1 
	If light2=7 Then Light025.state=0 
	If light2=8 Then Light019.state=0 
	If light2=10 Then Light017.state=0 
	light2=light2+1
	If light2>10 Then Timer005.enabled=0 : light2=0 
End Sub
Sub resetalllights
	Timer001.Enabled=0 : light7=0
	Timer002.enabled=0 : light6=0
	Timer004.enabled=0 : light1=0
	Timer005.enabled=0 : light2=0
End Sub

'********************
'*** Turret turn and fire 

Dim turrety,turretstatus,turretshooting
turrety=25 : turretshooting=2
Sub RLaser011_Timer
	LazerState=1
	If turretstatus<164 Then turretstatus=turretstatus+1 : Else turretY=25 : turretstatus=40 : turretshooting=Rnd(1)+0.01 
	If turretshooting>0.81 Then turretstatus=2 : turretshooting=0 
	If turretshooting>0.61 Then turretstatus=21 : turretshooting=0 
	If turretshooting>0.40 Then turretstatus=23 : turretshooting=0
	

	If turretstatus=2 Then RLaser011.state=2 : RLaser012.state=2 : ObjLevel(4) = 1 : FlasherFlash4_Timer : PlaySound SoundFX("XWingshotx2",DOFContactors), 0, .1*VolumeDial, AudioPan(RLaser012), 0.05,0,0,1,AudioFade(RLaser012) : 	DOF 137,2 : 'Debug.print "DOF 137, 2" 'red left
	If turretstatus=8 Then ObjLevel(4) = 1 : FlasherFlash4_Timer 
	If turretstatus=13 Then ObjLevel(4) = 1 : FlasherFlash4_Timer 
	If turretstatus=21 Then RLaser011.state=2 : RLaser012.state=2 : ObjLevel(4) = 1 : FlasherFlash4_Timer: PlaySound SoundFX("XWingshotx2",DOFContactors), 0, .1*VolumeDial, AudioPan(RLaser012), 0.05,0,0,1,AudioFade(RLaser012) : 	DOF 137,2 : 'Debug.print "DOF 137, 2" 'red left
	If turretstatus=28 Then ObjLevel(4) = 1 : FlasherFlash4_Timer 
	If turretstatus=35 Then ObjLevel(4) = 1 : FlasherFlash4_Timer
	If turretstatus=39 Then RLaser011.state=0 : RLaser012.state=0 : ObjLevel(4) = 1 : FlasherFlash4_Timer :		PlaySound SoundFX("fx_solenoidon",DOFContactors), 0, .7*VolumeDial, AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger) : 	DOF 137,2 : 'Debug.print "DOF 137, 2" 'red left
	If turretstatus>40 And turretstatus<70 Then turrety=turrety+1 
	If turretstatus>72 And turretstatus<132 Then turrety=turrety-1 
	If turretstatus>134 And turretstatus<164 Then turrety=turrety+1 
	Primitive019.RotY = turrety
End Sub

Sub RLaser012_Timer
	LazerState=0
	RLaser011.timerenabled=0 
	RLaser012.timerenabled=0
	RLaser011.state=0
	Rlaser012.state=0
End Sub


'*** Shipsmoving
Sub shiptrigger_Hit
	WingRState=2 : WingR_Blinks=1
	Light038.state=2
	DOF 112,1
'Debug.print "DOF 112, 1"  'apophis
	LiShip1.state=2
	PlaySound SoundFX("lever",DOFContactors), 0, 0.6*VolumeDial, AudioPan(shiptrigger), 0.05,0,0,1,AudioFade(shiptrigger)
	ShipTrigger.timerenabled=1
End Sub
Sub shiptrigger_UnHit
	PlaySound SoundFX("fx_shaker",DOFContactors), 0, 0.33*VolumeDial, AudioPan(shiptrigger), 0.05,0,0,1,AudioFade(shiptrigger)
End Sub

Dim wobbly, wobblyY, wobblyZ
wobblyY=-40
wobblyZ=-5

Sub ShipTrigger_Timer
	wobbly=wobbly+1
	if wobbly>10 and wobbly<21 Then WobblyY=WobblyY-2 : WobblyZ=WobblyZ-1
	if wobbly>0 and wobbly<11 Then 	WobblyY=WobblyY+2 : WobblyZ=WobblyZ+1
	if wobbly>30 and wobbly<41 Then WobblyY=WobblyY-1 : WobblyZ=WobblyZ-0.5
	if wobbly>20 and wobbly<31 Then 	WobblyY=WobblyY+1 : WobblyZ=WobblyZ+0.5
	Primitive003.RotY=WobblyY
	Primitive003.RotZ=WobblyZ
	If wobbly>40 then 
		wobbly=0 
		WobblyY=-40
		WobblyZ=-5
		ShipTrigger.timerenabled=0
		Liship1.state=0
		Light038.state=1
		DOF 112,0
'Debug.print "DOF 112, 0"  'apophis
	End If
End Sub

Sub shiptrigger2_Hit
	WingLState=2 : WingL_Blinks=1
	Light038.state=2
	DOF 112,1
'Debug.print "DOF 112, 1"  'apophis
	LiShip2.state=2
	PlaySound SoundFX("lever",DOFContactors), 0, 0.6*VolumeDial, AudioPan(shiptrigger2), 0.05,0,0,1,AudioFade(shiptrigger2)
	ShipTrigger2.timerenabled=1
End Sub
Sub shiptrigger2_UnHit
	PlaySound SoundFX("fx_shaker",DOFContactors), 0, 0.33*VolumeDial, AudioPan(shiptrigger2), 0.05,0,0,1,AudioFade(shiptrigger2)
End Sub

Dim wobbly2, wobblyY2, wobblyZ2
wobblyY2=337
wobblyZ2=-20

Sub ShipTrigger2_Timer
	wobbly2=wobbly2+1
	if wobbly2>10 and wobbly2<21 Then WobblyY2=WobblyY2-2 : WobblyZ2=WobblyZ2-2
	if wobbly2>0 and wobbly2<11 Then 	WobblyY2=WobblyY2+2 : WobblyZ2=WobblyZ2+3
	if wobbly2>30 and wobbly2<41 Then WobblyY2=WobblyY2-1 : WobblyZ2=WobblyZ2-1
	if wobbly2>20 and wobbly2<31 Then 	WobblyY2=WobblyY2+1 : WobblyZ2=WobblyZ2+1
	Primitive001.RotY=WobblyY2
	Primitive001.RotZ=WobblyZ2
	i=Int(rnd(1)*3)
'	If primitive001.material="metal gold dark" Then
'		primitive001.material="metal wire gold"
'	Else 
'		primitive001.material="metal gold dark"
'	End If

	If wobbly2>40 then
		wobbly2=0
		WobblyY2=337
		WobblyZ2=-20
		ShipTrigger2.timerenabled=0
		LiShip2.state=0
		Light038.state=1
		DOF 112,0
'Debug.print "DOF 112, 0"  'apophis
'		primitive001.material="metal wire gold"
	End If
End Sub



'**********
' wormhole
dim wormhole
Dim Mission(8)
Dim missionstatus(8)
wormhole = 0

'***droptargets hit
Dim DroptargetsCounter,DroptargetsScoring,DropTargetsStatus,ringsonly



'*******************************************************************
'* animated DT's oqqsan v1  ****************************************
'*******************************************************************
' (0=posup) 1=primitive 2=wall1 3=wall2 4=DoAction 5=countervariable 6=hitforcevariable 7=DropLength 8=material when down 9=material not dow

Const maxDT=7
Dim DropTarg(7,11) 
DT_init
' DT init  *********************************************************
Sub DT_init
	Set DropTarg(1,1) = pDT1 : Set DropTarg(1,2) = WallDT1 : Set DropTarg(1,3) = WallDT1b : DropTarg(1,8)="Plastic DarkDTdown" : DropTarg(1,9)="Plastic Dark"
	Set DropTarg(2,1) = pDT2 : Set DropTarg(2,2) = WallDT2 : Set DropTarg(2,3) = WallDT2b : DropTarg(2,8)="Plastic DarkDTdown" : DropTarg(2,9)="Plastic Dark"
	Set DropTarg(3,1) = pDT7 : Set DropTarg(3,2) = WallDT7 : Set DropTarg(3,3) = WallDT7b : DropTarg(3,8)="Plastic GreenBobaDown" : DropTarg(3,9)="Plastic GreenBobaUp" : DropTarg(3,10)=5.6
	Set DropTarg(4,1) = pDT6 : Set DropTarg(4,2) = WallDT6 : Set DropTarg(4,3) = WallDT6b : DropTarg(4,8)="Plastic GreenBobaDown" : DropTarg(4,9)="Plastic GreenBobaUp" : DropTarg(4,10)=5.6
	Set DropTarg(5,1) = pDT5 : Set DropTarg(5,2) = WallDT5 : Set DropTarg(5,3) = WallDT5b : DropTarg(5,8)="Plastic GreenBobaDown" : DropTarg(5,9)="Plastic GreenBobaUp" : DropTarg(5,10)=5.6
	Set DropTarg(6,1) = pDT4 : Set DropTarg(6,2) = WallDT4 : Set DropTarg(6,3) = WallDT4b : DropTarg(6,8)="Plastic GreenBobaDown" : DropTarg(6,9)="Plastic GreenBobaUp" : DropTarg(6,10)=5.6
	Set DropTarg(7,1) = pDT3 : Set DropTarg(7,2) = WallDT3 : Set DropTarg(7,3) = WallDT3b : DropTarg(7,8)="Plastic GreenBobaDown" : DropTarg(7,9)="Plastic GreenBobaUp" : DropTarg(7,10)=5.6
	For i = 1 to maxDT
		DropTarg(i,7) = -52 ' How far is the drop
		DropTarg(i,0) = DropTarg(i,1).z - 1 ' to set up height
	Next
End Sub

'* Code ** *********************************************************
Dim DTblink
DTTimer.interval=34
Sub DTTimer_Timer
	dim xx
	xx=blinkerDT-15 : xx=xx : If xx < 0 Then xx=0
	For i = 1 to MaxDT
		If i > 2 Then ' just boba's
			If DropTarg(i,1).z = DropTarg(i,0)+DropTarg(i,7) Then
				DropTarg(i,1).blenddisablelighting = 5 + xx/5
			ElseIf DTblink > 0 Then
				DTblink = DTblink - 0.25
				DropTarg(i,1).blenddisablelighting = (rnd(1)/2+0.5) * DropTarg(i,5) * DropTarg(i,10) - DropTarg(i,10)
			Else			
				DropTarg(i,1).blenddisablelighting = DropTarg(i,5) * ( DropTarg(i,10) + xx ) - DropTarg(i,10)
			End If
		End If

		Select Case DropTarg(i,4) ' action
			Case 1 : ' drop Item
				If DropTarg(i,1).z = DropTarg(i,0)+DropTarg(i,7) Then DropTarg(i,5) = 4 ' fast shake only		
				DropTarg(i,5) = DropTarg(i,5) + 1 		': debug.print "DT" & i & "  (nr,5)=" & DropTarg(i,5) & "   DOWN"
				If DropTarg(i,5) > 6 Then DropTarg(i,5) = 6
				If DropTarg(i,6) > 12 Then DropTarg(i,6) = 12
				Select Case DropTarg(i,5)
					case 1: DropTarg(i,1).rotx= DropTarg(i,6)/2
							DropTarg(i,1).z=DropTarg(i,0)+DropTarg(i,7)*0.077
					case 2: DropTarg(i,1).rotx= DropTarg(i,6)
							DropTarg(i,1).z=DropTarg(i,0)+DropTarg(i,7)*0.273
					case 3: DropTarg(i,1).rotx= DropTarg(i,6)*3/4
							DropTarg(i,1).z=DropTarg(i,0)+DropTarg(i,7)*0.503
					case 4: DropTarg(i,1).rotx= DropTarg(i,6)/2
							DropTarg(i,1).z=DropTarg(i,0)+DropTarg(i,7)*0.712
					case 5: DropTarg(i,1).rotx= DropTarg(i,6)/4
							DropTarg(i,1).z=DropTarg(i,0)+DropTarg(i,7)*1.057
					case 6: DropTarg(i,1).rotx= 0
							DropTarg(i,1).z=DropTarg(i,0)+DropTarg(i,7)
							If DropTarg(i,8) <> "" Then DropTarg(i,1).material = DropTarg(i,8)
							DropTarg(i,2).collidable = False
							DropTarg(i,3).collidable = False
							DropTarg(i,4) = 0 ' NO MORE ACTION
							DropTarg(i,6) = 0 ' hit force is reset
				End Select
			Case 2 : ' Raise it
				If DropTarg(i,1).z = DropTarg(i,0) Then DropTarg(i,5)=4 : DropTarg(i,2).collidable = True
				DropTarg(i,5) = DropTarg(i,5) + 1 '; debug.print "DT" & i & "  (nr,5)=" & DropTarg(i,5) & "   UP"
				If DropTarg(i,5) > 6 Then DropTarg(i,5) = 6
				If DropTarg(i,9) <> "" Then DropTarg(i,1).material = DropTarg(i,9)
				Select Case DropTarg(i,5)
					case 1: DropTarg(i,1).rotx= 0
					case 2: DropTarg(i,1).z=DropTarg(i,0)+DropTarg(i,7)*0.800
					case 3: DropTarg(i,1).z=DropTarg(i,0)+DropTarg(i,7)*0.554
							DropTarg(i,2).collidable = True
					case 4: DropTarg(i,1).z=DropTarg(i,0)+DropTarg(i,7)*0.212
					case 5: DropTarg(i,1).z=DropTarg(i,0)-DropTarg(i,7)*0.058
					case 6: DropTarg(i,1).z=DropTarg(i,0)
							DropTarg(i,3).collidable = True
							DropTarg(i,4) = 0 
				End Select
		End Select
		pDT003.z=DropTarg(7,1).z
		pDT004.z=DropTarg(6,1).z
		pDT005.z=DropTarg(5,1).z
		pDT006.z=DropTarg(4,1).z
		pDT007.z=DropTarg(3,1).z
	Next
End Sub
Sub Hit_DT(nr)
	If Mission(8)=1 Then DTBLINK=35 Else DTBlink=15		': debug.print "HITDT0" & nr
	DropTarg(nr,6) = ballvel(activeball)
	Drop_DT nr
	If nr > 2 Then Playsound "magnet1", 0, 0.8*BackglassVolumeDial, AudioPan(DropTarg(nr,1)), 0.05,0,0,1,AudioFade(DropTarg(nr,1))
End Sub
Sub Drop_5Bank : For i = 3 to 7 : Drop_DT i : Next : End Sub
Sub Drop_DT(nr)
	DropTarg(nr,2).collidable=False						' : debug.print "DropDT0" & nr
	DropTarg(nr,3).collidable=True
	DropTarg(nr,5) = 0
	DropTarg(nr,4) = 1
	If DropTarg(nr,1).z = DropTarg(nr,0)+DropTarg(nr,7) Then
		PlaySound SoundFX("Metal_Touch_9",DOFContactors), 0, 0.4*VolumeDial, AudioPan(DropTarg(nr,1)), 0.05,0,0,1,AudioFade(DropTarg(nr,1))
	else
		PlaySound SoundFX("Drop_Target_Down_" & int(rnd(1)*6)+1,DOFContactors), 0, 0.5*VolumeDial, AudioPan(DropTarg(nr,1)), 0.05,0,0,1,AudioFade(DropTarg(nr,1))
	End If
End Sub
Sub Raise_DT(nr)
	DropTarg(nr,5) = 0 ': debug.print "RaiseDT0" & nr
	DropTarg(nr,4) = 2
	If DropTarg(nr,1).z = DropTarg(nr,0) Then
		PlaySound SoundFX("Metal_Touch_4",DOFContactors), 0, 0.5*VolumeDial, AudioPan(DropTarg(nr,1)), 0.05,0,0,1,AudioFade(DropTarg(nr,1))
	Else
		PlaySound SoundFX("Drop_Target_Reset_" & int(rnd(1)*6)+1,DOFContactors), 0, 0.4*VolumeDial, AudioPan(DropTarg(nr,1)), 0.05,0,0,1,AudioFade(DropTarg(nr,1))
	End If
End Sub


'* DT walls HIT *********************************************************
Dim Dt01down : Dt01down=0
Dim Dt02down : Dt02down=0
Sub WallDT1_hit
	Hit_DT 1 ': debug.print "WallDT1_hit"
	DOF 134,2 : 'Debug.print "DOF 134, 2"  'apophis
	DroptargetsScoring=DroptargetsScoring+30
	DroptargetsCounter=DroptargetsCounter+1
	Dt01down=1
	wormhole=DT01down+DT02down 
	If wormhole=2 Then enterlight.timerenabled=1 : Else ringsonly=1 : End If
	scoring(DroptargetsScoring*3)

End Sub

Sub WallDT2_hit
	Hit_DT 2 ': debug.print "WallDT2_hit"
	DOF 134,2 :'Debug.print "DOF 134, 2"  'apophis
	DroptargetsScoring=DroptargetsScoring+30
	DroptargetsCounter=DroptargetsCounter+1
	Dt02down=1
	wormhole=DT01down+DT02down 
	If wormhole=2 Then enterlight.timerenabled=1 : Else ringsonly=1 : End If
	scoring(DroptargetsScoring*3)
End Sub

Sub WallDT7_Hit
	Hit_DT 3
	DOF 133,2

	If mission(8)=0 Then
		Mission7light.timerenabled=0
		Mission7update
	Else
		If bobaHIT1=0 Then bobaHIT1=1 : mission8counter=bobaHIT1+bobaHIT2+bobaHIT3+bobaHIT4+bobaHIT5
		If mission8counter=5 Then Mission8update
	End If
End Sub

Sub WallDT6_Hit
	Hit_DT 4
	DOF 133,2

	If mission(8)=0 Then
		Mission7light.timerenabled=0
		Mission7update
	Else
		If bobaHIT2=0 Then bobaHIT2=1 : mission8counter=bobaHIT1+bobaHIT2+bobaHIT3+bobaHIT4+bobaHIT5
		If mission8counter=5 Then Mission8update
	End If
End Sub

Sub WallDT5_Hit
	Hit_DT 5
	DOF 133,2

	If mission(8)=0 Then
		Mission7light.timerenabled=0
		Mission7update
	Else
		If bobaHIT3=0 Then bobaHIT3=1 : mission8counter=bobaHIT1+bobaHIT2+bobaHIT3+bobaHIT4+bobaHIT5
		If mission8counter=5 Then Mission8update
	End If
End Sub

Sub WallDT4_Hit
	Hit_DT 6
	DOF 133,2

	If mission(8)=0 Then
		Mission7light.timerenabled=0
		Mission7update
	Else
		If bobaHIT4=0 Then bobaHIT4=1 : mission8counter=bobaHIT1+bobaHIT2+bobaHIT3+bobaHIT4+bobaHIT5
		If mission8counter=5 Then Mission8update
	End If
End Sub

Sub WallDT3_Hit
	Hit_DT 7
	DOF 133,2

	If mission(8)=0 Then
		Mission7light.timerenabled=0
		Mission7update
	Else
		If bobaHIT5=0 Then bobaHIT5=1 : mission8counter=bobaHIT1+bobaHIT2+bobaHIT3+bobaHIT4+bobaHIT5
		If mission8counter=5 Then Mission8update
	End If
End Sub

'    **************
'******  End DT  ******
'    **************







Dim BlastdoorPos
Dim DoorOpener, LastWormhole, FlexBG,displayboss7status
Dooropener=15 : LastWormhole=0 
Dim Blastmovment
Sub BlastWall_hit

'	debug.print "BlastHit Ballvel=" & BallVel(activeball)
	Blastmovment = BallVel(activeball) * 1.5
	If Blastmovment < 1 Then Exit Sub
	If Blastmovment > 15 Then Blastmovment=15


	PlaySound SoundFX("WireRamp_Stop",DOFContactors), 0, VolumeDial*Blastmovment/16, AudioPan(pdt1), -0.2,0,0,1,AudioFade(pdt1)

	BlastWall.timerenabled=0	
	Blastwall.uservalue=0
	BlastWall.timerenabled=1
	Blastwall.uservalue=0
	Primitive071.RotX=90 + Blastmovment / 2	
End Sub

Blastwall.timerinterval=100
Sub BlastWall_Timer
	Blastwall.uservalue = Blastwall.uservalue+1
	If BlastWall.uservalue > 5 Then BlastWall.uservalue = 5

'	debug.print "blastuser=" & BlastWall.uservalue
	Select Case Blastwall.uservalue
		case 1 : Primitive071.RotX = 90 + Blastmovment
		case 2 : Primitive071.RotX = 90 + Blastmovment *3 / 4
		case 3 : Primitive071.RotX = 90 + Blastmovment / 2
		case 4 : Primitive071.RotX = 90 + Blastmovment / 4
		case 5 : Primitive071.RotX = 90 : BlastWall.timerenabled = 0
	End Select
End Sub

Dim workholeballs : workholeballs=0
Sub Trigger001_hit
	workholeballs=workholeballs+1
'	debug.print "wormholeBalls:" & workholeballs
End Sub
Sub Trigger001_unhit
	workholeballs=workholeballs-1
'	debug.print "wormholeBalls:" & workholeballs
End Sub

	
Sub OpenBlastDoor_Timer
	If LastWormhole<>wormhole Then LastWormhole=Wormhole : PlaySound "blastdoor", 0, 0.4*BackGlassVolumeDial, AudioPan(pdt2), 0.05,0,0,1,AudioFade(pdt2)

	If DoorOpener <3 Then
		Light046.state=2
	Else
		Light046.state=0 
	End If
	If DoorOpener > 13 Then 
		Light073.state=1
		BlastWall.collidable=False 
	Else
		Light073.state=0
		BlastWall.collidable=True 
	End If
	If int(DoorOpener) = 19 Then DoorOpener=15 : FlexBG="Door_15.jpg" : Light037.state=0
	If int(DoorOpener) = 18 Then DoorOpener=19
	If int(DoorOpener) = 17 Then DoorOpener=18 : If enterlight.state=2 Or mysterylight.state=2 Or extraballlight.state=2 Then FlexBG="Door_16.jpg" : Light037.state=1
	If int(DoorOpener) = 16 Then DoorOpener=17
	If int(DoorOpener) = 15 Then DoorOpener=16

	If wormhole>1 And DoorOpener<15 Then
		DoorOpener=DoorOpener+0.5 
		If DoorOpener=15 Then PlaySound SoundFX("eldoor1",DOFContactors), 0, 0.58*VolumeDial, AudioPan(pdt2), 0.05,0,0,1,AudioFade(pdt2) 
	End If
	If wormhole=1 And DoorOpener<7 Then DoorOpener=DoorOpener+0.5 
	If wormhole=1 And DoorOpener>7 Then DoorOpener=DoorOpener-0.5 : If DoorOpener>14 Then DoorOpener=14 : End If : End If
	If wormhole=0 And DoorOpener>1 Then 
		DoorOpener=DoorOpener-0.5
		If DoorOpener<1 Then DoorOpener=1
		If DoorOpener>14 Then DoorOpener=14
		If DoorOpener=1 Then PlaySound "jaildoor", 0, .52 *BackGlassVolumeDial, AudioPan(pdt2), 0.05,0,0,1,AudioFade(pdt2) : PlaySound SoundFX("eldoor1",DOFContactors), 0, 0.6*VolumeDial, AudioPan(pdt2), 0.05,0,0,1,AudioFade(pdt2)
	End If

	If DoorOpener<16 Then FlexBG="Door_"&int(DoorOpener)&".jpg"

	If workholeballs > 0 And DoorOpener < 14 Then DoorOpener=14
	BlastdoorPos=13-52/14*(DoorOpener-1)
	If BlastdoorPos < - 38 Then BlastdoorPos=-38
	Primitive071.Z=BlastdoorPos

	If Boss7Countdown Then
		If FlexDMD Then If NOT UMainDMD.isrendering Then If Gameoverstatus=0 And BonusStatus = 0 Then tempstr=FormatScore(P1score) : UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex FlexBG, "RAMPS" & cstr (boss7ramps) & "@"& cstr (boss7countdown) & "S", 0, 12, FlexScore , 15, 1, 14, 2, 14  
		DisplayBoss7Status=1
	Else
		If FlexDMD Then If NOT UMainDMD.isrendering Then If Gameoverstatus=0 And BonusStatus = 0 Then tempstr=FormatScore(P1score) : UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex FlexBG, "PLAYER." & currentplayer & "  Ball."& cstr (ballinplay), 0, 12, FlexScore , 15, 1, 14, 2, 14  
	End If
End Sub

Dim AGrantedonlyonce
Sub enterlight_Timer
	Dim i
	i=0
	If enterlight.state=1 Then
		PlaySound SoundFX("fx_solenoid",DOFContactors), 0, .7*VolumeDial, AudioPan(enterlight), 0.05,0,0,1,AudioFade(enterlight)
		enterlight.state=2 : i = 1 : EnterSeq.Play Seqblinking,,3,50
	bossblinker.Play SeqBlinking,,3,40

	End if	
	if mysterylight.state=1 then
		PlaySound SoundFX("fx_solenoid",DOFContactors), 0, .7*VolumeDial, AudioPan(enterlight), 0.05,0,0,1,AudioFade(enterlight)
		mysterylight.state=2 : i = 1 : MysterySeq.Play Seqblinking,,3,50
	End if
	if extraballlight.state=1 then
		'If extraball=0 And FlyAgain3.state=0 Then
			PlaySound SoundFX("fx_solenoid",DOFContactors), 0, .7*VolumeDial, AudioPan(enterlight), 0.05,0,0,1,AudioFade(enterlight)
			extraballlight.state=2 : i = 1 : ExtraSeq.Play Seqblinking,,3,50
		'End If 
	End if
	If i>0 And AGrantedonlyonce=0 Then
		ringsonly=0
		turningshipon=1 
		AGrantedonlyonce=1
		PlaySound "AGranted2",0,1*BackGlassVolumeDial
		If ManualLightSequencer.enabled=0 Then
			ManualLightSequencer.enabled=1
			MSeqCounter=190
			MSeqBigFL=20 
		End If
	End If
End sub


'***enterwh?
Dim enterlightscoring,enterlightstatus,missionsdone
Dim mysteryscoring,mysterystatus,mysteryreward
Dim extraballstatus,ExtraBall,TotalExtraBalls

Sub sw7_Hit
	Primitive068.z = Primitive068.z+7

	Dim i
	PlaySound SoundFX("fx_hole_enter",DOFContactors), 0, 0.42*VolumeDial, AudioPan(sw7), 0.05,0,0,1,AudioFade(sw7)
	' **** mission light !!

	If Tilted Then
		timer7.enabled=0
		light046.timerenabled=1
'		sw7.kick 170,2
'		scoring(200)
		Stoptalking
'		PlaySound "aimingfortheotherone", 0, .4*BackGlassVolumeDial, AudioPan(sw7), 0.05,0,0,1,AudioFade(sw7)
'		cutoutheoone.enabled=1
'		PlaySound SoundFX("fx_kicker",DOFContactors), 0, .42*VolumeDial, AudioPan(sw7), 0.05,0,0,1,AudioFade(sw7)
		Exit Sub
	End If

	if enterlight.state=2 Then

		If Bip=1 Then WormholeButton = True : buttonblink=11

	    ManualLightSequencer.enabled=1 : MSeqCounter=190 : MSeqBigFL=20 
		turningshipon=0
		attractwormhole=150
		stationonlycounter=250

		bangtimer.enabled=1
		LightSeq002.Play SeqUpOn,30,1,1
		LSMissions.Play SeqBlinking,,5,90


		Drop_DT 1

		Drop_DT 2

		bossblinker.Play SeqBlinking,,8,33

		Stoptalking: talkingdelay=27
		i=int(rnd(1)*7)
		If i=0 or i=3 Then PlaySound ("illtakethemall"),0,1*BackGlassVolumeDial
		If i=1 Then PlaySound ("bountypuck"),0,1*BackGlassVolumeDial : talkingdelay=17
		If i=2 Then PlaySound ("bountypucksmal"),0,1*BackGlassVolumeDial
		If i>3 Then PlaySound ("catchthemall"),0,1*BackGlassVolumeDial


		Objlevel(1) = 1 : FlasherFlash1_Timer
		Objlevel(2) = 1 : FlasherFlash2_Timer
		PlaySound SoundFX("fx_solenoidon",DOFContactors), 0, .7*VolumeDial, AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger)
		enterlightscoring=enterlightscoring+3000
		scoring(enterlightscoring)
		addletter(1)
		newmission : If doublemission=1 Then newmission
		li21.State=2
		KickbackFL.visible=1 : KickbackFL.timerenabled=1
		li21.timerenabled=1
		LiSpesialLeft.state=0
		LiSpesialLeftFL.visible=1 : LiSpesialLeftFL.timerenabled=1
		' REWARD KICKBACK LIGHT ON 
		lightHQmissionOFF
		timer7.Enabled = 1
		droptargetup.Enabled = 1
		AGrantedonlyonce=0
		enterlight.timerenabled=0
		Displaybuzy=0
		Displaytext "            ","            "
		enterlightstatus=1
		If mission(3)=1 Then stationonlycounter=0
	End If

	' **** mystery light !!
	If mysterylight.state=2 Then
		If Bip=1 Then WormholeButton = True : buttonblink=buttonblink+5 : if buttonblink<11 then ButtonBlink=11
		turningshipon=0
		bangtimer.enabled=1
		LightSeq002.Play SeqUpOn,30,1,1
		mysteryscoring=mysteryscoring+100000
		mysteryreward=int(rnd(1)*14)+1
		If mysteryreward>13 And lilock1.state=0 Then
			If MultiballActive=1 Then mysteryreward=int(rnd(1)*13)+1
		End If

		mysterystatus=1
		Displaybuzy=0
		Displaytext "            ","            "
		mysterylight.state=0 : MysterySeq.Play Seqblinking,,3,50 
		bumpermystery=0
		lightHQmissionOFF
		timer7.Enabled = 1
		droptargetup.Enabled = 1
		AGrantedonlyonce=0
		enterlight.timerenabled=0
	End If

	' **** extraball light !!
	If extraballlight.state=2 Then
		If Bip=1 Then WormholeButton = True : buttonblink=buttonblink+5 : if buttonblink<11 then ButtonBlink=11
		turningshipon=0
		extraballlight.state=0
		 bangtimer.enabled=1
		LightSeq002.Play SeqUpOn,30,1,1
		If FlyAgain3.state=2 Then
			Displaybuzy=0
			Displaytext "            ","            "
			extraballstatus=1
			DOF 215,2
'Debug.print "DOF 215, 2"  'apophis
			ExtraBall=1
			extraextraball=extraextraball+1
			TotalExtraBalls=TotalExtraBalls+1

			extraballlight.state=0 : ExtraSeq.Play Seqblinking,,3,50
		Else
			ExtraBall=1
			extraballstatus=1
			DOF 215,2
'Debug.print "DOF 215, 2"  'apophis
			TotalExtraBalls=TotalExtraBalls+1
			FlyAgain1.state=2 : pflyagain.blenddisablelighting=5
			FlyAgain2.state=2
			FlyAgain3.state=2
		End If
		lightHQmissionOFF
		timer7.Enabled = 1 
		droptargetup.Enabled = 1
		AGrantedonlyonce=0
		enterlight.timerenabled=0
	End If

	If timer7.enabled=0 Then
'		sw7.kick 170,2
		light046.timerenabled=1
		scoring(200)
		Stoptalking
		PlaySound "aimingfortheotherone", 0, .4*BackGlassVolumeDial, AudioPan(sw7), 0.05,0,0,1,AudioFade(sw7)
		cutoutheoone.enabled=1
'		PlaySound SoundFX("fx_kicker",DOFContactors), 0, .42*VolumeDial, AudioPan(sw7), 0.05,0,0,1,AudioFade(sw7)
	End If
End Sub

Sub Light046_Timer
		light046.timerenabled=0
		sw7.kick 170,2

		Primitive073.Y = 395 
		Primitive073.X = 316
		kicker007arm.enabled=1
		Kicker007up.enabled=1
		primitive068.Z=-24

'		PlaySound "aimingfortheotherone", 0, .4*BackGlassVolumeDial, AudioPan(sw7), 0.05,0,0,1,AudioFade(sw7)
'		cutoutheoone.enabled=1
		PlaySound SoundFX("fx_kicker",DOFContactors), 0, .42*VolumeDial, AudioPan(sw7), 0.05,0,0,1,AudioFade(sw7)
End Sub

Sub newmission
	Dim Dobey,dobey2
	Dobey2=0
	Dobey=(int(rnd(1)*8)+1)
	If dobey=1 and mission(1)=0 then startmission1 : dobey2=1
	If dobey=2 and mission(2)=0 then startmission2 : dobey2=1
	If dobey=3 and mission(3)=0 then startmission3 : dobey2=1
	If dobey=4 and mission(4)=0 then startmission4 : dobey2=1
	If dobey=5 and mission(5)=0 then startmission5 : dobey2=1
	If dobey=6 and mission(6)=0 then startmission6 : dobey2=1
	If dobey=7 and mission(7)=0 then startmission7 : dobey2=1
	If dobey=8 and mission(8)=0 then startmission8 : dobey2=1

	If dobey2=0 and mission(1)=0 then startmission1 : dobey2=1
	If dobey2=0 and mission(2)=0 then startmission2 : dobey2=1
	If dobey2=0 and mission(3)=0 then startmission3 : dobey2=1
	If dobey2=0 and mission(4)=0 then startmission4 : dobey2=1
	If dobey2=0 and mission(5)=0 then startmission5 : dobey2=1
	If dobey2=0 and mission(6)=0 then startmission6 : dobey2=1
	If dobey2=0 and mission(7)=0 then startmission7 : dobey2=1
	If dobey2=0 and mission(8)=0 then startmission8 : dobey2=1

	'Reset all mission light to same time blink01
		Mission1Light.state=0
		Mission1light003.state=0
		Mission1light004.state=0
		Mission1light005.state=0
		Mission2Light.state=0
		Mission2Light003.state=0
		Mission2Light004.state=0
		Mission2Light005.state=0
		Mission3Light.state=0
		Mission3Light003.state=0
		Mission3Light004.state=0
		Mission3Light005.state=0
		Mission4Light.state=0
		Mission4Light003.state=0
		Mission4Light004.state=0
		Mission4Light005.state=0
		Mission5Light.state=0
		Mission5Light003.state=0
		Mission5Light004.state=0
		Mission5Light005.state=0
		Mission6Light.state=0
		Mission6Light003.state=0
		Mission6Light004.state=0
		Mission6Light005.state=0
		Mission7Light.state=0
		Mission7Light003.state=0
		Mission7Light004.state=0
		Mission7Light005.state=0
		Mission8Light.state=0
		Mission8Light003.state=0
		Mission8Light004.state=0
		Mission8Light005.state=0

	If mission(1)=1 Then Mission1light.state=2 : Mission1Light003.state=2 : Mission1Light004.state=2  : Mission1Light005.state=2 ': If Mission1FL1.uservalue=0 Then Mission1FL1.timerenabled=1 : Mission1FL1.uservalue=1 
	If mission(2)=1 Then Mission2light.state=2 : Mission2Light003.state=2 : Mission2Light004.state=2  : Mission2Light005.state=2 ': If Mission2FL1.uservalue=0 Then Mission2FL1.timerenabled=1 : Mission2FL1.uservalue=1 
	If mission(3)=1 Then Mission3light.state=2 : Mission3Light003.state=2 : Mission3Light004.state=2  : Mission3Light005.state=2 ': If Mission3FL1.uservalue=0 Then Mission3FL1.timerenabled=1 : Mission3FL1.uservalue=1 
	If mission(4)=1 Then Mission4light.state=2 : Mission4Light003.state=2 : Mission4Light004.state=2  : Mission4Light005.state=2 ': If Mission4FL1.uservalue=0 Then Mission4FL1.timerenabled=1 : Mission4FL1.uservalue=1 
	If mission(5)=1 Then Mission5light.state=2 : Mission5Light003.state=2 : Mission5Light004.state=2  : Mission5Light005.state=2 ': If Mission5FL1.uservalue=0 Then Mission5FL1.timerenabled=1 : Mission5FL1.uservalue=1 
	If mission(6)=1 Then Mission6light.state=2 : Mission6Light003.state=2 : Mission6Light004.state=2  : Mission6Light005.state=2 ': If Mission6FL1.uservalue=0 Then Mission6FL1.timerenabled=1 : Mission6FL1.uservalue=1 
	If mission(7)=1 Then Mission7light.state=2 : Mission7Light003.state=2 : Mission7Light004.state=2  : Mission7Light005.state=2 ': If Mission7FL1.uservalue=0 Then Mission7FL1.timerenabled=1 : Mission7FL1.uservalue=1 
	If mission(8)=1 Then Mission8light.state=2 : Mission8Light003.state=2 : Mission8Light004.state=2  : Mission8Light005.state=2 ': If Mission8FL1.uservalue=0 Then Mission8FL1.timerenabled=1 : Mission8FL1.uservalue=1 
End Sub


'** MissionDone
Dim MissionsDoneCounter,MissionsDoneThisBall,ExtraBallIsLitStatus
Dim Missions4Evasive , BossStatus, BossLevel, BossActive, Boss7Countdown, boss7ramps

'   start other bosses here !!!!
'1+3+5 = evasiveone
'2=mudhorn
'4=spider
'6=dragon
'7=Darkies
dim mission1after1onlyonce

Dim WizardLevel


Sub MissionDone
	if mission1after1onlyonce=0 Then
		mission1after1onlyonce=1
		Bossactive=1
		bosslevel=1
		If FlexDMD Then UMainDMD.cancelrendering()
		BossStatus=1
		Boss1_Timer
	End If

	Missions4Evasive=Missions4Evasive+1

	If BossLevel=7 Then
		If EvasiveState=4 Then
			EvasiveState=5 ''' start gideon when if not running on when on darkies
		Else
			RaiseBigDT.enabled=1 : BigDTUD=1 : Wall025.collidable=true : Wall026.collidable=true
			PlaySound SoundFX("elengine",DOFContactors), 0, 0.8*VolumeDial, AudioPan(Primitive059), 0.05,0,0,1,AudioFade(Primitive059)
		End If
	End If

	If Missions4Evasive>=3 Then '
		If BossActive=0 Then
			BossActive=1
			BossLevel=BossLevel+1 
			If BossLevel=8 Then 
				BossLevel=7
'				Boss1.state=0
'				Boss2.state=0
'				Boss3.state=0
'				Boss4.state=0
'				Boss5.state=0
'				Boss6.state=0
'				Boss7.state=0
			End If
			If BossLevel=1 Or BossLevel=3 or BossLevel=5 Then ' movingfighter
				Missions4Evasive=Missions4Evasive-3
				If FlexDMD Then UMainDMD.cancelrendering()
				BossStatus=1 
'				If BossLevel=1 Then BOSS1.state=2 
'				If BossLevel=3 Then BOSS3.state=2
'				If BossLevel=5 Then BOSS5.state=2 
			End If
			If BossLevel=2 Then 'mudhorn
				light069.state=2

				BigDThitsCounter=0
				Missions4Evasive=Missions4Evasive-3
				If FlexDMD Then UMainDMD.cancelrendering()
				BossStatus=2 
'				BOSS2.state=2 
			End If
			If BossLevel=4 Then 'spidey
				light069.state=2

				BigDThitsCounter=0
				Missions4Evasive=Missions4Evasive-3
				If FlexDMD Then UMainDMD.cancelrendering()
				BossStatus=3
'				BOSS4.state=2 
			End If
			If BossLevel=6 Then 'dragon
				light069.state=2
				BigDThitsCounter=0
				Missions4Evasive=Missions4Evasive-3
				If FlexDMD Then UMainDMD.cancelrendering()
				BossStatus=4  
'				BOSS6.state=2 
			End If
			If BossLevel=7 Then 'darkies
				WizardLevel=WizardLevel+1
				If WizardLevel = 1 Then JackPotScoring=JackPotScoring+1000000
				Missions4Evasive=Missions4Evasive-3
				If FlexDMD Then UMainDMD.cancelrendering()
				BossStatus=5 ' 
'				BOSS7.state=2 
				Boss7Ramps=0

			End If
			Boss1_Timer
		End If
	End If

	letterblink=1
	MissionsDoneCounter=MissionsDoneCounter+1    ' **** for end of ball bonus  
	MissionsDoneThisBall=MissionsDoneThisBall+1
	If MissionsDoneThisBall=5 And Not TournamentMode Then
		If ExtraBall=0 Then 
			ExtraBallIsLitStatus=1
			extraballlight.state=1 : ExtraSeq.Play Seqblinking,,3,50 
		End If
	End If
End Sub

Sub Boss3_Timer ' wrong light as the boss7timer was useD'
	If BOSS3.timerenabled = False Then
		BOSS3.timerenabled = True
		Boss7.blinkpattern = "10100" 
		Boss7.blinkinterval= 40
		BOSS7.state=2
		Exit Sub
	End If
	Boss7.blinkpattern = "10" 
	Boss7.blinkinterval= 100
	Boss7.state=1
	boss3.timerenabled = False
End Sub

Dim WizardBonusLevel
'0=10 Ramps
'1=12 Ramps
'2=14 Ramps
'3=16 Ramps


Dim SUPERSCORING
Sub boss7_Timer
	boss7countdown=Boss7Countdown-1
	If boss7countdown<=0 Then 

		If Missions4Evasive > 1 then Missions4Evasive = 1 

		SUPERSCORING=1
		FinalRestart.enabled=1
		DisplayBoss7Status=0
		Boss7Countdown=0
		boss7.timerenabled=0
		BossStatus=10 
		LeftFlipper.RotateToStart : TopFlipper.RotateToStart : RightFlipper.RotateToStart
	End If

	if WizardBonusLevel > 3 Then WizardBonusLevel = 3
	If boss7ramps>9+WizardBonusLevel*2 Then '  should be >14..  ended up at 12 is oki now 10 12 14 16

		If Missions4Evasive > 1 then Missions4Evasive = 1  ' restartDarkies need minimum 2 new missions done !

		WizardBonusLevel=WizardBonusLevel+1



		boss7ramps=25
		SUPERSCORING=1
		FinalRestart.enabled=1
		DisplayBoss7Status=0
		Boss7Countdown=0
		boss7.timerenabled=0
		BossStatus=10 : scoring(JackPotScoring*(25 + WizardBonusLevel * 10 )) 
		LeftFlipper.RotateToStart : TopFlipper.RotateToStart : RightFlipper.RotateToStart
	End If

End Sub


Sub Boss1_Timer
	If boss1.timerenabled=0 Then
		boss1.timerenabled=1
		boss1.uservalue=0
	Else
		boss1.uservalue=boss1.uservalue+1
		Select Case boss1.uservalue
			case  1, 8,15,22 : BOSS7.state=0 : BOSS1.state=1
			case  2, 9,16,23 : BOSS1.state=0 : BOSS3.state=1 
			case  3,10,17,24 : BOSS3.state=0 : BOSS5.state=1
			case  4,11,18,25 : BOSS5.state=0 : BOSS6.state=1
			case  5,12,19,26 : BOSS6.state=0 : BOSS4.state=1
			case  6,13,20,27 : BOSS4.state=0 : BOSS2.state=1
			case  7,14,21,28 : BOSS2.state=0 : BOSS7.state=1
			Case  29		 : BOSS7.state=0
			Case  30,32 	 : BOSS1.state=1 : BOSS2.state=1 : BOSS3.state=1 : BOSS4.state=1 : BOSS5.state=1 : BOSS6.state=1 : BOSS7.state=1 
			Case  31,33 	 : BOSS1.state=0 : BOSS2.state=0 : BOSS3.state=0 : BOSS4.state=0 : BOSS5.state=0 : BOSS6.state=0 : BOSS7.state=0

		
			case 34   : If bosslevel > 0 Then Boss1.state=1
						If bosslevel > 1 Then Boss2.state=1
						If bosslevel > 2 Then Boss3.state=1
						If bosslevel > 3 Then Boss4.state=1
						If bosslevel > 4 Then Boss5.state=1
						If bosslevel > 5 Then Boss6.state=1
						If bosslevel > 6 Then Boss7.state=1
 						If bosslevel = 1 And BossActive=1 Then Boss1.state=2
 						If bosslevel = 2 And BossActive=1 Then Boss2.state=2
 						If bosslevel = 3 And BossActive=1 Then Boss3.state=2
 						If bosslevel = 4 And BossActive=1 Then Boss4.state=2
 						If bosslevel = 5 And BossActive=1 Then Boss5.state=2
 						If bosslevel = 6 And BossActive=1 Then Boss6.state=2
 						If bosslevel = 7 And BossActive=1 Then Boss7.state=2
						boss1.timerenabled=0
		End Select
	End If
End Sub


'*** Mission8_
Sub startmission8
	mission(8)=1
	missionstatus(8)=0
	mission8counter=0
	Mission8light.state=2
'	Mission8FL1.timerenabled=1 : Mission8FL1.uservalue=5
	Mission8Light003.state=2 
	Mission8Light004.state=2 
	Mission8Light005.state=2 
	Mission8light001.state=2
	Mission8light002.state=2
	Mission8light001.timerenabled=1
	Mission8Light.timerenabled=1
	If Not StopthingsRestartmissions Then

	Drop_5Bank
	Walldt7.timerenabled=1
	Walldt6.timerenabled=1
	Walldt5.timerenabled=1
	Walldt4.timerenabled=1
	Walldt3.timerenabled=1

	bobaHIT1=0
	bobaHIT2=0 : 
	bobaHIT3=0 : 
	bobaHIT4=0 : 
	bobaHIT5=0 :
End If
 
	If mission(7)=1 Then Mission7Light.TimerEnabled=1 ' get random boba after mission 8 is done

	' fix new sound for ambush
	PlaySound "motorlauch", 0, 0.09*BackGlassVolumeDial, AudioPan(sw7), 0.05,0,0,1,AudioFade(sw7)
End Sub

Dim mission8scoring,mission8status,mission8counter




Sub Mission8update
	If mission(7)=1 Then
		mission7light.timerenabled=0
		mission7light.timerenabled=1
	End If
'	Mission8FL1.timerenabled=1 : Mission8FL1.uservalue=7
	addletter(3)
	mission8scoring=mission8scoring+100000
	scoring(mission8scoring)
	PlaySound "missioncomplete", 0, BackGlassVolumeDial, AudioPan(Kicker001), 0.05,0,0,1,AudioFade(Kicker001)
	mission(8)=0
	Mission8light.state=0
	Mission8light003.state=0
	Mission8light004.state=0
	Mission8light005.state=0
	Mission8light001.state=1
	Mission8light002.state=1
	Mission8light001.timerenabled=1
	mission8status=1
	MissionDone



	Drop_5Bank

	Walldt7.timerenabled=0
	Walldt6.timerenabled=0
	Walldt5.timerenabled=0
	Walldt4.timerenabled=0
	Walldt3.timerenabled=0

End Sub

Sub Mission8light001_Timer
	Mission8light001.state=0
	Mission8light002.state=0
	Mission8light001.timerenabled=0
End Sub

'*** Mission7_
Sub startmission7
	mission(7)=1
	missionstatus(7)=0
	Mission7light.state=2
'	Mission7FL1.timerenabled=1 : Mission7FL1.uservalue=5
	Mission7Light003.state=2 
	Mission7Light004.state=2 
	Mission7Light005.state=2 
	Mission7light001.state=2
	Mission7light002.state=2
	Mission7light001.timerenabled=1
	Mission7Light.timerenabled=1
	Poppedboba=0
	getrandomboba
	If Not StopthingsRestartmissions Then PlaySound "motorlauch", 0, 0.09*BackGlassVolumeDial, AudioPan(sw7), 0.05,0,0,1,AudioFade(sw7)
End Sub

Dim Poppedboba


Sub getrandomboba
	If mission(8)=0 And mission(7)=1 Then
		i= Int(Rnd(1)*5)+1
		If poppedboba>0 Then Drop_DT 8-Poppedboba
		DropTarg(8-i,2).timerenabled=1
		Poppedboba=i
	End If
End Sub

Sub WallDT7_Timer
	If mission(8)=1 or mission(7)=1 Then
		Raise_DT 3
		WallDT7.timerenabled=0
	End If
End Sub

Sub WallDT6_Timer
	If mission(8)=1 or mission(7)=1 Then
		Raise_DT 4
		WallDT6.timerenabled=0
	End If
End Sub

Sub WallDT5_Timer
	If mission(8)=1 or mission(7)=1 Then
		Raise_DT 5
		WallDT5.timerenabled=0
	End If
End Sub

Sub WallDT4_Timer
	If mission(8)=1 or mission(7)=1 Then
		Raise_DT 6
		WallDT4.timerenabled=0
	End If
End Sub

Sub WallDT3_Timer
	If mission(8)=1 or mission(7)=1 Then
		Raise_DT 7
		WallDT3.timerenabled=0
	End If
End Sub



Sub Mission7Light_Timer
	If mission(8)=0 Then getrandomboba
End Sub





Dim bobaHIT1,bobaHIT2,bobaHIT3,bobaHIT4,bobaHIT5


Sub Mission7resetAll

	Drop_5Bank

	Walldt7.timerenabled=0
	Walldt6.timerenabled=0
	Walldt5.timerenabled=0
	Walldt4.timerenabled=0
	Walldt3.timerenabled=0

	Mission7Light.timerenabled=0
End Sub


Dim mission7scoring,mission7status
Sub Mission7update
'	Mission7FL1.timerenabled=1 : Mission7FL1.uservalue=7
	addletter(1) 
	mission7scoring=mission7scoring+50000
	scoring(mission7scoring)
	PlaySound "motorlauch", 0, .2*BackGlassVolumeDial, AudioPan(Kicker001), 0.05,0,0,1,AudioFade(Kicker001)
	mission(7)=0
	Mission7light.state=0
	Mission7light003.state=0
	Mission7light004.state=0
	Mission7light005.state=0
	Mission7light001.state=1
	Mission7light002.state=1
	Mission7light001.timerenabled=1
	mission7status=1
	MissionDone
End Sub


Sub Mission7light001_Timer
	Mission7light001.state=0
	Mission7light002.state=0
	Mission7light001.timerenabled=0
End Sub



'*** Mission 6
Sub startmission6
	mission(6)=1
	missionstatus(6)=0
	Mission6light.state=2
'	Mission6FL1.timerenabled=1 : Mission6FL1.uservalue=5
	Mission6Light003.state=2 
	Mission6Light004.state=2
 	Mission6Light005.state=2 
	Mission6light001.state=2
	Mission6light002.state=2
	Mission6light001.timerenabled=1
	LiSupply001.state=2 
	LiSupply007.state=2
	BumperFlasher1.visible=1
	SupplySeq1.play seqblinking,,10,33 
	If Not StopthingsRestartmissions Then PlaySound "motorlauch", 0, 0.08*BackGlassVolumeDial, AudioPan(sw7), 0.05,0,0,1,AudioFade(sw7)
	' numpers
End Sub

Dim mission6scoring,mission6status

Sub mission6update
'		Mission6FL1.timerenabled=1 : Mission6FL1.uservalue=7
		addletter(1) 
		mission6scoring=mission6scoring+50000
		scoring(mission6scoring)
		PlaySound "motorlauch", 0, .1*BackGlassVolumeDial, AudioPan(Kicker001), 0.05,0,0,1,AudioFade(Kicker001)
		mission(6)=0
		BumperFlasher1.visible=0

		LiSupply001.state=0
		LiSupply007.state=0
		SupplySeq1.play seqblinking,,10,33 

		Mission6light.state=0
		Mission6light003.state=0
		Mission6light004.state=0
		Mission6light005.state=0
		Mission6light001.state=1
		Mission6light002.state=1
		Mission6light001.timerenabled=1
		mission6status=1
		MissionDone
End Sub

Sub Mission6light001_Timer
	Mission6light001.state=0
	Mission6light002.state=0
	Mission6light001.timerenabled=0
End Sub




'*** Mission 5
Sub startmission5
	mission(5)=1
	missionstatus(5)=0
	Mission5light.state=2
'	Mission5FL1.timerenabled=1 : Mission5FL1.uservalue=5
	Mission5Light003.state=2
	Mission5Light004.state=2
	Mission5Light005.state=2
	Mission5light001.state=2
	Mission5light002.state=2
	Mission5light001.timerenabled=1
	LiSupply006.state=2 : SupplySeq6.play seqblinking,,10,33 
	LiSupply008.state=2 : SupplySeq8.play seqblinking,,10,33 
	If Not StopthingsRestartmissions Then PlaySound "motorlauch", 0, 0.08*BackGlassVolumeDial, AudioPan(sw7), 0.05,0,0,1,AudioFade(sw7)
	' left spinner right spinner
End Sub

Dim mission5scoring,mission5status


Sub mission5update
'		Mission5FL1.timerenabled=1 : Mission5FL1.uservalue=7
		addletter(1)
		mission5scoring=mission5scoring+25000
		scoring(mission5scoring)
		PlaySound "motorlauch", 0, .1*BackGlassVolumeDial, AudioPan(Kicker001), 0.05,0,0,1,AudioFade(Kicker001)
		mission(5)=0
		LiSupply006.state=0 : SupplySeq6.play seqblinking,,10,33 
		LiSupply008.state=0 : SupplySeq8.play seqblinking,,10,33 
		Mission5light.state=0
		Mission5Light003.state=0
		Mission5Light004.state=0
		Mission5Light005.state=0
		Mission5light001.state=2
		Mission5light002.state=2
		Mission5light001.timerenabled=1
		mission5status=1
		MissionDone
End Sub

Sub Mission5light001_Timer 
	Mission5light001.state=0 
	Mission5light002.state=0
	Mission5light001.timerenabled=0
End Sub


'*** Mission 4
Sub startmission4
	mission(4)=1
	missionstatus(4)=0
	Mission4light.state=2

	Mission4Light003.state=2
	Mission4Light004.state=2
	Mission4Light005.state=2
	Mission4light001.state=2
	Mission4light002.state=2
	Mission4light001.timerenabled=1
	LiSupply002.state=2 
	SupplySeq2.play seqblinking,,10,33 
	LiSupply004.state=2 
	SupplySeq4.play seqblinking,,10,33 

	If Not StopthingsRestartmissions Then PlaySound "motorlauch", 0, 0.08*BackGlassVolumeDial, AudioPan(sw7), 0.05,0,0,1,AudioFade(sw7)
	' leftrampscoring
	' rightrampscoring
End Sub

Dim mission4scoring,mission4status


Sub mission4update
		addletter(1)  
		mission4scoring=mission4scoring+25000
		scoring(mission4scoring)
		PlaySound "motorlauch", 0, .1*BackGlassVolumeDial, AudioPan(Kicker001), 0.05,0,0,1,AudioFade(Kicker001)
		mission(4)=0
		LiSupply002.state=0 
		SupplySeq2.play seqblinking,,10,33 
		LiSupply004.state=0 
		SupplySeq4.play seqblinking,,10,33 

		Mission4light.state=0
		Mission4light003.state=0
		Mission4light004.state=0
		Mission4light005.state=0
		Mission4light001.state=1
		Mission4light002.state=1
		Mission4light001.timerenabled=1
		mission4status=1
		MissionDone
End Sub
Sub Mission4light001_Timer : Mission4light001.state=0 : Mission4light002.state=0 : Mission4light001.timerenabled=0 : End Sub



'**** mission3 spacestation refuel / mistery ?
Sub startmission3
	mission(3)=1
	missionstatus(3)=0
	Mission3light.state=2

	Mission3Light003.state=2 
	Mission3Light004.state=2 
	Mission3Light005.state=2 
	Mission3light001.state=2
	Mission3Light002.state=2
	Mission3light001.timerenabled=1
	Lisupply005.state=2
	SupplySeq5.play seqblinking,,10,33  
	LiRefuel1.state=2
	If Not StopthingsRestartmissions Then PlaySound "motorlauch", 0, .1*BackGlassVolumeDial, AudioPan(sw7), 0.05,0,0,1,AudioFade(sw7)
End Sub




Sub Secretblast_Timer : Secretblast.state=0 : Secretblast.timerenabled=0 : End Sub
Sub lowbumperblast_Timer : lowbumperblast.state=0 : lowbumperblast.timerenabled=01 : End Sub
Sub Lockholeblast_Timer : lockholeblast.state=0 : lockholeblast.timerenabled=0 : End Sub
Sub Lockholeblast001_Timer : FlasherFlash002.visible=0 : lockholeblast001.state=0 : lockholeblast001.timerenabled=0 : End Sub
Sub LiSecret2FL_Timer : LiSecret2FL.visible=0 : LiSecret2FL.timerenabled=0 : End Sub


Dim mission3scoring,mission3status

Sub kicker001up_timer
	If Primitive058.Z > -5 Then kicker001up.enabled=0 : Exit Sub
	Primitive058.Z=Primitive058.Z+1
End Sub

Sub kicker001arm_timer
	If Primitive057.Y<517 Then kicker001arm.enabled=0
	Primitive057.Y=Primitive057.Y-1
	Primitive057.X=Primitive057.X+1
End Sub

Sub kicker007up_timer
	If Primitive068.Z > -5 Then kicker007up.enabled=0 : Exit Sub
	Primitive068.Z=Primitive068.Z+1
End Sub

Sub kicker007arm_timer
	If Primitive073.Y<351 Then kicker007arm.enabled=0
	Primitive073.Y=Primitive073.Y-1
	Primitive073.X=Primitive073.X-0.361
End Sub


Sub Kicker001_Hit

	PlaySound SoundFX("fx_kicker_enter",DOFContactors), 0,.47*VolumeDial,AudioPan(Kicker002),0.35,0,0,1,AudioFade(Kicker002)

	If mission(3)=1 Then
		addletter(1) 
		mission3scoring=mission3scoring+25000
		scoring(mission3scoring)
		PlaySound "motorlauch", 0, .25*BackGlassVolumeDial, AudioPan(Kicker001), 0.05,0,0,1,AudioFade(Kicker001)
		Kicker001.timerenabled=1
		mission(3)=0
		attractrefuel=250  '  turn alittle more after done
		Mission3light.state=0
		Mission3Light003.state=0
		Mission3Light004.state=0
		Mission3Light005.state=0
		Mission3light001.state=1
		Mission3light002.state=1
		Mission3light001.timerenabled=1
		Lisupply005.state=0
		SupplySeq5.play seqblinking,,10,33 
		LiRefuel1.state=0
		mission3status=1
		MissionDone
	Else
		FuelEmpty.enabled=1	
		PlaySound "rebound1", 0, .6*BackGlassVolumeDial, AudioPan(Kicker001), 0.05,0,0,1,AudioFade(Kicker001)
	End If
End Sub

Sub FuelEmpty_Timer
	PlaySound SoundFX("fx_kicker",DOFContactors), 0, 0.34*VolumeDial, AudioPan(Kicker001), 0.05,0,0,1,AudioFade(Kicker001)
	Kicker001.kick 240,4
	Primitive057.Y = 540 
	Primitive057.X = 642
	kicker001arm.enabled=1
	Kicker001up.enabled=1
	primitive058.Z=-24
	scoring(250)
	FuelEmpty.enabled=0
End Sub

Sub kicker001_Timer
	Kicker001.kick 240,4
	scoring(250)
	Kicker001.timerenabled=0
	PlaySound SoundFX("fx_kicker",DOFContactors), 0, 0.34*VolumeDial, AudioPan(Kicker001), 0.05,0,0,1,AudioFade(Kicker001)
	Primitive057.Y = 540 
	Primitive057.X = 642
	kicker001arm.enabled=1
	Kicker001up.enabled=1
	primitive058.Z=-24
End Sub

Sub Mission3light001_Timer
	Mission3light001.state=0
	Mission3Light002.state=0
	Mission3light001.timerenabled=0
End Sub

	'****mission 2 hideout
Sub startmission2
	mission(2)=1
	missionstatus(2)=0
	Mission2light.state=2 
	Mission2Light003.state=2
	Mission2Light004.state=2
	Mission2Light005.state=2
	Mission2light001.state=2
	Mission2light002.state=2
	Mission2light001.timerenabled=1
	LiSecret1.intensity=75
	LiSecret2.state=2 :  LiSecret2FL.visible=1 : LiSecret2FL.timerenabled=1
	LiSecret3.state=2
	Lisupply009.state=2 : SupplySeq9.play seqblinking,,10,33 

	If Not StopthingsRestartmissions Then PlaySound "domdomdom", 0, 0.27*BackGlassVolumeDial, AudioPan(sw7), 0.05,0,0,1,AudioFade(sw7)
End Sub

Dim mission2scoring,mission2status

Sub Kicker002_Hit
	PlaySound SoundFX("fx_kicker_enter",DOFContactors), 0,.45*VolumeDial,AudioPan(Kicker002),0,0,0,1,AudioFade(Kicker002)

    ManualLightSequencer.enabled=1 : MSeqCounter=190 : MSeqBigFL=20 
	If mission(2)=1 Then
		PlaySound "domdomdom", 0, .6*BackGlassVolumeDial, AudioPan(Kicker002), 0.05,0,0,1,AudioFade(Kicker002)
		addletter(1)
		mission2scoring=mission2scoring+50000
		scoring(mission2scoring)
		mission2status=1
		MissionDone
		Kicker002.timerenabled=1
		mission(2)=0
		Mission2light.state=0
		Mission2Light003.state=0
		Mission2Light004.state=0
		Mission2Light005.state=0
		Mission2light001.state=1
		Mission2light002.state=1

		Mission2light001.timerenabled=1
		LiSecret1.intensity=37
		LiSecret2.state=0 : :  LiSecret2FL.visible=1 : LiSecret2FL.timerenabled=1
		LiSecret3.state=0
		Lisupply009.state=0 : SupplySeq2.play seqblinking,,10,33 

		LiSecret2.timerenabled=1
	Else
		LiSecret1.timerenabled=1
	End If
End Sub

Sub LiSecret1_Timer
	LiSecret1.timerenabled=0
	Kicker002.kick 310,7 :	scoring(500) : PlaySound "notcompute",0,BackGlassVolumeDial : PlaySound SoundFX("fx_kicker",DOFContactors), 0, 0.34*VolumeDial, AudioPan(Kicker002), 0.05,0,0,1,AudioFade(Kicker002)
End Sub

Sub LiSecret2_Timer
	PlaySound SoundFX("fx_solenoidon3",DOFContactors), 0, 0.4*VolumeDial, AudioPan(Kicker002), 0.05,0,0,1,AudioFade(Kicker002)
	Secretblast.state=1 : Secretblast.timerenabled=1
	DOF 139,2 : 'Debug.print "DOF 139, 2"  'apophis
	ObjLevel(11) = 1 : FlasherFlash11_Timer
End Sub

Sub kicker002_Timer 
	Kicker002.kick 310,7
	scoring(10)
	Kicker002.timerenabled=0
	PlaySound SoundFX("fx_kicker",DOFContactors), 0, 0.4*VolumeDial, AudioPan(Kicker002), 0.05,0,0,1,AudioFade(Kicker002)
	LiSecret2.timerenabled=0
End Sub

Sub Mission2light001_Timer : Mission2light001.state=0 : Mission2light002.state=0 : Mission2light001.timerenabled=0 : End Sub

Sub CreditMissionDMD_Timer
	If NOT UMainDMD.isrendering Then 
		If mission(1)=1 Then
			creditsmissionstatus=1
			FormatScore(12000-(missionstatus(1)*1000)) : UMainDMD.DisplayScene00ex "coins1.wmv", "GET CREDITS", 0, 12, FlexScore & " MISSING", 15, 1, 10, 1500, 8
			
			CreditMissionDMD.enabled=0
		Else
			CreditMissionDMD.enabled=0
		End If
	End If
End Sub

Dim mission1scoring,mission1status,creditsmissionstatus
Sub mission1update
	If mission(1)=1 Then
		missionstatus(1)=missionstatus(1)+1

		if missionstatus(1)=12 Then
			creditsmissionstatus=0
			lisupply003.state=0
			lisupply010.state=0
			
			SupplySeq3.play seqblinking,,10,33 

			Mission1light.state=0
			Mission1light003.state=0
			Mission1light004.state=0
			Mission1light005.state=0
			Mission1light001.state=1
			Mission1light002.state=1
			Mission1light001.timerenabled=1
			mission(1)=0
			addletter(1)
			mission1scoring=mission1scoring+50000
			scoring(mission1scoring)
			mission1status=1
			MissionDone
			PlaySound "holocomp", 0, .7*BackGlassVolumeDial, AudioPan(Mission1light), 0.05,0,0,1,AudioFade(Mission1light)

			liTargetTop(1)=0
			liTargetTop(2)=0
			liTargetTop(3)=0
			liTargetTop(4)=0
			liTargetTop(5)=0
			liTargetTop(6)=0
			liTargetTop(7)=0
			liTargetTop(8)=0
			liTargetTop(9)=0
			liTargetTop(10)=0

		Else
			
			If FlexDMD Then CreditMissionDMD.enabled=1
		End If
	End If
End Sub


Dim LiTargetTop(10)
Dim LitargetTopCount
Dim targettop : targettop=array(0,dt001,dt002,dt003,dt004,dt005,dt006,dt007,dt008,dt009,dt010)
Sub TargetTopBlink
		Dim tmp, x
		LitargetTopCount=LitargetTopCount+1
		If LitargetTopCount > 50 Then LitargetTopCount = 0
										tmp=6
		If LitargetTopCount < 13 Then	tmp=LitargetTopCount/2
		If LitargetTopCount > 25 Then	tmp=(37-LitargetTopCount)/2
		If LitargetTopCount > 37 Then	tmp = 0
		For x = 1 to 10
			If LiTargetTop(x) = 0 And LitargetTopCount=0 Then LiTargetTop(x) = 1 ' off
			If LiTargetTop(x) <> 1 Then
			If tmp> 3 Then TargetTop(x).image ="1000creditstest2" Else TargetTop(x).image="1000creditstest"				
			TargetTop(x).blenddisablelighting = tmp/15' + 0.24
			End If
		Next
End Sub



Sub startmission1
	If mission(1)=0 Then
		LiSupply003.state=2 
		LiSupply010.state=2 
		
		SupplySeq3.play seqblinking,,10,33  
		mission(1)=1
		missionstatus(1)=0
		If Not StopthingsRestartmissions Then PlaySound "cloakdevice", 0, .5*BackGlassVolumeDial, AudioPan(sw7), 0.05,0,0,1,AudioFade(sw7)
		Mission1light.state=2

		Mission1light003.state=2 
		Mission1light004.state=2 
		Mission1light005.state=2 
		Mission1light001.state=2
		Mission1light002.state=2
		Mission1light001.timerenabled=1

liTargetTop(1)=2
liTargetTop(2)=2
liTargetTop(3)=2
liTargetTop(4)=2
liTargetTop(5)=2
liTargetTop(6)=2
liTargetTop(7)=2
liTargetTop(8)=2
liTargetTop(9)=2
liTargetTop(10)=2

'		if dt001.isdropped=false then TargetTop(1)=2
'		if dt002.isdropped=false then TargetTop(2)=2
'		if dt003.isdropped=false then TargetTop(3)=2
'		if dt004.isdropped=false then TargetTop(4)=2
'		if dt005.isdropped=false then TargetTop(5)=2
'		if dt006.isdropped=false then TargetTop(6)=2
'		if dt007.isdropped=false then TargetTop(7)=2
'		if dt008.isdropped=false then TargetTop(8)=2
'		if dt009.isdropped=false then TargetTop(9)=2
'		if dt010.isdropped=false then TargetTop(10)=2
	End If
End Sub

Sub Mission1light001_Timer 
	Mission1light001.state=0 
	Mission1light002.state=0
	Mission1light001.timerenabled=0
End Sub

Dim enterflashing
enterflashing=0

Sub timer7_Timer
	enterflashing=enterflashing+1
	If enterflashing>5 and enterflashing<12 Then
		lockholeblast001.state=1 : lockholeblast001.timerenabled=1 : FlasherFlash002.visible=1
	DOF 140,2 : 'Debug.print "DOF 140, 2"  'apophis
		PlaySound SoundFX("fx_solenoidon3",DOFContactors), 0, 0.4*VolumeDial, AudioPan(sw7), 0.05,0,0,1,AudioFade(sw7)
	End If
	If enterflashing>11 Then
		enterflashing=0

'		PlaySound SoundFX("danger",DOFContactors), 0, 0.5, AudioPan(sw7), 0.05,0,0,1,AudioFade(sw7)
		PlaySound SoundFX("fx_slingshot",DOFContactors), 0, .7*VolumeDial, AudioPan(sw7), 0.05,0,0,1,AudioFade(sw7)
		Objlevel(1) = 1 : FlasherFlash1_Timer
		Objlevel(2) = 1 : FlasherFlash2_Timer
		timer7.Enabled = 0
		sw7.kick 170,2

		Primitive073.Y = 395 
		Primitive073.X = 316
		kicker007arm.enabled=1
		Kicker007up.enabled=1
		primitive068.Z=-24

		PlaySound SoundFX("fx_kicker",DOFContactors), 0,.7*VolumeDial,AudioPan(BallRelease),0.25,0,0,1,AudioFade(BallRelease)
		scoring(500)
	End If
End Sub

Sub droptargetup_Timer

		Raise_DT 1

		Raise_DT 2

		droptargetup.Enabled = 0
		wormhole=0
		DT01down=0
		DT02down=0
end Sub

'**** other targets at top
Sub dt001_Hit 
	DOF 124,2
'Debug.print "DOF 124, 2"  'apophis
	liTargetTop(1)=0
	DroptargetsScoring=DroptargetsScoring+30
	DroptargetsCounter=DroptargetsCounter+1
	scoring(DroptargetsScoring)
	dt001.timerenabled=1
	PlaySound SoundFX("bell131",DOFContactors), 0, .18*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
	mission1update
End Sub

Sub dt002_Hit
	DOF 124,2
'Debug.print "DOF 124, 2"  'apophis
	liTargetTop(2)=0
	DroptargetsScoring=DroptargetsScoring+30
	DroptargetsCounter=DroptargetsCounter+1
	scoring(DroptargetsScoring) 
	dt002.timerenabled=1
	PlaySound SoundFX("bell131",DOFContactors), 0, .18*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
	mission1update
End Sub

Sub dt003_Hit 
	DOF 124,2
'Debug.print "DOF 124, 2"  'apophis
	liTargetTop(3)=0
	DroptargetsScoring=DroptargetsScoring+30
	DroptargetsCounter=DroptargetsCounter+1 
	scoring(DroptargetsScoring)
	dt003.timerenabled=1
	PlaySound SoundFX("bell131",DOFContactors), 0, .18*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) 
	mission1update 
End Sub

Sub dt004_Hit
	DOF 124,2
'Debug.print "DOF 124, 2"  'apophis
	liTargetTop(4)=0
	DroptargetsScoring=DroptargetsScoring+30
	DroptargetsCounter=DroptargetsCounter+1
	scoring(DroptargetsScoring)
	dt004.timerenabled=1
	PlaySound SoundFX("bell131",DOFContactors), 0, .18*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
	mission1update
End Sub

Sub dt005_Hit
	DOF 124,2
'Debug.print "DOF 124, 2"  'apophis
	liTargetTop(5)=0
	DroptargetsScoring=DroptargetsScoring+30
	DroptargetsCounter=DroptargetsCounter+1
	scoring(DroptargetsScoring) 
	dt005.timerenabled=1
	PlaySound SoundFX("bell131",DOFContactors), 0, .18*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
	mission1update
End Sub

Sub dt006_Hit
	DOF 124,2
'Debug.print "DOF 124, 2"  'apophis
	liTargetTop(6)=0
	DroptargetsScoring=DroptargetsScoring+30
	DroptargetsCounter=DroptargetsCounter+1 
	scoring(DroptargetsScoring) 
	dt006.timerenabled=1
	PlaySound SoundFX("bell131",DOFContactors), 0, .18*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) 
	mission1update 
End Sub 

Sub dt007_Hit
	DOF 124,2
'Debug.print "DOF 124, 2"  'apophis
	liTargetTop(7)=0
	DroptargetsScoring=DroptargetsScoring+30
	DroptargetsCounter=DroptargetsCounter+1
	scoring(DroptargetsScoring)
	dt007.timerenabled=1
	PlaySound SoundFX("bell131",DOFContactors), 0, .18*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) 
	mission1update 
End Sub

Sub dt008_Hit
	DOF 124,2
'Debug.print "DOF 124, 2"  'apophis
	liTargetTop(8)=0
	DroptargetsScoring=DroptargetsScoring+30
	DroptargetsCounter=DroptargetsCounter+1
	scoring(DroptargetsScoring)
	dt008.timerenabled=1
	PlaySound SoundFX("bell131",DOFContactors), 0, .18*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
	mission1update
End Sub

Sub dt009_Hit
	DOF 124,2
'Debug.print "DOF 124, 2"  'apophis
	liTargetTop(9)=0
	DroptargetsScoring=DroptargetsScoring+30
	DroptargetsCounter=DroptargetsCounter+1
	scoring(DroptargetsScoring)
	dt009.timerenabled=1
	PlaySound SoundFX("bell131",DOFContactors), 0, .18*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) 
	mission1update 
End Sub



Sub dt010_Hit
	DOF 124,2
'Debug.print "DOF 124, 2"  'apophis
	liTargetTop(10)=0
	DroptargetsScoring=DroptargetsScoring+30
	DroptargetsCounter=DroptargetsCounter+1
	scoring(DroptargetsScoring)
	dt010.timerenabled=1
	PlaySound SoundFX("bell131",DOFContactors), 0, .18*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
	mission1update 
End Sub

Sub dt001_Timer 
	dt001.timerenabled=0
	dt001.IsDropped = False
	if mission(1)=1 Then liTargetTop(1)=2
	PlaySound SoundFX("fx_resetdrop",DOFContactors), 0, 0.2*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
End Sub

Sub dt002_Timer
	dt002.timerenabled=0
	dt002.IsDropped = False 
	if mission(1)=1 Then liTargetTop(2)=2
	PlaySound SoundFX("fx_resetdrop",DOFContactors), 0, 0.2*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) 
End Sub

Sub dt003_Timer
	dt003.timerenabled=0
	dt003.IsDropped = False
	if mission(1)=1 Then liTargetTop(3)=2
	PlaySound SoundFX("fx_resetdrop",DOFContactors), 0, 0.2*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
End Sub

Sub dt004_Timer
	dt004.timerenabled=0
	dt004.IsDropped = False
	if mission(1)=1 Then liTargetTop(4)=2
	PlaySound SoundFX("fx_resetdrop",DOFContactors), 0, 0.23*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
End Sub

Sub dt005_Timer
	dt005.timerenabled=0
	dt005.IsDropped = False
	if mission(1)=1 Then liTargetTop(5)=2
	PlaySound SoundFX("fx_resetdrop",DOFContactors), 0, 0.23*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
End Sub

Sub dt006_Timer
	dt006.timerenabled=0
	dt006.IsDropped = False
	if mission(1)=1 Then liTargetTop(6)=2
	PlaySound SoundFX("fx_resetdrop",DOFContactors), 0, 0.23*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
End Sub

Sub dt007_Timer 
	dt007.timerenabled=0
	dt007.IsDropped = False
	if mission(1)=1 Then liTargetTop(7)=2
	PlaySound SoundFX("fx_resetdrop",DOFContactors), 0, 0.23*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
End Sub

Sub dt008_Timer
	dt008.timerenabled=0
	dt008.IsDropped = False
	if mission(1)=1 Then liTargetTop(8)=2
	PlaySound SoundFX("fx_resetdrop",DOFContactors), 0, 0.23*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) 
End Sub

Sub dt009_Timer 
	dt009.timerenabled=0
	dt009.IsDropped = False
	if mission(1)=1 Then liTargetTop(9)=2
	PlaySound SoundFX("fx_resetdrop",DOFContactors), 0, 0.23*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) 
End Sub

Sub dt010_Timer
	dt010.timerenabled=0
	dt010.IsDropped = False
	if mission(1)=1 Then liTargetTop(10)=2
	PlaySound SoundFX("fx_resetdrop",DOFContactors), 0, 0.23*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
End Sub


'**********
' outlanes
'**********
dim kickback,kbstatus
kickback=0 : kbstatus=0

Sub leftoutlane_Hit  

	PlaySound SoundFX("fx_metalwire",DOFContactors), 0, 0.4*VolumeDial, AudioPan(leftoutlane), 0.05,0,0,1,AudioFade(leftoutlane)

	If Tilted Then Exit Sub

	If li21.State>0 Then 
		scoring(2220)
		PlaySound SoundFX("fx_plunger",DOFContactors), 0, .7*VolumeDial, AudioPan(leftoutlane), 0.05,0,0,1,AudioFade(leftoutlane)
		kickback=1
		plunger1.Fire
		DOF 144,2 : 'Debug.print "DOF 144, 2"  'apophis
		li21.state=0
		KickbackFL.visible=1 : KickbackFL.timerenabled=1
		kbstatus=1
	Else
		scoring(250000)
		LiSpesialLeft.state=2
		LiSpesialLeft.timerenabled=1
		LiSpesialLeftFL.visible=1 : LiSpesialLeftFL.timerenabled=1
		PlaySound "off", 0, .7*BackGlassVolumeDial, AudioPan(leftoutlane), 0.05,0,0,1,AudioFade(leftoutlane)
	End If
End Sub
Sub li21_Timer : li21.state=1 : KickbackFL.visible=1 : KickbackFL.timerenabled=1 : li21.timerenabled=0 : PlaySound SoundFX("fx_apron",DOFContactors), 0, 0.33*VolumeDial, AudioPan(li21), 0.05,0,0,1,AudioFade(li21) : End sub

Sub leftoutlane_Unhit
	LiSpesialLeft.state=1
	LiSpesialLeftFL.visible=1 : LiSpesialLeftFL.timerenabled=1
	kickback=0
	kbstatus=0
	scoring(1000)
	If lireplay.state>0 Or LiReplay2.state>0 Then Li21.state=1 : LiSpesialLeft.state=0 : 
	plunger1.pullback
	PlaySound SoundFX("fx_plungerpull",DOFContactors),0,.81*VolumeDial,AudioPan(Plunger1),0.25,0,0,1,AudioFade(Plunger1)
End Sub	

Sub plunger1_Timer
	If kickback=1 Then
		If kbstatus=2 Then
			kbstatus=3
			Plunger1.Fire
DOF 144,2 : 'Debug.print "DOF 144, 2"  'apophis
			PlaySound SoundFX("fx_plunger",DOFContactors), 0, .7*VolumeDial, AudioPan(leftoutlane), 0.05,0,0,1,AudioFade(leftoutlane)
		End If
		If kbstatus=1 Then:Plunger1.Pullback:kbstatus=2 : PlaySound "fx_plungerpull",0,.81*VolumeDial,AudioPan(Plunger1),0.25,0,0,1,AudioFade(Plunger1)
		If kbstatus=3 Then kbstatus=1: End If
	End If
End Sub

'* rightoutlane
Dim extraextraball
Sub rightoutlane_Hit
	PlaySound SoundFX("fx_metalwire",DOFContactors), 0, 0.4*VolumeDial, AudioPan(rightoutlane), 0.05,0,0,1,AudioFade(rightoutlane)
	If LiSpecialRight.state=2 Then
		If FlyAgain3.state<2 Then
			displaybuzy=0
			extraballstatus=1
			DOF 215,2
'Debug.print "DOF 215, 2"  'apophis
			TotalExtraBalls=TotalExtraBalls+1
			FlyAgain1.state=2 : pflyagain.blenddisablelighting=5
			FlyAgain2.state=2
			FlyAgain3.state=2
			LiSpecialRight.state=0
			LiSpecialRight2.state=1
			LiSpecialrightFL.visible=1 : LiSpecialrightFL.timerenabled=1 
		Else
			displaybuzy=0
			extraballstatus=1
			DOF 215,2
'Debug.print "DOF 215, 2"  'apophis
			extraextraball=extraextraball+1
			TotalExtraBalls=TotalExtraBalls+1
			LiSpecialRight.state=0
			LiSpecialRight2.state=1
			LiSpecialrightFL.visible=1 : LiSpecialrightFL.timerenabled=1
		End If
	End If

	If  LiSpecialRight2.state=1 Then
		scoring(250000)
		LiSpecialRight2.state=2
		LiSpecialright2FL.visible=1 : LiSpecialright2FL.timerenabled=1
		LiSpecialRight2.timerenabled=1
	Else
		scoring(50000)
	End If
	PlaySound "off", 0, .7*BackGlassVolumeDial, AudioPan(rightoutlane), 0.05,0,0,1,AudioFade(rightoutlane)
End Sub

Sub LiSpecialRight2_Timer
	LiSpecialRight2.state=1
	LiSpecialright2FL.visible=1 : LiSpecialright2FL.timerenabled=1
	LiSpecialRight2.timerenabled=0
End Sub

Sub LiSpesialLeft_Timer
	If Li21.state=0 Then 
		LiSpesialLeft.state=1
		LiSpesialLeftFL.visible=1 : LiSpesialLeftFL.timerenabled=1
	Else
		LiSpesialLeft.state=0
		LiSpesialLeftFL.visible=1 : LiSpesialLeftFL.timerenabled=1
	End If
	LiSpesialLeft.timerenabled=0
End Sub


Sub leftinlane_Hit
	If Lileftinlane.uservalue=1 Then
		Lileftinlane.uservalue=2
		scoring(50000)
		Lileftinlane.fadespeeddown=5
		Lileftinlane.fadespeedup=5
		Lileftinlane.blinkpattern=10
		LiLeftinlane.timerenabled=1
		LiLeftinlaneFL.visible=1 : LiLeftinlaneFL.timerenabled=1
		PlaySound "lit10k",0, 0.17*BackGlassVolumeDial
	Else
		LiRightinlane_Timer
		Lileftinlane.state=2 : 	Lileftinlane.uservalue=1	
		LiLeftinlaneFL.visible=1 :	LiLeftinlaneFL.timerenabled=1
		scoring(80)
	End If
	PlaySound SoundFX("fx_metalwire",DOFContactors), 0, 0.3*VolumeDial, AudioPan(leftinlane), 0.05,0,0,1,AudioFade(leftinlane)
End Sub

Sub Rightinlane_Hit
	If Lirightinlane.uservalue=1 Then
		Lirightinlane.uservalue=2
		scoring(50000)
		LiRightinlane.fadespeeddown=5
		LiRightinlane.fadespeedup=5
		LiRightinlane.blinkpattern=10
		LiRightinlane.timerenabled=1
		LiRightinlaneFL.visible=1 : LiRightinlaneFL.timerenabled=1
		PlaySound "lit10k",0, 0.17*BackGlassVolumeDial
	Else
		Lileftinlane_Timer
		LiRightinlane.state=2 : LiRightinlane.uservalue=1
		LiRightinlaneFL.visible=1 :	LiRightinlaneFL.timerenabled=1

		scoring(80)
	End If

	PlaySound SoundFX("fx_metalwire",DOFContactors), 0, 0.3*VolumeDial, AudioPan(rightinlane), 0.05,0,0,1,AudioFade(rightinlane)
End Sub

Sub LiRightinlaneFL_Timer : LiRightinlaneFL.visible=0 : LiRightinlaneFL.timerenabled=0 : End Sub
Sub LiLeftinlaneFL_Timer : LiLeftinlaneFL.visible=0 : LiLeftinlaneFL.timerenabled=0 : End Sub


Sub Lileftinlane_Timer
	LiLeftinlaneFL.visible=1 : LiLeftinlaneFL.timerenabled=1
	Lileftinlane.state=0
	Lileftinlane.uservalue=0
	Lileftinlane.fadespeeddown=0.51
	Lileftinlane.fadespeedup=0.5
	Lileftinlane.blinkpattern=100110
	Lileftinlane.timerenabled=0
End Sub

Sub LiRightinlane_Timer
	LiRightinlaneFL.visible=1 : LiRightinlaneFL.timerenabled=1
	Lirightinlane.state=0
	Lirightinlane.uservalue=0
	LiRightinlane.fadespeeddown=0.51
	LiRightinlane.fadespeedup=0.5
	LiRightinlane.blinkpattern=100110
	Lirightinlane.timerenabled=0
End Sub



'************************************
' Add score and bonus
'************************************
Dim knockeronce
Sub scoring(Addtoscore)

	If Tilted Then exit Sub

	If doubletripple>0 Then P1scorebonus=2 : else : P1scorebonus=1 : End If
	If doubletripple=3 Then P1scorebonus=3 : End If
	P1score=P1score+(Addtoscore*P1scorebonus)

	If Lidouble2.state>0 Then doubleblinker.stopplay : doubleblinker.Play SeqBlinking,,1,30  
	If LiTripple2.state>0 Then trippleblinker.stopplay : trippleblinker.Play SeqBlinking,,1,30  
	LightSeq004.StopPlay
	LightSeq004.Play SeqBlinking,,1,50


	If knockeronce=0 Then
		If P1score >= ReplayGoals And GameOverStatus=0 Then
			trippleknocker.enabled=1
			knockeronce = 1
			Credits = Credits + 1

			ReplayGoals = ReplayGoals + 25000000

			ReplayGoalDown=0
		End If
	End If

	i = (Int(addtoscore/1000))*70
	If i>20000 then i=20000
	JackPotScoring=JackPotScoring + i	
	If WizardLevel < 1 And JackPotScoring>2000000 Then JackPotScoring=2000000
'	debug.print "Added=" & i & "  JP=" & jackpotscoring & "  WiZL=" & WizardLevel ' fixing


	PlaySound SoundFX("fx_motor",DOFContactors), 0, 0.4*VolumeDial, AudioPan(LiReplay2), 0.05,0,0,1,AudioFade(LiReplay2)
	ObjLevel(8) = 1 : FlasherFlash8_Timer
	ObjLevel(9) = 1 : FlasherFlash9_Timer
	PlaySound SoundFX("fx_solenoidon",DOFContactors), 0, .7*VolumeDial, AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger)
	If HideDesktop=1 Then controller.B2SSetScorePlayer1 P1score 

End Sub


'************************************
'*** top left gate 

Sub opengate_Hit
	Gate002.collidable=0
	' SCORING right lane
	If lasthit=4 Then 
		scoring(1000) 
		addletter(1)
		PlaySound "phasertype2", 0, 0.12*BackGlassVolumeDial, AudioPan(opengate), 0.05,0,0,1,AudioFade(opengate) 
	End If
	lasthit=0
End Sub

Sub closegate_Hit
	Light042.intensity=350   'triggerstarlightblink
	Light042.timerenabled=1 
	If skillshot=1 And Not Tilted Then 
		
		'If Not TournamentMode Then
		liReplay.state=2 : lireplay.blinkinterval=200 : LiReplay.timerenabled=1
		lireplay001.state = 2 : lireplay001.blinkinterval=200 : pLireplay.blenddisablelighting=5

		LiSkillshot1.state=0 
		LiSkillshot2.state=0
		Light003.state=0
		Light051.state=2
		superskillshot.enabled=1
		lipassage.state=2
	If bossactive=1 Then DOF 118,1
	End If
	
	skillshot=0
	Plungerispulled=0
	Plunger001.timerenabled=0
	shoottheballstatus=0 
	Gate002.collidable=1 
	' SCOIRING leftlane

	If lasthit=3 Then 
		scoring(1000)
		addletter(1)
		PlaySound "phasertype2", 0, 0.12*BackGlassVolumeDial, AudioPan(closegate), 0.05,0,0,1,AudioFade(closegate)
	End If
End Sub

Sub superskillshot_Timer
	superskillshot.enabled=0
	LiPassage.state=0
End Sub

'sound hitting gates
Sub Gate001_Hit
	PlaySound SoundFX("fx_gate",DOFContactors), 0, VolumeDial, AudioPan(Gate001), 0.05,0,0,1,AudioFade(Gate001)
End Sub
Sub Gate002_Hit
	PlaySound SoundFX("fx_gate",DOFContactors), 0, VolumeDial, AudioPan(Gate002), 0.05,0,0,1,AudioFade(Gate002)
End Sub
Sub Gate003_Hit
	PlaySound SoundFX("fx_gate",DOFContactors), 0, VolumeDial, AudioPan(Gate002), 0.05,0,0,1,AudioFade(Gate002)
End Sub
Sub Gate004_Hit
	PlaySound SoundFX("fx_gate",DOFContactors), 0, VolumeDial, AudioPan(Gate002), 0.05,0,0,1,AudioFade(Gate002)
End Sub
Sub Gate005_Hit
	PlaySound SoundFX("fx_gate",DOFContactors), 0, VolumeDial, AudioPan(Gate002), 0.05,0,0,1,AudioFade(Gate002)
End Sub

'******************
'*** SLINGSHOTS ***
'******************
Dim LStep, RStep
Sub LeftSlingShot_Slingshot

	LS.VelocityCorrect(ActiveBall)

	LightUnderDisp1.state=2
	LightUnderDisp2.state=2
	LightUnderDisp2.timerenabled=True
	Objlevel(3) = 1 : FlasherFlash3_Timer : 	DOF 135,2 : 'Debug.print "DOF 135, 2" ' blue left  'apophis
	DOF 103,2
'Debug.print "DOF 103, 2"  'apophis
	Light037_2.state=1 : Light037_2.timerenabled=1
	If bonustime.enabled=0 And LockedReleseGO.enabled=0 And ReleaseTimer.enabled=0 Then
		table1.ColorGradeImage = "LUT0"
	End If
	LiRightinlane.state=2 : Lileftinlane.state=0
	LiRightinlaneFL.visible=1 : LiRightinlaneFL.timerenabled=1
	LiRightinlane.uservalue=1 : LiLeftinlane.uservalue=0
	Flasher001_Timer

    PlaySoundAt SoundFX("Sling_L" & int(rnd(1)*10)+1 , DOFContactors), Lemk

	scoring(250)

    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	Objlevel(1) = 1 : FlasherFlash1_Timer
		PlaySound SoundFX("fx_solenoidon",DOFContactors), 0, .81*VolumeDial, AudioPan(skillshottrigger), 0.12,0,0,1,AudioFade(skillshottrigger)
	
			' 1=green 2=red 3=blue
	Select Case BorderColor
		Case 1:	Light041.colorfull = RGB(150,2,2) :	Light041.color = RGB(255,2,2) : Wall138.sidematerial = "Plastic RedB"
		Case 2: Light041.colorfull = RGB(2,150,2) : Light041.color = RGB(2,255,2) : Wall138.sidematerial = "Plastic Green" 
		Case 3: Light041.colorfull = RGB(150,2,2) :	Light040.color = RGB(255,2,2) : Wall138.sidematerial = "Plastic RedB"
		Case 4: Light041.colorfull = RGB(150,2,2) :	Light040.color = RGB(255,2,2) : Wall138.sidematerial = "Plastic RedB"
	End Select
	Light041.timerenabled=1
End Sub


Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14 : Primitive054.transZ=-14 : primitive054.RotX=70
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2 : Primitive054.transZ=-4 : primitive054.RotX=60
        Case 3:LeftSLing2.Visible = 0:Lemk.RotX = -10:LeftSlingShot.TimerEnabled = 0: Primitive054.transZ=0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot

	RS.VelocityCorrect(ActiveBall)

	LightUnderDisp1.state=2
	LightUnderDisp2.state=2
	LightUnderDisp2.timerenabled=True
	Objlevel(3) = 1 : FlasherFlash3_Timer : 	DOF 135,2 : 'Debug.print "DOF 135, 2" ' blue left  'apophis
	DOF 104,2
'Debug.print "DOF 104, 2"  'apophis
	Light034_2.state=1 : Light034_2.timerenabled=1
	If bonustime.enabled=0 And LockedReleseGO.enabled=0 And ReleaseTimer.enabled=0 Then
		table1.ColorGradeImage = "LUT0"
	End If

	LiRightinlane.state=0 : Lileftinlane.state=2
	LiRightinlane.uservalue=0 : LiLeftinlane.uservalue=1
	Flasher001_Timer

    PlaySoundAt SoundFX("Sling_R" & int(rnd(1)*8)+1, DOFContactors), Remk
	scoring(250)
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	Objlevel(2) = 1 : FlasherFlash2_Timer
		PlaySound SoundFX("fx_solenoidon",DOFContactors), 0, .7*VolumeDial, AudioPan(skillshottrigger), 0.15,0,0,1,AudioFade(skillshottrigger)
				' 1=green 2=red 3=blue
	Select Case BorderColor
		Case 1:	Light040.colorfull = RGB(150,2,2) :	Light040.color = RGB(255,2,2) : Wall079.sidematerial = "Plastic RedB"
		Case 2: Light040.colorfull = RGB(2,150,2) : Light040.color = RGB(2,255,2) : Wall079.sidematerial = "Plastic Green"
		Case 3: Light040.colorfull = RGB(150,2,2) :	Light040.color = RGB(255,2,2) : Wall079.sidematerial = "Plastic RedB"
		Case 4: Light040.colorfull = RGB(150,2,2) :	Light040.color = RGB(255,2,2) : Wall079.sidematerial = "Plastic RedB"
	End Select
	Light040.timerenabled=1

End Sub

Sub Light034_2_Timer
	If bonustime.enabled=0 And LockedReleseGO.enabled=0 And ReleaseTimer.enabled=0 Then
		lutcounter=8 : luttarget=2
		table1.ColorGradeImage = "LUT8" 
	End If
	Light034_2.state=0 : Light034_2.timerenabled=0 
End Sub



Sub Light037_2_Timer 
	If bonustime.enabled=0 And LockedReleseGO.enabled=0 And ReleaseTimer.enabled=0 Then
		lutcounter=8 : luttarget=2
		table1.ColorGradeImage = "LUT8" 
	End If
Light037_2.state=0 : Light037_2.timerenabled=0 
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14 : Primitive053.transZ=14 :  primitive053.RotX=70
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2 : Primitive053.transZ=4 :  primitive053.RotX=60
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0 : Primitive053.transZ=0
    End Select
    RStep = RStep + 1
End Sub


Sub RedBorders
wall138.sidematerial = "Plastic RedB"
wall079.sidematerial = "Plastic RedB"
Wall080.sidematerial = "Plastic RedB"
wall077.sidematerial = "Plastic RedB"
wall078.sidematerial = "Plastic RedB"
wall020.sidematerial = "Plastic RedB"
wall022.sidematerial = "Plastic RedB"
wall086.sidematerial = "Plastic RedB"
wall080.sidematerial = "Plastic RedB"
	TopFlasher1.color = RGB(255,2,2) 
	DOF 123,0
	DOF 122,0
	DOF 120,1 
	DOF 121,0
'Debug.print "DOF 123, 0"  'apophis
'Debug.print "DOF 122, 0"  'apophis
'Debug.print "DOF 121, 0"  'apophis
'Debug.print "DOF 120, 1"  'apophis
	BorderColor=2
	Light040.colorfull = RGB(150,2,2) : Light040.color = RGB(255,2,2) 
	Light041.colorfull = RGB(150,2,2) : Light041.color = RGB(255,2,2) 
	Light035.colorfull = RGB(150,2,2) : Light035.color = RGB(255,2,2) 
	Light036.colorfull = RGB(150,2,2) : Light036.color = RGB(255,2,2) 
	Light052.colorfull = RGB(150,2,2) : Light052.color = RGB(255,2,2) 
	Light053.colorfull = RGB(150,2,2) : Light053.color = RGB(255,2,2) 
	Light056.colorfull = RGB(150,2,2) : Light056.color = RGB(255,2,2) 
	Light057.colorfull = RGB(150,2,2) : Light057.color = RGB(255,2,2) 
	Light058.colorfull = RGB(150,2,2) : Light058.color = RGB(255,2,2) 
End Sub

Dim BorderColor
BorderColor=1  ' 1=green 2=red 3=blue
'All green
Sub GreenBorders
wall138.sidematerial = "Plastic Green"
wall079.sidematerial = "Plastic Green"
Wall080.sidematerial = "Plastic Green"
wall077.sidematerial = "Plastic Green"
wall078.sidematerial = "Plastic Green"
wall020.sidematerial = "Plastic Green"
wall022.sidematerial = "Plastic Green"
wall086.sidematerial = "Plastic Green"
wall080.sidematerial = "Plastic Green"


	TopFlasher1.color = RGB(2,255,2)
	DOF 123,0
	DOF 122,1
	DOF 120,0 
	DOF 121,0
'Debug.print "DOF 123, 0"  'apophis
'Debug.print "DOF 122, 1"  'apophis
'Debug.print "DOF 120, 0"  'apophis
'Debug.print "DOF 121, 0"  'apophis
	BorderColor=1
	Light040.colorfull = RGB(2,150,2) : Light040.color = RGB(2,255,2)
	Light041.colorfull = RGB(2,150,2) : Light041.color = RGB(2,255,2)
	Light035.colorfull = RGB(2,150,2) : Light035.color = RGB(2,255,2)
	Light036.colorfull = RGB(2,150,2) : Light036.color = RGB(2,255,2)
	Light052.colorfull = RGB(2,150,2) : Light052.color = RGB(2,255,2)
	Light053.colorfull = RGB(2,150,2) : Light053.color = RGB(2,255,2)
	Light056.colorfull = RGB(2,150,2) : Light056.color = RGB(2,255,2)
	Light057.colorfull = RGB(2,150,2) : Light057.color = RGB(2,255,2)
	Light058.colorfull = RGB(2,150,2) : Light058.color = RGB(2,255,2)
End Sub

Sub BlueBorders
wall138.sidematerial = "Plastic BlueB"
wall079.sidematerial = "Plastic BlueB"
Wall080.sidematerial = "Plastic BlueB"
wall077.sidematerial = "Plastic BlueB"
wall078.sidematerial = "Plastic BlueB"
wall020.sidematerial = "Plastic BlueB"
wall022.sidematerial = "Plastic BlueB"
wall086.sidematerial = "Plastic BlueB"
wall080.sidematerial = "Plastic BlueB"
	TopFlasher1.color = RGB(2,2,255)
	DOF 123,1
	DOF 122,0
	DOF 120,0 
	DOF 121,0
'Debug.print "DOF 123, 1"  'apophis
'Debug.print "DOF 122, 0"  'apophis
'Debug.print "DOF 120, 0"  'apophis
'Debug.print "DOF 121, 0"  'apophis
	BorderColor=3
	Light040.colorfull = RGB(2,2,152) : Light040.color = RGB(2,2,255)
	Light041.colorfull = RGB(2,2,152) : Light041.color = RGB(2,2,255)
	Light035.colorfull = RGB(2,2,152) : Light035.color = RGB(2,2,255)
	Light036.colorfull = RGB(2,2,152) : Light036.color = RGB(2,2,255)
	Light052.colorfull = RGB(2,2,152) : Light052.color = RGB(2,2,255)
	Light053.colorfull = RGB(2,2,152) : Light053.color = RGB(2,2,255)
	Light056.colorfull = RGB(2,2,152) : Light056.color = RGB(2,2,255)
	Light057.colorfull = RGB(2,2,152) : Light057.color = RGB(2,2,255)
	Light058.colorfull = RGB(2,2,152) : Light058.color = RGB(2,2,255)
End Sub

Sub YellowBorders
wall138.sidematerial = "Plastic YellowB"
wall079.sidematerial = "Plastic YellowB"
Wall080.sidematerial = "Plastic YellowB"
wall077.sidematerial = "Plastic YellowB"
wall078.sidematerial = "Plastic YellowB"
wall020.sidematerial = "Plastic YellowB"
wall022.sidematerial = "Plastic YellowB"
wall086.sidematerial = "Plastic YellowB"
wall080.sidematerial = "Plastic YellowB"
	TopFlasher1.color = RGB(255,200,44)
	DOF 123,0
	DOF 122,0
	DOF 120,0 
	DOF 121,1
'Debug.print "DOF 123, 0"  'apophis
'Debug.print "DOF 122, 0"  'apophis
'Debug.print "DOF 120, 0"  'apophis
'Debug.print "DOF 121, 1"  'apophis
	BorderColor=4
	Light040.colorfull = RGB(255,200,44) : Light040.color = RGB(255,200,44)
	Light041.colorfull = RGB(255,200,44) : Light041.color = RGB(255,200,44)
	Light035.colorfull = RGB(255,200,44) : Light035.color = RGB(255,200,44)
	Light036.colorfull = RGB(255,200,44) : Light036.color = RGB(255,200,44)
	Light052.colorfull = RGB(255,200,44) : Light052.color = RGB(255,200,44)
	Light053.colorfull = RGB(255,200,44) : Light053.color = RGB(255,200,44)
	Light056.colorfull = RGB(255,200,44) : Light056.color = RGB(255,200,44)
	Light057.colorfull = RGB(255,200,44) : Light057.color = RGB(255,200,44)
	Light058.colorfull = RGB(255,200,44) : Light058.color = RGB(255,200,44)

End Sub

Sub OrangeBorders
wall138.sidematerial = "Plastic OrangeB"
wall079.sidematerial = "Plastic OrangeB"
Wall080.sidematerial = "Plastic OrangeB"
wall077.sidematerial = "Plastic OrangeB"
wall078.sidematerial = "Plastic OrangeB"
wall020.sidematerial = "Plastic OrangeB"
wall022.sidematerial = "Plastic OrangeB"
wall086.sidematerial = "Plastic OrangeB"
wall080.sidematerial = "Plastic OrangeB"
	TopFlasher1.color = RGB(205,50,20)
	DOF 123,0
	DOF 122,0
	DOF 120,0 
	DOF 121,1
'Debug.print "DOF 123, 0"  'apophis
'Debug.print "DOF 122, 0"  'apophis
'Debug.print "DOF 120, 0"  'apophis
'Debug.print "DOF 121, 1"  'apophis
	BorderColor=5
	Light040.colorfull = RGB(205,50,20) : Light040.color = RGB(205,50,20)
	Light041.colorfull = RGB(205,50,20) : Light041.color = RGB(205,50,20)
	Light035.colorfull = RGB(205,50,20) : Light035.color = RGB(205,50,20)
	Light036.colorfull = RGB(205,50,20) : Light036.color = RGB(205,50,20)
	Light052.colorfull = RGB(205,50,20) : Light052.color = RGB(205,50,20)
	Light053.colorfull = RGB(205,50,20) : Light053.color = RGB(205,50,20)
	Light056.colorfull = RGB(205,50,20) : Light056.color = RGB(205,50,20)
	Light057.colorfull = RGB(205,50,20) : Light057.color = RGB(205,50,20)
	Light058.colorfull = RGB(205,50,20) : Light058.color = RGB(205,50,20)
End Sub




Sub Light040_Timer 			  ' 1=green 2=red 3=blue
	Select Case BorderColor
		Case 2:	Light040.colorfull = RGB(150,2,2) :	Light040.color = RGB(255,2,2) : Wall079.sidematerial = "Plastic RedB"
		Case 1: Light040.colorfull = RGB(2,150,2) : Light040.color = RGB(2,255,2) : Wall079.sidematerial = "Plastic Green" 
		Case 3: Light040.colorfull = RGB(2,2,150) : Light040.color = RGB(2,2,255) : Wall079.sidematerial = "Plastic BlueB"
		Case 4: Light040.colorfull = RGB(255,200,44) : Light040.color = RGB(255,200,44) : Wall079.sidematerial = "Plastic YellowB"
	End Select
	Light040.timerenabled=0
End Sub

Sub Light041_Timer
	Select Case BorderColor
		Case 2:	Light041.colorfull = RGB(150,2,2) :	Light041.color = RGB(255,2,2) : Wall138.sidematerial = "Plastic RedB"
		Case 1: Light041.colorfull = RGB(2,150,2) : Light041.color = RGB(2,255,2) : Wall138.sidematerial = "Plastic Green" 
		Case 3: Light041.colorfull = RGB(2,2,150) : Light041.color = RGB(2,2,255) : Wall138.sidematerial = "Plastic BlueB"
		Case 4:	Light041.colorfull = RGB(255,200,44) : Light041.color = RGB(255,200,44) : Wall138.sidematerial = "Plastic YellowB"
	End Select
	Light041.timerenabled=0
End Sub

'************************************
' Diverse Collection Hit Sounds v3.0
'************************************
Sub RubberPost009_hit:PlaySoundAtBall "Metal_Touch_" & int(rnd(1)*13)+1:End Sub
Sub aMetalWalls_Hit(idx) : PlaySoundAtBall "Metal_Touch_" & int(rnd(1)*13)+1:End Sub


Sub aMetalWires_Hit(idx):PlaySoundAtBall "Metal_Touch_" & int(rnd(1)*13)+1:End Sub   ' MBY THE SOUND IS IN THERE ALREADY
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
	Sub aRubber_LongBands_Hit(idx):PlaySoundAtBall "fx_rubber_longband":RubbersD.dampen Activeball:End Sub

	Sub aRubber_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub ' pinout
Sub aRubber_Pegs_Hit(idx):PlaySoundAtBall "fx_rubber_peg":End Sub
	Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub
Sub aTargets_Hit(idx):PlaySoundAtBall "Rubber_" & int(rnd(1)*8)+1 : End Sub

'***************************************************************
'             Supporting Ball & Sound Functions v3.0
'  includes random pitch in PlaySoundAt and PlaySoundAtBall
Dim TableWidth, TableHeight
TableWidth = Table1.width
TableHeight = Table1.height

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball)^2) * 0.001 
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / TableWidth-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function
Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function
Function BallVel(ball) 'Calculates the ball speed
	BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function
Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / TableHeight-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 0.27 * VolumeDial, AudioPan(tableobj), 0,Rnd(1)*2500,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
	PlaySound soundname, 1, 0.27 * VolumeDial * Vol(activeball), AudioPan(activeball), 0,Rnd(1)*2500,0, 1, AudioFade(activeball)
End Sub


Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / table1.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function





'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v3.0
'   uses a collection of shadows, aBallShadow
Const tnob = 5   'total number of balls, 20 balls, from 0 to 19
Const lob = 0     'number of locked balls
Const maxvel = 47 'max ball velocity
ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Const BallRollVolume = 0.22

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub
Sub RollingUpdate()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls and hide the shadow
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("BallRoll_" & b)
'        aBallShadow(b).Y = 3000
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
'        aBallShadow(b).X = BOT(b).X
'       aBallShadow(b).Y = BOT(b).Y

        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
'                ballpitch = Pitch(BOT(b))
'                ballvol = Vol(BOT(b))
''            Else
 '               ballpitch = Pitch(BOT(b)) + 35000 'increase the pitch on a ramp
  '              ballvol = Vol(BOT(b)) * 10
            End If
            rolling(b) = True
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial * 0.7, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
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


        ' jps ball speed control
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactorx
                BOT(b).VelY = BOT(b).VelY * speedfactorx
            End If
            If speedfactory < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactory
                BOT(b).VelY = BOT(b).VelY * speedfactory
            End If
        End If
    Next
End Sub

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^2 * 10
End Function


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

Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor
BallBouncePlayfieldSoftFactor = 0.0025									'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.0025									'volume multiplier; must not be zero

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd*7)+1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = Csng((ball.velz) ^2)
End Function

'**********************
' Ball Collision Sound
Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound ("Ball_Collide_" & int(rnd(1)*7)+1), 0, Csng(velocity) ^2 / 2000*VolumeDial, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)

End Sub

'*********************************************************************
'                     Supporting Ball & Sound Functions
'Dim tablewidth, tableheight : tablewidth = table1.width : tableheight = table1.height




' #####################################
' ######      FLASHER SCRIPT      #####
' #####################################

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.35	' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 0.35	' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherOffBrightness = 0.3		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objlight(20)
'Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
InitFlasher 1, "purple" : InitFlasher 2, "purple" : InitFlasher 3, "blue"
InitFlasher 4, "yellow" : InitFlasher 5, "blue" : InitFlasher 6, "red" : InitFlasher 7, "yellow" 
InitFlasher 8, "white" :InitFlasher 9, "white" : InitFlasher 10, "yellow" :  InitFlasher 11, "red" 
' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 4,17 : RotateFlasher 5,0 : RotateFlasher 6,90
'RotateFlasher 7,0 : RotateFlasher 8,0 
'RotateFlasher 9,-45 : RotateFlasher 10,90 : RotateFlasher 11,90

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
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
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col ': objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col ': objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col ': objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,132,4) : objflasher(nr).color = RGB(255,132,4)
		Case "purple" : objlight(nr).color = RGB(12,200,200) : objflasher(nr).color = RGB(12,200,200) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59)
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
		PlaySound "Relay_On",0,0.7*VolumeDial
		objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1
		If nr <> 1 And nr<>2 And nr<>11 Then objlit(nr).visible = 1
	End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
If nr = 4 Then Flasherside4.opacity = 770 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
If nr = 3 Then Flasherside3.opacity = 770 *  FlasherFlareIntensity * ObjLevel(nr)^2.5 : Flasherside3b.opacity = 770 *  FlasherFlareIntensity * ObjLevel(nr)^2.5 



	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	If nr<2 And nr <11 Then objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	

	if nr = 9 or nr = 8 Then
		whitebloom.opacity=ObjLevel(nr)^3 * 50
		objlight(nr).IntensityScale = 0.02 * FlasherLightIntensity * ObjLevel(nr)^3
	Else
		objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	End If

	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objlit(nr).visible = 0 : End If
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
Sub FlasherFlash10_Timer() : FlashFlasher(10) :End Sub
Sub FlasherFlash11_Timer() : FlashFlasher(11) : End Sub

' ###################################
' ###### copy script until here #####
' ###################################



'Sub BossDelayTimer_Timer
'	BossDelayTimer.enabled=0
'	Light059.intensity=1000
'	EvasiveState = 5 
'	UpdateBossLights 
'	DOF 118,1
'End Sub


Dim Annomando
Sub AnnoydMando
	If MultiballActive=0 Then 

		Annomando=Annomando+int(rnd(1)*2)+1
		If Annomando>9 Then Annomando=AnnoMando-9
		StopTalking  
		Select Case Annomando
			Case 0 : PlaySound "mandoannoyd1", 0, .81*BackGlassVolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
			Case 1 : PlaySound "mandoannoyd2", 0, .81*BackGlassVolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
			Case 2 : PlaySound "mandoannoyd3", 0, .81*BackGlassVolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
			Case 3 : PlaySound "costme", 0, .81*BackGlassVolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
			Case 4 : PlaySound "notfinish", 0, .81*BackGlassVolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
			Case 5 : PlaySound "keepshooting", 0, .81*BackGlassVolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
			Case 6 : PlaySound "highstakes", 0, .81*BackGlassVolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
			Case 7 : PlaySound "isthereaproblem", 0, .81*BackGlassVolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
			Case 8 : PlaySound "tightspot", 0, .81*BackGlassVolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
			Case 9 : PlaySound "avoided", 0, .81*BackGlassVolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004)
		End Select

	End If
End Sub
Sub Finalflashers_Timer
	If Finalflashers.enabled=0 Then
		Finalflashers.enabled=1
	End If
	If ManualLightSequencer.enabled=0 Then ManualLightSequencer.enabled=1 : MSeqCounter=100 : MSeqBigFL=30
objLevel(3) = 1 : FlasherFlash3_Timer  : DOF 135,2 : 'Debug.print "DOF 135, 2" ' blue left  'apophis
objLevel(4) = 1 : FlasherFlash4_Timer  : DOF 137,2 : 'Debug.print "DOF 137, 2" 'red left
objLevel(10) = 1: FlasherFlash10_Timer : DOF 136,2 : 'Debug.print "DOF 136, 2" 'red left

End Sub




Dim DelaySwapplayer
Sub DelayNextBall_Timer
	DelaySwapplayer=DelaySwapplayer+1

	If DelaySwapplayer = 1 Then
		If CurrentPlayer = 1 Then Playsound "Player1",0,BackGlassVolumeDial
		If CurrentPlayer = 2 Then Playsound "Player2",0,BackGlassVolumeDial
		If CurrentPlayer = 3 Then Playsound "Player3",0,BackGlassVolumeDial
		If CurrentPlayer = 4 Then Playsound "Player4",0,BackGlassVolumeDial
	End If


	If DelaySwapplayer = 2 Then
		DelayNextBall.Enabled=0
		If BallinPlay= 1 Then DelayStartNewGame.enabled=1
		DelaySwapplayer=0
		nextBallPlz
	End If
End Sub


Dim MonsterEND_Level
Sub Delay_MonsterEnd_Timer
	Delay_MonsterEnd.enabled=0
	If MonsterEND_Level=2 Then  Playsound "mudhorn" ,0,BackGlassVolumeDial
	If MonsterEND_Level=4 Then  Playsound "cosyinthecockpit" ,0,BackGlassVolumeDial
	If MonsterEND_Level=6 Then  Playsound "pathscrossagain" ,0,BackGlassVolumeDial
End Sub


Dim Displaybuzy,DisplayString1,DisplayString2,DisplayEffect
Dim DisplayBlank,DisplayCounter
Dim GetReadyToPlay,DisplayPause,HurryBonus,teststr


Dim BonusScoring, missioninfo, HighScoreblink , initialletter, CRAPRESTARTS
missioninfo=0
Sub DisplayUpdate_Timer
	If Not Tilted And TiltValue > 0 Then TiltValue = TiltValue -0.01 'debug.print TiltValue
	If Displaybuzy=0 And spinnerstatus=1 Then ' only centerdisplay
		Displaybuzy=1 : spinnerstatus=0
		DisplayString1="            "
		DisplayString2="            "
		DisplayEffect=9 : DisplayCounter=0 : DisplayBlank=0
	End If


	If PlayerUpStatus > 0 Then
			Displaybuzy=1
			DisplayString1="  PLAYER " & PlayerUpStatus & "  "
			PlayerUpStatus = 0
			DisplayString2=" GET  READY " 
			If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex FlexBG, DisplayString1, 15, 3, DisplayString2, 15, 3, 11, 2000, 14
			DisplayEffect=4
			DisplayCounter=0
			DisplayBlank=0
			DelayNextBall.Enabled=1
	End If
'Displaybuzy=0 And
	If GetReadyToPlay=10 Then
		GetReadyToPlay=0 : Displaybuzy=1
		DisplayString1="  THIS  IS  "
		DisplayString2="  THE  WAY  " 
		DisplayEffect=5 : DisplayCounter=0 : DisplayBlank=0 
		PlaySound "thisistheway3" ,0,BackGlassVolumeDial
		If FlexDMD Then  UMainDMD.DisplayScene00Ex "theway2.wmv", Displaystring1, 0, 12, DisplayString2 , 15, 1, 14, 1600, 11 
	End If



	If Displaybuzy=0 And WaitforInitialsstatus = 2 Then 
		WaitforInitialsstatus = 3
		Displaybuzy=2 
		Displaystring2=""
		initialletter=65
		initialSelect=0
	End If

	If Displaybuzy=0 And WaitforInitialsstatus = 1 Then

			DisplayEnterDelay=0
			Displaybuzy=1
			PlaySound "thisistheway1" ,0,BackGlassVolumeDial
			WaitforInitialsstatus = 2
			DisplayString1=" NEW  TOP 5 "
			DisplayString2=" HIGH SCORE " 
			If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex FlexBG, DisplayString1, 15, 3, DisplayString2, 15, 3, 11, 2000, 14
			DisplayEffect=4
			DisplayCounter=0
			DisplayBlank=0
	End If




	If BossStatus=10 Then
		BossStatus=11  


		If boss7ramps > 24 Then
			boss7ramps=0
			Displaybuzy=1  
			DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0	
			DisplayString1=" DARK BONUS "

			DisplayString2=FormatScore((JackPotScoring*(25 + WizardBonusLevel * 10 )) *P1scorebonus)
			If FlexDMD Then UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "coins1.wmv", DisplayString1, 0, 12, Flexscore , 15, 1, 14, 2700, 14 : ClearAllStatus
			PlaySound "fireworks" ,0,BackGlassVolumeDial
			DOF 142,2
			DOF 112,1
			DOF 118,0
			bossdof=0
			DOF 135,2
			DOF 136,2
			DOF 137,2
			DOF 117,2
			Finalflashers_Timer
		End If

		EvasiveOneScoring=EvasiveOneScoring+1000000
		BossActive=0
		BOSS7.state=1
		StopSound "DarkTrooperMusic"
		If MultiBallActive Then
			PlaySound "Cantina1",-1, 0.3 * MusicVolumeDial ,0, 0,0,0,0,0 : MusicVolume = 0.01
		Else
			MusicVolume = PlayVolume (SongNr) * MusicVolumeDial 
		End If
	End If

	If Displaybuzy=0 And GideonStatus = 1 Then
		GideonStatus=0
		Displaybuzy=1  
		DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0	
		AnnoydMando
		DisplayString1=" GIDEON HIT "
		DisplayString2=FormatScore((EvasiveOneScoring*P1scorebonus))
		DisplayEffect=5 : DisplayCounter=0 : DisplayBlank=0 
		If FlexDMD Then UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "boss.wmv", DisplayString1 , 0, 12, Flexscore , 15, 1, 14, 2000, 14: ClearAllStatus
	End If
	If Displaybuzy=0 And GideonStatus = 2 Then
		GideonStatus=0
		Displaybuzy=1  
		DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0	
		AnnoydMando
		DisplayString1="MONSTERS HIT"
		DisplayString2=FormatScore((EvasiveOneScoring*P1scorebonus))
		DisplayEffect=5 : DisplayCounter=0 : DisplayBlank=0 
		If FlexDMD Then UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "boss.wmv", DisplayString1 , 0, 12, Flexscore , 15, 1, 14, 2000, 14: ClearAllStatus
	End If


	If Displaybuzy=0 And BossStatus=11 Then
		BossStatus=12  
		Displaybuzy=1  
		DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0	
		DisplayString1="DARK TROOPERS"
		DisplayString2="  COMPLETED  "
		If FlexDMD Then 
			UMainDMD.cancelrendering()
			UMainDMD.DisplayScene00Ex "DarkEnd.wmv", DisplayString1, 0, 12, DisplayString2 , 15, 1, 14, 12000, 14
			ClearAllStatus
		End If
		playsound "DarkEnd" ,0,BackGlassVolumeDial

		DOF 118,0
'Debug.print "DOF 118, 0" 'apophis
	End If

	If Displaybuzy=0 And BossStatus=12 then
		boss7status=0
		BossStatus=13
		Displaybuzy=1  
		DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0	
		DisplayString1="DARK TROOPERS"
		DisplayString2="  COMPLETED  "
		DOF 118,0 : 'Debug.print "DOF 118, 0" 'apophis
	End If


	If BossStatus=5 Then ' muddywaters
		BossStatus=6

		'Fixing   WizardLevel

		LiReplay.state=2 : lireplay001.state = 2
		pLireplay.blenddisablelighting=5

		LiReplay2.state=0
		LiReplay2.timerenabled=0
		LiReplay.timerenabled=0
		'If Not TournamentMode Then
		LiReplay.timerenabled=1 : lireplay.blinkinterval=200 : lireplay001.blinkinterval=200 : pLireplay.blenddisablelighting=5
		BossDelayBlinker.enabled=1 
'		Light059.intensity=30
		for i = 60 to 10 step -4
			LightSeq003.Play SeqBlinking,,2,i   
		Next


		Stoptalking
		Displaybuzy=1  
		DisplayString1="DARKTROOPERS" : DisplayString2="DEFEAT  THEM" 
		DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 
		If FlexDMD then 
			UmainDMD.cancelrendering()
			UMainDMD.DisplayScene00ExWithId "attract1",false, "DarkStart.wmv", "DARK TROOPERS", 0, 12, "DEFEAT THEM" , 15, 1, 11, 11000, 14
			ClearAllStatus
		End If
		PlaySound "DarkStart" ,0,BackGlassVolumeDial

		scoring((EvasiveOneScoring+2000000)/20)
	End If

	If Displaybuzy=0 And BossStatus=7 Then

		doubletimeonmulti=2
		If WizardLevel=2 Then doubletimeonmulti=1
		If WizardLevel>2 Then doubletimeonmulti=0
		If WizardLevel<4 Then

			LiReplay.state=2 : lireplay001.state = 2
		pLireplay.blenddisablelighting=5

			LiReplay2.state=0
			LiReplay2.timerenabled=0
			LiReplay.timerenabled=0
			'If Not TournamentMode Then 
			LiReplay.timerenabled=1 : lireplay.blinkinterval=200 : lireplay001.blinkinterval=200 : pLireplay.blenddisablelighting=5
		End If

		BossStatus=0
		Boss7Countdown=99
		boss7.timerenabled=1

		If WizardLevel = 1 Then
			ballswaiting=ballswaiting+1
			BIP=BIP+1 
		End If

		StopSound "cantina1"
		PlaySound "DarkTrooperMusic",-1, 0.4 * MusicVolumeDial,0, 0,0,0,0,0 : MusicVolume = 0.01
	End If

	If Displaybuzy=0 And BossStatus=6 Then ' muddywaters
		BossStatus=7
		Displaybuzy=1  
		DisplayString1="TIMED  EVENT" : DisplayString2= 10+WizardBonusLevel*2 &" RAMPS 99S" 
		DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 
		If FlexDMD then UMainDMD.ModifyScene00 "attract1", "TIMED EVENT", 10+WizardBonusLevel*2 &" RAMPS 99S" 
	End If

	If Boss7Status>0 Then 

		bossfights.Play SeqBlinking,,6,50
		LightSeq003.Play SeqBlinking,,6,50 
		Stoptalking
		Displaybuzy=1  
		DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 
		DisplayString1="DARKTROOPERS"

		If Boss7status = 1 Then DisplayString2=FormatScore(JackPotScoring*3) 
		If Boss7status = 2 Then DisplayString2=FormatScore(JackPotScoring*2+ScoringLeftRamp) 
		If Boss7status = 3 Then DisplayString2=FormatScore(JackPotScoring*2+ScoringRightRamp) 
		If Boss7status = 4 Then DisplayString2=FormatScore(JackPotScoring*2+ScoringLeftRamp+ScoringRightRamp) 

		Boss7Status=0

		If FlexDMD then 
			UmainDMD.cancelrendering()
			UMainDMD.DisplayScene00Ex "Ramps.wmv", "DARK TROOPERS", 0, 12, Flexscore , 15, 1, 11, 3800, 14
			ClearAllStatus
		End If
	End If



	If BossStatus=2 Or BossStatus=3 Or BossStatus=4 Then ' muddywaters
		BossStatus=0
		BossDelayBlinker.enabled=1 
'		Light059.intensity=30
		for i = 60 to 10 step -4
			LightSeq003.Play SeqBlinking,,2,i   
		Next
		Stoptalking
		Displaybuzy=1  
		If bosslevel=2 Then	DisplayString1="  MUDHORN   " : DisplayString2=" DEFEAT HIM " 
		If bosslevel=4 Then	DisplayString1="SPIDER QUEEN" : DisplayString2="TAKE HER OUT" 
		If bosslevel=6 Then	DisplayString1="KRAYT DRAGON" : DisplayString2="TAKE HIM OUT" 

		DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 
		If FlexDMD then 
			UmainDMD.cancelrendering()
			If BossLevel=2 Then UMainDMD.DisplayScene00Ex "MudhornStart.wmv", "MUDHORN", 0, 12, "TAKE HIM OUT" , 15, 1, 11, 8300, 14
			If BossLevel=4 Then UMainDMD.DisplayScene00Ex "SpiderStart.wmv", "SPIDER QUEEN", 0, 12, "TAKE HER OUT" , 15, 1, 11, 8000, 14
			If BossLevel=6 Then UMainDMD.DisplayScene00Ex "DragonStart.wmv", "KRAYT DRAGON", 0, 12, "TAKE HIM OUT" , 15, 1, 11, 9500, 14
			ClearAllStatus
		End If

		If BossLevel=2 Then PlaySound "MudhornStart" ,0 ,0.4 * BackGlassVolumeDial
		If BossLevel=4 Then PlaySound "SpiderStart"  ,0 ,0.4 * BackGlassVolumeDial
		If BossLevel=6 Then PlaySound "DragonStart"  ,0 ,0.4 * BackGlassVolumeDial


		scoring((EvasiveOneScoring+2000000)/20)
	End If


	If Displaybuzy=0 And NewBossStatus<8 And NewBossStatus>0 Then 
		bossfights.Play SeqBlinking,,6,50
		LightSeq003.Play SeqBlinking,,6,50 
		NewBossStatus=0
		Displaybuzy=1 
		DisplayEffect=5 : DisplayCounter=0 : DisplayBlank=0 
		DisplayString2=FormatScore((EvasiveOneScoring+2000000)/20*P1scorebonus)

		If BossLevel=2 Then 
			DisplayString1=" MUDHORN +"& BigDThitsCounter & "  "
			If FlexDMD Then UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "mudhorn.jpg", DisplayString1 , 0, 12, Flexscore , 15, 1, 14, 1600, 14: ClearAllStatus
		End If

		If BossLevel=4 Then
			DisplayString1=" SPIDERS +"& BigDThitsCounter & "  "
			If FlexDMD Then UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "spider.jpg", DisplayString1 , 0, 12, Flexscore , 15, 1, 14, 1600, 14: ClearAllStatus
		End If
		If BossLevel=6 Then
			DisplayString1="  DRAGON +"& BigDThitsCounter & "  "
			If FlexDMD Then UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "dragon.jpg", DisplayString1 , 0, 12, Flexscore , 15, 1, 14, 1600, 14: ClearAllStatus
		End If
		scoring((EvasiveOneScoring+2000000)/20)
		If Lidouble2.state>0 Then  doubleblinker.Play SeqBlinking,,2,80  
		If LiTripple2.state>0 Then trippleblinker.Play SeqBlinking,,2,80  
	End If

	If Displaybuzy=0 And NewBossStatus=8 Then 
		If ManualLightSequencer.enabled=0 Then ManualLightSequencer.enabled=1 : MSeqCounter=160 : MSeqBigFL=20
		bossfights.Play SeqBlinking,,6,50
		LightSeq003.Play SeqBlinking,,6,50
		
		NewBossStatus=0

		Delay_MonsterEnd.enabled=1
		MonsterEND_Level=BossLevel

		Displaybuzy=1 
		DisplayEffect=5 : DisplayCounter=0 : DisplayBlank=0 
		DisplayString2=FormatScore((EvasiveOneScoring+2000000)*P1scorebonus)

		If BossLevel=2 Then 
			DisplayString1="MUDHORN +" & BigDThitsCounter & "  "
			If FlexDMD Then UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "MudhornEnd.wmv", DisplayString1 , 0, 12, Flexscore , 15, 1, 14, 7800, 14: ClearAllStatus
			Playsound "mudhornend" ,0,0.8*BackGlassVolumeDial
		End If

		If BossLevel=4 Then
			DisplayString1="SPIDERS +" & BigDThitsCounter & "  "
			If FlexDMD Then UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "spiderend.wmv", DisplayString1 , 0, 12, Flexscore , 15, 1, 14, 6000, 14: ClearAllStatus
			Playsound "SpiderEnd" ,0,0.8*BackGlassVolumeDial
		End If
		If BossLevel=6 Then
			DisplayString1=" DRAGON +" & BigDThitsCounter & "  "
			If FlexDMD Then UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "dragonend.wmv", DisplayString1 , 0, 12, Flexscore , 15, 1, 14, 6800, 14: ClearAllStatus
			Playsound "dragonend" ,0,0.8*BackGlassVolumeDial
		End If
		scoring(EvasiveOneScoring+2000000)
		EvasiveOneScoring=EvasiveOneScoring+1000000
		If Lidouble2.state>0 Then  doubleblinker.Play SeqBlinking,,2,80  
		If LiTripple2.state>0 Then trippleblinker.Play SeqBlinking,,2,80  
		BossActive=0
		bossdof=0
		DOF 118,0
'Debug.print "DOF 118, 0" 'apophis
	End If



	If  BossStatus=1 Then 
		BossStatus=0
'		BossDelayTimer.enabled=1 
		BossDelayBlinker.enabled=1 
'		Light059.intensity=30
		for i = 60 to 10 step -4
			LightSeq003.Play SeqBlinking,,2,i   
		Next
		Stoptalking
		Displaybuzy=1  
		DisplayString1=" BOSS  FIGHT " : DisplayString2="TAKE HIM OUT" 
		DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 
		If FlexDMD then 
			UmainDMD.cancelrendering()
			UMainDMD.DisplayScene00Ex "Somethingiwant.wmv", "BOSS FIGHT", 0, 12, "TAKE HIM OUT" , 15, 1, 11, 7250, 14
			ClearAllStatus
		End If
		PlaySound "youhavesomething" ,0,0.7*BackGlassVolumeDial

		scoring((EvasiveOneScoring+2000000)/20)
	End If
	
	If Displaybuzy=0 And EvasiveDestroydStatus=1 Then 
		EvasiveDestroydStatus=0
		AnnoydMando
		DisplayString1="BOSS  5 MORE"
		Displaybuzy=1  
		DisplayString2=FormatScore((EvasiveOneScoring+2000000)/20*P1scorebonus)
		DisplayEffect=5 : DisplayCounter=0 : DisplayBlank=0 
		If FlexDMD Then UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "boss.wmv", DisplayString1 , 0, 12, Flexscore , 15, 1, 14, 4000, 14: ClearAllStatus
		scoring((EvasiveOneScoring+2000000)/20)
		If Lidouble2.state>0 Then  doubleblinker.Play SeqBlinking,,2,80  
		If LiTripple2.state>0 Then trippleblinker.Play SeqBlinking,,2,80  
	End If

	If Displaybuzy=0 And EvasiveDestroydStatus=2 Then 
		EvasiveDestroydStatus=0
		AnnoydMando
		Displaybuzy=1  
		DisplayString1="BOSS  4 MORE"
		DisplayString2=FormatScore((EvasiveOneScoring+2000000)/20*P1scorebonus)
		DisplayEffect=5 : DisplayCounter=0 : DisplayBlank=0
		If FlexDMD Then UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "Hit11.wmv", DisplayString1, 0, 12, Flexscore , 15, 1, 14, 2900, 14 : ClearAllStatus
		scoring((EvasiveOneScoring+2000000)/20)
		If Lidouble2.state>0 Then  doubleblinker.Play SeqBlinking,,2,80  
		If LiTripple2.state>0 Then trippleblinker.Play SeqBlinking,,2,80 
	End If

	If Displaybuzy=0 And EvasiveDestroydStatus=3 Then 
		EvasiveDestroydStatus=0
		AnnoydMando
		Displaybuzy=1  
		DisplayString1="BOSS  3 MORE"
		DisplayString2=FormatScore((EvasiveOneScoring+2000000)/20*P1scorebonus)
		DisplayEffect=5 : DisplayCounter=0 : DisplayBlank=0
		If FlexDMD Then UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "Hit21.wmv", DisplayString1, 0, 12, Flexscore , 15, 1, 14, 3900, 14 : ClearAllStatus
		scoring((EvasiveOneScoring+2000000)/20)
		If Lidouble2.state>0 Then  doubleblinker.Play SeqBlinking,,2,80  
		If LiTripple2.state>0 Then trippleblinker.Play SeqBlinking,,2,80 
	End If
	If Displaybuzy=0 And EvasiveDestroydStatus=4 Then 
		EvasiveDestroydStatus=0
		AnnoydMando
		Displaybuzy=1  
		DisplayString1="BOSS  2 MORE"
		DisplayString2=FormatScore((EvasiveOneScoring+2000000)/20*P1scorebonus)
		DisplayEffect=5 : DisplayCounter=0 : DisplayBlank=0
		If FlexDMD Then UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "Hit21.wmv", DisplayString1, 0, 12, Flexscore , 15, 1, 14, 3900, 14 : ClearAllStatus
		scoring((EvasiveOneScoring+2000000)/20)
		If Lidouble2.state>0 Then  doubleblinker.Play SeqBlinking,,2,80  
		If LiTripple2.state>0 Then trippleblinker.Play SeqBlinking,,2,80 
	End If
	If Displaybuzy=0 And EvasiveDestroydStatus=5 Then 
		EvasiveDestroydStatus=0
		AnnoydMando
		Displaybuzy=1  
		DisplayString1="ONE HIT LEFT"
		DisplayString2=FormatScore((EvasiveOneScoring+2000000)/20*P1scorebonus)
		DisplayEffect=5 : DisplayCounter=0 : DisplayBlank=0
		If FlexDMD Then UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "Hit31.wmv", DisplayString1, 0, 12, Flexscore , 15, 1, 14, 2000, 14 : ClearAllStatus
		scoring((EvasiveOneScoring+2000000)/20)
		If Lidouble2.state>0 Then  doubleblinker.Play SeqBlinking,,2,80  
		If LiTripple2.state>0 Then trippleblinker.Play SeqBlinking,,2,80 
	End If
	If Displaybuzy=0 And EvasiveDestroydStatus=6 Then 
		EvasiveDestroydStatus=0
		DisplayString1="TARGET  DOWN"
		DisplayString2=FormatScore(EvasiveOneScoring*P1scorebonus)
		Displaybuzy=1  
		DisplayEffect=5 : DisplayCounter=0 : DisplayBlank=0
		If FlexDMD Then UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "Hit41.wmv", DisplayString1, 0, 12, Flexscore , 15, 1, 14, 5000, 14 : ClearAllStatus
		If int(rnd(1)*2)=1 Then 
			Stoptalking: talkingdelay=11 : PlaySound "bescarbelong", 0, BackGlassVolumeDial, AudioPan(Light059), 0.05,0,0,1,AudioFade(Light059)
		Else
			Stoptalking: talkingdelay=11 : PlaySound "orderofthings", 0, BackGlassVolumeDial, AudioPan(Light059), 0.05,0,0,1,AudioFade(Light059)
		End If
		scoring(EvasiveOneScoring)
		If Lidouble2.state>0 Then  doubleblinker.Play SeqBlinking,,10,40  
		If LiTripple2.state>0 Then trippleblinker.Play SeqBlinking,,10,40 
		If BossLevel=1 Then BOSS1.state=1 
		If BossLevel=3 Then BOSS3.state=1
		If BossLevel=5 Then BOSS5.state=1 
		BossActive=0
	End If

	If replaystatus=1 Then
		replaystatus=0
		Displaybuzy=1  
		DisplayString1="  YOU  WIN  "
		DisplayString2="FREE  REPLAY"
		DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0
		If FlexDMD Then UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "Hit11.wmv", DisplayString1, 0, 12, DisplayString2 , 15, 1, 14, 2900, 14 : ClearAllStatus
	End If

	If Displaybuzy=0 And BonusStatus=1 Then 
		If bonusover=1 Then BonusStatus=9 : Exit Sub
		HurryBonus=1
		PlaySound SoundFX("Consolebeeps2",DOFContactors), 0, .7*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) : Displaybuzy=1 : BonusStatus=2 : 
		DisplayString1="   BONUS    " : DisplayString2="  SCORING   " : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 
		If lastplayd<>2 Then If FlexDMD then UmainDMD.cancelrendering() : UMainDMD.DisplayScene00ExWithId "attract1",true,vid7, Displaystring1, 0, 12, DisplayString2, 15, 1, 14, 15000, 14

	End If

	If Displaybuzy=0 And BonusStatus=2 Then 
		PlaySound SoundFX("Consolebeeps2",DOFContactors), 0, .7*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) 
		Displaybuzy=1 : BonusStatus=3 
		DisplayString1=CStr(abs(RampsTotal)) & " RAMPS        " : BonusScoring=RampsTotal*5000*P1scorebonus : DisplayString2=FormatScore(BonusScoring) : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 
		If FlexDMD Then UMainDMD.ModifyScene00 "attract1",CStr(abs(RampsTotal)) & " RAMPS", Flexscore
	End If

	If Displaybuzy=0 And BonusStatus=3 Then
		PlaySound SoundFX("Consolebeeps2",DOFContactors), 0, .7*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) : Displaybuzy=1 : BonusStatus=4 
		DisplayString1=" "&CStr(abs(ComboTotalRamps)) & " COMBOS       " : BonusScoring=BonusScoring+(ComboTotalRamps*25000*P1scorebonus) : DisplayString2=FormatScore(ComboTotalRamps*25000*P1scorebonus) : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0
		If FlexDMD Then UMainDMD.ModifyScene00 "attract1",CStr(abs(ComboTotalRamps)) & " COMBOS", Flexscore
	End If

	If Displaybuzy=0 And BonusStatus=4 Then 
		PlaySound SoundFX("Consolebeeps2",DOFContactors), 0, .7*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) : Displaybuzy=1 : BonusStatus=5 
		DisplayString1=" "&CStr(abs(DropTargetsCounter)) & " CREDITS       " : BonusScoring=BonusScoring+(DropTargetsCounter*1000*P1scorebonus) : DisplayString2=FormatScore(DropTargetsCounter*1000*P1scorebonus) : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 
		If FlexDMD Then UMainDMD.ModifyScene00 "attract1",CStr(abs(DropTargetsCounter)) & " CREDITS", Flexscore
	End If

	If Displaybuzy=0 And BonusStatus=5 Then 
		PlaySound SoundFX("Consolebeeps2",DOFContactors), 0, .7*VolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) : Displaybuzy=1 : BonusStatus=6 
		DisplayString1=" "&CStr(abs(MissionsDoneCounter)) & " MISSION       " : BonusScoring=BonusScoring+(MissionsDoneCounter*20000*P1scorebonus) : DisplayString2=FormatScore(MissionsDoneCounter*20000*P1scorebonus)  : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0
		If FlexDMD Then UMainDMD.ModifyScene00 "attract1",CStr(abs(MissionsDoneCounter)) & " MISSIONS", Flexscore
	End If

	If Displaybuzy=0 And BonusStatus=6 Then 

		If bonusmultiplyer>0 Then BonusX001_Timer
		If bonusmultiplyer>1 Then BonusX002_Timer
		If bonusmultiplyer>2 Then BonusX003_Timer
		If bonusmultiplyer>3 Then BonusX004_Timer
		If bonusmultiplyer>4 Then BonusX005_Timer
		If bonusmultiplyer>5 Then BonusX006_Timer
		If bonusmultiplyer>6 Then BonusX007_Timer
		If bonusmultiplyer>7 Then BonusX008_Timer
		If bonusmultiplyer>8 Then BonusX009_Timer


		PlaySound "bonus21", 0, 0.45*BackGlassVolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) : Displaybuzy=1 : BonusStatus=7 
	
		If bonusmultiplyer>0 And bonusmultiplyer<9 Then
			BonusScoring=BonusScoring*bonusmultiplyer*2 : 	DisplayString1=" "&CStr(abs(bonusmultiplyer*2)) & " X MULTI      " : tempstr= CStr(abs(bonusmultiplyer*2)) & "X MULTIPLYER"
		Else
			If	bonusmultiplyer=9 Then
				BonusScoring=BonusScoring*20 : DisplayString1=" 20 X MULTI " : tempstr="20 X MULTIPLYER"
			Else
				DisplayString1=" 1 X MULTI        " : tempstr="NO MULTIPLYER"
			End If
		End If

		DisplayString2=FormatScore(BonusScoring)  : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0
		If FlexDMD Then UMainDMD.ModifyScene00 "attract1",tempstr, Flexscore

	End If

	If Displaybuzy=0 And BonusStatus=7 Then

		PlaySound "bonusup", 0, 0.3*BackGlassVolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) : Displaybuzy=1 : BonusStatus=8
		DisplayString2="TOTAL  BONUS"  : DisplayString1=FormatScore(BonusScoring)  : P1Score=P1score+BonusScoring
		If HideDesktop=1 Then Controller.B2SSetScorePlayer1 P1score	
		DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 
			If FlexDMD Then UMainDMD.ModifyScene00 "attract1",Flexscore, DisplayString2 
	End If
	If Displaybuzy=0 And BonusStatus=8 Then
		If FlyAgain3.state<2 And BIP=3 Then closebonus_timer
		BonusStatus=9
		If LastPlayd=2 Then
			Displaybuzy=1  
			DisplayString1="FINAL  SCORE"
			DisplayString2=FormatScore(P1score)
			DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 
			If FlexDMD Then UMainDMD.ModifyScene00 "attract1", DisplayString1, Flexscore 
		End If
	End If

	If Displaybuzy=0 And BonusStatus=9 Then BonusStatus=10 : HurryBonus=0

	If Displaybuzy=0 And BonusStatus=20 Then
		Displaybuzy=1 : BonusStatus=21 
		DisplayString1="            "
		DisplayString2="  NO BONUS  " 
		DisplayEffect=6 : DisplayCounter=0 : DisplayBlank=0
		If FlexDMD Then
			If GAMEISOVER=1 Then
				UMainDMD.ModifyScene00 "attract1", DisplayString1, DisplayString2 
			Else
				UmainDMD.cancelrendering() : UMainDMD.DisplayScene00ExWithId "attract1",true,vid7, Displaystring1, 0, 12, DisplayString2, 15, 1, 14, 2000, 14
			End If
		End If
	End If
	If Displaybuzy=0 And BonusStatus=21 Then
		BonusStatus=10
		HurryBonus=0
	End If

	If Displaybuzy=0 And AddplayerStatus>0 Then
		Displaybuzy=1 
		DisplayString1="  PLAYER " & AddplayerStatus & "  " : AddplayerStatus=0 
		DisplayString2="  JOINING   " 
		DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0
		If FlexDMD Then UmainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "Door_15.jpg", DisplayString1, 0, 12, DisplayString2 , 15, 1, 11, 1300, 14
	End If


	
	If WaitforInitialsstatus>3 Then
		If Displaybuzy=0 And GameOverStatus=1 Then
			Displaybuzy=1
			GameOverStatus=2
			DisplayString1=" GAME  OVER "
			DisplayString2="            "
			DisplayEffect=6
			DisplayCounter=0
			DisplayBlank=0
			If FlexDMD Then UMainDMD.ModifyScene00 "attract1","G A M E", "O V E R"
		End If


		If Displaybuzy=0 And GameOverStatus=2 Then
			Displaybuzy=1
			GameOverStatus=3
			DisplayString1=" GAME  OVER "
			DisplayString2="            "
			DisplayEffect=6
			DisplayCounter=0
			DisplayBlank=0
			If FlexDMD Then UMainDMD.ModifyScene00 "attract1","G A M E", "O V E R"
		End If
		If Displaybuzy=0 And GameOverStatus=3 Then GameOverStatus=4	
	End If 

	If Tiltstatus=1 Then 
		Displaybuzy=1 : Tiltstatus=0 
		DisplayString1="            "
		DisplayString2="  WARNING   "
		DisplayEffect=6 : DisplayCounter=0 : DisplayBlank=0
		If FlexDMD Then
		'	UMainDMD.DisplayScene00Ex vid7, Displaystring1, 0, 12, DisplayString2 , 15, 1, 10, 2000, 14
			If attract1mode=0 Then
				UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "Door_15.jpg", DisplayString1, 0, 12, DisplayString2 , 15, 1, 11, 1300, 14  : ClearAllStatus
			End If
		End If 
	End If
	If Tiltstatus=2 Then 
		Displaybuzy=1 : Tiltstatus=0 
		DisplayString1="            "
		DisplayString2="    TILT    "
		DisplayEffect=6 : DisplayCounter=0 : DisplayBlank=0
		If FlexDMD Then
		'	UMainDMD.DisplayScene00Ex vid7, Displaystring1, 0, 12, DisplayString2 , 15, 1, 10, 2000, 14
			If attract1mode=0 Then
				UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "Door_15.jpg", DisplayString1, 0, 12, DisplayString2 , 15, 1, 11, 1300, 14  : ClearAllStatus
			End If
		End If 
	End If
	If Tiltstatus=3 Then 
		Displaybuzy=1 : Tiltstatus=0 
		DisplayString1="            "
		DisplayString2="RESTART GAME"
		DisplayEffect=6 : DisplayCounter=0 : DisplayBlank=0
		If FlexDMD Then
		'	UMainDMD.DisplayScene00Ex vid7, Displaystring1, 0, 12, DisplayString2 , 15, 1, 10, 2000, 14
			If attract1mode=0 Then
				UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "Door_15.jpg", DisplayString1, 0, 12, DisplayString2 , 15, 1, 11, 1300, 14  : ClearAllStatus
			End If
		End If 
	End If
		
	If Displaybuzy=2 Then
		InsertCoinStatus=0
		addcreditsstatus=0
	End If

	If InsertCoinStatus=1 Then 
		Displaybuzy=1 : InsertCoinStatus=0 
		DisplayString1="  CREDITS   " : DisplayString2=FormatScore(Credits)
		DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0
		If FlexDMD Then
		'	UMainDMD.DisplayScene00Ex vid7, Displaystring1, 0, 12, DisplayString2 , 15, 1, 10, 2000, 14
			If attract1mode=0 Then
				UMainDMD.cancelrendering() : UMainDMD.DisplayScene00Ex "blink1.wmv", DisplayString1, 0, 12, flexscore , 15, 1, 14, 1300, 14 : ClearAllStatus
			Else
				UMainDMD.ModifyScene00 "attract1",DisplayString1, DisplayString2 
			End If
		End If 
	End If

	If addcreditsstatus=1 Then 
		Displaybuzy=1 : addcreditsstatus=0 
		DisplayString1="ADD  CREDITS" : DisplayString2="  TO  PLAY  "
		DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0
		If FlexDMD Then
			UMainDMD.ModifyScene00 "attract1",DisplayString1, DisplayString2 
		End If 
	End If

	If ballsavedstatus=1 Then
		ballsavedstatus=0
		DOF 219,2
'Debug.print "DOF 119, 2" 'apophis
		If MultiballActive=0 Then
			Displaybuzy=1 
			DisplayString1=" BALL SAVED " : DisplayString2="            "
			DisplayEffect=6 : DisplayCounter=0 : DisplayBlank=0
			If FlexDMD Then UMainDMD.DisplayScene00Ex  vid3, Displaystring1, 0, 12, DisplayString2 , 15, 1, 11, 1600, 14 
		End If
	End If


	If Displaybuzy=0 And JackPotScoringStatus=1 Then
		Displaybuzy=1
		JackPotScoringStatus=0
		DisplayString1="  JACKPOT   "
		DisplayString2=FormatScore(JackPotScoring*P1scorebonus)
		DisplayEffect=1
		DisplayCounter=0
		DisplayBlank=0
		If FlexDMD Then  UMainDMD.DisplayScene00Ex vid8, Displaystring1, 0, 12, Flexscore , 15, 1, 10, 2000, 14 
	End If

	If Displaybuzy=0 And FlyAgainStatus=1 Then
		Displaybuzy=1
		FlyAgainStatus=0
		DisplayString1="DONT GO AWAY"
		DisplayString2=" EXTRA BALL "
		DisplayEffect=4
		DisplayCounter=0
		DisplayBlank=0
		If FlexDMD Then  UMainDMD.DisplayScene00Ex vid4, Displaystring1, 0, 12, DisplayString2 , 15, 1, 10, 2000, 14 
	End If

	If Displaybuzy=0 And Doubleinfo=1 Then Displaybuzy=1 : Doubleinfo=0 :  DisplayString1="ALL  SCORING" : DisplayString2="   DOUBLE   " : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 : PlaySound "bonusup", 0, 0.5*BackGlassVolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) : If FlexDMD Then  UMainDMD.DisplayScene00Ex vid5, Displaystring1, 0, 12, DisplayString2 , 15, 1, 10, 2000, 14 : End If
	If Displaybuzy=0 And Doubleinfo=2 Then Displaybuzy=1 : Doubleinfo=0 :  DisplayString1="ALL  SCORING" : DisplayString2="   TRIPLE   " : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 : PlaySound "bonusup", 0, 0.5*BackGlassVolumeDial, AudioPan(dt004), 0.05,0,0,1,AudioFade(dt004) : If FlexDMD Then  UMainDMD.DisplayScene00Ex vid5, Displaystring1, 0, 12, DisplayString2 , 15, 1, 10, 2000, 14 : End If


	If  replaygoaldisplaystatus=1 Then
		Displaybuzy=1
		replaygoaldisplaystatus=0
		DisplayString1=" REPLAY  AT "
		DisplayString2=FormatScore (ReplayGoals)
		DisplayEffect=4
		DisplayCounter=0
		DisplayBlank=0
		If FlexDMD Then  UMainDMD.DisplayScene00Ex vid4, Displaystring1, 0, 12, Flexscore , 15, 1, 10, 2000, 14 
	End If




	' ball locked
	
	If Displaybuzy=0 And BallIsLockedStatus=1 Then
		Displaybuzy=1
		BallIsLockedStatus=0
		If LockedBalls=1 Then
			DisplayString1="BALLCAPTURED"
			DisplayString2="            "
			DisplayEffect=6
			If FlexDMD Then  UMainDMD.DisplayScene00Ex "spots2.wmv", "B A L L  1", 0, 12, "L O C K E D" , 15, 1, 11, 2000, 14 
		Else
			DisplayString1="ONE MORE FOR"
			DisplayString2=" MULTIBALL  "
			DisplayEffect=4
			If FlexDMD Then  UMainDMD.DisplayScene00Ex "spots2.wmv", "LOCK 1 MORE", 0, 12, "FOR MULTIBALL" , 15, 1, 11, 2000, 14 
		End if
		DisplayCounter=0
		DisplayBlank=0
	End If

	'	MULTIBALL	
	If Displaybuzy=0 And MultiBallStatus=1 Then
		Displaybuzy=1
		MultiBallStatus=0
		DisplayString1=" MULTIBALL  "
		DisplayString2="            "
		DisplayEffect=6
		DisplayCounter=0
		DisplayBlank=0
		If FlexDMD Then  UMainDMD.DisplayScene00Ex "multi1.wmv", "GET READY FOR", 0, 12, "MULTIBALL" , 15, 1, 10, 5000, 14 
	End If


	' MaxBalls
	
	If Displaybuzy=0 And MaxBallsStatus=1 Then
		PlaySound "Collected", 0, 0.5*BackGlassVolumeDial, AudioPan(opengate), 0.05,0,0,1,AudioFade(opengate)
		PlaySound "XWingexplode", 5, .7*BackGlassVolumeDial, AudioPan(sw7), 0.05,0,0,1,AudioFade(sw7)
		PlaySound "protoss", 0, .7*BackGlassVolumeDial, AudioPan(drain), 0.05,0,0,1,AudioFade(drain)
		Displaybuzy=1
		MaxBallsStatus=0
		DisplayString1=" MAX  BALLS "
		DisplayString2= FormatScore(MaxBallsScoring*P1scorebonus)
		DisplayEffect=4
		DisplayCounter=0
		DisplayBlank=0
		If FlexDMD Then  UMainDMD.DisplayScene00Ex "coins1.wmv", "MAX BALLS SCORE", 0, 12, Flexscore , 15, 1, 11, 2800, 14 
	End If


	'skillshot
'	If Displaybuzy=0 And SkillShotStatus=1 Then
	If SkillShotStatus=1 Then
		Displaybuzy=1
		SkillShotStatus=0
		DisplayString1=" SKILL SHOT "
		DisplayString2= FormatScore(SkillShotScoring*P1scorebonus)
		DisplayEffect=2
		DisplayCounter=0
		DisplayBlank=0
		If FlexDMD Then  UMainDMD.DisplayScene00Ex vid8, DisplayString1, 0, 12, Flexscore , 15, 1, 11, 2000, 14 
	End If
	If SkillShotStatus=2 Then
		Displaybuzy=1
		SkillShotStatus=0
		DisplayString1=" FLYING ACE "
		DisplayString2= FormatScore(2 * SkillShotScoring*P1scorebonus)
		DisplayEffect=2
		DisplayCounter=0
		DisplayBlank=0
		If FlexDMD Then  UMainDMD.DisplayScene00Ex vid8, DisplayString1, 0, 12, Flexscore , 15, 1, 11, 2000, 14 
	End If
	'secretpassage
	If Displaybuzy=0 And SecretStatus=1 Then
		Displaybuzy=1
		SecretStatus=0
		DisplayString1="FLYING SKILL"
		DisplayString2= FormatScore(SecretScoring*P1scorebonus)
		DisplayEffect=4 
		DisplayCounter=0
		DisplayBlank=0
		If FlexDMD Then  UMainDMD.DisplayScene00Ex vid8, DisplayString1, 0, 12, Flexscore , 15, 1, 11, 2000, 14 
	End If
	'million for fullbonus
	If Displaybuzy=0 And FullbonusStatus=1 Then
		Displaybuzy=1
		FullbonusStatus=0
		DisplayString1=" MAX  BONUS "
		DisplayString2= FormatScore(1000000)
		DisplayEffect=3 
		DisplayCounter=0
		DisplayBlank=0
		If FlexDMD Then  UMainDMD.DisplayScene00Ex vid8, DisplayString1, 0, 12, Flexscore , 15, 1, 11, 2000, 14 
	End If
	'moreballs !
	If Displaybuzy=0 And quickMBstatus=1 Then 
		Displaybuzy=1 : 
		quickMBstatus=0 :    
		DisplayString1="    QUICK    " : 
		DisplayString2=" MULITIBALL " : 
		DisplayEffect=4 :
		DisplayCounter=0
		DisplayBlank=0
		If FlexDMD Then  UMainDMD.DisplayScene00Ex vid6, DisplayString1, 0, 12, DisplayString2 , 15, 1, 11, 2000, 14 
	End If


	'extraball
	If Displaybuzy=0 And ExtraBallStatus=1 Then
		PlaySound "extra2ball",0,BackGlassVolumeDial
		Displaybuzy=1
		extraballstatus=0
		DisplayString1=" EXTRA BALL "
		DisplayString2="            "
		If extraextraball>0 Then 		DisplayString2= FormatScore(extraextraball+1)
		DisplayEffect=6 
		DisplayCounter=0 
		DisplayBlank=0
 		If FlexDMD Then  UMainDMD.DisplayScene00Ex vid4, "E X T R A", 0, 12, "B A L L" , 15, 1, 10, 2000, 14 
	End If


	'commandcenter enterlight
	'new mission
	If Displaybuzy=0 And enterlightstatus=1 Then
		Displaybuzy=1
		enterlightstatus=0
		DisplayString1="NEW  MISSION"
		DisplayString2="            "
		DisplayEffect=6
		DisplayCounter=0 
		DisplayBlank=0
		If FlexDMD Then  UMainDMD.DisplayScene00Ex "door2.wmv", "STARTING", 0, 12, "MISSIONS" , 15, 1, 11, 1600, 14 

	End If



	'mystery
	If Displaybuzy=0 And mysterystatus=1 Then 
		DisplayString1="   MYSTERY   " 
		Displaybuzy=1
		mysterystatus=0
		DisplayEffect=7
		DisplayCounter=0
		DisplayBlank=0
		If mysteryreward=1 Then	DisplayString2= FormatScore(mysteryscoring*5*P1scorebonus) : If FlexDMD Then  UMainDMD.DisplayScene00Ex "gold1.jpg", DisplayString1, 0, 12, Flexscore , 15, 1, 11, 1600, 14 
		If mysteryreward=3 Then	DisplayString2= FormatScore(mysteryscoring*P1scorebonus)  : If FlexDMD Then  UMainDMD.DisplayScene00Ex "gold1.jpg", DisplayString1, 0, 12, Flexscore , 15, 1, 11, 1600, 14  
		If mysteryreward=4 Then	DisplayString2= FormatScore(mysteryscoring*2*P1scorebonus)  : If FlexDMD Then  UMainDMD.DisplayScene00Ex "gold1.jpg", DisplayString1, 0, 12, Flexscore , 15, 1, 11, 1600, 14 
		If mysteryreward=2 Or mysteryreward=9 Then	DisplayString2="EXTRAMISSION" : If FlexDMD Then  UMainDMD.DisplayScene00Ex "door2.wmv", DisplayString1, 0, 12, DisplayString2 , 15, 1, 11, 1600, 14 
		If mysteryreward=5 Then	DisplayString2=" ADV  BONUS " : If FlexDMD Then  UMainDMD.DisplayScene00Ex "yoda1.jpg", DisplayString1, 0, 12, DisplayString2 , 15, 1, 11, 1600, 14 
		If mysteryreward=6 Then	DisplayString2="ADV 2LETTERS" : If FlexDMD Then  UMainDMD.DisplayScene00Ex "yoda1.jpg", DisplayString1, 0, 12, DisplayString2 , 15, 1, 11, 1600, 14 
		If mysteryreward=7 Then	DisplayString2="2 X MISSIONS" : If FlexDMD Then  UMainDMD.DisplayScene00Ex "door2.wmv", DisplayString1, 0, 12, DisplayString2 , 15, 1, 11, 1600, 14
		If mysteryreward=8 Then	DisplayString2="2 X MISSIONS" : If FlexDMD Then  UMainDMD.DisplayScene00Ex "door2.wmv", DisplayString1, 0, 12, DisplayString2 , 15, 1, 11, 1600, 14
		If mysteryreward=10 Then DisplayString2="3 X MISSIONS" : If FlexDMD Then  UMainDMD.DisplayScene00Ex "door2.wmv", DisplayString1, 0, 12, DisplayString2 , 15, 1, 11, 1600, 14
		If mysteryreward=11 Then DisplayString2="2 X MISSIONS" : If FlexDMD Then  UMainDMD.DisplayScene00Ex "door2.wmv", DisplayString1, 0, 12, DisplayString2 , 15, 1, 11, 1600, 14
		If mysteryreward=12 Then DisplayString2="3 X MISSIONS" : If FlexDMD Then  UMainDMD.DisplayScene00Ex "door2.wmv", DisplayString1, 0, 12, DisplayString2 , 15, 1, 11, 1600, 14
		If mysteryreward=13 Then DisplayString2="ADV 4LETTERS" : If FlexDMD Then  UMainDMD.DisplayScene00Ex "door2.wmv", DisplayString1, 0, 12, DisplayString2 , 15, 1, 11, 1600, 14
		If mysteryreward=14 Then DisplayString2="LOCK IS  LIT" : If FlexDMD Then  UMainDMD.DisplayScene00Ex "door2.wmv", DisplayString1, 0, 12, DisplayString2 , 15, 1, 11, 1600, 14
		If mysteryreward=15 Then DisplayString2="LOCK IS  LIT" : If FlexDMD Then  UMainDMD.DisplayScene00Ex "door2.wmv", DisplayString1, 0, 12, DisplayString2 , 15, 1, 11, 1600, 14
		
	End If

	
	If Displaybuzy=0 And LockIsLitStatus=1 Then
		Displaybuzy=1
		LockIsLitStatus=0
		DisplayString1="LOCK IS LIT "
		DisplayString2="            "
		DisplayEffect=6
		DisplayCounter=0
		DisplayBlank=0 
		If FlexDMD Then  UMainDMD.DisplayScene00Ex "greenlight.wmv", "L O C K", 0, 12, "I S   L I T" , 15, 1, 11, 1600, 14 

	End If

	If Displaybuzy=0 And LockIsLitStatus=2 Then
		Displaybuzy=1
		LockIsLitStatus=0
		DisplayString1=" MULTIBALL  "
		DisplayString2="  IS  LIT   "
		DisplayEffect=4
		DisplayCounter=0
		DisplayBlank=0 
		If FlexDMD Then  UMainDMD.DisplayScene00Ex "greenlight.wmv", "MULTIBALL", 0, 12, "I S   L I T" , 15, 1, 11, 1600, 14 

	End If


	If Displaybuzy=0 And ComboStatus=1 Then
		Displaybuzy=1
		ComboStatus=0
		DisplayString1="COMBO  SCORE"
		DisplayString2= FormatScore((ScoringRightRamp+ScoringLeftRamp)*P1scorebonus)
		DisplayEffect=2
		DisplayCounter=0
		DisplayBlank=0 
		If FlexDMD Then  UMainDMD.DisplayScene00Ex  vid3, Displaystring1, 0, 12, FlexScore , 15, 1, 11, 1600, 14 
	End If


	If Displaybuzy=0 And LeftRampStatus=1 Then
		Displaybuzy=1
		LeftRampStatus=0
		DisplayString1=" LEFT  RAMP "
		DisplayString2= FormatScore(ScoringLeftRamp*P1scorebonus)
		DisplayEffect=2
		DisplayCounter=0
		DisplayBlank=0
		If FlexDMD Then  UMainDMD.DisplayScene00Ex vid2, Displaystring1, 0, 12, FlexScore , 15, 1, 11, 1600, 14 
	End If


	If Displaybuzy=0 And RightRampStatus=1 Then Displaybuzy=1 : RightRampStatus=0 :  DisplayString1=" RIGHT RAMP " : DisplayString2= FormatScore(ScoringRightRamp*P1scorebonus) : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 : 	If FlexDMD Then UMainDMD.DisplayScene00Ex vid2, Displaystring1, 0, 12, FlexScore , 15, 1, 11, 1600, 14 : End If

	If Displaybuzy=0 And Mission1status=1 Then Displaybuzy=1 : Mission1status=0 :    DisplayString1="GET 12CREDIT": DisplayString2= FormatScore(Mission1scoring*P1scorebonus) : DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex vid5, Displaystring1, 0, 12, FlexScore , 15, 1, 11, 1600, 14 : End If
	If Displaybuzy=0 And Mission2status=1 Then Displaybuzy=1 : Mission2status=0 :    DisplayString1="SECRET  BASE" : DisplayString2= FormatScore(Mission2scoring*P1scorebonus) : DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex vid5, Displaystring1, 0, 12, FlexScore , 15, 1, 11, 1600, 14 : End If
	If Displaybuzy=0 And Mission3status=1 Then Displaybuzy=1 : Mission3status=0 :    DisplayString1="REFUEL  SHIP" : DisplayString2= FormatScore(Mission3scoring*P1scorebonus) : DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex vid5, Displaystring1, 0, 12, FlexScore , 15, 1, 11, 1600, 14 : End If
	If Displaybuzy=0 And Mission4status=1 Then Displaybuzy=1 : Mission4status=0 :    DisplayString1=" SUPPLY RUN " : DisplayString2= FormatScore(Mission4scoring*P1scorebonus) : DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex vid5, Displaystring1, 0, 12, FlexScore , 15, 1, 11, 1600, 14 : End If
	If Displaybuzy=0 And Mission5status=1 Then Displaybuzy=1 : Mission5status=0 :    DisplayString1="HELP  NEEDED" : DisplayString2= FormatScore(Mission5scoring*P1scorebonus) : DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex vid5, Displaystring1, 0, 12, FlexScore , 15, 1, 11, 1600, 14 : End If
	If Displaybuzy=0 And Mission6status=1 Then Displaybuzy=1 : Mission6status=0 :    DisplayString1=" ASTEROIDS  " : DisplayString2= FormatScore(Mission6scoring*P1scorebonus) : DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex vid5, Displaystring1, 0, 12, FlexScore , 15, 1, 11, 1600, 14 : End If
	
	If Displaybuzy=0 And Mission7status=1 Then Displaybuzy=1 : Mission7status=2 :    DisplayString1="EVASIVE  ONE" : DisplayString2="  CAPTURED  " : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 : HurryBonus=1 : If FlexDMD Then  UMainDMD.DisplayScene00Ex vid5, Displaystring1, 0, 12, DisplayString2 , 15, 1, 11, 1600, 14 :  End If
	If Displaybuzy=0 And Mission7status=2 Then Displaybuzy=1 : Mission7status=0 :    DisplayString1="   REWARD   " : DisplayString2= FormatScore(Mission7scoring*P1scorebonus) : DisplayEffect=4 : HurryBonus=0 : PlaySound SoundFX("spotpassage",DOFContactors), 0, .85*VolumeDial, AudioPan(enterlight), 0.05,0,0,1,AudioFade(enterlight) : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex vid5, Displaystring1, 0, 12, FlexScore , 15, 1, 11, 1600, 14 : End If

	If Displaybuzy=0 And Mission8status=1 Then Displaybuzy=1 : Mission8status=0 :    DisplayString1="   AMBUSH   " : DisplayString2= FormatScore(Mission8scoring*P1scorebonus) : DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex vid5, Displaystring1, 0, 12, FlexScore , 15, 1, 11, 1600, 14 : End If

	If Displaybuzy=0 And ExtraBallIsLitStatus=1 Then Displaybuzy=1 : ExtraBallIsLitStatus=0 : DisplayString1="  GET  THE  " : DisplayString2=" EXTRA BALL " : DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex vid4, Displaystring1, 0, 12, DisplayString2 , 15, 1, 11, 2000, 14 : End If


	If DisplayPause=0 Then	
		If Displaybuzy=0 And GetReadyToPlay=1 Then GetReadyToPlay=2 : DisplayPause=150 : Displaybuzy=1 : DisplayString1="GET READY TO" : DisplayString2=" PLAY  BALL " : DisplayEffect=5 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex "mando1.jpg", Displaystring1, 0, 12, DisplayString2 , 15, 1, 11, 1500, 14 : End If
		If Displaybuzy=0 And GetReadyToPlay=2 Then GetReadyToPlay=3 : DisplayPause=150 : Displaybuzy=1 : DisplayString1=" SHOOT  THE " : DisplayString2="BALL TOSTART" : DisplayEffect=5 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex "mando1.jpg", Displaystring1, 0, 12, DisplayString2 , 15, 1, 11, 1500, 14 : End If
		If Displaybuzy=0 And GetReadyToPlay=3 Then GetReadyToPlay=4 : DisplayPause=150 : Displaybuzy=1 : DisplayString1="  STARWARS  " : DisplayString2="BOUNTYHUNTER" : DisplayEffect=5 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex "mando1.jpg", Displaystring1, 0, 12, DisplayString2 , 15, 1, 11, 1500, 14 : End If
		If Displaybuzy=0 And GetReadyToPlay=4 Then GetReadyToPlay=5 : DisplayPause=150 : Displaybuzy=1 : DisplayString1="RAMPS  LIGHT" : DisplayString2="    LOCK    " : DisplayEffect=5 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex "mando1.jpg", Displaystring1, 0, 12, DisplayString2 , 15, 1, 11, 1500, 14 : End If
		If Displaybuzy=0 And GetReadyToPlay=5 Then GetReadyToPlay=6 : DisplayPause=150 : Displaybuzy=1 : DisplayString1="  SPINNERS  " : DisplayString2="FOR  MISSION" : DisplayEffect=5 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex "mando1.jpg", Displaystring1, 0, 12, DisplayString2 , 15, 1, 11, 1500, 14 : End If
		If Displaybuzy=0 And GetReadyToPlay=6 Then GetReadyToPlay=7 : DisplayPause=150 : Displaybuzy=1 : DisplayString1="TARGETS GIVE" : DisplayString2="FAST SCORING" : DisplayEffect=5 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex "mando1.jpg", Displaystring1, 0, 12, DisplayString2 , 15, 1, 11, 1500, 14 : End If
		If Displaybuzy=0 And GetReadyToPlay=7 Then GetReadyToPlay=8 : Displaybuzy=1 : DisplayString1="ALL  SCORING" : DisplayString2="  INCREASE  " : DisplayEffect=5 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex "mando1.jpg", Displaystring1, 0, 12, DisplayString2 , 15, 1, 11, 1500, 14 : End If
		If Displaybuzy=0 And GetReadyToPlay=8 Then GetReadyToPlay=9 : DisplayPause=150 : Displaybuzy=1 : DisplayString1=" NEXT TIME  " : DisplayString2="THEY GET HIT" : DisplayEffect=5 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex "mando1.jpg", Displaystring1, 0, 12, DisplayString2 , 15, 1, 11, 1500, 14 : End If
		If Displaybuzy=0 And GetReadyToPlay=9 Then GetReadyToPlay=1 : DisplayPause=150 : Displaybuzy=1 : DisplayString1="CAN YOU GET " : DisplayString2=" BEST SCORE " : DisplayEffect=5 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex "mando1.jpg", Displaystring1, 0, 12, DisplayString2 , 15, 1, 11, 1500, 14 : End If
	End If
	If Displaybuzy=0 And DisplayPause>0 Then DisplayPause=DisplayPause-1 : End If

	'*** WAITING FOR Plunger001
	If Displaybuzy=0 And shoottheballstatus=1 Then shoottheballstatus=7 : Displaybuzy=1 : DisplayString1=" AIM FOR THE" : DisplayString2=" SKILLSHOT  " : DisplayEffect=2 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex vid6, Displaystring1, 0, 12, DisplayString2 , 15, 1, 11, 1500, 11 : End If
	If Displaybuzy=0 And shoottheballstatus=2 Then shoottheballstatus=7 : Displaybuzy=1 : DisplayString1=" SHOOT  THE " : DisplayString2="BALL TOSTART" : DisplayEffect=2 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex vid6, Displaystring1, 0, 12, DisplayString2 , 15, 1, 11, 1500, 11 : End If
	If Displaybuzy=0 And shoottheballstatus=3 Then shoottheballstatus=7 : Displaybuzy=1 : DisplayString1=" NOONE HERE " : DisplayString2="HUMAN  ERROR" : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex vid6, Displaystring1, 0, 12, DisplayString2 , 15, 1, 11, 1500, 11 : End If
	If Displaybuzy=0 And shoottheballstatus=4 Then shoottheballstatus=7 : Displaybuzy=1 : DisplayString1="  USE THE   " : DisplayString2="   FORCE !  " : DisplayEffect=2 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex vid6, Displaystring1, 0, 12, DisplayString2 , 15, 1, 11, 1500, 11 : End If

	'**JackPot
	If Displaybuzy=0 And JackPotStatus=1 Then Displaybuzy=1 : JackPotStatus=0 : DisplayString1="JACKPOTVALUE" : DisplayString2= FormatScore(JackPotScoring*P1scorebonus) : DisplayEffect=2 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex vid6, Displaystring1, 0, 12, Flexscore , 15, 1, 11, 2000, 14 :  End If
	' Droptargets
	If Displaybuzy=0 And DroptargetsStatus=1 Then Displaybuzy=1 : DroptargetsStatus=0 : DisplayString1="CREDIT VALUE" : DisplayString2= FormatScore(DroptargetsScoring*P1scorebonus) : DisplayEffect=2 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex vid6, Displaystring1, 0, 12, Flexscore , 15, 1, 11, 2000, 14 :  End If
	'bumpers
	If Displaybuzy=0 And BumperStatus=1 Then Displaybuzy=1 : BumperStatus=0 : DisplayString1="BUMPER VALUE" : DisplayString2= FormatScore(BumperScoring*P1scorebonus) : DisplayEffect=2 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex vid6, Displaystring1, 0, 12, Flexscore , 15, 1, 11, 2000, 14 :  End If

	'attractmode
	If Displaybuzy=0 And attractModeStatus=1 Then Displaybuzy=1 : attractModeStatus=2 : DisplayString1=" GAME  OVER " : DisplayString2="            " : DisplayEffect=6 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1"," ", "GAME OVER " : End If

	If Credits = 0 Then
		If Displaybuzy=0 And attractModeStatus=2 Then Displaybuzy=1 : attractModeStatus=3 : DisplayString1="            " : DisplayString2="INSERT  COIN" : DisplayEffect=6 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, DisplayString2 : End If
	Else
		If Displaybuzy=0 And attractModeStatus=2 Then Displaybuzy=1 : attractModeStatus=3 : DisplayString1="1UP TO START" : DisplayString2="  NEW GAME  " : DisplayEffect=2 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, DisplayString2 : End If
	End If

	If Displaybuzy=0 And attractModeStatus=3 Then
		If ShowScores(0) < 1 Then
			attractModeStatus=7		
		Else
			If ShowScores(0) = 1 Then
				DisplayString1="LAST  PLAYER"
			Else
				DisplayString1="  PLAYER 1  "
			End If
			DisplayString2=FormatScore(ShowScores(1))
			Displaybuzy=1 : attractModeStatus=4
			DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0
			If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, Flexscore
		End If
	End If
	If Displaybuzy=0 And attractModeStatus=4 Then
		If ShowScores(0) < 2 Then
			attractModeStatus=7
		Else
			DisplayString1="  PLAYER 2  "
			DisplayString2=FormatScore(ShowScores(2))
			Displaybuzy=1 : attractModeStatus=5
			DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0
			If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, Flexscore
		End If
	End If
	If Displaybuzy=0 And attractModeStatus=5 Then
		If ShowScores(0) < 3 Then
			attractModeStatus=7
		Else
			DisplayString1="  PLAYER 3  "
			DisplayString2=FormatScore(ShowScores(3))
			Displaybuzy=1 : attractModeStatus=6
			DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0
			If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, Flexscore
		End If
	End If
	If Displaybuzy=0 And attractModeStatus=6 Then
		If ShowScores(0) < 4 Then
			attractModeStatus=7
		Else
			DisplayString1="  PLAYER 4  "
			DisplayString2=FormatScore(ShowScores(4))
			Displaybuzy=1 : attractModeStatus=7
			DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0
			If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, Flexscore
		End If
	End If


'		ShowScores(1)=PlayerSaved(1,0)
'		ShowScores(2)=PlayerSaved(2,0)
'		ShowScores(3)=PlayerSaved(3,0)
'		ShowScores(4)=PlayerSaved(4,0)
'		ShowScores(CurrentPlayer) = P1score
'		ShowScores(0)=PlayersPlaying



	If Displaybuzy=0 And attractModeStatus=7 Then Displaybuzy=1 : attractModeStatus=8   : DisplayString1="            " : DisplayString2="            " : DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, DisplayString2 : End If
	If Displaybuzy=0 And attractModeStatus=8 Then Displaybuzy=1 : attractModeStatus=9   : DisplayString1="  TARTZANI  " : DisplayString2="  PRESENTS  " : DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, DisplayString2 : End If
	If Displaybuzy=0 And attractModeStatus=9 Then Displaybuzy=1 : attractModeStatus=10   : DisplayString1="  STARWARS  " : DisplayString2="BOUNTYHUNTER" : DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, DisplayString2 : End If
	If Displaybuzy=0 And attractModeStatus=10 And TodaysTopScore=0 Then attractModeStatus=12 
	If Displaybuzy=0 And attractModeStatus=10 Then Displaybuzy=1 : attractModeStatus=11   : DisplayString1="TODAYS  BEST" : DisplayString2=Formatscore(TodaysTopScore) : DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, Flexscore : End If
	If Displaybuzy=0 And attractModeStatus=11 Then Displaybuzy=1 : attractModeStatus=12   : DisplayString1="            " : DisplayString2="            " : DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, DisplayString2 : End If
	If Displaybuzy=0 And attractModeStatus=12 Then Displaybuzy=1 : attractModeStatus=13   : DisplayString1="CHAMPION "&highscorename(1)& space(3) : DisplayString2=FormatScore(hscore(1)) : DisplayEffect=2 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, Flexscore : End If
	If Displaybuzy=0 And attractModeStatus=13 Then Displaybuzy=1 : attractModeStatus=14   : DisplayString1="CHAMPION "&highscorename(1)& space(3) : DisplayString2=FormatScore(hscore(1)) : DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, Flexscore : End If

	If Displaybuzy=0 And attractModeStatus=14 Then Displaybuzy=1 : attractModeStatus=15   : DisplayString1="2'HUNTER "&highscorename(2)& space(3) : DisplayString2=FormatScore(hscore(2)) : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, Flexscore : End If
	If Displaybuzy=0 And attractModeStatus=15 Then Displaybuzy=1 : attractModeStatus=16  : DisplayString1="3'HUNTER "&highscorename(3)& space(3) : DisplayString2=FormatScore(hscore(3)) : DisplayEffect=2 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, Flexscore : End If
	If Displaybuzy=0 And attractModeStatus=16 Then Displaybuzy=1 : attractModeStatus=17 : DisplayString1="4'HUNTER "&highscorename(4)& space(3) : DisplayString2=FormatScore(hscore(4)) : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, Flexscore : End If
	If Displaybuzy=0 And attractModeStatus=17 Then Displaybuzy=1 : attractModeStatus=18 : DisplayString1="5'HUNTER "&highscorename(5)& space(3) : DisplayString2=FormatScore(hscore(5)) : DisplayEffect=2 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, Flexscore : End If

	If Displaybuzy=0 And attractModeStatus=18 Then Displaybuzy=1 : attractModeStatus=19 : DisplayString1="            " : DisplayString2="            " : DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, DisplayString2 : End If
	If Displaybuzy=0 And attractModeStatus=19 Then Displaybuzy=1 : attractModeStatus=20 : DisplayString1="LOOKING  FOR" : DisplayString2="NEW  HUNTERS" : DisplayEffect=1 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, DisplayString2 : End If
	If Displaybuzy=0 And attractModeStatus=20 Then Displaybuzy=1 : attractModeStatus=21 : DisplayString1=" DO YOU GOT " : DisplayString2="WHAT WE NEED" : DisplayEffect=1 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, DisplayString2 : End If
	If Displaybuzy=0 And attractModeStatus=21 Then Displaybuzy=1 : attractModeStatus=22 : DisplayString1="            " : DisplayString2="            " : DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, DisplayString2 : End If

	If Displaybuzy=0 And attractModeStatus=22 Then Displaybuzy=1 : attractModeStatus=23 : DisplayString1="  CREDITS   " : DisplayString2=FormatScore(Credits) : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, DisplayString2 : End If
	If Credits = 0 Then
		If Displaybuzy=0 And attractModeStatus=23 Then Displaybuzy=1 : attractModeStatus=24 : DisplayString1="            " : DisplayString2="INSERT  COIN" : DisplayEffect=6 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, DisplayString2 : End If
	Else
		If Displaybuzy=0 And attractModeStatus=23 Then Displaybuzy=1 : attractModeStatus=24 : DisplayString1="1UP TO START" : DisplayString2="  NEW GAME  " : DisplayEffect=2 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, DisplayString2 : End If
	End If
	If Displaybuzy=0 And attractModeStatus=24 Then Displaybuzy=1 : attractModeStatus=25 : DisplayString1=" REPLAY  AT " : DisplayString2=FormatScore(ReplayGoals) : DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, Flexscore : End If
	If Displaybuzy=0 And attractModeStatus=25 Then Displaybuzy=1 : attractModeStatus=26 : DisplayString1="TOTAL  GAMES" : DisplayString2=FormatScore(GamesPlayd) : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",Flexscore,DisplayString1  : End If
	If Displaybuzy=0 And attractModeStatus=26 Then Displaybuzy=1 : attractModeStatus=27 : DisplayString1="            " : DisplayString2="            " : DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, DisplayString2 : End If
	If Displaybuzy=0 And attractModeStatus=27 Then Displaybuzy=1 : attractModeStatus=28 : DisplayString1=" GAME  OVER " : DisplayString2="            " : DisplayEffect=6 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1","GAME OVER ", " " : End If
	If Displaybuzy=0 And attractModeStatus=28 Then Displaybuzy=1 : attractModeStatus=29 : DisplayString1=" GAME  OVER " : DisplayString2="            " : DisplayEffect=6 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1"," ", "GAME OVER " : End If
	If Displaybuzy=0 And attractModeStatus=29 Then Displaybuzy=1 : attractModeStatus=30 : DisplayString1="            " : DisplayString2="            " : DisplayEffect=6 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.ModifyScene00 "attract1",DisplayString1, DisplayString2 : End If
	If Displaybuzy=0 And attractModeStatus=30 Then attractModeStatus=1

	'*********
	'info mode
	'*********


	If Displaybuzy=0 And infostatus=1 Then
		If BossActive Then
			Displaybuzy=1 : infostatus=2 
			DisplayString1="  BOSS  IS  " 
			DisplayString2="   ACTIVE   "
			DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 
			If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex "mando3.jpg", Displaystring1, 0, 12, Displaystring2 , 15, 1, 10, 1500, 14 
		Else
			Displaybuzy=1 : infostatus=2 
			DisplayString1=" BOSS  GAME " 
			i =  3-Missions4Evasive : If i < 1 Then i = 1
			DisplayString2= "+ " & i & " MISSIONS"
			DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 
			If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex "mando3.jpg", Displaystring1, 0, 12, "+" & i & " Missions" , 15, 1, 10, 1500, 14 
		End If
	End If
	If Displaybuzy=0 And infostatus=2 Then Displaybuzy=1 : infostatus=3 :      DisplayString1="  MISSIONS  " : DisplayString2=" R+Y LIGHTS " : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex "mando3.jpg", Displaystring1, 0, 12, DisplayString2 , 15, 1, 10, 1500, 14 : End If
	If Displaybuzy=0 And infostatus=3 Then  ' missions
		if mission(8)=1 and missioninfo=7 then Displaybuzy=1 : infostatus=4  : DisplayString1="   AMBUSH   " : DisplayString2="  5 TARGETS " : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex "mando3.jpg", Displaystring1, 0, 12, DisplayString2 , 15, 1, 10, 1500, 14 : End If
		if mission(7)=1 and missioninfo=6 then Displaybuzy=1 : missioninfo=7 : DisplayString1=" NEW BOUNTY " : DisplayString2=" CATCH  HIM " : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex "mando3.jpg", Displaystring1, 0, 12, DisplayString2 , 15, 1, 10, 1500, 14 : End If
		if mission(6)=1 and missioninfo=5 then Displaybuzy=1 : missioninfo=6 : DisplayString1=" ASTEROIDS  " : DisplayString2="BUMPER  RIDE" : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex "mando3.jpg", Displaystring1, 0, 12, DisplayString2 , 15, 1, 10, 1500, 14 : End If
		if mission(5)=1 and missioninfo=4 then Displaybuzy=1 : missioninfo=5 : DisplayString1=" HELP NEEDED" : DisplayString2="BOTH SPINNER" : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex "mando3.jpg", Displaystring1, 0, 12, DisplayString2 , 15, 1, 10, 1500, 14 : End If
		if mission(4)=1 and missioninfo=3 then Displaybuzy=1 : missioninfo=4 : DisplayString1=" SUPPLY RUN " : DisplayString2=" BOTH RAMPS " : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex "mando3.jpg", Displaystring1, 0, 12, DisplayString2 , 15, 1, 10, 1500, 14 : End If
		if mission(3)=1 and missioninfo=2 then Displaybuzy=1 : missioninfo=3 : DisplayString1="   REFUEL   " : DisplayString2=" R  STATION " : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex "mando3.jpg", Displaystring1, 0, 12, DisplayString2 , 15, 1, 10, 1500, 14 : End If
		if mission(2)=1 and missioninfo=1 then Displaybuzy=1 : missioninfo=2 : DisplayString1=" SECRETBASE " : DisplayString2=" ENTER BASE " : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex "mando3.jpg", Displaystring1, 0, 12, DisplayString2 , 15, 1, 10, 1500, 14 : End If
		if mission(1)=1 and missioninfo=0 then Displaybuzy=1 : missioninfo=1 : DisplayString1="  COLLECT   " : DisplayString2=" GET CREDITS" : DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex "mando3.jpg", Displaystring1, 0, 12, DisplayString2 , 15, 1, 10, 1500, 14 : End If
		if Displaybuzy=0 Then missioninfo=missioninfo+1 
		if missioninfo=8 Then infostatus=4
	End If


	If Displaybuzy=0 And infostatus=4 Then
		If MissionsDoneThisBall>4 Then
			infostatus=5 
		Else
			missioninfo=0
			
			Displaybuzy=1 : infostatus=5 
			DisplayString1=" LITE E / B " 
			DisplayString2=" +"& CStr(abs (5-MissionsDoneThisBall)) & " MISSIONS  "
			DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0
			If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex "mando3.jpg", "LITE EXTRABALL", 0, 12, DisplayString2 , 15, 1, 10, 1500, 14 
		End If
	End If

	If Displaybuzy=0 And infostatus=5 Then
		Displaybuzy=1 : infostatus=6 : JackPotStatus=1 
		If LockedBalls=2 Then DisplayString2="LOCK 1  MORE"
		If LockedBalls=1 Then DisplayString2="LOCK 2  MORE"
		If LockedBalls=0 Then DisplayString2="LOCK 3  MORE"
		DisplayString1="  MULTIBALL " 
		DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0
		If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex "mando3.jpg", DisplayString1, 0, 12, DisplayString2 , 15, 1, 10, 1500, 14 
	End If


	If Displaybuzy=0 And infostatus=6 Then Displaybuzy=1 : infostatus=7 :      DisplayString1="TIE  FIGHTER" : DisplayString2="TURNS=" & CStr(abs (TotalTurned)) & "       " : DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 : If FlexDMD Then  UMainDMD.DisplayScene00Ex "mando3.jpg", DisplayString1, 0, 12, "TURNS=" & CStr(abs (TotalTurned)) , 15, 1, 10, 1500, 14 : End If

	If Displaybuzy=0 And infostatus=7 Then  ' P1score
		If PlayersPlaying = 1 Then
			infostatus=8
		Else
			Displaybuzy=1 : infostatus=8
			DisplayString1="  PLAYER 1  "
			If currentplayer = 1 Then
				DisplayString2=FormatScore(P1score) 
			Else
				DisplayString2=FormatScore(PlayerSaved(1,0)) 
			End If
			DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0
			If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex "mando3.jpg", DisplayString1, 0, 12, Flexscore , 15, 1, 10, 1500, 14 
		End If
	End If 
	If Displaybuzy=0 And infostatus=8 Then  ' P2score
		If Playersplaying < 2 Then
			infostatus=9
		Else
			Displaybuzy=1 : infostatus=9
			DisplayString1="  PLAYER 2  "
			If currentplayer = 2 Then
				DisplayString2=FormatScore(P1score) 
			Else
				DisplayString2=FormatScore(PlayerSaved(2,0)) 
			End If
			DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0
			If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex "mando3.jpg", DisplayString1, 0, 12, Flexscore , 15, 1, 10, 1500, 14 
		End If
	End If 

	If Displaybuzy=0 And infostatus=9 Then  ' P3score
		If Playersplaying < 3 Then
			infostatus=10
		Else
			Displaybuzy=1 : infostatus=10
			DisplayString1="  PLAYER 3  "
			If currentplayer = 3 Then
				DisplayString2=FormatScore(P1score) 
			Else
				DisplayString2=FormatScore(PlayerSaved(3,0)) 
			End If
			DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0
			If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex "mando3.jpg", DisplayString1, 0, 12, Flexscore , 15, 1, 10, 1500, 14 
		End If
	End If 

	If Displaybuzy=0 And infostatus=10 Then  ' P4score
		If Playersplaying < 4 Then
			infostatus=11
		Else
			Displaybuzy=1 : infostatus=11
			DisplayString1="  PLAYER 4  "
			If currentplayer = 4 Then
				DisplayString2=FormatScore(P1score) 
			Else
				DisplayString2=FormatScore(PlayerSaved(4,0)) 
			End If
			DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0
			If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex "mando3.jpg", DisplayString1, 0, 12, Flexscore , 15, 1, 10, 1500, 14 
		End If
	End If 


	If Displaybuzy=0 And infostatus=11 Then
			Displaybuzy=1 : infostatus=12 
			DisplayString1=" HIGH SCORE " 
			DisplayString2= FormatScore(hscore(1))
			DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0 
			If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex "mando3.jpg", Displaystring1, 0, 12, Displaystring2 , 15, 1, 10, 1500, 14 
	End If




	If Displaybuzy=0 And infostatus=12 Then 
		If knockeronce=0 Then
			Displaybuzy=1 : infostatus=13
			DisplayString1=" REPLAY  AT " : DisplayString2=FormatScore(ReplayGoals) 
			DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0
			If FlexDMD Then UMainDMD.CancelRendering() :  UMainDMD.DisplayScene00Ex "mando3.jpg", DisplayString1, 0, 12, Flexscore , 15, 1, 10, 1500, 14 
		Else
			infostatus=13
		End If
	End If 
	
	If Displaybuzy=0 And infostatus=13 Then infostatus=1 : Displaytext "            ","            "
	



	If Displaybuzy=0 And creditsmissionstatus=1 Then 
		Displaybuzy=1 : creditsmissionstatus=0
		if missionstatus(1) >11 Then Exit Sub
		DisplayString1="GET  CREDITS" : DisplayString2=" " & CStr(abs (12-(missionstatus(1)))) & " MISSING    "
		DisplayEffect=2 : DisplayCounter=0 : DisplayBlank=0
	End If 

	If Displaybuzy=0 And StartupStatus=1 Then
		Displaybuzy=1 : StartupStatus=2
		DisplayString1=" ++ +  + ++ "
		DisplayString2="*  * ** *  *"
		DisplayEffect=6 : DisplayCounter=0 : DisplayBlank=0
	End If
	If Displaybuzy=0 And StartupStatus=2 Then
		Displaybuzy=1 : StartupStatus=3
		DisplayString1="    " & cGameVersion & "    "
		DisplayString2="BOUNTYHUNTER"
		DisplayEffect=3 : DisplayCounter=0 : DisplayBlank=0
	End If
	If Displaybuzy=0 And StartupStatus=3 Then
		Displaybuzy=1 : StartupStatus=4
		DisplayString1="    " & cGameVersion & "    "
		DisplayString2="BOUNTYHUNTER"
		DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0
	End If
	If Displaybuzy=0 And StartupStatus=4 Then
		Displaybuzy=1 : StartupStatus=5
		DisplayString1=". . . . . . "
		DisplayString2=" . . . . . ."
		DisplayEffect=6 : DisplayCounter=0 : DisplayBlank=0
	End If
	If Displaybuzy=0 And StartupStatus=5 Then
		Displaybuzy=1 : StartupStatus=6
		DisplayString1=" STAR  WARS "
		DisplayString2="            "
		DisplayEffect=6 : DisplayCounter=0 : DisplayBlank=0
	End If
	If Displaybuzy=0 And StartupStatus=6 Then
		Displaybuzy=1 : StartupStatus=0
		DisplayString1="BOUNTYHUNTER "
		DisplayString2="            "
		DisplayEffect=6 : DisplayCounter=0 : DisplayBlank=0
	End If
	
	If Displaybuzy=0 And DisplayBoss7Status=1 Then
		DisplayBoss7Status=0
		Displaybuzy=1  
		DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 
		DisplayString1="TIME LEFT" & Boss7Countdown & "   "
		Displaystring2="RAMPS  " & boss7ramps & "/" & 10+WizardBonusLevel*2 & "   "
'FIXING is more than 12 oki ?
	End If


	'***********
	'Select display effect''
	If DisplayBuzy=1 Then
		If DisplayBlank=1 Then    	' very fast displayblinks
			DisplayBlank=0 
			Displaytext "            ","            "
		Else
			DisplayBlank=1
			DisplayCounter=DisplayCounter+1
			Select Case DisplayEffect
				Case 1 : DisplayEffectNormalDisplay
				Case 2 : DisplayEffectScrollLeft
				Case 3 : DisplayEffectScrollRight
				Case 4 : DisplayEffectLongBlinks
				Case 5 : DisplayEffectTypeWriter
				Case 6 : DisplayEffectSwapBlink
				Case 7 : DisplayEffectMYSTERY
				Case 8 : DisplayEffectFastBonus
				Case 9 : DisplayEffectSPiNNeR
			End Select
		End If
	End If
	If	DisplayBuzy=2 Then
		DisplayString1=" ENTER NAME " 
		If DisplayBlank=1 Then    	' very fast displayblinks
			DisplayBlank=0 : Displaytext "            ","            "
		else 
			DisplayCounter=DisplayCounter+1
			DisplayBlank=1 
			If DisplayCounter>4 Then
				Displaytext DisplayString1, " >>>" & Displaystring2 & chr(initialletter) & "<<<    "
				If FlexDMD Then  UMainDMD.CancelRendering() : tempstr=" >>>" & Displaystring2 & chr(initialletter) & "<<< " : UMainDMD.DisplayScene00Ex FlexBG, DisplayString1, 15, 3, tempstr, 15, 3, 14, 1, 14
			Else
				Displaytext DisplayString1, "             "
				If FlexDMD Then UMainDMD.CancelRendering() : UMainDMD.DisplayScene00Ex FlexBG, DisplayString1, 15, 3, " ", 15, 3, 14, 1, 14
			End If
			If DisplayCounter>9 then DisplayCounter=0
			If initialSelect=1 Then
				initialSelect=0
				Displaystring2=DisplayString2 & chr(initialletter)
				If len(DisplayString2)>2 Then

					Playsound "shouldbeinteresting",0,BackGlassVolumeDial

					WaitforInitialsstatus=5
					displaybuzy=0
					Displaytext "            ","            "
					for i = 5 to WaitforInitialsPlace step-1
						highscorename(i)=highscorename(i-1)
					Next
					highscorename(WaitforInitialsPlace)=DisplayString2

				End If
			End If
		End If

	End If
End Sub
Dim spinnbothways
Sub DisplayEffectSPiNNeR
	If spinnbothways=0 Then
		Select Case DisplayCounter
			Case 1,9,17,25  : Displaystring1="------------" 
			Case 3,11,19,27  : Displaystring1="////////////"
			Case 5,13,21,29 : Displaystring1="||||||||||||"
			Case 7,15,23,31  : Displaystring1="(((((((((((("
			Case 32 : spinnbothways=1
		End Select
	Else
		Select Case DisplayCounter
			Case 1,9,17,25  : Displaystring1="------------" 
			Case 3,11,19,27  : Displaystring1="(((((((((((("
			Case 5,13,21,29 : Displaystring1="||||||||||||"
			Case 7,15,23,31  : Displaystring1="////////////"
			Case 32 : spinnbothways=0
		End Select
	End If

	DisplayString2=Displaystring1
	If DisplayCounter=33 Then 
		DisplayBuzy=0 : Displaytext "            ","            " 
	Else
		Displaytext DisplayString1,DisplayString2
	End If
End Sub

Sub DisplayEffectFastBonus
	If DisplayCounter=4 Then
		Displaybuzy=0 :	Displaytext "            ","            "
	Else
		Displaytext DisplayString1,DisplayString2
	End If
End Sub

Sub ClearAllStatus

	InsertCoinStatus=0
	JackPotScoringStatus=0
	FlyAgainStatus=0
	Doubleinfo=0
	BallIsLockedStatus=0
	MultiBallStatus=0
	SkillShotStatus=0
	SecretStatus=0
	quickMBstatus=0
	enterlightstatus=0
	mysterystatus=0
	LockIsLitStatus=0
	ComboStatus=0
	LeftRampStatus=0
	RightRampStatus=0
	Mission1status=0
	Mission2status=0
	Mission3status=0
	Mission4status=0
	Mission5status=0
	Mission6status=0
	Mission7status=0
	Mission8status=0
	ExtraBallIsLitStatus=0
	HunterStatus=0
	HunterCompleteStatus=0
	BountyStatus=0
	BountyCompleteStatus=0
	GetReadyToPlay=0
	shoottheballstatus=0
	JackPotStatus=0
	DroptargetsStatus=0
	BumperStatus=0
	letterscoringstatus=0
	'attractModeStatus=0
	infostatus=0

End Sub



Sub DisplayEffectMYSTERY
	Dim tempstr(6),text2
	tempstr(1)=FormatScore(mysteryscoring*5*P1scorebonus) 
	tempstr(2)="EXTRAMISSION"
	tempstr(3)=FormatScore(mysteryscoring*P1scorebonus) 
	tempstr(4)=FormatScore(mysteryscoring*2*P1scorebonus) 
	tempstr(5)=" ADV  BONUS " 
	tempstr(6)="ADV 2LETTERS"
	text2=tempstr(int(rnd(1)*6)+1)
	If DisplayCounter=10 Then 	PlaySound "chance",0,BackGlassVolumeDial
	If DisplayCounter<30 Then
		Displaytext DisplayString1,text2
	Else
		If DisplayCounter<55 Then
			If DisplayCounter=32 Then
				If mysteryreward=1 Then scoring(mysteryscoring*4) 
				If mysteryreward=3 Then scoring(mysteryscoring*1) 
				If mysteryreward=2 Or mysteryreward=9 Then newmission 
				If mysteryreward=5 Then
					If bonusmultiplyer<8 Then
						bonusmultiplyer=bonusmultiplyer+1 
						Scoring (1010)
						PlaySound SoundFX("bonusup",DOFContactors), 0, .7*VolumeDial, AudioPan(bonustrigger2), 0.05,0,0,1,AudioFade(bonustrigger2) 
						ObjLevel(7) = 1 : FlasherFlash7_Timer

						If bonusmultiplyer>0 Then BonusX001_Timer
						If bonusmultiplyer>1 Then BonusX002_Timer
						If bonusmultiplyer>2 Then BonusX003_Timer
						If bonusmultiplyer>3 Then BonusX004_Timer
						If bonusmultiplyer>4 Then BonusX005_Timer
						If bonusmultiplyer>5 Then BonusX006_Timer
						If bonusmultiplyer>6 Then BonusX007_Timer
						If bonusmultiplyer>7 Then BonusX008_Timer
						If bonusmultiplyer>8 Then BonusX009_Timer

					else
						mysteryreward=4 : DisplayString2= FormatScore(mysteryscoring*2*P1scorebonus) 
					End if
				End If

				If mysteryreward=6 Then addletter(2) 
				If mysteryreward=4 Then scoring(mysteryscoring*2) 
				If mysteryreward=7 Then newmission : newmission
				If mysteryreward=8 Then newmission : newmission
				If mysteryreward=10 Then newmission : newmission : newmission
				If mysteryreward=11 Then newmission : newmission
				If mysteryreward=12 Then newmission : newmission : newmission
				If mysteryreward=13 Then addletter(4)
				If mysteryreward=14 Then LiteLock
'				If mysteryreward=15 Then LiteLock
			else
				Displaytext DisplayString1,DisplayString2
			End If
		Else
			Displaybuzy=0 :	Displaytext "            ","            "
		End If	
	End If
End Sub


Sub DisplayEffectSwapBlink
	If int(DisplayCounter/3)*3=DisplayCounter Then
	dim tempstr
	tempstr=DisplayString2
	DisplayString2=DisplayString1
	DisplayString1=tempstr
	End If
	If DisplayCounter=45 Then 
		DisplayBuzy=0 : Displaytext "            ","            " 
	Else
		Displaytext DisplayString1,DisplayString2
	End If
End Sub


Sub DisplayEffectTypeWriter
	Dim temp1,temp2
	If DisplayCounter<13 Then
		temp1=Left(Displaystring1,DisplayCounter) & Space(13-DisplayCounter)
		temp2=Left(Displaystring2,DisplayCounter) & Space(13-DisplayCounter)
		Displaytext temp1,temp2
	End If
	If DisplayCounter>17 And DisplayCounter<28 Then Displaytext DisplayString1,DisplayString2 : End If 
	If DisplayCounter>33 And DisplayCounter<44 Then Displaytext DisplayString1,DisplayString2 : End If 
	If DisplayCounter>43 And DisplayCounter<54 Then
		temp1=Space(DisplayCounter-43) & Right(Displaystring1,55-DisplayCounter)
		temp2=Space(DisplayCounter-43) & Right(Displaystring2,55-DisplayCounter)
		Displaytext temp1,temp2
	End If
	If DisplayCounter=55 Then Displaybuzy=0 : Displaytext "            ","            " : End If
End Sub

Sub DisplayEffectScrollLeft
	If DisplayCounter=1 Then DisplayString1= Space(11) & DisplayString1 : DisplayString2= Space(11) & DisplayString2  : End If
	If DisplayCounter<13 Then
		Displaytext mid(DisplayString1,DisplayCounter,12), mid(DisplayString2,DisplayCounter,12)
	Else
		If DisplayCounter=43 Then DisplayBuzy=0 : Displaytext "            ","            " : Else : Displaytext mid(DisplayString1,12,12), mid(DisplayString2,12,12) : End If
	End If 
End Sub

Sub DisplayEffectScrollRight
	If DisplayCounter=1 Then DisplayString1= DisplayString1 & Space(11) : DisplayString2= DisplayString2 & Space(11) : End If
	If DisplayCounter<13 Then
		Displaytext mid(DisplayString1,13-DisplayCounter,12), mid(DisplayString2,13-DisplayCounter,12)
	Else
		If DisplayCounter=26 And HurryBonus=1 Then DisplayBuzy=0 : Displaytext "            ","            "
		If DisplayCounter=43 Then DisplayBuzy=0 : Displaytext "            ","            " : Else : Displaytext mid(DisplayString1,1,12), mid(DisplayString2,1,12) : End If
	End If 
	
	
End Sub

Sub DisplayEffectNormalDisplay
	If DisplayCounter=53 Then DisplayBuzy=0 : Displaytext "            ","            " :Else : Displaytext DisplayString1,DisplayString2 : End If   
End Sub

Sub DisplayEffectLongBlinks
	If DisplayCounter<20 Then Displaytext DisplayString1,DisplayString2 : End If 
	If DisplayCounter>27 And DisplayCounter<47 Then Displaytext DisplayString1,DisplayString2 : End If 
	If DisplayCounter>54 And DisplayCounter<74 Then  Displaytext DisplayString1,DisplayString2 : End If 
	If DisplayCounter=74 Then DisplayBuzy=0 : Displaytext "            ","            " : End If
End Sub

Sub Displaytext(TextLine1,TextLine2)
	Dim i
	For i = 1 to 12
		FlashDisplay(i-1).imageA=Chars(asc(mid(TextLine1,i,1)))
		FlashDisplay(i+11).imageA=Chars(asc(mid(TextLine2,i,1)))
	Next
End Sub

Dim Flexscore
Function FormatScore(ByVal Num)
    dim i
    dim NumString
    NumString = CStr(abs(Num) )
    if len(NumString)>9 then NumString = left(NumString, Len(NumString)-10) & chr(asc(right(NumString, 10) ) + 48) & right(NumString,9) 
    if len(NumString)>6 then NumString = left(NumString, Len(NumString)-7) & chr(asc(right(NumString, 7) ) + 48) & right(NumString,6) 
    if len(NumString)>3 then NumString = left(NumString, Len(NumString)-4) & chr(asc(right(NumString, 4) ) + 48) & right(NumString,3)

	FlexScore = Cstr(abs(Num) )
	if len(FlexScore)>9 then FlexScore = left(FlexScore, Len(FlexScore)-9) & "." & right(FlexScore,9)  
	if len(FlexScore)>6 then FlexScore = left(FlexScore, Len(FlexScore)-6) & "." & right(FlexScore,6) 
    if len(FlexScore)>3 then FlexScore = left(FlexScore, Len(FlexScore)-3) & "." & right(FlexScore,3)

	i=(12-len(NumString))/2
	NumString = Space(i) & NumString & Space(i)
	If len(NumString)<12 Then NumString=NumString& space(1) 
	FormatScore = NumString
End function

'****************************************
'  Loading Highscore file 
'**************************************** 
Dim FlipperColor


Sub LoadHighScore
	Dim FileObj, ScoreFile, TextStr
	Dim i2, i3
	Dim SavedDataTemp(17) 

    Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "BountyHunter.txt") then
		DefaultHighScores
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "BountyHunter.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			DefaultHighScores
			Exit Sub
		End if
		SavedDataTemp(1) = TextStr.ReadLine 
		If Not SavedDataTemp(1) = cHighscoreVersion Then 

			DefaultHighScores
			Set ScoreFile = Nothing
			Set FileObj = Nothing
			Exit Sub 
		End If

		For i = 1 to 16
			If (TextStr.AtEndOfStream=True) then
				TextStr.Close
				DefaultHighScores
				Set ScoreFile = Nothing 
				Set FileObj = Nothing 
				Exit Sub
			End if
			SavedDataTemp(i)=TextStr.ReadLine 
			If SavedDataTemp(i)="" Then 
				TextStr.Close
				DefaultHighScores
				Set ScoreFile = Nothing 
				Set FileObj = Nothing 
				Exit Sub
			End If
		Next

		TextStr.Close
		Set ScoreFile = Nothing
	    Set FileObj = Nothing

		for i = 1 to 5
			
			hscore(i)=int( SavedDataTemp(i) )
	
		Next
		GamesPlayd=int ( SavedDataTemp(6) )
	    LastScore=int ( SavedDataTemp(7) )
		For i = 1 To 5
		highscorename(i)= SavedDataTemp(7+i)
		Next
		Credits= int ( SavedDataTemp(13) )
		ReplayGoals=int ( SavedDataTemp(14) )
		ReplayGoalDown=int ( SavedDataTemp(15) )
        If SavedDataTemp(16) = "" Then 
			FlipperColor=1
		Else	
			FlipperColor= Int (SavedDataTemp(16) )
		End If
		Flippermaterial
End Sub


Sub DefaultHighScores
	'highscore reset  . errors unavoidable
	Displaybuzy=1
	DisplayString1=" HIGH SCORE " : DisplayString2="   RESET    "
	DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 
	If FlexDMD Then  UMainDMD.DisplayScene00Ex "blank.jpg", Displaystring1, 0, 12, DisplayString2 , 15, 1, 14, 2000, 14 : End If

	highscorename(1)="VPX"
	highscorename(2)="VPX"
    highscorename(3)="VPX"
    highscorename(4)="VPX"
    highscorename(5)="VPX"

	hscore(1)=30000000
	hscore(2)=25000000
	hscore(3)=20000000
	hscore(4)=15000000
	hscore(5)=10000000

	Credits=0
	GamesPlayd=0
	LastScore=0
	ReplayGoals=20000000
	ReplayGoalDown=0
	FlipperColor=1
	Flippermaterial
End Sub

Dim ReplayGoals,ReplayGoalDown
Sub SaveHighScore

		Dim FileObj
		Dim ScoreFile
		Set FileObj=CreateObject("Scripting.FileSystemObject")
		If Not FileObj.FolderExists(UserDirectory) then 
			Exit Sub
		End if
		Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "BountyHunter.txt",True)
	
			ScoreFile.WriteLine cHighscoreVersion
			for i = 1 to 5
				ScoreFile.WriteLine hscore(i)
			Next	
			
			ScoreFile.WriteLine GamesPlayd
			ScoreFile.WriteLine LastScore
			For i = 1 to 5
			ScoreFile.WriteLine highscorename(i)
			Next
			ScoreFile.WriteLine Credits
			ScoreFile.WriteLine ReplayGoals
			ScoreFile.WriteLine ReplayGoalDown
			ScoreFile.WriteLine FlipperColor
	

			ScoreFile.Close	
		Set ScoreFile=Nothing
		Set FileObj=Nothing
End Sub

Sub LoadHighScore2
	Dim FileObj, ScoreFile, TextStr
	Dim i2, i3
	Dim SavedDataTemp(17) 

    Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "BountyHunterTM.txt") then
		DefaultHighScores2
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "BountyHunterTM.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			DefaultHighScores2
			Exit Sub
		End if
		SavedDataTemp(1) = TextStr.ReadLine 
		If Not SavedDataTemp(1) = cHighscoreVersion Then 

			DefaultHighScores2
			Set ScoreFile = Nothing
			Set FileObj = Nothing
			Exit Sub 
		End If

		For i = 1 to 16
			If (TextStr.AtEndOfStream=True) then
				TextStr.Close
				DefaultHighScores2
				Set ScoreFile = Nothing 
				Set FileObj = Nothing 
				Exit Sub
			End if
			SavedDataTemp(i)=TextStr.ReadLine 
			If SavedDataTemp(i)="" Then 
				TextStr.Close
				DefaultHighScores2
				Set ScoreFile = Nothing 
				Set FileObj = Nothing 
				Exit Sub
			End If
		Next

		TextStr.Close
		Set ScoreFile = Nothing
	    Set FileObj = Nothing

		for i = 1 to 5
			
			hscore(i)=int( SavedDataTemp(i) )
	
		Next
		GamesPlayd=int ( SavedDataTemp(6) )
	    LastScore=int ( SavedDataTemp(7) )
		For i = 1 To 5
		highscorename(i)= SavedDataTemp(7+i)
		Next
		Credits= int ( SavedDataTemp(13) )
		ReplayGoals=int ( SavedDataTemp(14) )
		ReplayGoalDown=int ( SavedDataTemp(15) )
        If SavedDataTemp(16) = "" Then 
			FlipperColor=1
		Else	
			FlipperColor= Int (SavedDataTemp(16) )
		End If
		Flippermaterial
End Sub


Sub DefaultHighScores2
	'highscore reset  . errors unavoidable
	Displaybuzy=1
	DisplayString1=" HIGH SCORE " : DisplayString2="   RESET    "
	DisplayEffect=4 : DisplayCounter=0 : DisplayBlank=0 
	If FlexDMD Then  UMainDMD.DisplayScene00Ex "blank.jpg", Displaystring1, 0, 12, DisplayString2 , 15, 1, 14, 2000, 14 : End If

	highscorename(1)="VPX"
	highscorename(2)="VPX"
    highscorename(3)="VPX"
    highscorename(4)="VPX"
    highscorename(5)="VPX"

	hscore(1)=10000000
	hscore(2)=5000000
	hscore(3)=4000000
	hscore(4)=3000000
	hscore(5)=1000000

	Credits=0
	GamesPlayd=0
	LastScore=0
	ReplayGoals=10000000
	ReplayGoalDown=0
	FlipperColor=1
	Flippermaterial
End Sub


Sub SaveHighScore2

		Dim FileObj
		Dim ScoreFile
		Set FileObj=CreateObject("Scripting.FileSystemObject")
		If Not FileObj.FolderExists(UserDirectory) then 
			Exit Sub
		End if
		Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "BountyHunterTM.txt",True)
	
			ScoreFile.WriteLine cHighscoreVersion
			for i = 1 to 5
				ScoreFile.WriteLine hscore(i)
			Next	
			
			ScoreFile.WriteLine GamesPlayd
			ScoreFile.WriteLine LastScore
			For i = 1 to 5
			ScoreFile.WriteLine highscorename(i)
			Next
			ScoreFile.WriteLine Credits
			ScoreFile.WriteLine ReplayGoals
			ScoreFile.WriteLine ReplayGoalDown
			ScoreFile.WriteLine FlipperColor
	

			ScoreFile.Close	
		Set ScoreFile=Nothing
		Set FileObj=Nothing
End Sub


'SuB Cabinet
''	lrail.visible=0
''	rrail.visible=0
''	B2sBHtimer.enabled=1
'	controller.B2SSetData 13,1
'	' hide lights and ball in play used in DT view
'	' hide unused reels
'	EMBallinPlay.visible=0
'	DTScorereelOff
'End Sub


Sub DTScorereelOff
	For i = 0 to 9
		Digits2(i).visible=0
	Next
	EMBallinPlay.visible=0
	PlayerEMBox.visible=0
	EMPlayerNr.visible=0

End Sub


Dim FlashDisplay

Sub DisplyInit
	backglasstimer.enabled=1  : Letterblink=15 '  needed not only BG has letterblinks too
	Digits2= Array (EMtest010,EMtest001,EMtest002,EMtest003,EMtest004,EMtest005,EMtest006,EMtest007,EMtest008,EMtest009)
	Crazyballs= Array ( "fennec","lukeball","bobacoin","razerball","helmetball","babyball","gina","client","mandomoon","Kuiil","cobbvanth","mythrol","moffgideon","ahsoka","mayfield","bokatan","armorer","greef","ig11","mandologo9a")
	Playlist= Array (LiNewPlaylist001,LiNewPlaylist002,LiNewPlaylist003,LiNewPlaylist004,LiNewPlaylist005,LiNewPlaylist006,LiNewPlaylist007,LiNewPlaylist008,LiNewPlaylist009)
	FlashDisplay= Array(Disp1,Disp2,Disp3,Disp4,Disp5,Disp6,Disp7,Disp8,Disp9,Disp10,Disp11,Disp12,Disp13,Disp14,Disp15,Disp16,Disp17,Disp18,Disp19,Disp20,Disp21,Disp22,Disp23,DISP24)

	Dim i
    For i = 0 to 255
'		Chars(i) = 0
		Chars(i) = "C00"

	Next
	Chars(42) = "C01" ' *
	Chars(43) = "C02" ' +
	Chars(48) = "C03" ' 0
	Chars(49) = "C04" ' 1
	Chars(50) = "C05"
	Chars(51) = "C06"
	Chars(52) = "C07"
	Chars(53) = "C08"
	Chars(54) = "C09"
	Chars(55) = "C10"
	Chars(56) = "C11"
	Chars(57) = "C12"
	Chars(65) = "C13" ' A
    Chars(66) = "C14"    'B
    Chars(67) = "C15"    'C
    Chars(68) = "C16"    'D
    Chars(69) = "C17"    'E
    Chars(70) = "C18"    'F
    Chars(71) = "C19"    'G
    Chars(72) = "C20"    'H
    Chars(73) = "C21"    'I
    Chars(74) = "C22"    'J
    Chars(75) = "C23"    'K
    Chars(76) = "C24"    'L
    Chars(77) = "C25"    'M
    Chars(78) = "C26"    'N
    Chars(79) = "C27"    'O
    Chars(80) = "C28"    'P
    Chars(81) = "C29"   'Q
    Chars(82) = "C30"   'R
    Chars(83) = "C31"    'S
    Chars(84) = "C32"    'T
    Chars(85) = "C33"    'U
    Chars(86) = "C34"    'V
    Chars(87) = "C35"   'W
    Chars(88) = "C36"    'X
    Chars(89) = "C37"    'Y
    Chars(90) = "C38"    'Z
    Chars(96) = "C39"  '0,
    Chars(97) = "C40"  '1,
    Chars(98) = "C41"  '2,
    Chars(99) = "C42"  '3,
    Chars(100) = "C43" '4,
    Chars(101) = "C44" '5,
    Chars(102) = "C45" '6,
    Chars(103) = "C46" '7,
    Chars(104) = "C47" '8,
    Chars(105) = "C48" '9,
    Chars(60) = "C49"    '<
    Chars(62) = "C50"   '>
    Chars(45) = "C51"   '-
    Chars(46) = "C55"   '.
    Chars(47) = "C54"   '/
    Chars(40) = "C52"   ' turned^^ / =  (
   Chars(124) = "C53"   ' |

End Sub

'****************************************************************************
'nFozzy PHYSICS DAMPENERS

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx) 
	PlaySound SoundFX("Rubber_" & int(rnd(1)*8)+1,DOFContactors), 0, 1*volumedial, pan(ActiveBall), 0.4, 0, 0, 0, AudioFade(ActiveBall)
	RubbersD.dampen Activeball

End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
End Sub

dim RubbersD : Set RubbersD = new Dampener	'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 0.935 '0.96	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.935 '0.96
RubbersD.addpoint 2, 5.76, 0.942 '0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener	'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'Class Dampener
'	Public Print, debugOn 'tbpOut.text
'	public name, Threshold 	'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
'	Public ModIn, ModOut
'	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): End Sub 
'
'	Public Sub AddPoint(aIdx, aX, aY) 
'		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
'		if gametime > 100 then Report
'	End Sub
'
'	public sub Dampen(aBall)
'		if threshold then if BallSpeed(aBall) < threshold then exit sub end if end if
'		dim RealCOR, DesiredCOR, str, coef
'		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
'	
'		If cor.BallVel(aball.id) = Not 0 Then
' 			RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
'			coef = desiredcor / realcor 
'			if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
'			"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
'			if Print then Debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)
'			aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
'			'playsound "fx_knocker"	
'			if debugOn then TBPout.text = str
'		End If
'	End Sub
'
'	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
'		dim x : for x = 0 to uBound(aObj.ModIn)
'			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
'		Next
'	End Sub
'
'
'	Public Sub Report() 	'debug, reports all coords in tbPL.text
'		if not debugOn then exit sub
'		dim a1, a2 : a1 = ModIn : a2 = ModOut
'		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
'		TBPout.text = str
'	End Sub
'	
'
'End Class

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

Sub RDampen_Timer()
	Cor.Update
End Sub

Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	'Clamp if on the boundry lines
	'if L=1 and Y < yLvl(LBound(yLvl) ) then Y = yLvl(lBound(yLvl) )
	'if L=uBound(xKeyFrame) and Y > yLvl(uBound(yLvl) ) then Y = yLvl(uBound(yLvl) )
	'clamp 2.0
	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
End Function

'*******************************************************
'	End nFozzy Dampening'
'******************************************************

'*FLEX DMD
If FlexDMD Or VRroom Then FlexDMDInit

Dim MainDMD
Dim CounterDMD
Dim UMainDMD
Dim CMainDMD
Dim fso
dim curdir
Dim vid0,vid1,vid2,vid3,vid4,vid5,vid6,vid7,vid8,vid9

 
Sub FlexDMDinit
	dim tmp
	Set MainDMD = CreateObject("FlexDMD.FlexDMD")
    If MainDMD is Nothing Then
        MsgBox "FlexDMD was not found.  This table will NOT run without it."
        Exit Sub
    End If

	With MainDMD
		.RenderMode = 2
		.Width = 128
		.Height = 32
		.GameName = cGameName + "_main"
		.Run = True
		.Color = RGB(255,150,88)
	End With
	If VRroom Then MainDMD.show=False
	If FlexDMD And MiddleDisplay=2 And Not VRroom Then
		Flasher008.visible=1
		Screw008.visible=0
		Screw015.visible=0
		Screw016.visible=0
		Screw019.visible=0
		flexDMDonPF.enabled=1
		MainDMD.Show=false
	End If
If MiddleDisplay<>1 Then
	For i= 0 to 23
		FlashDisplay(i).visible=0
	Next
End If
	Set UMainDMD = MainDMD.NewUltraDMD() 
	UMainDMD.Init


	vid0 = UMainDMD.RegisterVideo(1, FALSE, "Trailer2.wmv")
	vid1 = UMainDMD.RegisterVideo(1, FALSE, "CrashLanding.wmv")
	vid2 = UMainDMD.RegisterVideo(1, FALSE, "ramp21.wmv")
	vid3 = UMainDMD.RegisterVideo(1, FALSE, "ramps.wmv")
	vid4 = UMainDMD.RegisterVideo(1, FALSE, "star1.wmv")
	vid5 = UMainDMD.RegisterVideo(1, FALSE, "arrow1.wmv")
	vid6 = UMainDMD.RegisterVideo(1, FALSE, "circle1.wmv")
	vid7 = UMainDMD.RegisterVideo(1, FALSE, "bluestars.wmv")
	vid8 = UMainDMD.RegisterVideo(1, FALSE, "blink1.wmv")
	vid9 = UMainDMD.RegisterVideo(1, TRUE, "robot1.wmv")




	Set fso = CreateObject("Scripting.FileSystemObject")
    curDir = fso.GetAbsolutePathName(".")
    UMainDMD.SetProjectFolder curDir & "\BountyHunterDMD"

'**show version at startup
	UmainDMD.DisplayScene00Ex "blank.jpg", cGameVersion, 15, 2, "BOUNTYHUNTER", 4, 15, 2, 3000, 14
	UmainDMD.DisplayScene00Ex "blank.jpg", cGameVersion, 15, 5, "BOUNTYHUNTER", 4, 5, 14, 500, 14
	UmainDMD.DisplayScene00Ex "blank.jpg", cGameVersion, 15, 2, "BOUNTYHUNTER", 4, 15, 14, 500, 14
	UmainDMD.DisplayScene00Ex "blank.jpg", cGameVersion, 15, 5, "BOUNTYHUNTER", 4, 5, 14, 400, 14
	UmainDMD.DisplayScene00Ex "blank.jpg", cGameVersion, 15, 2, "BOUNTYHUNTER", 4, 15, 14, 400, 14
	UmainDMD.DisplayScene00Ex "blank.jpg", cGameVersion, 15, 5, "BOUNTYHUNTER", 4, 5, 14, 250, 14
	UmainDMD.DisplayScene00Ex "blank.jpg", cGameVersion, 15, 2, "BOUNTYHUNTER", 4, 15, 14, 250, 14
	UmainDMD.DisplayScene00Ex "blank.jpg", cGameVersion, 15, 5, "BOUNTYHUNTER", 4, 5, 14, 125, 14
	UmainDMD.DisplayScene00Ex "blank.jpg", ".. .. .. ..", 15, 5, "", 4, 5, 14, 1000, 14
	UmainDMD.DisplayScene00Ex "blank.jpg", "STAR WARS", 2, 13, "", 4, 15, 2, 2500, 14
	UmainDMD.DisplayScene00Ex "blank.jpg", "BOUNTYHUNTER", 2, 13, "", 4, 15, 2, 2500, 14

	If VRroom = 1 Then
		flexDMDonPF.enabled=1
		MainDMD.Show=false
		HideDesktop=0
	End If
End Sub 

 
Sub flexDMDonPF_Timer 
	Dim DMDp 
	DMDp = MainDMD.DmdColoredPixels 
	If Not IsEmpty(DMDp) Then 
		DMDWidth = mainDMD.Width 
		DMDHeight = MainDMD.Height 
		DMDColoredPixels = DMDp 
	End If 
End Sub

 
'*orbital scoreboard
'****************************
' POST SCORES
'****************************



'	Dim osbtempscore:osbtempscore = 0
	Sub SubmitOSBScore(osbtempscore)
		If TournamentMode Then Exit Sub
		If osbkey="" Then Exit Sub
'		pupDMDDisplay "-","Uploading Your^Score to OSB",dmdnote,3,0,10
		On Error Resume Next
		if osbactive = 1 or osbactive = 2 Then
		Dim objXmlHttpMain, Url, strJSONToSend 

		Url = "https://hook.integromat.com/82bu988v9grj31vxjklh2e4s6h97rnu0"

		strJSONToSend = "{""auth"":""" & osbkey &""",""player id"": """ & osbid & """,""player initials"": """ & osbdefinit &""",""score"": " & CStr(osbtempscore) & ",""table"":"""& cGameName & """,""version"":""" & OSDversion & """}"

		Set objXmlHttpMain = CreateObject("Msxml2.ServerXMLHTTP")
		objXmlHttpMain.open "PUT",Url, False
		objXmlHttpMain.setRequestHeader "Content-Type", "application/json"
		objXmlHttpMain.setRequestHeader "application", "application/json"

		objXmlHttpMain.send strJSONToSend
		end if
	End Sub	

