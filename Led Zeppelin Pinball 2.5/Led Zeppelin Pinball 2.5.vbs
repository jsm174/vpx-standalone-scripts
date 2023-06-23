
'*
'*        Bally's Freedom (1976)
'*        Table primary build/scripted by Loserman76
'*        VPM Table version by Allknowing2012 & Hauntfreaks
'*        Source photos for EM Backglass by Wrd1972
'*
'*
'*			Brought to you by iDigstuff & Apophis & Bord
'*          Original Pup Design by Pinballfan2018
'*			Real world mod designed by David Peck
'*			Original artwork by Brad Albright

' Version 2.0 updates (apophis)
' - Updated physics to latest VPW standards
' - Updated drop targets to roth DTs
' - Updated mechanical sound effects to Fleep standard
' - Updated ball rolling sound effects
' - Fixed kicker double scoring issue
' - Fixed visual issue around right inlane wire

' Version 2.0 updates (idigstuff)
' - Added pup scripting 
' - Reduced sling strength
' - Updated music code and triggers for non pup users
' - Reconfigured user options

' Version 3.0 updates (idigstuff)
'-Added Hauntfreaks Color Mod
'-Fixed outlane trigger alignment
'-Fixed lamp under plastics near right spinner
'-Optimized files to reduce tables size
'-Insert lighting tweaks
'-Fixed GiLighting brightness
'-Fixed desktop backglass indicator lights



option explicit
Randomize
ExecuteGlobal GetTextFile("core.vbs")

On Error Resume Next
ExecuteGlobal GetTextFile("Controller.vbs")
If Err Then MsgBox "Unable to open Controller.vbs. Ensure that it is in the scripts folder."
On Error Goto 0

Const cGameName = "ledzeppelin"

'************************************************************************************
'LED ZEPPELIN 2.0 
'Combines both previous releases (PUP DMD, MUSIC ONLY) into one complete version with options
'Please follow the directions for your desired setup

'MEDIA LINKS

'MUSIC (NON PUP)
'******************************************************  (PUT IN VPX MUSIC FOLDER)


'PUP PACK (PUP USERS)
'******************************************************  (PUT IN PUP VIDEOS FOLDER)


'***********************************************************************************************************************
'START OF USER OPTIONS
'***********************************************************************************************************************

'----- PupDMD Options -----

Const HasPuP = False    'Set to True if you want the PupDMD (See Options below for Pup Users) - Do Not use directb2S when this is true
					   'Set to False if you want music player no pup (See Options below for Non Pup Users) - Use directB2s when this is false


'----- General Sound Options -----
Const VolumeDial = 0.8				'Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Const BallRollVolume = 0.5 			'Level of ball rolling volume. Value between 0 and 1

'----- Shadow Options -----
Const ShadowFlippersOn = false
Const ShadowBallOn = false

Const ColorMod = 0 			 '1 = Psychedelic color overlay by Hauntfreaks |  0 = Original BW version

'*********************************************************************************************************************
'FOR NON PUP USERS ONLY : MUST PUT MUSIC IN FOLDER

'Place as many songs (MP3 ONLY) as you want in the Zeppelin music folder making sure to rename the files 1.mp3 2.mp3 3.mp3 etc...
'Then come here and change the NumMusicTracks value to the total number of songs in the folder. All Done!


Const NumMusicTracks = 13     'Total number of songs in the Zeppelin music folder  

Const MusicMode = 0          ' 0 = Player controls songs manually with magnasave (Jukebox Mode) 1 = Player controls songs with magnasave + Song changes when ball drains                         

'**********************************************************************************************************************
'END OF USER OPTIONS
'**********************************************************************************************************************


Dim MusicIsOn : MusicIsOn = False
Const ShadowConfigFile = false

dim PuPDMDDriverType: PuPDMDDriverType=2   ' 0=LCD DMD, 1=RealDMD 2=FULLDMD (large/High LCD)
dim useRealDMDScale : useRealDMDScale=1    ' 0 or 1 for RealDMD scaling.  
dim useDMDVideos    : useDMDVideos=true    ' true or false to use DMD splash videos.
dim pGameName       : pGameName="ledzeppelin"  'pupvideos foldername, probably set to cGameName in realworld

Dim Controller	' B2S
Dim B2SScore	' B2S Score Displayed
Const HSFileName="LedZeppelin.txt"

Const B2STableName="LedZeppelin"
Const LMEMTableConfig="LMEMTables.txt"
Const LMEMShadowConfig="LMEMShadows.txt"
Dim EnableFlipperShadow

'* this value adjusts score motor behavior - 0 allows you to continue scoring while the score motor is running - 1 sets score motor to behave more like a real EM
Const ScoreMotorAdjustment=1

'* this is a debug setting to use an older scoring routine vs a newer score routine - don't change this value
Const ScoreAdditionAdjustment=1

'Const ChimesOn=0
Const ReflipAngle = 20
Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height

dim ScoreChecker
dim CheckAllScores
dim sortscores(4)
dim sortplayers(4)

Dim ChimesOn
Dim B2SOn
Dim B2SFrameCounter
Dim BackglassBallFlagColor
Dim TextStr,TextStr2
Dim i
Dim obj
Dim BallsPerGame
Dim InProgress
Dim BallInPlay
Dim CreditsPerCoin
Dim Score100K(4)
Dim Score(4)
Dim ScoreDisplay(4)
Dim HighScorePaid(4)
Dim HighScore
Dim HighScoreReward
Dim BonusMultiplier
Dim Credits
Dim Match
Dim Replay1
Dim Replay2
Dim Replay3
Dim Replay4
Dim Replay1Paid(4)
Dim Replay2Paid(4)
Dim Replay3Paid(4)
Dim Replay4Paid(4)
Dim TableTilted
Dim TiltCount

Dim OperatorMenu

Dim TargetSetting
Dim BonusBooster
Dim BonusBoosterCounter
Dim BonusCounter
Dim HoleCounter

Dim ZeroNineCounter
Dim Ones
Dim Tens
Dim Hundreds
Dim Thousands

Dim Player
Dim Players

Dim rst
Dim bonuscountdown
Dim TempMultiCounter
dim TempPlayerup
dim RotatorTemp

Dim bump1
Dim bump2
Dim bump3
Dim bump4

Dim LastChime10
Dim LastChime100
Dim LastChime1000

Dim Score10
Dim Score100

Dim targettempscore
Dim SpecialLightCounter
Dim SpecialLightOption
Dim HorseshoeCounter
Dim DropTargetCounter

Dim LeftTargetCounter
Dim RightTargetCounter

Dim MotorRunning
Dim Replay1Table(18)
Dim Replay2Table(18)
Dim Replay3Table(18)
Dim Replay4Table(18)
Dim ReplayTableSet
Dim ReplayLevel
Dim ReplayTableMax

Dim BonusSpecialThreshold
Dim TargetLightsOn
Dim AdvanceLightCounter
dim bonustempcounter

dim raceRflag
dim raceAflag
dim raceCflag
dim raceEflag
dim race5flag

Dim LStep, LStep2, RStep, L2Step, R2Step, xx

Dim ReelCounter
Dim BallCounter
Dim BallReelAddStart(12)
Dim BallReelAddFrames(12)
Dim BallReelDropStart(12)
Dim BallReelDropFrames(12)

Dim EightLit

Dim X
Dim Y
Dim Z
Dim AddABall
Dim AddABallFrames
Dim DropABall
Dim DropABallFrames
Dim CurrentFrame
Dim BonusMotorRunning
Dim QueuedBonusAdds
Dim QueuedBonusDrops


Dim TempLightTracker

Dim TargetLeftFlag
Dim TargetCenterFlag
Dim TargetRightFlag

Dim TargetSequenceComplete

Dim SpecialLightsFlag

Dim AlternatingRelay

Dim ZeroToNineUnit

Dim Kicker1Hold,Kicker2Hold,Kicker3Hold

Dim mHole,mHole2, mHole3, mHole4, mHole5

Dim SpinPos,Spin,Count,Count1,Count2,Count3,Reset,VelX,VelY,LitSpinner

Dim GoldBonusCounter,SilverBonusCounter

Dim ScoreMotorStepper

Dim GameOption,CircusSetting

Dim LeftSpinnerCounter

Dim bgpos, dooralreadyopen

Dim ABCDCounter, tKickerTCount, DropTargetCompletedCount, DropTargetsDownCount, DropTargetsSetting, AddCounter,DoubleBonusCount

Dim MainBanksCount


Sub Table1_Init()

	LoadEM
	LoadLMEMConfig2
	PlaySound"Kashmir",-1
	

	If Table1.ShowDT = false then
		For each obj in DesktopCrap
			obj.visible=False
		next
	End If

	OperatorMenuBackdrop.image = "PostitBL"
	For XOpt = 1 to MaxOption
		Eval("OperatorOption"&XOpt).image = "PostitBL"
	next

	For XOpt = 1 to 256
		Eval("Option"&XOpt).image = "PostItBL"
	next



	CircusSetting=0
	LeftSpinnerCounter=0
	DropTargetsSetting=1
	Count=0
	Count1=0
	Count2=0
	Count3=0
	Reset=0
	ZeroToNineUnit=Int(Rnd*10)
	Kicker1Hold=0
	Kicker2Hold=0
	Kicker3Hold=0

	EightLit=0

	BallCounter=0
	ReelCounter=0
	AddABall=0
	DropABall=0
	HideOptions
	SetupReplayTables
	PlasticsOff
	BumpersOff
	OperatorMenu=0
	HighScore=0
	MotorRunning=0
	HighScoreReward=1
	Credits=0
	BallsPerGame=5
	ReplayLevel=1
	ChimesOn=0
	TargetSetting=0
	AlternatingRelay=1
	ZeroNineCounter=0
	SpecialLightOption=2
	BackglassBallFlagColor=1
	GameOption=2
	loadhs
	if HighScore=0 then HighScore=500000

	flames1.opacity = 0
	flames2.opacity = 0
	flames3.opacity = 0
	flames4.opacity = 0

	TableTilted=false

	Match=int(Rnd*10)
	MatchReel.SetValue((Match)+1)
	Match=Match*10

	CanPlay001.state=0
	CanPlay002.state=0
	CanPlay003.state=0
	CanPlay004.state=0
	'GameOverReel.SetValue(1)
	LightGameOver.state=1



	For each obj in PlayerHuds
		obj.state=0
	next
	For each obj in PlayerScores
		obj.ResetToZero
	next

	For each obj in Flashers
		obj.state=1
	next

	For each obj in GoldBonus
		obj.state=0
	next


	ChimesOn=0
	dooralreadyopen=0
	Replay1=Replay1Table(ReplayLevel)
	Replay2=Replay2Table(ReplayLevel)
	Replay3=Replay3Table(ReplayLevel)
	Replay4=Replay4Table(ReplayLevel)

	BonusCounter=0
	HoleCounter=0

	BallCard.image="BC"+FormatNumber(BallsPerGame,0)

	InstructCard.image="IC"+FormatNumber(BallsPerGame,0)+FormatNumber(GameOption,0)
'	CardLight1.State = ABS(CardLight1.State-1)
	RefreshReplayCard

	CurrentFrame=0

	AdvanceLightCounter=0

	Players=0
	RotatorTemp=1
	InProgress=false
	TargetLightsOn=false


	RollReel001.Setvalue(0)
	RollReel002.Setvalue(0)
	RollReel003.Setvalue(0)
	RollReel004.Setvalue(0)

	If B2SOn Then

		if Match=0 then
			Controller.B2SSetMatch 100
		else
			Controller.B2SSetMatch Match
		end if
		Controller.B2SSetScoreRolloverPlayer1 0
		Controller.B2SSetScoreRolloverPlayer2 0
		Controller.B2SSetScoreRolloverPlayer3 0
		Controller.B2SSetScoreRolloverPlayer4 0


		Controller.B2SSetTilt 0
		Controller.B2SSetCredits Credits
		Controller.B2SSetGameOver 1
	End If

	for i=1 to 4
		player=i
		If B2SOn Then 
			Controller.B2SSetScorePlayer player, 0
		End If
	next
	bump1=1
	bump2=1
	InitPauser5.enabled=true
	If Credits > 0 Then 
		DOF 127, 1
		CreditLight.state=1
	end if


PUPInit

End Sub

Sub Table1_exit()
	savehs
	SaveLMEMConfig
	SaveLMEMConfig2
	If B2SOn Then Controller.Stop
end sub

Sub Delay( seconds )
	Dim startTime : startTime = timer
	Debug.Print "Delay start=" & startTime & ", seconds=" & seconds
	Do
	Loop While timer - startTime < seconds
	Debug.Print "Delay end " & timer
End Sub

Sub MusicOn
	Dim x
	Pupevent 51
	If HasPup = True then Exit Sub 
	x = INT(NumMusicTracks * Rnd + 1)
	PlayMusic "Zeppelin\"& x &".mp3" 
	MusicIsOn = True

End Sub

Sub Table1_MusicDone()
	MusicOn
End Sub


Sub Table1_KeyDown(ByVal keycode)

	If KeyCode = LeftMagnaSave and InProgress = True Then EndMusic : MusicIsOn = False
	If KeyCode = RightMagnaSave and InProgress = True Then MusicOn : PupEvent 53

	' GNMOD
	if EnteringInitials then
		CollectInitials(keycode)
		exit sub
	else
	'	If Keycode = StartGameKey Then EndMusic
		If KeyCode = StartGameKey And Credits > 0 Then StopSound "Kashmir"	
		If KeyCode = StartGameKey And Credits > 0 Then StopSound "stairway"							
		If Keycode = StartGameKey And Credits > 0 Then PlaySound "dxGoodEvening":Pupevent 77:Pupevent 52 :End If
		If KeyCode = StartGameKey And Credits < 1 Then EndMusic : MusicIsOn = False
		If KeyCode = StartGameKey And InProgress = True Then MusicOn

	end if

	if EnteringOptions then
		CollectOptions(keycode)
		exit sub
	end if

	If keycode = PlungerKey Then
		Plunger.PullBack
		PlungerPulled = 1
		PuPEvent 53
		SoundPlungerPull
	End If

	if keycode = LeftFlipperKey and InProgress = false then
		OperatorMenuTimer.Enabled = true
	end if
	' END GNMOD

	If keycode = LeftFlipperKey and InProgress=true and TableTilted=false Then
		FlipperActivate LeftFlipper, LFPress
		LF.Fire  'leftflipper.rotatetoend
		'PlaySound "mxBuzzL"
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If	
		DOF 101, 1
	End If

	If keycode = RightFlipperKey  and InProgress=true and TableTilted=false Then
		FlipperActivate RightFlipper, RFPress
		RF.Fire 'rightflipper.rotatetoend
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
		'PlaySound "mxBuzz"
		DOF 102, 1
	End If

	If keycode = LeftTiltKey Then
		Nudge 90, 3
		SoundNudgeLeft
		TiltIt
	End If

	If keycode = RightTiltKey Then
		Nudge 270, 3
		SoundNudgeRight
		TiltIt
	End If

	If keycode = CenterTiltKey Then
		Nudge 0, 3
		SoundNudgeCenter
		TiltIt
	End If

	If keycode = MechanicalTilt Then
		TiltCount=2
		SoundNudgeCenter
		TiltIt
	End If


	If keycode = AddCreditKey or keycode = 4 then
		If B2SOn Then
			'Controller.B2SSetScorePlayer6 HighScore

		End If
		RandomCoinInSound
		CreditLight.state=1
		AddSpecial2
		delay 1
		playsound"BallyCoinIn1Credits"
	end if

	if keycode = 5 then 
		RandomCoinInSound
		CreditLight.state=1
		AddSpecial2
		delay 1
		playsound"BallyCoinIn1Credits"
		keycode= StartGameKey
	end if


	if keycode = StartGameKey and Credits>0 and InProgress=true and Players>0 and Players<4 and BallInPlay<2 then
		SoundStartButton
		Credits=Credits-1
		If Credits < 1 Then
			DOF 127, 0
			CreditLight.state=0
		end if
		CreditsReel.SetValue(Credits)
		Players=Players+1
		If Players=2 then
			CanPlay001.state=0
			CanPlay002.state=1
		end if
		If Players=3 then
			CanPlay002.state=0
			CanPlay003.state=1
		end if
		If Players=4 then
			CanPlay003.state=0
			CanPlay004.state=1
		end if
		playsound "BallyStartButtonPlayers2-4"
		RollReel002.Setvalue(0)
		RollReel003.Setvalue(0)
		RollReel004.Setvalue(0)
		If B2SOn Then
			Controller.B2SSetCanPlay Players
			If Players=2 Then
				Controller.B2SSetScoreRolloverPlayer2 0
			End If
			If Players=3 Then
				Controller.B2SSetScoreRolloverPlayer3 0
			End If
			If Players=4 Then
				Controller.B2SSetScoreRolloverPlayer4 0
			End If
			Controller.B2SSetCredits Credits
		End If
	end if

	if keycode=StartGameKey and Credits>0 and InProgress=false and Players=0 and EnteringOptions = 0 then
		'GNMOD
		OperatorMenuTimer.Enabled = false
		'END GNMOD
		Credits=Credits-1
		If Credits < 1 Then
			DOF 127, 0
			CreditLight.state=0
		end if
		CreditsReel.SetValue(Credits)
		Players=1
		CanPlay001.state=1
		MatchReel.SetValue(0)
		Player=1
		playsound "mxStartup"
		pDMDStartGame
		TempPlayerUp=Player
'		PlayerUpRotator.enabled=true
		rst=0
		BallInPlay=1
		InProgress=true
		resettimer.enabled=true
		BonusMultiplier=1
		TimerTitle1.enabled=0
		TimerTitle2.enabled=0
		TimerTitle3.enabled=0
		TimerTitle4.enabled=0
		RollReel001.setvalue(0)
		RollReel002.setvalue(0)
		RollReel003.setvalue(0)
		RollReel004.setvalue(0)
		If B2SOn Then
			Controller.B2SSetCanPlay Players
			Controller.B2SSetTilt 0
			Controller.B2SSetGameOver 0
			Controller.B2SSetMatch 0
			Controller.B2SSetCredits Credits

			Controller.B2SSetCanPlay 1
			Controller.B2SSetPlayerUp 1
			Controller.B2SSetData 81,1
			Controller.B2SSetBallInPlay BallInPlay
			Controller.B2SSetScoreRolloverPlayer1 0
			Controller.B2SSetScoreRolloverPlayer2 0
			Controller.B2SSetScoreRolloverPlayer3 0
			Controller.B2SSetScoreRolloverPlayer4 0
			Controller.B2SSetShootAgain 0


		End If
		If Table1.ShowDT = True then
			For each obj in PlayerScores
				'				obj.ResetToZero
				obj.Visible=true
			next
			For each obj in PlayerScoresOn
				'				obj.ResetToZero
				obj.Visible=false
			next

			For each obj in PlayerHuds
				obj.state=0
			next
			For each obj in PlayerHUDScores
				obj.state=0
			next
			PlayerHuds(Player-1).state=1
			PlayerHUDScores(Player-1).state=1
			PlayerScores(Player-1).Visible=0
			PlayerScoresOn(Player-1).Visible=1
		end If
		'GameOverReel.SetValue(0)
		LightGameOver.state=0


	end if



End Sub

Sub Table1_KeyUp(ByVal keycode)

	' GNMOD
	if EnteringInitials then
		exit sub
	end if

	If keycode = PlungerKey Then

		if PlungerPulled = 0 then
			exit sub
		end if

		SoundPlungerReleaseBall
		Plunger.Fire
	End If

	if keycode = LeftFlipperKey then
		OperatorMenuTimer.Enabled = false
	end if

	' END GNMOD  

	If keycode = LeftFlipperKey and InProgress=true and TableTilted=false Then
		FlipperDeActivate LeftFlipper, LFPress
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
		StopSound "mxBuzzL"
		DOF 101, 0
	End If

	If keycode = RightFlipperKey and InProgress=true and TableTilted=false Then
		FlipperDeActivate RightFlipper, RFPress
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
		StopSound "mxbuzz"
		DOF 102, 0
	End If

End Sub



Sub Drain_Hit()

	If musicmode = 1 and BallinPlay < 5 then EndMusic : MusicIsOn = False
	DOF 124, 2
	Drain.DestroyBall
	RandomSoundDrain Drain 
	Pause4Bonustimer.enabled=true
	DropTargetsDownCount=0
	DoubleBonuspupdmd
	NoDoubleBonuspupdmd
	if hasPUP=false then Exit Sub
	Pupevent 60
	puPlayer.LabelShowPage pDMD,9,2,""
	puPlayer.LabelSet pDMD, "Bonusfont"," Bonus: ",1,"{'mt':2, 'shadowcolor':2949120, 'shadowstate':1,'xoffset':2, 'yoffset':6, 'bold':1, 'outline':2 }"
	puPlayer.LabelSet pDMD, "Total",FormatNumber ((GoldBonusCounter*1000)*(DoubleBonusCount),0),1,"{'mt':1,'at':1,'fq':100,'len':60000 }"
	puPlayer.LabelSet pDMD, "Totalunder",FormatNumber ((GoldBonusCounter*1000)*(DoubleBonusCount),0),1,"{'mt':2, 'shadowcolor':16777215, 'shadowstate':1,'xoffset':1, 'yoffset':1, 'bold':1, 'outline':1 }"

End Sub

Sub Trigger0_Unhit()	
	DOF 126, 2
End Sub

Sub Gate2_hit()	
	If Not MusicIsOn and HasPup = False then MusicOn
End Sub

Sub Pause4Bonustimer_timer
	Pause4Bonustimer.enabled=0
	ScoreGoldBonus
End Sub

Sub NewBonusHolder_timer
	if NewBonusTimer.enabled=0 then
		NewBonusHolder.enabled=0
		NextBallDelay.enabled=true
	end if

end sub


' Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub

'***********************
'     Animated business
'***********************
'
'Function PI()
'	PI = 4*Atn(1)
'End Function

Sub UpdateFlipperLogos_Timer
	Pgate002.rotx = -GateR.currentangle*0.5
	Pgate001.rotx = -GateL.currentangle*0.5
	gates005.rotx = gate1.currentangle

	Dim SpinnerRadius: SpinnerRadius=7

	SpinnerRod.TransZ = (cos((spinnerleft.CurrentAngle + 180) * (PI/180))+1) * SpinnerRadius
	SpinnerRod.TransY = sin((spinnerleft.CurrentAngle) * (PI/180)) * -SpinnerRadius
	SpinnerRod001.TransZ = (cos((spinnerright.CurrentAngle + 180) * (PI/180))+1) * SpinnerRadius
	SpinnerRod001.TransY = sin((spinnerright.CurrentAngle) * (PI/180)) * -SpinnerRadius
End Sub

'***********************
' slingshots
'

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(activeball)
	RandomSoundSlingshotRight Sling1
	DOF 104, 2
	RSling0.Visible = 0
	RSling1.Visible = 1
	sling1.TransZ = -20
	RStep = 0
	RightSlingShot.TimerEnabled = 1
	AddScore 10
End Sub

Sub RightSlingShot_Timer
	Select Case RStep
		Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
		Case 4:RSLing2.Visible = 0:RSling0.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
	End Select
	RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(activeball)
	RandomSoundSlingshotLeft Sling2
	DOF 103, 2
	LSling0.Visible = 0
	LSling1.Visible = 1
	sling2.TransZ = -20
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
	AddScore 10
End Sub

Sub LeftSlingShot_Timer
	Select Case LStep
		Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
		Case 4:LSLing2.Visible = 0:LSLing0.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
	End Select
	LStep = LStep + 1
End Sub

'***********************************
' Walls
'***********************************

Sub RubberWallSwitches_Hit(idx)
	if TableTilted=false then
		AddScore(10)

	end if
end Sub


'***********************************
' Bumpers
'***********************************


Sub Bumper1_Hit
	If TableTilted=false then

		RandomSoundBumperTop Bumper1
		DOF 105, 2
		bump1 = 1
		AddScore(10)
	ZofoGif_PuPGIF

	end if

End Sub

Sub Bumper2_Hit
	If TableTilted=false then

		RandomSoundBumperMiddle Bumper2
		DOF 106, 2
		bump2 = 1
		AddScore(10)
	FeatherGif_PuPGIF

	end if

End Sub

Sub Bumper3_Hit
	If TableTilted=false then

		RandomSoundBumperBottom Bumper3
		DOF 106, 2
		bump3 = 1
		AddScore(10)
	HexStar_PuPGIF

	end if

End Sub

Sub Bumper4_Hit
	If TableTilted=false then

		RandomSoundBumperBottom Bumper4
		DOF 106, 2
		bump4 = 1
		AddScore(10)
	CircleGif_PuPGIF

	end if

End Sub

'************************************
'  Rollover lanes
'************************************



Sub LeftOutlane_Hit
	if TableTilted=false then
		DOF 110, 2
		CollectCenterSpinner
	end if
end sub

Sub LeftInlane_Hit
	if TableTilted=false then
		DOF 111, 2
		SetMotor(500)
		PuPEvent 31
		IncreaseGoldBonus		
	end if
end sub

Sub RightOutlane_Hit
	if TableTilted=false then
		DOF 113, 2
		CollectCenterSpinner
	end if
end sub

Sub RightInlane_Hit
	if TableTilted=false then
		DOF 112, 2
		SetMotor(500)
		PuPEvent 31
		IncreaseGoldBonus		
	end if
end sub

Sub TriggerPlunge_Hit
	if TableTilted=false then
		DOF 132, 2	'apophis
	end if
end sub

'******************************************
' Spinners


Sub SpinnerLeft_Spin()
	If TableTilted=false and MotorRunning=0 then
		DOF 122, 2
		if Left100.state=1 then
			AddScore(100)
			if hasPUP=false then Exit Sub
			Blimp100_PuPGIF
	'		BlankGifScreen
			ToggleAlternatingRelay
		elseif Left1000.state=1 then
			AddScore(1000)
			Blimp1000_PuPGIF
			ToggleAlternatingRelay
		else
			AddScore(10)
			Blimp10_PuPGIF

		end if

	end if

end Sub

Sub SpinnerRight_Spin()
	If TableTilted=false and MotorRunning=0 then
		DOF 123, 2		
		if Right100.state=1 then
			AddScore(100)
			Blimp100_PuPGIF
		'	BlankGifScreen
			ToggleAlternatingRelay
		elseif Right1000.state=1 then
			AddScore(1000)
			Blimp1000_PuPGIF
			ToggleAlternatingRelay
		else
			AddScore(10)
			Blimp10_PuPGIF

		end if

	end if

end Sub

'************Kickers
Dim rkickstep, lkickstep, ktimer, kickFill, rKickerHit, lKickerHit


Dim kickBalls
Dim kickerBall1
Sub kickBall(kball, kangle, kvel, kvelz, kzlift)
	dim rangle
	rangle = 3.14 * (kangle+RndNum(-1,1) - 90) / 180
	kball.z = kball.z + kzlift
	kball.velz = kvelz
	kball.velx = cos(rangle)*kvel
	kball.vely = sin(rangle)*kvel
End Sub


'****************************************************
' Kickers
'****************************************************
KickerHolder1.interval = 200
KickerHolder2.interval = 200
sub Kicker1_UnHit : KickerHolder1.enabled=0 : End Sub
sub Kicker2_UnHit : KickerHolder2.enabled=0 : End Sub

sub Kicker1_Hit()
	set kickerBall1 = activeball
	if TableTilted=true then
		tKickerTCount=0
		Kicker1.Timerenabled=1
		exit sub
	end if
	KickerHolder1.enabled=1
	SoundSaucerLock
end sub

Sub KickerHolder1_timer
	If MotorRunning=0 then
		KickerHolder1.enabled=0
		Kicker1.timerinterval=500
		Kicker1.timerenabled=1
		tKickerTCount=0
		CollectCenterSpinner
	end if
end sub

Sub Kicker1_Timer()
	tKickerTCount=tKickerTCount+1
	select case tKickerTCount
		case 1:
			if kicker1.ballcntover > 0 Then
				Pkickarm1.rotz=15
				SoundSaucerKick 1,Kicker1
				DOF 111, 2
				kickBall KickerBall1, 152, 8, 5, 30
			else
			end if
		case 2:
			Kicker1.timerenabled=0
			Pkickarm1.rotz=0
	end Select
end sub


sub Kicker2_Hit()
	set kickerBall1 = activeball
	if TableTilted=true then
		tKickerTCount=0
		Kicker2.Timerenabled=1
		exit sub
	end if
	KickerHolder2.enabled=1	
	SoundSaucerLock
end sub

Sub KickerHolder2_timer
	If MotorRunning=0 then
		KickerHolder2.enabled=0
		Kicker2.timerinterval=500
		Kicker2.timerenabled=1
		tKickerTCount=0
		CollectCenterSpinner
	end if
end sub

Sub Kicker2_Timer()
	tKickerTCount=tKickerTCount+1
	select case tKickerTCount
		case 1:
			if kicker2.ballcntover > 0 Then
				Pkickarm2.rotz=15
				SoundSaucerKick 1,Kicker2
				DOF 112, 2
				kickBall KickerBall1, 215, 10, 5, 30
			Else
			End if
		case 2:
			Kicker2.timerenabled=0
			Pkickarm2.rotz=0
	end Select
end sub




'****************************************************
'*   Star Triggers
'****************************************************

Sub StarTrigger_Hit
	If TableTilted=false then
		SetMotor(40)
	PuPEvent 10
	end if
end sub


'****************************************************
'*   Stationary Targets
'****************************************************

Sub RightTarget_Hit
	If TableTilted=false then
		DOF 119, 2
		DOF 233, 2 'apophis
		SetMotor(500)
		IncreaseGoldBonus		
		PuPEvent 31
	end if
end sub	


'****************************************************
'*   Drop Targets
'****************************************************

'DropTargetCompletedCount, DropTargetsDownCount

Sub Target1_hit
	if TableTilted=false then
		DTHit 1
		TargetBouncer Activeball, 1
	end if
end sub

Sub Target2_hit
	if TableTilted=false then
		DTHit 2
		TargetBouncer Activeball, 1
	end if
end sub

Sub Target3_hit
	if TableTilted=false then
		DTHit 3
		TargetBouncer Activeball, 1
	end if
end sub

Sub Target4_hit
	if TableTilted=false then
		DTHit 4
		TargetBouncer Activeball, 1
	end if
end sub

Sub Target5_hit
	if TableTilted=false then
		DTHit 5
		TargetBouncer Activeball, 1
	end if
end sub

Sub CheckAllDrops
	If DropTargetsDownCount=5 then
		DropsCompleted.enabled=true
	If MainBanksCount >= 5 Then
	end if
end if
end sub

Sub DropsCompleted_timer

	If MotorRunning<>0 then exit sub
	DropsCompleted.enabled=false
	DropTargetCompletedCount=DropTargetCompletedCount+1
	SetMotor(5000)
	DropTargetsDownCount=0
	TargetcounterPUP
	ResetDropTargets.enabled=1
	SpecialLight.state=1
	PuPEvent 15

	If DropTargetCompletedCount=1 AND GameOption=2 then
		ShootAgainLight.state=1
		LightSamePlayer.state=1
		'ShootAgainReel.setvalue(1)
		SpecialLight.state=1
		

		If B2SOn then
			Controller.B2SSetShootAgain 1
		end if
	end if
	If DropTargetCompletedCount=2 then
		ShootAgainLight.state=1
		LightSamePlayer.state=1
		'ShootAgainReel.setvalue(1)
		AddSpecial
		If B2SOn then
			Controller.B2SSetShootAgain 1
		end if
	end if

end sub


Sub ResetDropTargets_timer
	If MotorRunning<>0 then exit sub
	ResetDropTargets.enabled=0
	DTRaise 1
	DTRaise 2
	DTRaise 3
	DTRaise 4
	DTRaise 5
	RandomSoundDropTargetReset Target3p
	DropTargetsDownCount=0
End Sub

'****************************************************
' Target Pupdmd Add On

Sub Targetdownfirst
	If DropTargetsDownCount=1 Then
	PuPEvent 70
  End If
End Sub

Sub Targetdownsecond
	If DropTargetsDownCount=2 Then
	PuPEvent 71
  End If
End Sub

Sub Targetdownthird
	If DropTargetsDownCount=3 Then
	PuPEvent 72
  End If
End Sub

Sub Targetdownfourth
	If DropTargetsDownCount=4 Then
	PuPEvent 73
  End If
End Sub

Sub Shootagainvid
	If ShootAgainLight.state=1 Then	
	PuPEvent 95
	PlaySound"Xball"
	End If
End Sub

'****************************************************
' Blimp Fire Pupdmd Add On
	dim firepos:firepos = 0
	sub fireball_timer
		flames1.opacity = 0
		flames2.opacity = 0
		flames3.opacity = 0
		flames4.opacity = 0
		firepos = firepos + 1
		select case firepos
			case 1 : flames1.opacity = 1000
			case 2 : flames2.opacity = 1000
			case 3 : flames3.opacity = 1000
			case 4 : flames4.opacity = 1000
			case 5 : flames2.opacity = 1000
			case 6 : flames3.opacity = 1000
			case 7 : flames4.opacity = 1000
			case 8 : flames4.opacity = 0 : firepos = 0 : fireball.enabled = false
		end Select
	end Sub

'****************************************************
' Double Bonus Pupdmd Add On

Sub DoubleBonuspupdmd
	If DoubleBonus.state=1 Then
	DoubleBonusCount=2
	End If
End Sub

Sub NoDoubleBonuspupdmd
	If DoubleBonus.state=0 Then
	DoubleBonusCount=1
	End If
End Sub


'****************************************************
' Blimp Trigger Pupdmd Add On

Sub TriggerBlimpR_Hit()
	If Right1000.state=1 or Right100.state=1 Then
		Pupevent 43
	End If
	If Right1000.state=0 or Right100.state=0 Then
		PuPEvent 40
	End If
End Sub


Sub TriggerBlimpL_Hit()
	If Left1000.state=1 or Left100.state=1  Then
		Pupevent 43
		fireball.enabled = True
		Playsound "fire"
	End If
	If Left1000.state=0 or Left100.state=0 Then
		PuPEvent 40
	End If
End Sub

'****************************************************
'****************************************************

Sub CheckABCDSequence
end sub

Sub CheckCenterTargetSpecials
end sub

Sub CenterSpinnerLights
	For each obj in SpinnerLights
		obj.state=0
	next
	SpinnerLights(LeftSpinnerCounter).state=1
	SpinnerLights(LeftSpinnerCounter+10).state=1

end sub

sub CollectCenterSpinner
	Select Case LeftSpinnerCounter
		case 0,2:
			SetMotor(5000)
			PuPEvent 14
		case 1,7:
			SetMotor(500)
			DoubleBonus.state=1
			Pupevent 11
		case 3,9:
			SetMotor(3000)
			Advance3Bonus
			PuPEvent 12
		case 4,6,8:
			SetMotor(500)
			IncreaseGoldBonus
			PuPEvent 13
		case 5:
			SetMotor(500)
			Pupevent 17
			if BallsPerGame=3 then
				Left1000.state=1
				Right1000.state=1
			else
				Left100.state=1
				Right100.state=1
			end if
	end select
end sub

Sub Advance3Bonus
	AddCounter=3
	Add3Bonus.enabled=true
end sub

Sub Add3Bonus_timer
	IncreaseGoldBonus
	AddCounter=AddCounter-1
	if AddCounter<1 then
		Add3Bonus.enabled=false
	end if
end sub


'**************************************

Sub AddSpecial()
	If ChimesOn=1 then
		PlaySound"Circus-BiriBiri"
	else
		PlaySound"knocker_1" : PuPEvent 16
	end if
	DOF 128, 2
	Credits=Credits+1
	DOF 127, 1
	CreditLight.state=1
	if Credits>36 then Credits=36
	If B2SOn Then
		Controller.B2SSetCredits Credits
	End If
	CreditsReel.SetValue(Credits)
End Sub

Sub AddSpecial2()
	PlaySound"click"
	Credits=Credits+1
	DOF 127, 1
	CreditLight.state=1
	if Credits>36 then Credits=36
	If B2SOn Then
		Controller.B2SSetCredits Credits
	End If
	CreditsReel.SetValue(Credits)
End Sub

Sub CloseGateTrigger_Hit()
	if dooralreadyopen=1 then
		closeg.enabled=true

	end if
End Sub


sub openg_timer


end sub 

sub closeg_timer

end sub   



Sub ToggleAlternatingRelay
	LeftSpinnerCounter=LeftSpinnerCounter+1
	If LeftSpinnerCounter>9 then
		LeftSpinnerCounter=0
	end if
	CenterSpinnerLights

end sub

Sub ToggleRedBumper



	BumpersOn

end sub

Sub ResetABCD


end sub


Sub ResetBallDrops

	For each obj in GoldBonus
		obj.state=0
	next


	DoubleBonus.state=0

	DropTargetCompletedCount=0
	SpecialLight.state=0
	DoubleBonus.state=0
	Left100.state=0
	Left1000.state=0
	Right100.state=0
	Right1000.state=0

	ResetDropTargets.enabled=1
	HoleCounter=0
	SilverBonusCounter=0
	GoldBonusCounter=0
	BumpLight1.state=1
	BumpLight2.state=1
	BumpLight3.state=1
	BumpLight4.state=1

End Sub



Sub LightsOut

	BonusCounter=0
	HoleCounter=0
	BumpLight1.state=0
	BumpLight2.state=0
	BumpLight3.state=0
	BumpLight4.state=0

end sub

Sub ResetBalls()
	StopSound"BallyBuzzer"


	TempMultiCounter=BallsPerGame-BallInPlay

	ResetBallDrops
	BonusMultiplier=1
	TableTilted=false
	If B2SOn then

		Controller.B2SSetTilt 0
		Controller.B2SSetData 80+Player,1
	end if
	LightTilt.state = 0
	'TiltReel.SetValue(0)
	GoldBonusCounter=1
	GoldBonus(GoldBonusCounter).state=1
	PlasticsOn
	'CreateBallID BallRelease
	Ballrelease.CreateSizedBall 25
	Ballrelease.Kick 40,7
	RandomSoundBallRelease Ballrelease
	DOF 125, 2
	BallInPlayReel.SetValue(BallInPlay)
	BallInPlay001.state=0
	BallInPlay002.state=0
	BallInPlay003.state=0
	BallInPlay004.state=0
	BallInPlay005.state=0
	EVAL("BallInPlay00"&BallInPlay).state=1

	'	InstructCard.image="IC"+FormatNumber(BallsPerGame,0)+FormatNumber(TargetSetting,0)+FormatNumber(BallInPlay,0)
	'	CardLight1.State = ABS(CardLight1.State-1)


End Sub




sub IncreaseGoldBonus
	if GoldBonusCounter<15 then
		If GoldBonusCounter<10 then
			GoldBonus(GoldBonusCounter).state=0
			GoldBonusCounter=GoldBonusCounter+1
			GoldBonus(GoldBonusCounter).state=1
		elseIf GoldBonusCounter>9 then
			GoldBonus(GoldBonusCounter-10).state=0
			GoldBonusCounter=GoldBonusCounter+1
			GoldBonus(10).state=1
			GoldBonus(GoldBonusCounter-10).state=1	
		end if
	end if
end sub

sub ScoreSilverBonus

	If DoubleBonus.state=0 then
		ScoreMotorStepper=0

	else
		ScoreMotorStepper=0

	end if
	CollectSilverBonus.enabled=1
end sub

sub CollectSilverBonus_timer
	if MotorRunning=1 then
		exit sub
	end if
	if GoldBonusCounter<1 then
		CollectSilverBonus.enabled=0
		GoldBonusCounter=1
		GoldBonus(GoldBonusCounter).state=1
	else
		if ScoreMotorStepper<5 then
			'			If TableTilted=false then
			'				SetMotor(5000)
			'			end if
			If DoubleBonus.state=0 then
				if TableTilted=false then
					SetMotor(5000)
				end if
			else
				if TableTilted=false then
					AddScore(10000)
				end if
			end if
			GoldBonus(GoldBonusCounter).state=0
			GoldBonusCounter=GoldBonusCounter-1
			if GoldBonusCounter>=0 then
				GoldBonus(GoldBonusCounter).state=1
			end if
			ScoreMotorStepper=ScoreMotorStepper+1
		else
			If DoubleBonus.state=0 then
				ScoreMotorStepper=0
			else
				ScoreMotorStepper=0

			end if
		end if
	end if
end sub



sub ScoreGoldBonus
	ScoreMotorStepper=0
	CollectGoldBonus.interval=135
	CollectGoldBonus.enabled=1
end sub

sub CollectGoldBonus_timer
	if MotorRunning=1 then
		exit sub
	end if
	if GoldBonusCounter<1 then
		CollectGoldBonus.enabled=0
		NextBallDelay.enabled=true
	else
		If DoubleBonus.state=1 then
			Select case ScoreMotorStepper
				case 0,1,3,4:
					AddScore(1000)

				case 2,5:
					PlaySound"BallyClunk"
					If GoldBonusCounter>10 then
						GoldBonus(GoldBonusCounter-10).state=0
					else
						GoldBonus(GoldBonusCounter).state=0

					end if

					GoldBonusCounter=GoldBonusCounter-1
					if GoldBonusCounter>=0 then
						If GoldBonusCounter<=10 then
							GoldBonus(GoldBonusCounter).state=1
						elseif GoldBonusCounter>10 then
							GoldBonus(10).state=1
							GoldBonus(GoldBonusCounter-10).state=1
						end if
					end if
				case 6:


				case 7:


			end select
			ScoreMotorStepper=ScoreMotorStepper+1
			If ScoreMotorStepper>7 then
				ScoreMotorStepper=0
			end if
		else
			Select Case ScoreMotorStepper
				case 0,1,2,3,4:
					AddScore(1000)
					If GoldBonusCounter>10 then
						GoldBonus(GoldBonusCounter-10).state=0
					else
						GoldBonus(GoldBonusCounter).state=0

					end if
					GoldBonusCounter=GoldBonusCounter-1
					if GoldBonusCounter>=0 then
						If GoldBonusCounter<=10 then
							GoldBonus(GoldBonusCounter).state=1
						elseif GoldBonusCounter>10 then
							GoldBonus(10).state=1
							GoldBonus(GoldBonusCounter-10).state=1
						end if
					end if
				case 5:
					PlaySound"BallyClunk"


			end select
			ScoreMotorStepper=ScoreMotorStepper+1
			If ScoreMotorStepper>5 then
				ScoreMotorStepper=0
			end if
		end if
	end if
end sub			






sub OffRollovers


end sub



sub resettimer_timer
	rst=rst+1
	if rst>1 and rst<12 then
		ResetReelsToZero(1)
	end if  
	if rst=13 then
		playsound "StartBall1"
	end if
	if rst=14 then
		newgame
		resettimer.enabled=false
	end if
end sub

Sub ResetReelsToZero(reelzeroflag)
	dim d1(5)
	dim d2(5)
	dim scorestring1, scorestring2

	If reelzeroflag=1 then
		scorestring1=CStr(Score(1))
		scorestring2=CStr(Score(2))
		scorestring1=right("00000" & scorestring1,5)
		scorestring2=right("00000" & scorestring2,5)
		for i=0 to 4
			d1(i)=CInt(mid(scorestring1,i+1,1))
			d2(i)=CInt(mid(scorestring2,i+1,1))
		next
		for i=0 to 4
			if d1(i)>0 then 
				d1(i)=d1(i)+1
				if d1(i)>9 then d1(i)=0
			end if
			if d2(i)>0 then 
				d2(i)=d2(i)+1
				if d2(i)>9 then d2(i)=0
			end if

		next
		Score(1)=(d1(0)*10000) + (d1(1)*1000) + (d1(2)*100) + (d1(3)*10) + d1(4)
		Score(2)=(d2(0)*10000) + (d2(1)*1000) + (d2(2)*100) + (d2(3)*10) + d2(4)
		If B2SOn Then
			Controller.B2SSetScorePlayer 1, Score(1)
			Controller.B2SSetScorePlayer 2, Score(2)
		End If
		PlayerScores(0).SetValue(Score(1))
		PlayerScoresOn(0).SetValue(Score(1))
		PlayerScores(1).SetValue(Score(2))
		PlayerScoresOn(1).SetValue(Score(2))

		scorestring1=CStr(Score(3))
		scorestring2=CStr(Score(4))
		scorestring1=right("00000" & scorestring1,5)
		scorestring2=right("00000" & scorestring2,5)
		for i=0 to 4
			d1(i)=CInt(mid(scorestring1,i+1,1))
			d2(i)=CInt(mid(scorestring2,i+1,1))
		next
		for i=0 to 4
			if d1(i)>0 then 
				d1(i)=d1(i)+1
				if d1(i)>9 then d1(i)=0
			end if
			if d2(i)>0 then 
				d2(i)=d2(i)+1
				if d2(i)>9 then d2(i)=0
			end if

		next
		Score(3)=(d1(0)*10000) + (d1(1)*1000) + (d1(2)*100) + (d1(3)*10) + d1(4)
		Score(4)=(d2(0)*10000) + (d2(1)*1000) + (d2(2)*100) + (d2(3)*10) + d2(4)
		If B2SOn Then
			Controller.B2SSetScorePlayer 3, Score(3)
			Controller.B2SSetScorePlayer 4, Score(4)
		End If
		PlayerScores(2).SetValue(Score(3))
		PlayerScoresOn(2).SetValue(Score(3))
		PlayerScores(3).SetValue(Score(4))
		PlayerScoresOn(3).SetValue(Score(4))

	end if

end sub



sub NextBallDelay_timer()
	NextBallDelay.enabled=false
	nextball

end sub

sub newgame
	InProgress=true
	queuedscore=0
	for i = 1 to 4
		Score(i)=0
		Score100K(1)=0
		HighScorePaid(i)=false
		Replay1Paid(i)=false
		Replay2Paid(i)=false
		Replay3Paid(i)=false
	next
	If B2SOn Then
		Controller.B2SSetTilt 0
		Controller.B2SSetGameOver 0
		Controller.B2SSetMatch 0
		'		Controller.B2SSetScorePlayer1 0
		'		Controller.B2SSetScorePlayer2 0
		'		Controller.B2SSetScorePlayer3 0
		'		Controller.B2SSetScorePlayer4 0
		Controller.B2SSetBallInPlay BallInPlay

	End if

	BumpLight1.state=1
	BumpLight2.state=1
	BumpLight3.state=1
	AlternatingRelay=0
	ZeroNineCounter=0
	'LeftSpinnerCounter=0
	BumpersOn
	BonusCounter=0
	BallCounter=0
	TargetLeftFlag=1
	TargetCenterFlag=1
	TargetRightFlag=1
	TargetSequenceComplete=0


	AlternatingRelay=1
	ToggleAlternatingRelay
	'	IncreaseBonus
	'	ToggleBumper
	EightLit=1
	ResetBalls
End sub

sub nextball
	Shootagainvid
	If B2SOn Then
		Controller.B2SSetTilt 0
		Controller.B2SSetData 81,0
		Controller.B2SSetData 82,0
		Controller.B2SSetData 83,0
		Controller.B2SSetData 84,0
	End If

	If ShootAgainLight.state=0 then
		Player=Player+1
	else
		ShootAgainLight.state=0
		LightSamePlayer.state=0
		'ShootAgainReel.SetValue(0)
		if B2SOn then
			Controller.B2SSetShootAgain 0
		end if
	end if

	'GameOver 

	If Player>Players Then
		BallInPlay=BallInPlay+1
		If BallInPlay>BallsPerGame then
			PlaySound("MotorLeer"):PuPEvent 78
			PlaySound"dxGoodnight":PuPEvent 61
			EndMusic:MusicIsOn=False
			PlaySound"stairway",-1
			InProgress=false

			If B2SOn Then
				Controller.B2SSetGameOver 1
				Controller.B2SSetPlayerUp 0
				Controller.B2SSetBallInPlay 0
				Controller.B2SSetCanPlay 0
			End If
			For each obj in PlayerHuds
				obj.state=0
			next
			If Table1.ShowDT = True then
				For each obj in PlayerScores
					obj.visible=1
				Next
				For each obj in PlayerScoresOn
					obj.visible=0
				Next
				For each obj in PlayerHUDScores
					obj.state=0
				Next
			end If
			'GameOverReel.SetValue(1)
			LightGameOver.state=1
			'			InstructCard.image="IC"+FormatNumber(BallsPerGame,0)+FormatNumber(TargetSetting,0)+"0"
			'			CardLight1.State = ABS(CardLight1.State-1)
			BallInPlayReel.SetValue(0)
			BallInPlay001.state=0
			BallInPlay002.state=0
			BallInPlay003.state=0
			BallInPlay004.state=0
			BallInPlay005.state=0
			CanPlay001.state=0
			CanPlay002.state=0
			CanPlay003.state=0
			CanPlay004.state=0
			LeftFlipper.RotateToStart
			RightFlipper.RotateToStart
			LightsOut
			BumpersOff
			PlasticsOff
			checkmatch
			CheckHighScore
			Players=0


			TimerTitle1.enabled=1
			TimerTitle2.enabled=1
			TimerTitle3.enabled=1
			TimerTitle4.enabled=1
			TimerTitle4.enabled=1
			HighScoreTimer.interval=100
			HighScoreTimer.enabled=True
		Else
			Player=1
			If B2SOn Then
				Controller.B2SSetPlayerUp Player
				Controller.B2SSetBallInPlay BallInPlay

			End If
			'			PlaySound("RotateThruPlayers")
			TempPlayerUp=Player
			'			PlayerUpRotator.enabled=true
			PlayStartBall.enabled=true
			For each obj in PlayerHuds
				obj.state=0
			next
			If Table1.ShowDT = True then
				For each obj in PlayerScores
					obj.visible=1
				Next
				For each obj in PlayerScoresOn
					obj.visible=0
				Next
				For each obj in PlayerHUDScores
					obj.state=0
				Next
				PlayerHuds(Player-1).state=1
				PlayerHUDScores(Player-1).state=1
				PlayerScores(Player-1).visible=0
				PlayerScoresOn(Player-1).visible=1
			end If

			ResetBalls
		End If
	Else 
		If B2SOn Then
			Controller.B2SSetPlayerUp Player
			Controller.B2SSetBallInPlay BallInPlay
		End If
		'		PlaySound("RotateThruPlayers")
		TempPlayerUp=Player
		'		PlayerUpRotator.enabled=true
		PlayStartBall.enabled=true
		For each obj in PlayerHuds
			obj.state=0
		next
		If Table1.ShowDT = True then
			For each obj in PlayerScores
				obj.visible=1
			Next
			For each obj in PlayerScoresOn
				obj.visible=0
			Next
			For each obj in PlayerHUDScores
				obj.state=0
			Next
			PlayerHuds(Player-1).state=1
			PlayerHUDScores(Player-1).state=1
			PlayerScores(Player-1).visible=0
			PlayerScoresOn(Player-1).visible=1
		end If
		ResetBalls
	End If

End sub

sub CheckHighScore
	Dim playertops
	dim si
	dim sj
	dim stemp
	dim stempplayers
	for i=1 to 4
		sortscores(i)=0
		sortplayers(i)=0
	next
	playertops=0
	for i = 1 to Players
		sortscores(i)=Score(i)
		sortplayers(i)=i
	next

	for si = 1 to Players
		for sj = 1 to Players-1
			if sortscores(sj)>sortscores(sj+1) then
				stemp=sortscores(sj+1)
				stempplayers=sortplayers(sj+1)
				sortscores(sj+1)=sortscores(sj)
				sortplayers(sj+1)=sortplayers(sj)
				sortscores(sj)=stemp
				sortplayers(sj)=stempplayers
			end if
		next
	next
	ScoreChecker=4
	CheckAllScores=1
	NewHighScore sortscores(ScoreChecker),sortplayers(ScoreChecker)
	savehs
end sub


sub checkmatch
	Dim tempmatch
	tempmatch=Int(Rnd*10)
	Match=tempmatch*10
	MatchReel.SetValue(tempmatch+1)

	If B2SOn Then
		If Match = 0 Then
			Controller.B2SSetMatch 100
		Else
			Controller.B2SSetMatch Match
		End If
	End if
	for i = 1 to Players
		if (Match*10)=(Score(i) mod 100) then
			AddSpecial
		end if
	next
end sub

Sub TiltTimer_Timer()
	if TiltCount > 0 then TiltCount = TiltCount - 1
	if TiltCount = 0 then
		TiltTimer.Enabled = False
	end if
end sub

Sub TiltIt()
	TiltCount = TiltCount + 1
	if TiltCount = 3 then
		TableTilted=True
		PlasticsOff
		BumpersOff
		PuPEvent 59
		LeftFlipper.RotateToStart
		RightFlipper.RotateToStart
		StopSound"mxbuzz"
		StopSound"mxbuzzL"
		LightTilt.state = 1
		'TiltReel.SetValue(1)
		If B2Son then
			Controller.B2SSetTilt 1
		end if
	else
		TiltTimer.Interval = 500
		TiltTimer.Enabled = True
	end if

end sub



Sub PlayStartBall_timer()

	PlayStartBall.enabled=false
	PlaySound("StartBall1")
end sub

Sub PlayerUpRotator_timer()
	If RotatorTemp<5 then
		TempPlayerUp=TempPlayerUp+1
		If TempPlayerUp>4 then
			TempPlayerUp=1
		end if
		If B2SOn Then
			Controller.B2SSetPlayerUp TempPlayerUp
		End If

	else
		if B2SOn then
			Controller.B2SSetPlayerUp Player
		end if
		PlayerUpRotator.enabled=false
		RotatorTemp=1
	end if
	RotatorTemp=RotatorTemp+1


end sub

sub savehs
	' Based on Black's Highscore routines
	Dim FileObj
	Dim ScoreFile
	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & HSFileName,True)
	ScoreFile.WriteLine 0
	ScoreFile.WriteLine Credits
	scorefile.writeline BallsPerGame
	scorefile.writeline ChimesOn
	scorefile.writeline ReplayLevel
	scorefile.writeline GameOption
	scorefile.writeline CircusSetting
	for xx=1 to 5
		scorefile.writeline HSScore(xx)
	next
	for xx=1 to 5
		scorefile.writeline HSName(xx)
	next
	ScoreFile.Close
	Set ScoreFile=Nothing
	Set FileObj=Nothing
end sub

sub SaveLMEMConfig
	Dim FileObj
	Dim LMConfig
	dim temp1
	dim tempb2s
	tempb2s=0
	if B2SOn=true then
		tempb2s=1
	else
		tempb2s=0
	end if
	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	Set LMConfig=FileObj.CreateTextFile(UserDirectory & LMEMTableConfig,True)
	LMConfig.WriteLine tempb2s
	LMConfig.Close
	Set LMConfig=Nothing
	Set FileObj=Nothing

end Sub

sub LoadLMEMConfig
	Dim FileObj
	Dim LMConfig
	dim tempC
	dim tempb2s

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & LMEMTableConfig) then
		Exit Sub
	End if
	Set LMConfig=FileObj.GetFile(UserDirectory & LMEMTableConfig)
	Set TextStr2=LMConfig.OpenAsTextStream(1,0)
	If (TextStr2.AtEndOfStream=True) then
		Exit Sub
	End if
	tempC=TextStr2.ReadLine
	TextStr2.Close
	tempb2s=cdbl(tempC)
	if tempb2s=0 then
		B2SOn=false
	else
		B2SOn=true
	end if
	Set LMConfig=Nothing
	Set FileObj=Nothing
end sub

sub SaveLMEMConfig2
	If ShadowConfigFile=false then exit sub
	Dim FileObj
	Dim LMConfig2
	dim temp1
	dim temp2
	dim tempBS
	dim tempFS

	if EnableFlipperShadow=true then
		tempFS=1
	else
		tempFS=0
	end if

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	Set LMConfig2=FileObj.CreateTextFile(UserDirectory & LMEMShadowConfig,True)
	LMConfig2.WriteLine tempBS
	LMConfig2.WriteLine tempFS
	LMConfig2.Close
	Set LMConfig2=Nothing
	Set FileObj=Nothing

end Sub

sub LoadLMEMConfig2
	If ShadowConfigFile=false then
		EnableFlipperShadow = ShadowFlippersOn
		exit sub
	end if
	Dim FileObj
	Dim LMConfig2
	dim tempC
	dim tempD
	dim tempFS
	dim tempBS

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & LMEMShadowConfig) then
		Exit Sub
	End if
	Set LMConfig2=FileObj.GetFile(UserDirectory & LMEMShadowConfig)
	Set TextStr2=LMConfig2.OpenAsTextStream(1,0)
	If (TextStr2.AtEndOfStream=True) then
		Exit Sub
	End if
	tempC=TextStr2.ReadLine
	tempD=TextStr2.Readline
	TextStr2.Close
	tempBS=cdbl(tempC)
	tempFS=cdbl(tempD)
	if tempFS=0 then
		EnableFlipperShadow=false
		FlipperLSh.visible=false
		FLipperRSh.visible=false
	else
		EnableFlipperShadow=true
	end if
	Set LMConfig2=Nothing
	Set FileObj=Nothing

end sub


sub loadhs
	' Based on Black's Highscore routines
	Dim FileObj
	Dim ScoreFile
	dim temp1
	dim temp2
	dim temp3
	dim temp4
	dim temp5
	dim temp6
	dim temp7

	dim temp8
	dim temp9
	dim temp10
	dim temp11
	dim temp12
	dim temp13
	dim temp14
	dim temp15
	dim temp16
	dim temp17

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & HSFileName) then
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & HSFileName)
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
	If (TextStr.AtEndOfStream=True) then
		Exit Sub
	End if
	temp1=TextStr.ReadLine
	temp2=textstr.readline
	temp3=textstr.readline
	temp4=textstr.readline
	temp5=textstr.readline
	temp6=textstr.readline
	temp7=textstr.readline

	HighScore=cdbl(temp1)
	if HighScore<1 then
		temp8=textstr.readline
		temp9=textstr.readline
		temp10=textstr.readline
		temp11=textstr.readline
		temp12=textstr.readline
		temp13=textstr.readline
		temp14=textstr.readline
		temp15=textstr.readline
		temp16=textstr.readline
		temp17=textstr.readline
	end if

	TextStr.Close
	Credits=cdbl(temp2)
	BallsPerGame=cdbl(temp3)
	ChimesOn=cdbl(temp4)
	ReplayLevel=cdbl(temp5)
	GameOption=cdbl(temp6)
	CircusSetting=cdbl(temp7)
	if HighScore<1 then
		HSScore(1) = int(temp8)
		HSScore(2) = int(temp9)
		HSScore(3) = int(temp10)
		HSScore(4) = int(temp11)
		HSScore(5) = int(temp12)

		HSName(1) = temp13
		HSName(2) = temp14
		HSName(3) = temp15
		HSName(4) = temp16
		HSName(5) = temp17
	end if
	Set ScoreFile=Nothing
	Set FileObj=Nothing
end sub

Sub DisplayHighScore


end sub

sub InitPauser5_timer

	DisplayHighScore
	CreditsReel.SetValue(Credits)
	InitPauser5.enabled=false
end sub



sub BumpersOff

	BumpLight1.visible=0
	BumpLight2.visible=0
	BumpLight3.visible=0

	if colormod=0 then playfield_off.visible=1 
	if colormod=1 then playfield_off.visible=1 : playfield_off.image="pf2_off"

end sub

sub BumpersOn


	BumpLight1.visible=1
	BumpLight2.visible=1
	BumpLight3.visible=1

	if colormod = 0 Then playfield_off.visible=0
	if colormod = 1 Then playfield_off.visible=1 : playfield_off.image="pf2_on"

end sub

Sub PlasticsOn

	For each obj in Flashers
		obj.visible=1
		layer1.image="layer1"
		layer3.image="layer3"
		metal.image="metal"
		backmetal.image="backmetal"
		bracket.image="bakery"
	next

	if colormod = 0 Then layer2.image="layer2"
	if colormod = 1 Then layer2.image="layer2color"

end sub

Sub PlasticsOff

	For each obj in Flashers
		obj.visible=0
		layer1.image="layer1off"
		layer3.image="layer3off"
		metal.image="metaloff"
		backmetal.image="backmetaloff"
		bracket.image="bakeryoff"
	next

	if colormod=0 then layer2.image="layer2off"
	if colormod=1 then layer2.image="layer2offcolor"

	StopSound "mxbuzz"
	StopSound "mxbuzzL"
end sub

Sub SetupReplayTables

	Replay1Table(1)=59000
	Replay1Table(2)=59000
	Replay1Table(3)=72000
	Replay1Table(4)=80000
	Replay1Table(5)=88000
	Replay1Table(6)=88000
	Replay1Table(7)=94000
	Replay1Table(8)=96000
	Replay1Table(9)=100000
	Replay1Table(10)=100000
	Replay1Table(11)=112000
	Replay1Table(12)=112000
	Replay1Table(13)=116000
	Replay1Table(14)=120000
	Replay1Table(15)=120000
	Replay1Table(16)=120000
	Replay1Table(17)=132000
	Replay1Table(18)=138000

	Replay2Table(1)=114000
	Replay2Table(2)=140000
	Replay2Table(3)=118000
	Replay2Table(4)=118000
	Replay2Table(5)=120000
	Replay2Table(6)=132000
	Replay2Table(7)=132000
	Replay2Table(8)=132000
	Replay2Table(9)=134000
	Replay2Table(10)=134000
	Replay2Table(11)=134000
	Replay2Table(12)=140000
	Replay2Table(13)=138000
	Replay2Table(14)=138000
	Replay2Table(15)=162000
	Replay2Table(16)=139000
	Replay2Table(17)=134000
	Replay2Table(18)=170000

	Replay3Table(1)=166000
	Replay3Table(2)=168000
	Replay3Table(3)=164000
	Replay3Table(4)=162000
	Replay3Table(5)=162000
	Replay3Table(6)=9990000
	Replay3Table(7)=166000
	Replay3Table(8)=9990000
	Replay3Table(9)=9990000
	Replay3Table(10)=168000
	Replay3Table(11)=168000
	Replay3Table(12)=9990000
	Replay3Table(13)=170000
	Replay3Table(14)=9990000
	Replay3Table(15)=9990000
	Replay3Table(16)=168000
	Replay3Table(17)=9990000
	Replay3Table(18)=9990000


	Replay4Table(1)=9990000
	Replay4Table(2)=9990000
	Replay4Table(3)=9990000
	Replay4Table(4)=9990000
	Replay4Table(5)=9990000
	Replay4Table(6)=9990000
	Replay4Table(7)=9990000
	Replay4Table(8)=9990000
	Replay4Table(9)=9990000
	Replay4Table(10)=9990000
	Replay4Table(11)=9990000
	Replay4Table(12)=9990000
	Replay4Table(13)=9990000
	Replay4Table(14)=9990000
	Replay4Table(15)=9990000
	Replay4Table(16)=9990000
	Replay4Table(17)=9990000
	Replay4Table(18)=9990000

	ReplayTableMax=18


end sub

Sub RefreshReplayCard
	Dim tempst1
	Dim tempst2

	tempst1=FormatNumber(BallsPerGame,0)
	tempst2=FormatNumber(ReplayLevel,0)


	ReplayCard.image = "SC" + tempst2
	Replay1=Replay1Table(ReplayLevel)
	Replay2=Replay2Table(ReplayLevel)
	Replay3=Replay3Table(ReplayLevel)
	Replay4=Replay4Table(ReplayLevel)
'	CardLight2.State = ABS(CardLight2.State-1)
end sub


'****************************************
'  SCORE MOTOR
'****************************************

ScoreMotorTimer.Enabled = 1
ScoreMotorTimer.Interval = 135 '135
AddScoreTimer.Enabled = 1
AddScoreTimer.Interval = 135

Dim queuedscore
Dim MotorMode
Dim MotorPosition

Sub SetMotor(y)
	Select Case ScoreMotorAdjustment
		Case 0:
			queuedscore=queuedscore+y
		Case 1:
			If MotorRunning<>1 And InProgress=true then
				queuedscore=queuedscore+y
			end if
	end Select
end sub

Sub SetMotor2(x)
	If MotorRunning<>1 And InProgress=true then
		MotorRunning=1

		Select Case x
			Case 40:
				MotorMode=10
				MotorPosition=4
			Case 50:
				MotorMode=10
				MotorPosition=5

			Case 500:
				MotorMode=100
				MotorPosition=5

			Case 1000:
				MotorMode=1000
				MotorPosition=1

			Case 3000:
				MotorMode=1000
				MotorPosition=3

			Case 5000:
				MotorMode=1000
				MotorPosition=5

			Case 10000:
				MotorMode=10000
				MotorPosition=1
			Case 20000:
				MotorMode=10000
				MotorPosition=2

			Case 30000:
				MotorMode=10000
				MotorPosition=3

			Case 40000:
				MotorMode=10000
				MotorPosition=4

			Case 50000:
				MotorMode=10000
				MotorPosition=5

		End Select
	End If
End Sub

Sub AddScoreTimer_Timer
	Dim tempscore


	If MotorRunning<>1 And InProgress=true then
		if queuedscore>=50000 then
			tempscore=50000
			queuedscore=queuedscore-50000
			SetMotor2(50000)
			exit sub
		end if
		if queuedscore>=40000 then
			tempscore=4000
			queuedscore=queuedscore-40000
			SetMotor2(40000)
			exit sub
		end if

		if queuedscore>=30000 then
			tempscore=30000
			queuedscore=queuedscore-30000
			SetMotor2(30000)
			exit sub
		end if

		if queuedscore>=20000 then
			tempscore=20000
			queuedscore=queuedscore-20000
			SetMotor2(20000)
			exit sub
		end if

		if queuedscore>=10000 then
			tempscore=10000
			queuedscore=queuedscore-10000
			SetMotor2(10000)
			exit sub
		end if

		if queuedscore>=5000 then
			tempscore=5000
			queuedscore=queuedscore-5000
			SetMotor2(5000)
			exit sub
		end if

		if queuedscore>=3000 then
			tempscore=3000
			queuedscore=queuedscore-3000
			SetMotor2(3000)
			exit sub
		end if

		if queuedscore>=1000 then
			tempscore=1000
			queuedscore=queuedscore-1000
			SetMotor2(1000)
			exit sub
		end if

		if queuedscore>=500 then
			tempscore=500
			queuedscore=queuedscore-500
			SetMotor2(500)
			exit sub
		end if	

		if queuedscore>=50 then
			tempscore=50
			queuedscore=queuedscore-50
			SetMotor2(50)
			exit sub
		end if	

		if queuedscore>=40 then
			tempscore=40
			queuedscore=queuedscore-40
			SetMotor2(40)
			exit sub
		end if	

	End If


end Sub

Sub ScoreMotorTimer_Timer
	If MotorPosition > 0 Then
		Select Case MotorPosition
			Case 5,4,3,2:
				If MotorMode=10000 Then 
					AddScore(10000)
				end if
				if MotorMode=1000 then
					AddScore(1000)
				End If
				if MotorMode=100 then
					AddScore(100)
				End if
				if MotorMode=10 then
					AddScore(10)
				End if
				MotorPosition=MotorPosition-1
			Case 1:
				If MotorMode=10000 Then 
					AddScore(10000)
				end if
				If MotorMode=1000 then 
					AddScore(1000)
				End If
				if MotorMode=100 then
					AddScore(100)
				End if
				if MotorMode=10 then
					AddScore(10)
				End if
				MotorPosition=0:MotorRunning=0
		End Select
	End If
End Sub


Sub AddScore(x)
	If TableTilted=true then exit sub
	Select Case ScoreAdditionAdjustment
		Case 0:
			AddScore1(x)
		Case 1:
			AddScore2(x)
	end Select

end sub


Sub AddScore1(x)
	'	debugtext.text=score
	Select Case x
		Case 10:
			PlayChime(10)
			Score(Player)=Score(Player)+10
			'			If DoubleBonus.state=1 then
			'				Score(Player)=Score(Player)+10
			'			end if
			ToggleAlternatingRelay
		Case 100:
			PlayChime(100)
			Score(Player)=Score(Player)+100
			'			If DoubleBonus.state=1 then
			'				Score(Player)=Score(Player)+100
			'			end if

			'			debugscore=debugscore+10

		Case 1000:
			PlayChime(1000)
			Score(Player)=Score(Player)+1000
			'			If DoubleBonus.state=1 then
			'				Score(Player)=Score(Player)+1000
			'			end if

			'			debugscore=debugscore+100


		Case 10000:
			PlayChime(10000)
			Score(Player)=Score(Player)+10000
			'			If DoubleBonus.state=1 then
			'				Score(Player)=Score(Player)+10000
			'			end if

			'			debugscore=debugscore+1000

		Case 100000:
			PlayChime(10000)
			Score(Player)=Score(Player)+100000
			'			If DoubleBonus.state=1 then
			'				Score(Player)=Score(Player)+10000
			'			end if

			'			debugscore=debugscore+1000


	End Select
	PlayerScores(Player-1).AddValue(x)
	PlayerScoresOn(Player-1).AddValue(x)
	'	if DoubleBonus.state=1 then
	'		PlayerScores(Player-1).AddValue(x)
	'	end if
	If ScoreDisplay(Player)<100000 then
		ScoreDisplay(Player)=Score(Player)
	Else
		Score100K(Player)=Int(Score(Player)/100000)
		ScoreDisplay(Player)=Score(Player)-100000
	End If
	if Score(Player)=>100000 then
		EVAL("RollReel00"&Player).Setvalue(Score100K(Player))

		If B2SOn Then
			If Player=1 Then
				Controller.B2SSetScoreRolloverPlayer1 Score100K(Player)
			End If
			If Player=2 Then
				Controller.B2SSetScoreRolloverPlayer2 Score100K(Player)
			End If

			If Player=3 Then
				Controller.B2SSetScoreRolloverPlayer3 Score100K(Player)
			End If

			If Player=4 Then
				Controller.B2SSetScoreRolloverPlayer4 Score100K(Player)
			End If
		End If
	End If
	If B2SOn Then
		Controller.B2SSetScorePlayer Player, ScoreDisplay(Player)
	End If
	If Score(Player)>Replay1 and Replay1Paid(Player)=false then
		Replay1Paid(Player)=True
		AddSpecial
	End If
	If Score(Player)>Replay2 and Replay2Paid(Player)=false then
		Replay2Paid(Player)=True
		AddSpecial
	End If
	If Score(Player)>Replay3 and Replay3Paid(Player)=false then
		Replay3Paid(Player)=True
		AddSpecial
	End If
	If Score(Player)>Replay4 and Replay4Paid(Player)=false then
		Replay4Paid(Player)=True
		AddSpecial
	End If
	'	ScoreText.text=debugscore
End Sub

Sub AddScore2(x)
	Dim OldScore, NewScore, OldTestScore, NewTestScore
	OldScore = Score(Player)

	Select Case x
		Case 10:
			Score(Player)=Score(Player)+10
			'			If DoubleBonus.state=1 then
			'				Score(Player)=Score(Player)+10
			'			end if

		Case 100:
			Score(Player)=Score(Player)+100
			'			If DoubleBonus.state=1 then
			'				Score(Player)=Score(Player)+100
			'			end if

		Case 1000:
			Score(Player)=Score(Player)+1000
			'			If DoubleBonus.state=1 then
			'				Score(Player)=Score(Player)+1000
			'			end if

		Case 10000:
			Score(Player)=Score(Player)+10000
			'			If DoubleBonus.state=1 then
			'				Score(Player)=Score(Player)+10000
			'			end if

		Case 100000:
			Score(Player)=Score(Player)+100000
			'			If DoubleBonus.state=1 then
			'				Score(Player)=Score(Player)+100000
			'			end if

	End Select
	if Score(Player)=>100000 then
		If Score100K(Player)<1 then
			PlaySound"Ballymxmxbuzzfer"
			EVAL("RollReel00"&Player).Setvalue(1)
			If B2SOn Then
				If Player=1 Then
					Controller.B2SSetScoreRolloverPlayer1 1
				End If
				If Player=2 Then
					Controller.B2SSetScoreRolloverPlayer2 1
				End If

				If Player=3 Then
					Controller.B2SSetScoreRolloverPlayer3 1
				End If

				If Player=4 Then
					Controller.B2SSetScoreRolloverPlayer4 1
				End If
			End If
		end if
		Score100K(Player)=1
	End If
	NewScore = Score(Player)

	OldTestScore = OldScore
	NewTestScore = NewScore
	Do
		if OldTestScore < Replay1 and NewTestScore >= Replay1 then
			AddSpecial()
			NewTestScore = 0
		Elseif OldTestScore < Replay2 and NewTestScore >= Replay2 then
			AddSpecial()
			NewTestScore = 0
		Elseif OldTestScore < Replay3 and NewTestScore >= Replay3 then
			AddSpecial()
			NewTestScore = 0
		Elseif OldTestScore < Replay4 and NewTestScore >= Replay4 then
			AddSpecial()
			NewTestScore = 0
		End if
		NewTestScore = NewTestScore - 1000000
		OldTestScore = OldTestScore - 1000000
	Loop While NewTestScore > 0

	OldScore = int(OldScore / 10)	' divide by 10 for games with fixed 0 in 1s position, by 1 for games with real 1s digits
	NewScore = int(NewScore / 10)	' divide by 10 for games with fixed 0 in 1s position, by 1 for games with real 1s digits
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore&", OldScore Mod 10="&OldScore Mod 10 & ", NewScore % 10="&NewScore Mod 10)

	if (OldScore Mod 10 <> NewScore Mod 10) then
		PlayChime(10)
		ToggleAlternatingRelay
	end if

	OldScore = int(OldScore / 10)
	NewScore = int(NewScore / 10)
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore)
	if (OldScore Mod 10 <> NewScore Mod 10) then
		PlayChime(100)


	end if

	OldScore = int(OldScore / 10)
	NewScore = int(NewScore / 10)
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore)
	if (OldScore Mod 10 <> NewScore Mod 10) then
		PlayChime(1000)



	end if

	OldScore = int(OldScore / 10)
	NewScore = int(NewScore / 10)
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore)
	if (OldScore Mod 10 <> NewScore Mod 10) then
		PlayChime(10000)
	end if

	If B2SOn Then
		Controller.B2SSetScorePlayer Player, Score(Player)
	End If

	OldScore = int(OldScore / 10)
	NewScore = int(NewScore / 10)
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore)
	if (OldScore Mod 10 <> NewScore Mod 10) then
		PlayChime(10000)
	end if

	If B2SOn Then
		Controller.B2SSetScorePlayer Player, Score(Player)
	End If
	'	EMReel1.SetValue Score(Player)
	PlayerScores(Player-1).AddValue(x)
	PlayerScoresOn(Player-1).AddValue(x)
	'	If DoubleBonus.state=1 then
	'		PlayerScores(Player-1).AddValue(x)
	'	end if
End Sub



Sub PlayChime(x)
	if ChimesOn=0 then
		Select Case x
			Case 10

				PlaySound SoundFXDOF("mxBell0",141,DOFPulse,DOFChimes)

			Case 100

				PlaySound SoundFXDOF("mxBell000",142,DOFPulse,DOFChimes)
			Case 1000

				PlaySound SoundFXDOF("mxBell0000",143,DOFPulse,DOFChimes)
		End Select
	else
		Select Case x
			Case 10
				If LastChime10=1 Then
					PlaySound SoundFXDOF("SJ_Chime_10a",141,DOFPulse,DOFChimes)
					LastChime10=0
				Else
					PlaySound SoundFXDOF("SJ_Chime_10b",141,DOFPulse,DOFChimes)
					LastChime10=1
				End If
			Case 100
				If LastChime100=1 Then
					PlaySound SoundFXDOF("SJ_Chime_100a",142,DOFPulse,DOFChimes)
					LastChime100=0
				Else
					PlaySound SoundFXDOF("SJ_Chime_100b",142,DOFPulse,DOFChimes)
					LastChime100=1
				End If
			Case 1000
				If LastChime1000=1 Then
					PlaySound SoundFXDOF("SJ_Chime_1000a",143,DOFPulse,DOFChimes)
					LastChime1000=0
				Else
					PlaySound SoundFXDOF("SJ_Chime_1000b",143,DOFPulse,DOFChimes)
					LastChime1000=1
				End If
		End Select
	end if
End Sub


Sub HideOptions()

end sub

'*****************************************
'			FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()

End Sub


'******************************************************
'****  BALL ROLLING AND DROP SOUNDS
'******************************************************
'
' Be sure to call RollingUpdate in a timer with a 10ms interval see the GameTimer_Timer() sub
Const tnob = 2 ' total number of balls
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

Sub RollingSoundTimer_Timer()
	Dim b, BOT
	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob - 1
		' Comment the next line if you are not implementing Dyanmic Ball Shadows
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
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

'		' "Static" Ball Shadows
'		' Comment the next If block, if you are not implementing the Dyanmic Ball Shadows
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
	Next
End Sub


'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************



'*****************************************
'			BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow001,BallShadow002,BallShadow003)

Sub BallShadowUpdate_timer()
	Dim BOT, b
	BOT = GetBalls
	' hide shadow of deleted balls
	If UBound(BOT)<(tnob-1) Then
		For b = (UBound(BOT) + 1) to (tnob-1)
			BallShadow(b).visible = 0
		Next
	End If
	' exit the Sub if no balls on the table
	If UBound(BOT) = -1 Then Exit Sub
	' render the shadow for each ball
	For b = 0 to UBound(BOT)
		If BOT(b).X < Table1.Width/2 Then
			BallShadow(b).X = ((BOT(b).X) + ((BOT(b).X - (Table1.Width/2))/7))
		Else
			BallShadow(b).X = ((BOT(b).X) + ((BOT(b).X - (Table1.Width/2))/7))
		End If
		ballShadow(b).Y = BOT(b).Y + 10
		If BOT(b).Z > 20 Then
			BallShadow(b).visible = 1
		Else
			BallShadow(b).visible = 0
		End If
	Next
End Sub

' ============================================================================================
' GNMOD - Multiple High Score Display and Collection
' ============================================================================================
Dim EnteringInitials		' Normally zero, set to non-zero to enter initials
EnteringInitials = 0

Dim PlungerPulled
PlungerPulled = 0

Dim SelectedChar			' character under the "cursor" when entering initials

Dim HSTimerCount			' Pass counter for HS timer, scores are cycled by the timer
HSTimerCount = 5			' Timer is initially enabled, it'll wrap from 5 to 1 when it's displayed

Dim InitialString			' the string holding the player's initials as they're entered

Dim AlphaString				' A-Z, 0-9, space (_) and backspace (<)
Dim AlphaStringPos			' pointer to AlphaString, move forward and backward with flipper keys
AlphaString = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_<"

Dim HSNewHigh				' The new score to be recorded

Dim HSScore(5)				' High Scores read in from config file
Dim HSName(5)				' High Score Initials read in from config file

' default high scores, remove this when the scores are available from the config file
HSScore(1) = 75000
HSScore(2) = 70000
HSScore(3) = 60000
HSScore(4) = 55000
HSScore(5) = 50000

HSName(1) = "AAA"
HSName(2) = "ZZZ"
HSName(3) = "XXX"
HSName(4) = "ABC"
HSName(5) = "BBB"

Sub HighScoreTimer_Timer

	if EnteringInitials then
		if HSTimerCount = 1 then
			SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
			HSTimerCount = 2
		else
			SetHSLine 3, InitialString
			HSTimerCount = 1
		end if
	elseif InProgress then
		SetHSLine 1, "HIGH SCORE1"
		SetHSLine 2, HSScore(1)
		SetHSLine 3, HSName(1)
		HSTimerCount = 5	' set so the highest score will show after the game is over
		HighScoreTimer.enabled=false
	elseif CheckAllScores then
		NewHighScore sortscores(ScoreChecker),sortplayers(ScoreChecker)

	else
		If pInAttract=False Then GameoverDelay

		' cycle through high scores
		HighScoreTimer.interval=2000
		HSTimerCount = HSTimerCount + 1
		if HsTimerCount > 5 then
			HSTimerCount = 1
		End If
		SetHSLine 1, "HIGH SCORE"+FormatNumber(HSTimerCount,0)
		SetHSLine 2, HSScore(HSTimerCount)
		SetHSLine 3, HSName(HSTimerCount)
	end if
End Sub

Function GetHSChar(String, Index)
	dim ThisChar
	dim FileName
	ThisChar = Mid(String, Index, 1)
	FileName = "PostIt"
	if ThisChar = " " or ThisChar = "" then
		FileName = FileName & "BL"
	elseif ThisChar = "<" then
		FileName = FileName & "LT"
	elseif ThisChar = "_" then
		FileName = FileName & "SP"
	else
		FileName = FileName & ThisChar
	End If
	GetHSChar = FileName
End Function

Sub SetHsLine(LineNo, String)
	dim Letter
	dim ThisDigit
	dim ThisChar
	dim StrLen
	dim LetterLine
	dim Index
	dim StartHSArray
	dim EndHSArray
	dim LetterName
	dim xfor
	StartHSArray=array(0,1,12,22)
	EndHSArray=array(0,11,21,31)
	StrLen = len(string)
	Index = 1
	
if EnteringInitials and HasPup = True then pDMDSetPage(8):PuPlayer.LabelSet pDMD, "EnterHS" & LineNo, String ,1,"":PuPEvent 120

	for xfor = StartHSArray(LineNo) to EndHSArray(LineNo)
		Eval("HS"&xfor).image = GetHSChar(String, Index)
		Index = Index + 1
	next

End Sub

Sub NewHighScore(NewScore, PlayNum)
	if NewScore > HSScore(5) then
		Playsound "hsfinal"

		HighScoreTimer.interval = 500
		HSTimerCount = 1
		AlphaStringPos = 1		' start with first character "A"
		EnteringInitials = 1	' intercept the control keys while entering initials
		InitialString = ""		' initials entered so far, initialize to empty
		SetHSLine 1, "PLAYER "+FormatNumber(PlayNum,0)
		SetHSLine 2, "ENTER NAME"
		SetHSLine 3, MID(AlphaString, AlphaStringPos, 1)
		HSNewHigh = NewScore
		For xx=1 to HighScoreReward
			AddSpecial
		next
	End if
	ScoreChecker=ScoreChecker-1
	if ScoreChecker=0 then
		CheckAllScores=0
	end if
End Sub

Sub CollectInitials(keycode)
	If keycode = LeftFlipperKey Then
		' back up to previous character
		AlphaStringPos = AlphaStringPos - 1
		if AlphaStringPos < 1 then
			AlphaStringPos = len(AlphaString)		' handle wrap from beginning to end
			if InitialString = "" then
				' Skip the backspace if there are no characters to backspace over
				AlphaStringPos = AlphaStringPos - 1
			End if
		end if
		SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
		PlaySound "target_hit_1"
	elseif keycode = RightFlipperKey Then
		' advance to next character
		AlphaStringPos = AlphaStringPos + 1
		if AlphaStringPos > len(AlphaString) or (AlphaStringPos = len(AlphaString) and InitialString = "") then
			' Skip the backspace if there are no characters to backspace over
			AlphaStringPos = 1
		end if
		SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
		PlaySound "target_hit_1"
	elseif keycode = StartGameKey or keycode = PlungerKey Then
		SelectedChar = MID(AlphaString, AlphaStringPos, 1)
		if SelectedChar = "_" then
			InitialString = InitialString & " "
			PlaySound("mxBell000")
		elseif SelectedChar = "<" then
			InitialString = MID(InitialString, 1, len(InitialString) - 1)
			if len(InitialString) = 0 then
				' If there are no more characters to back over, don't leave the < displayed
				AlphaStringPos = 1
			end if
			PlaySound("mxBell0000")
		else
			InitialString = InitialString & SelectedChar
			PlaySound("mxBell000")
		end if
		if len(InitialString) < 3 then
			SetHSLine 3, InitialString & SelectedChar
		End If
	End If
	if len(InitialString) = 3 then
		SetHSLine 3, InitialString
		' save the score
		for i = 5 to 1 step -1
			if i = 1 or (HSNewHigh > HSScore(i) and HSNewHigh <= HSScore(i - 1)) then
				' Replace the score at this location
				if i < 5 then
					' MsgBox("Moving " & i & " to " & (i + 1))
					HSScore(i + 1) = HSScore(i)
					HSName(i + 1) = HSName(i)
				end if
				' MsgBox("Saving initials " & InitialString & " to position " & i)
				EnteringInitials = 0
				HSScore(i) = HSNewHigh
				HSName(i) = InitialString
				HSTimerCount = 5
				HighScoreTimer_Timer
				HighScoreTimer.interval = 2000
				PlaySound "knocker_1" : PupEvent 121 
				PupEvent 122
				exit sub
			elseif i < 5 then
				' move the score in this slot down by 1, it's been exceeded by the new score
				' MsgBox("Moving " & i & " to " & (i + 1))
				HSScore(i + 1) = HSScore(i)
				HSName(i + 1) = HSName(i)
			end if
		next
	End If

End Sub
' END GNMOD
' ============================================================================================
' GNMOD - New Options menu
' ============================================================================================
Dim EnteringOptions
Dim CurrentOption
Dim OptionCHS
Dim MaxOption
Dim OptionHighScorePosition
Dim XOpt
Dim StartingArray
Dim EndingArray

StartingArray=Array(0,1,2,30,33,61,89,117,145,173,201,229)
EndingArray=Array(0,1,29,32,60,88,116,144,172,200,228,256)
EnteringOptions = 0
MaxOption = 9
OptionCHS = 0
OptionHighScorePosition = 0
Const OptionLinesToMark="111010011"
Const OptionLine1="" 'do not use this line
Const OptionLine2="" 'do not use this line
Const OptionLine3="" 'do not use this line
Const OptionLine4=""
Const OptionLine5="Game Option"
Const OptionLine6=""
Const OptionLine7=""
Const OptionLine8="" 'do not use this line
Const OptionLine9="" 'do not use this line

Sub OperatorMenuTimer_Timer
	EnteringOptions = 1
	OperatorMenuTimer.enabled=false
	ShowOperatorMenu
end sub

sub ShowOperatorMenu
	OperatorMenuBackdrop.image = "OperatorMenu"

	OptionCHS = 0
	CurrentOption = 1
	DisplayAllOptions
	OperatorOption1.image = "BluePlus"
	SetHighScoreOption

End Sub

Sub DisplayAllOptions
	dim linecounter
	dim tempstring
	For linecounter = 1 to MaxOption
		tempstring=Eval("OptionLine"&linecounter)
		Select Case linecounter
			Case 1:
				tempstring=tempstring + FormatNumber(BallsPerGame,0)
				SetOptLine 1,tempstring
			Case 2:
				if Replay3Table(ReplayLevel)=9990000 then
					tempstring = FormatNumber(Replay1Table(ReplayLevel),0) + "/" + FormatNumber(Replay2Table(ReplayLevel),0)
				elseif Replay4Table(ReplayLevel)=9990000 then
					tempstring = FormatNumber(Replay1Table(ReplayLevel),0) + "/" + FormatNumber(Replay2Table(ReplayLevel),0) + "/" + FormatNumber(Replay3Table(ReplayLevel),0)
				else
					tempstring = FormatNumber(Replay1Table(ReplayLevel),0,0,0,0) + "/" + FormatNumber(Replay2Table(ReplayLevel),0,0,0,0) + "/" + FormatNumber(Replay3Table(ReplayLevel),0,0,0,0) + "/" + FormatNumber(Replay4Table(ReplayLevel),0,0,0,0)
				end if
				SetOptLine 2,tempstring
			Case 3:
				If OptionCHS=0 then
					tempstring = "NO"
				else
					tempstring = "YES"
				end if
				SetOptLine 3,tempstring
			Case 4:
				SetOptLine 4, tempstring


				SetOptLine 5, tempstring
			Case 5:
				SetOptLine 6, tempstring
				if GameOption=1 then
					tempstring = "Conservative"
				else
					tempstring = "Liberal"
				end if
				SetOptLine 7, tempstring

			Case 6:
				SetOptLine 8, tempstring


				SetOptLine 9, tempstring

			Case 7:
				SetOptLine 10, tempstring
				SetOptLine 11, tempstring

			Case 8:

			Case 9:


		End Select

	next
end sub

sub MoveArrow
	do 
		CurrentOption = CurrentOption + 1
		If CurrentOption>Len(OptionLinesToMark) then
			CurrentOption=1
		end if
	loop until Mid(OptionLinesToMark,CurrentOption,1)="1"
end sub

sub CollectOptions(ByVal keycode)
	if Keycode = LeftFlipperKey then
		PlaySound "Drop_Target_Down_1"
		For XOpt = 1 to MaxOption
			Eval("OperatorOption"&XOpt).image = "PostitBL"
		next
		MoveArrow
		if CurrentOption<8 then
			Eval("OperatorOption"&CurrentOption).image = "BluePlus"
		elseif CurrentOption=8 then
			Eval("OperatorOption"&CurrentOption).image = "GreenCheck"
		else
			Eval("OperatorOption"&CurrentOption).image = "RedX"
		end if

	elseif Keycode = RightFlipperKey then
		PlaySound "DropTargetDropped_2"
		if CurrentOption = 1 then
			If BallsPerGame = 3 then
				BallsPerGame = 5
			else
				BallsPerGame = 3
			end if
			DisplayAllOptions
		elseif CurrentOption = 2 then
			ReplayLevel=ReplayLevel+1
			If ReplayLevel>ReplayTableMax then
				ReplayLevel=1
			end if
			DisplayAllOptions
		elseif CurrentOption = 3 then
			if OptionCHS = 0 then
				OptionCHS = 1

			else
				OptionCHS = 0

			end if
			DisplayAllOptions


		elseif CurrentOption = 5 then
			if GameOption=1 then
				GameOption=2

			else
				GameOption=1

			end if
			DisplayAllOptions

		elseif CurrentOption = 8 or CurrentOption = 9 then
			if OptionCHS=1 then
				HSScore(1) = 75000	
				HSScore(2) = 70000
				HSScore(3) = 60000
				HSScore(4) = 55000
				HSScore(5) = 50000

				HSName(1) = "AAA"
				HSName(2) = "ZZZ"
				HSName(3) = "XXX"
				HSName(4) = "ABC"
				HSName(5) = "BBB"
			end if

			if CurrentOption = 8 then
				savehs
			else
				loadhs
			end if
			OperatorMenuBackdrop.image = "PostitBL"
			For XOpt = 1 to MaxOption
				Eval("OperatorOption"&XOpt).image = "PostitBL"
			next

			For XOpt = 1 to 256
				Eval("Option"&XOpt).image = "PostItBL"
			next
			RefreshReplayCard
			BallCard.image="BC"+FormatNumber(BallsPerGame,0)
			InstructCard.image="IC"+FormatNumber(BallsPerGame,0)+FormatNumber(GameOption,0)
			EnteringOptions = 0

		end if
	end if
End Sub

Sub SetHighScoreOption

End Sub

Function GetOptChar(String, Index)
	dim ThisChar
	dim FileName
	ThisChar = Mid(String, Index, 1)
	FileName = "PostIt"
	if ThisChar = " " or ThisChar = "" then
		FileName = FileName & "BL"
	elseif ThisChar = "<" then
		FileName = FileName & "LT"
	elseif ThisChar = "_" then
		FileName = FileName & "SP"
	elseif ThisChar = "/" then
		FileName = FileName & "SL"
	elseif ThisChar = "," then
		FileName = FileName & "CM"
	else
		FileName = FileName & ThisChar
	End If
	GetOptChar = FileName
End Function

dim LineLengths(22)	' maximum number of lines
Sub SetOptLine(LineNo, String)
	Dim DispLen
	Dim StrLen
	dim xfor
	dim Letter
	dim ThisDigit
	dim ThisChar
	dim LetterLine
	dim Index
	dim LetterName
	StrLen = len(string)
	Index = 1

	StrLen = len(String)
	DispLen = StrLen
	if (DispLen < LineLengths(LineNo)) Then
		DispLen = LineLengths(LineNo)
	end If

	for xfor = StartingArray(LineNo) to StartingArray(LineNo) + DispLen
		Eval("Option"&xfor).image = GetOptChar(string, Index)
		Index = Index + 1
	next
	LineLengths(LineNo) = StrLen

End Sub

'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity


'*******************************************
' Late 70's to early 80's

Sub InitPolarity()
        dim x, a : a = Array(LF, RF)
        for each x in a
                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
                x.enabled = True
                x.TimeDelay = 80
        Next

        AddPt "Polarity", 0, 0, 0
        AddPt "Polarity", 1, 0.05, -2.7        
        AddPt "Polarity", 2, 0.33, -2.7
        AddPt "Polarity", 3, 0.37, -2.7        
        AddPt "Polarity", 4, 0.41, -2.7
        AddPt "Polarity", 5, 0.45, -2.7
        AddPt "Polarity", 6, 0.576,-2.7
        AddPt "Polarity", 7, 0.66, -1.8
        AddPt "Polarity", 8, 0.743, -0.5
        AddPt "Polarity", 9, 0.81, -0.5
        AddPt "Polarity", 10, 0.88, 0

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

LFState = 1
RFState = 1
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

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.055  'EM's
'Const EOSReturn = 0.045  'late 70's to mid 80's
'Const EOSReturn = 0.035  'mid 80's to early 90's
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

' Note, cor.update must be called in a 10 ms timer. The example table uses the GameTimer for this purpose, but sometimes a dedicated timer call RDampen is used.
'
Sub RDampen_Timer
	Cor.Update
End Sub



'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************


'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.9 	'Level of bounces. Recommmended value of 0.7

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






'******************************************************
'****  DROP TARGETS by Rothbauerw
'******************************************************
' This solution improves the physics for drop targets to create more realistic behavior. It allows the ball 
' to move through the target enabling the ability to score more than one target with a well placed shot.
' It also handles full drop target animation, including deflection on hit and a slight lift when the drop 
' targets raise, switch handling, bricking, and popping the ball up if it's over the drop target when it raises.
'
'Add a Timer named DTAnim to editor to handle drop & standup target animations, or run them off an always-on 10ms timer (GameTimer)
'DTAnim.interval = 10
'DTAnim.enabled = True

Sub DTAnim_Timer
	DoDTAnim
End Sub

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
Dim DT1, DT2, DT3, DT4, DT5

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

Set DT1 = (new DropTarget)(Target1, Target1a, Target1p, 1, 0, false)
Set DT2 = (new DropTarget)(Target2, Target2a, Target2p, 2, 0, false)
Set DT3 = (new DropTarget)(Target3, Target3a, Target3p, 3, 0, false)
Set DT4 = (new DropTarget)(Target4, Target4a, Target4p, 4, 0, false)
Set DT5 = (new DropTarget)(Target5, Target5a, Target5p, 5, 0, false)

Dim DTArray
DTArray = Array(DT1, DT2, DT3, DT4, DT5)
Dim DTIsDropped(5)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 90 'in milliseconds
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
	DTIsDropped(switch) = 0

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
			DTAction switchid
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
			Dim b, BOT
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
	DTIsDropped(switchid) = 1
	Select Case switchid
		Case 1:
			SetMotor(500)
			DOF 232, 2 'apophis
			IncreaseGoldBonus
			DropTargetsDownCount=DropTargetsDownCount+1
			MainBanksCount=MainBanksCount+1
			CheckAllDrops
			TargetcounterPUP
			Targetdownfirst
			Targetdownsecond
			Targetdownthird
			Targetdownfourth
		Case 2:
			SetMotor(500)
			DOF 232, 2 'apophis
			IncreaseGoldBonus
			DropTargetsDownCount=DropTargetsDownCount+1
			MainBanksCount=MainBanksCount+1
			CheckAllDrops
			TargetcounterPUP
			Targetdownfirst
			Targetdownsecond
			Targetdownthird
			Targetdownfourth
		Case 3:
			SetMotor(500)
			DOF 232, 2 'apophis
			IncreaseGoldBonus
			DropTargetsDownCount=DropTargetsDownCount+1
			MainBanksCount=MainBanksCount+1
			CheckAllDrops
			TargetcounterPUP
			Targetdownfirst
			Targetdownsecond
			Targetdownthird
			Targetdownfourth
		Case 4:
			SetMotor(500)
			DOF 232, 2 'apophis
			IncreaseGoldBonus
			DropTargetsDownCount=DropTargetsDownCount+1
			MainBanksCount=MainBanksCount+1
			CheckAllDrops
			TargetcounterPUP
			Targetdownfirst
			Targetdownsecond
			Targetdownthird
			Targetdownfourth
		Case 5:
			SetMotor(500)
			DOF 232, 2 'apophis
			IncreaseGoldBonus
			DropTargetsDownCount=DropTargetsDownCount+1
			MainBanksCount=MainBanksCount+1
			CheckAllDrops
			TargetcounterPUP
			Targetdownfirst
			Targetdownsecond
			Targetdownthird
			Targetdownfourth
	End Select
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
'****  END DROP TARGETS
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
' Many places in the script need to be modified to include the correct sound effect subroutine calls. The tutorial videos linked below demonstrate 
' how to make these updates. But in summary the following needs to be updated:	
'	- Nudging, plunger, coin-in, start button sounds will be added to the keydown and keyup subs.
'	- Flipper sounds in the flipper solenoid subs. Flipper collision sounds in the flipper collide subs.
'	- Bumpers, slingshots, drain, ball release, knocker, spinner, and saucers in their respective subs
'	- Ball rolling sounds sub
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


'/////////////////////////////  COIN IN  ////////////////////////////

Sub RandomCoinInSound()
	Select Case Int(rnd*3)
		Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
		Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
		Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////








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
'      5>  go to your table1_init or table first startup function and call PUPINIT function
'      6>  Go to bottom on framework here and setup game to call the appropriate events like pStartGame (call that in your game code where needed)...etc
'      7>  attractmodenext at bottom is setup for you already,  just go to each case and add/remove as many as you want and setup the messages to show.  
'      8>  Have fun and use pDMDDisplay(xxxx)  sub all over where needed.  remember its best to make a bunch of mp4 with text animations... looks the best for sure!
'
'
'Note:  for *Future Pinball* "pupDMDupdate_Timer()" timer needs to be renamed to "pupDMDupdate_expired()"  and then all is good.
'       and for future pinball you need to add the follow lines near top
'Need to use BAM and have com idll enabled.
'				Dim icom : Set icom = xBAM.Get("icom") ' "icom" is name of "icom.dll" in BAM\Plugins dir
'				if icom is Nothing then MSGBOX "Error cannot run without icom.dll plugin"
'				Function CreateObject(className)       
'   					Set CreateObject = icom.CreateObject(className)   
'				End Function




Const pTopper=0
Const pDMD=1
Const pBackglass=2
Const pPlayfield=3
Const pMusic=4
Const pMusic2=5
Const pCallouts=6
Const pBackglass2=7
Const pTopper2=8
Const pPopUP=9
Const pPopUP2=10


'pages
Const pDMDBlank=0
Const pScores=1
Const pBigLine=2
Const pThreeLines=3
Const pTwoLines=4
Const pTargerLetters=5

'dmdType
Const pDMDTypeLCD=0
Const pDMDTypeReal=1
Const pDMDTypeFULL=2






Dim PuPlayer
dim PUPDMDObject  'for realtime mirroring.
Dim pDMDlastchk: pDMDLastchk= -1    'performance of updates
Dim pDMDCurPage: pDMDCurPage= 0     'default page is empty.
Dim pInAttract : pInAttract=false   'pAttract mode




'*************  starts PUP system,  must be called AFTER b2s/controller running so put in last line of table1_init
Sub PuPInit
if hasPUP=false then Exit Sub
Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")   
PuPlayer.B2SInit "", pGameName

if (PuPDMDDriverType=pDMDTypeReal) and (useRealDMDScale=1) Then 
       PuPlayer.setScreenEx pDMD,0,0,128,32,0  'if hardware set the dmd to 128,32
End if

PuPlayer.LabelInit pDMD


if PuPDMDDriverType=pDMDTypeReal then
Set PUPDMDObject = CreateObject("PUPDMDControl.DMD") 
PUPDMDObject.DMDOpen
PUPDMDObject.DMDPuPMirror
PUPDMDObject.DMDPuPTextMirror
PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":33 }"             'set pupdmd for mirror and hide behind other pups
PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":32, ""FQ"":3 }"   'set no antialias on font render if real
END IF


pSetPageLayouts

pDMDSetPage(pDMDBlank)   'set blank text overlay page.
 pDMDStartUP' firsttime running for like an startup video..


End Sub 'end PUPINIT



'PinUP Player DMD Helper Functions

Sub pDMDLabelHide(labName)
PuPlayer.LabelSet pDMD,labName,"",0,""   
end sub




Sub pDMDScrollBig(msgText,timeSec,mColor)
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
end sub

Sub pDMDScrollBigV(msgText,timeSec,mColor)
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'yps':1,'ype':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
end sub


Sub pDMDSplashScore(msgText,timeSec,mColor)
PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':1,'fq':250,'len':"& (timeSec*1000) &",'fc':" & mColor & "}"
end Sub

Sub pDMDSplashScoreScroll(msgText,timeSec,mColor)
PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':"& (timeSec*1000) &", 'mlen':"& (timeSec*1000) &",'tt':0, 'fc':" & mColor & "}"
end Sub

Sub pDMDZoomBig(msgText,timeSec,mColor)  'new Zoom
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':3,'hstart':5,'hend':80,'len':" & (timeSec*1000) & ",'mlen':" & (timeSec*500) & ",'tt':5,'fc':" & mColor & "}"
end sub

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
                          'if using a fixed font width then set spaces to just one space.

For i=1 To Len(msgInfo)
    curChar="" & Mid(msgInfo,i,1)
    if curChar="0" Then
            backText=backText & Mid(msgText,i,1)
            middleText=middleText & spaces
            flashText=flashText & spaces          
            offchars=offchars+1
    End If
    if curChar="1" Then
            backText=backText & spaces
            middleText=middleText & Mid(msgText,i,1)
            flashText=flashText & spaces
    End If
    if curChar="2" Then
            backText=backText & spaces
            middleText=middleText & spaces
            flashText=flashText & Mid(msgText,i,1)
    End If   
Next 

if offchars=0 Then 'all litup!... flash entire string
   backText=""
   middleText=""
   FlashText=msgText
end if  

PuPlayer.LabelSet pDMD,"Back5"  ,backText  ,1,""
PuPlayer.LabelSet pDMD,"Middle5",middleText,1,""
PuPlayer.LabelSet pDMD,"Flash5" ,flashText ,0,"{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) & "}"   
end Sub


Sub pDMDSetPage(pagenum)
	if hasPUP=false then Exit Sub	
    PuPlayer.LabelShowPage pDMD,pagenum,0,""   'set page to blank 0 page if want off
    PDMDCurPage=pagenum
end Sub

Sub pHideOverlayText(pDisp)
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& pDisp &", ""FN"": 34 }"             'hideoverlay text during next videoplay on DMD auto return
end Sub



Sub pDMDShowLines3(msgText,msgText2,msgText3,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,3,timeSec,""
PuPlayer.LabelSet pDMD,"Splash3a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash3b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash3c",msgText3,vis,pLine3Ani
end Sub


Sub pDMDShowLines2(msgText,msgText2,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,4,timeSec,""
PuPlayer.LabelSet pDMD,"Splash4a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash4b",msgText2,vis,pLine2Ani
end Sub

Sub pDMDShowCounter(msgText,msgText2,msgText3,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,6,timeSec,""
PuPlayer.LabelSet pDMD,"Splash6a",msgText,vis, pLine1Ani
PuPlayer.LabelSet pDMD,"Splash6b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash6c",msgText3,vis,pLine3Ani
end Sub


Sub pDMDShowBig(msgText,timeSec, mColor)
if hasPUP=false then Exit Sub
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,vis,pLine1Ani
end sub


Sub pDMDShowHS(msgText,msgText2,msgText3,timeSec) 'High Score
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,7,timeSec,""
PuPlayer.LabelSet pDMD,"Splash7a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash7b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash7c",msgText3,vis,pLine3Ani
end Sub


Sub pDMDSetBackFrame(fname)
  PuPlayer.playlistplayex pDMD,"PUPFrames",fname,0,1    
end Sub

Sub pDMDStartBackLoop(fPlayList,fname)
  PuPlayer.playlistplayex pDMD,fPlayList,fname,0,1
  PuPlayer.SetBackGround pDMD,1
end Sub

Sub pDMDStopBackLoop
  PuPlayer.SetBackGround pDMD,0
  PuPlayer.playstop pDMD
end Sub


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
'*************************                see docs for examples                ************************************************
'****************************************   DONT TOUCH THIS CODE   ************************************************************

Sub pupDMDDisplay(pEventID, pText, VideoName,TimeSec, pAni,pPriority)
' pEventID = reference if application,  
' pText = "text to show" separate lines by ^ in same string
' VideoName "gameover.mp4" will play in background  "@gameover.mp4" will play and disable text during gameplay.
' also global variable useDMDVideos=true/false if user wishes only TEXT
' TimeSec how long to display msg in Seconds
' animation if any 0=none 1=Flasher
' also,  now can specify color of each line (when no animation).  "sometext|12345"  will set label to "sometext" and set color to 12345

DIM curPos
if pDMDCurPriority>pPriority then Exit Sub  'if something is being displayed that we don't want interrupted.  same level will interrupt.
pDMDCurPriority=pPriority
if timeSec=0 then timeSec=1 'don't allow page default page by accident


pLine1=""
pLine2=""
pLine3=""
pLine1Ani=""
pLine2Ani=""
pLine3Ani=""


if pAni=1 Then  'we flashy now aren't we
pLine1Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
pLine2Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
pLine3Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
end If

curPos=InStr(pText,"^")   'Lets break apart the string if needed
if curPos>0 Then 
   pLine1=Left(pText,curPos-1) 
   pText=Right(pText,Len(pText) - curPos)
   
   curPos=InStr(pText,"^")   'Lets break apart the string
   if curPOS>0 Then
      pLine2=Left(pText,curPos-1) 
      pText=Right(pText,Len(pText) - curPos)

      curPos=InStr("^",pText)   'Lets break apart the string   
      if curPos>0 Then
         pline3=Left(pText,curPos-1) 
      Else 
        if pText<>"" Then pline3=pText 
      End if 
   Else 
      if pText<>"" Then pLine2=pText
   End if    
Else 
  pLine1=pText  'just one line with no break 
End if


'lets see how many lines to Show
pNumLines=0
if pLine1<>"" then pNumLines=pNumlines+1
if pLine2<>"" then pNumLines=pNumlines+1
if pLine3<>"" then pNumLines=pNumlines+1

if pDMDVideoPlaying and (VideoName="") Then 
			PuPlayer.playstop pDMD
			pDMDVideoPlaying=False

End if


if (VideoName<>"") and (useDMDVideos) Then  'we are showing a splash video instead of the text.
    
    PuPlayer.playlistplayex pDMD,"DMDSplash",VideoName,pDMDDefVolume,pPriority  'should be an attract background (no text is displayed)
    pDMDVideoPlaying=true
end if 'if showing a splash video with no text




if StrComp(pEventID,"shownum",1)=0 Then              'check eventIDs
    pDMDShowCounter pLine1,pLine2,pLine3,timeSec
Elseif StrComp(pEventID,"target",1)=0 Then              'check eventIDs
    pDMDTargetLettersInfo pLine1,pLine2,timeSec
Elseif StrComp(pEventID,"highscore",1)=0 Then              'check eventIDs
    pDMDShowHS pLine1,pLine2,pline3,timeSec
Elseif (pNumLines=3) Then                'depends on # of lines which one to use.  pAni=1 will flash.
    pDMDShowLines3 pLine1,pLine2,pLine3,TimeSec
Elseif (pNumLines=2) Then
    pDMDShowLines2 pLine1,pLine2,TimeSec
Elseif (pNumLines=1) Then
    pDMDShowBig pLine1,timeSec, curLine1Color
Else
    pDMDShowBig pLine1,timeSec, curLine1Color
End if

PriorityReset=TimeSec*1000
End Sub 'pupDMDDisplay message

Sub pupDMDupdate_Timer()
	pUpdateScores    

    if PriorityReset>0 Then  'for splashes we need to reset current prioirty on timer
       PriorityReset=PriorityReset-pupDMDUpdate.interval
       if PriorityReset<=0 Then 
            pDMDCurPriority=-1            
            if pInAttract then pAttractReset=pAttractBetween ' pAttractNext  call attract next after 1 second
			pDMDVideoPlaying=false			
			End if
    End if

    if pAttractReset>0 Then  'for splashes we need to reset current prioirty on timer
       pAttractReset=pAttractReset-pupDMDUpdate.interval
       if pAttractReset<=0 Then 
            pAttractReset=-1            
            if pInAttract then pAttractNext
			End if
    end if 
End Sub

'Sub PuPEvent(EventNum)
'if hasPUP=false then Exit Sub
'PuPlayer.B2SData "E"&EventNum,1  'send event to puppack driver  
'End Sub


'********************* END OF PUPDMD FRAMEWORK v1.0 *************************
'******************** DO NOT MODIFY STUFF ABOVE THIS LINE!!!! ***************
'****************************************************************************

'*****************************************************************
'   **********  PUPDMD  MODIFY THIS SECTION!!!  ***************
'PUPDMD Layout for each Table1
'Setup Pages.  Note if you use fonts they must be in FONTS folder of the pupVideos\tablename\FONTS  "case sensitive exact naming fonts!"
'*****************************************************************

Sub pSetPageLayouts

DIM dmddef
DIM dmdalt
DIM dmdscr
DIM dmdfixed

'labelNew <screen#>, <Labelname>, <fontName>,<size%>,<colour>,<rotation>,<xalign>,<yalign>,<xpos>,<ypos>,<PageNum>,<visible>
'***********************************************************************'
'<screen#>, in standard we'd set this to pDMD ( or 1)
'<Labelname>, your name of the label. keep it short no spaces (like 8 chars) although you can call it anything really. When setting the label you will use this labelname to access the label.
'<fontName> Windows font name, this must be exact match of OS front name. if you are using custom TTF fonts then double check the name of font names.
'<size%>, Height as a percent of display height. 20=20% of screen height.
'<colour>, integer value of windows color.
'<rotation>, degrees in tenths   (900=90 degrees)
'<xAlign>, 0= horizontal left align, 1 = center horizontal, 2= right horizontal
'<yAlign>, 0 = top, 1 = center, 2=bottom vertical alignment
'<xpos>, this should be 0, but if you want to 'force' a position you can set this. it is a % of horizontal width. 20=20% of screen width.
'<ypos> same as xpos.
'<PageNum> IMPORTANT this will assign this label to this page or group.
'<visible> initial state of label. visible=1 show, 0 = off.




if PuPDMDDriverType=pDMDTypeFULL THEN  'Using FULL BIG LCD PuPDMD  ************ lcd **************

	'dmddef="Impact"
	dmdalt="Impact"    
    dmdfixed="Magazine"
	dmdscr="Impact"  'main score font
	dmddef="Impact"

	'Page 1 (default score display)
		PuPlayer.LabelNew pDMD,"gcredit" ,dmddef,6, 16777215  ,1,2,0,98,92,1,0
		PuPlayer.LabelNew pDMD,"creditname" ,dmddef,6, 16777215  ,1,2,0,95,92,1,0
		PuPlayer.LabelNew pDMD,"Play1"   ,dmddef,8, 16777215 ,1,2,0,7,90,1,0
		PuPlayer.LabelNew pDMD,"Playername"   ,dmddef,8, 16777215 ,1,2,0,12,83,1,0
		PuPlayer.LabelNew pDMD,"Ball"    ,dmddef,8, 16777215 ,1,2,0,98,83,1,0
		PuPlayer.LabelNew pDMD,"Ballname"    ,dmddef,8, 16777215  ,1,2,0,96,83,1,0
		PuPlayer.LabelNew pDMD,"MsgScore",dmddef,45,33023   ,0,1,0, 0,40,1,0
		PuPlayer.LabelNew pDMD,"CurScore",dmdscr,20,16053492,0,1,1, 0,90,1,0
		PuPlayer.LabelNew pDMD,"Blimp10Gif" ,dmddef,20,33023   ,1,1,0,0,10,1,0
		PuPlayer.LabelNew pDMD,"Blimp100Gif" ,dmddef,20,33023   ,1,1,0,0,10,1,0
		PuPlayer.LabelNew pDMD,"Blimp1000Gif" ,dmddef,20,33023   ,1,1,0,0,10,1,0
		PuPlayer.LabelNew pDMD,"Circlebumper" ,dmddef,5,33023   ,1,0,0,70,53,1,0
		PuPlayer.LabelNew pDMD,"Zofobumper" ,dmddef,5,33023   ,1,0,0,20,53,1,0
		PuPlayer.LabelNew pDMD,"Featherbumper" ,dmddef,5,33023   ,1,0,0,4,53,1,0
		PuPlayer.LabelNew pDMD,"Hexbumper" ,dmddef,5,33023   ,1,0,0,55,53,1,0



	'Page 2 (default Text Splash 1 Big Line)
		PuPlayer.LabelNew pDMD,"Splash"  ,dmdalt,40,33023,0,1,1,0,0,2,0


	'Page 3 (default Text 3 Lines)
		PuPlayer.LabelNew pDMD,"Splash3a",dmddef,30,8454143,0,1,0,0,2,3,0
		PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,30,33023,0,1,0,0,30,3,0
		PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,25,33023,0,1,0,0,57,3,0


	'Page 4 (default Text 2 Line)
		PuPlayer.LabelNew pDMD,"Splash4a",dmddef,40,8454143,0,1,0,0,0,4,0
		PuPlayer.LabelNew pDMD,"Splash4b",dmddef,30,33023,0,1,2,0,75,4,0

	'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
		PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80,8421504,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80,65535  ,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80,65535  ,0,1,1,0,0,5,0

	'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
		PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90,65280,0,0,0,15,1,6,0
		PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50,33023,0,1,0,60,0,6,0
		PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40,33023,0,1,0,60,50,6,0

	'Page 7 (Show High Scores Fixed Fonts)
		PuPlayer.LabelNew pDMD,"Splash7a",dmddef,20,8454143,0,1,0,0,2,7,0
		PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,40,33023,0,1,0,0,20,7,0
		PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,40,33023,0,1,0,0,50,7,0

	'Page 8 (Sequence Text 3 Lines)
		puPlayer.LabelNew pDMD,"EnterHS1",dmddef,		5,   62207 	,1,2,0 ,90,87   ,8,0				
		puPlayer.LabelNew pDMD,"EnterHS2",dmdfixed,		5,  16777215  	,1,2,0 ,93,80   ,8,0				' HS Entry
		puPlayer.LabelNew pDMD,"EnterHS3",dmdfixed,		10,  62207	,1,2,0 ,91,70   ,8,0				' HS Entry

	'Page 9 (Attract Text 3 Lines)
		PuPlayer.LabelNew pDMD,"AttractA",dmddef,20,2292994,0,1,0,0,10,9,0
		PuPlayer.LabelNew pDMD,"AttractB",dmdscr,25,16777215,0,1,0,0,37,9,0
		PuPlayer.LabelNew pDMD,"AttractC",dmddef,25,65021,0,1,0,0,69,9,0
		PuPlayer.LabelNew pDMD,"score1",dmdscr,10,16777215,1,2,0,35,16,9,0
		PuPlayer.LabelNew pDMD,"score2",dmdscr,10,16777215,1,2,0,37,38,9,0
		PuPlayer.LabelNew pDMD,"score3",dmdscr,10,16777215,1,2,0,39,59,9,0
		PuPlayer.LabelNew pDMD,"score4",dmdscr,10,16777215,1,2,0,41,79,9,0
		PuPlayer.LabelNew pDMD,"Total",dmdscr,20, 16318588 ,0,1,1, 0,90,9,0
		PuPlayer.LabelNew pDMD,"Totalunder",dmdscr,20, 0 ,0,1,1, 0,90,9,0
		PuPlayer.LabelNew pDMD,"Bonusfont",dmdscr,20,16053492,0,1,1, 0,71,9,0
		PuPlayer.LabelNew pDMD,"gc" ,dmddef,8, 16777215  ,1,2,0,96,70,9,0
		PuPlayer.LabelNew pDMD,"gcn" ,dmddef,8, 16777215  ,1,2,0,92,70,9,0
END IF  ' use PuPDMDDriver




end Sub 'page Layouts


'*****************************************************************
'        PUPDMD Custom SUBS/Events for each Table1
'     **********    MODIFY THIS SECTION!!!  ***************
'*****************************************************************
'
'
'  we need to somewhere in code if applicable
'
'   call pDMDStartGame,pDMDStartBall,pGameOver,pAttractStart
'
'
'
'
'


Sub pDMDStartGame
PuPEvent 51
pInAttract=false
pDMDSetPage(pScores)   'set blank text overlay page.

end Sub


Sub pDMDStartBall
end Sub

Sub pDMDGameOver
pAttractStart

end Sub

Sub pAttractStart
pDMDSetPage(pDMDBlank)   'set blank text overlay page.
pCurAttractPos=0
pInAttract=True          'Startup in AttractMode
pAttractNext
end Sub

Sub pDMDStartUP
end Sub

DIM pCurAttractPos: pCurAttractPos=0


'********************** gets called auto each page next and timed already in DMD_Timer.  make sure you use pupDMDDisplay or it wont advance auto.
Sub pAttractNext
if hasPUP=false then Exit Sub	
pCurAttractPos=pCurAttractPos+1
  Select Case pCurAttractPos

  Case 1 pupDMDDisplay "attract","","",3,0,10:PuPEvent 48
  Case 2 pupDMDDisplay "attract","","",11,0,10:PuPEvent 49
	pDMDSetPage(9):puPlayer.LabelSet pDMD,"gcn","CREDITS: ",1,"{'mt':2,'color':16777215, 'shadowcolor':2949120, 'shadowstate':1,'xoffset':2, 'yoffset':6, 'bold':1, 'outline':2}":puPlayer.LabelSet pDMD,"gc",""  & Credits ,1,"{'mt':2,'color':16777215, 'shadowcolor':2949120, 'shadowstate':1,'xoffset':2, 'yoffset':6, 'bold':1, 'outline':2}":puPlayer.LabelSet pDMD, "score1", FormatNumber((sortscores(1)),0),1,"{'mt':2,'color':16777215, 'shadowcolor':2949120, 'shadowstate':1,'xoffset':2, 'yoffset':6, 'bold':1, 'outline':2}":puPlayer.LabelSet pDMD, "score2", FormatNumber((sortscores(2)),0),1,"{'mt':2,'color':16777215, 'shadowcolor':2949120, 'shadowstate':1,'xoffset':2, 'yoffset':6, 'bold':1, 'outline':2}":puPlayer.LabelSet pDMD, "score3", FormatNumber((sortscores(3)),0),1,"{'mt':2,'color':16777215, 'shadowcolor':2949120, 'shadowstate':1,'xoffset':2, 'yoffset':6, 'bold':1, 'outline':2}":puPlayer.LabelSet pDMD, "score4", FormatNumber((sortscores(4)),0),1,"{'mt':2,'color':16777215, 'shadowcolor':2949120, 'shadowstate':1,'xoffset':2, 'yoffset':6, 'bold':1, 'outline':2}"
  Case 3 pupDMDDisplay "attract","","",5,0,10:PuPEvent 80
	pDMDSetPage(9):puPlayer.LabelSet pDMD, "AttractB", FormatNumber((HSScore(1)),0),1,"{'mt':2,'color':16777215}":puPlayer.LabelSet pDMD, "AttractC", HSName(1),1,"{'mt':2,'color':255}"
  Case 4 pupDMDDisplay "attract","","",5,0,10:PuPEvent 81
	pDMDSetPage(9):puPlayer.LabelSet pDMD, "AttractB", FormatNumber((HSScore(2)),0),1,"{'mt':2,'color':16777215}":puPlayer.LabelSet pDMD, "AttractC", HSName(2),1,"{'mt':2,'color':255}"
  Case 5 pupDMDDisplay "attract","","",5,0,10:PuPEvent 82
	pDMDSetPage(9):puPlayer.LabelSet pDMD, "AttractB", FormatNumber((HSScore(3)),0),1,"{'mt':2,'color':16777215}":puPlayer.LabelSet pDMD, "AttractC", HSName(3),1,"{'mt':2,'color':255}"
  Case 6 pupDMDDisplay "attract","","",5,0,10:PuPEvent 83
	pDMDSetPage(9):puPlayer.LabelSet pDMD, "AttractB", FormatNumber((HSScore(4)),0),1,"{'mt':2,'color':16777215}":puPlayer.LabelSet pDMD, "AttractC", HSName(4),1,"{'mt':2,'color':255}"
  Case 7 pupDMDDisplay "attract","","",5,0,10:PuPEvent 84
	pDMDSetPage(9):puPlayer.LabelSet pDMD, "AttractB", FormatNumber((HSScore(5)),0),1,"{'mt':2,'color':16777215}":puPlayer.LabelSet pDMD, "AttractC", HSName(5),1,"{'mt':2,'color':255}"
	Case Else
    pCurAttractPos=0
    pAttractNext 'reset to beginning
  end Select

end Sub

Sub TargetcounterPUP
	if hasPUP=false then Exit Sub
	pDMDSetPage(1):puPlayer.LabelSet pDMD, "", FormatNumber(DropTargetsDownCount,0),1,"{'mt':2,'color':16777215}"
	PupDmdtimer.interval=1000
	PupDmdtimer.enabled = True
End Sub

Sub PupDmdtimer_Timer
	PupDmdtimer.enabled = false
	pDMDSetPage(1):puPlayer.LabelSet pDMD, "","",1,""
End Sub
'************************ called during gameplay to update Scores ***************************

Sub pUpdateScores 'call this ONLY on timer 300ms is good enough
if pDMDCurPage <> pScores then Exit Sub
'puPlayer.LabelSet pDMD,"Credits","CREDITS " & ""& Credits ,1,""
'puPlayer.LabelSet pDMD,"Play1","Player 1",1,""
'puPlayer.LabelSet pDMD,"Ball"," "&pDMDCurPriority ,1,""

puPlayer.LabelSet pDMD,"CurScore",""  & FormatNumber(Score(Player),0),1,"{'mt':2, 'shadowcolor':2949120, 'shadowstate':1,'xoffset':2, 'yoffset':6, 'bold':1, 'outline':2 }"
puPlayer.LabelSet pDMD,"Play1","" & Player,1,""
puPlayer.LabelSet pDMD,"Playername","PLAYER:",1,""
puPlayer.LabelSet pDMD,"Ball",""  & BallInPlay ,1,""
puPlayer.LabelSet pDMD,"gcredit",""  & Credits ,1,""
puPlayer.LabelSet pDMD,"Ballname","BALL: ",1,""
puPlayer.LabelSet pDMD,"creditname","CREDITS: ",1,""
end Sub


Sub Blimp10_PuPGIF
	if hasPUP=false then Exit Sub
	puPlayer.LabelSet pDMD,"Blimp10Gif","Gif\\10-blimp-gif_1.gif",1,"{'mt':2,'color':111111, 'width': 80, 'height': 30.2916, 'anigif': 150 ,}"
	pDMD_GIF_Splash10.interval=2000
	pDMD_GIF_Splash10.enabled=true
End Sub


Sub Blimp100_PuPGIF
	if hasPUP=false then Exit Sub
	puPlayer.LabelSet pDMD,"Blimp100Gif","Gif\\100-blimp-gif-1.gif",1,"{'mt':2,'color':111111, 'width': 80, 'height': 30.2916, 'anigif': 150 ,}"
	pDMD_GIF_Splash100.interval=2000
	pDMD_GIF_Splash100.enabled=true
End Sub

Sub Blimp1000_PuPGIF
	if hasPUP=false then Exit Sub
	puPlayer.LabelSet pDMD,"Blimp1000Gif","Gif\\1000-blimp_1.gif",1,"{'mt':2,'color':111111, 'width': 80, 'height': 30.2916, 'anigif': 150 ,}"
	pDMD_GIF_Splash1000.interval=2000
	pDMD_GIF_Splash1000.enabled=true
End Sub

Sub pDMD_GIF_Splash10_Timer
	if hasPUP=false then Exit Sub
	pDMD_GIF_Splash10.enabled = false
	puPlayer.LabelSet pDMD,"Blimp10Gif","Gif\\blank.gif",1,"{'mt':2,'color':111111, 'width': 100, 'height': 40.2916, 'anigif': 150 ,}"
End Sub

Sub pDMD_GIF_Splash100_Timer
	if hasPUP=false then Exit Sub
	pDMD_GIF_Splash100.enabled = false
	puPlayer.LabelSet pDMD,"Blimp100Gif","Gif\\blank.gif",1,"{'mt':2,'color':111111, 'width': 100, 'height': 40.2916, 'anigif': 150 ,}"
End Sub

Sub pDMD_GIF_Splash1000_Timer
	if hasPUP=false then Exit Sub
	pDMD_GIF_Splash1000.enabled = false
	puPlayer.LabelSet pDMD,"Blimp1000Gif","Gif\\blank.gif",1,"{'mt':2,'color':111111, 'width': 100, 'height': 40.2916, 'anigif': 150 ,}"
End Sub

Sub CircleGif_PuPGIF
	if hasPUP=false then Exit Sub
	puPlayer.LabelSet pDMD,"Circlebumper","Gif\\circles-gif.gif",1,"{'mt':2,'color':111111, 'width': 25, 'height':25, 'anigif': 130,}"
		pDMD_GIF_Splash11.interval=1000
		pDMD_GIF_Splash11.enabled=true
	PuPEvent 20
End Sub

Sub FeatherGif_PuPGIF
	if hasPUP=false then Exit Sub
	puPlayer.LabelSet pDMD,"Featherbumper","Gif\\feather-gif.gif",1,"{'mt':2,'color':111111, 'width': 25, 'height': 25, 'anigif': 130 ,}"
		pDMD_GIF_Splash12.interval=1000
		pDMD_GIF_Splash12.enabled=true
	PuPEvent 21
End Sub

Sub ZofoGif_PuPGIF
	if hasPUP=false then Exit Sub
	puPlayer.LabelSet pDMD,"Zofobumper","Gif\\zofo-gif.gif",1,"{'mt':2,'color':111111, 'width': 25, 'height': 25., 'anigif': 130 ,}"
		pDMD_GIF_Splash13.interval=1000
		pDMD_GIF_Splash13.enabled=true
	PuPEvent 22
End Sub

Sub HexStar_PuPGIF
	if hasPUP=false then Exit Sub
	puPlayer.LabelSet pDMD,"Hexbumper","Gif\\Hex-Star.gif",1,"{'mt':2,'color':111111, 'width': 25, 'height': 25., 'anigif': 130 ,}"
		pDMD_GIF_Splash14.interval=1000
		pDMD_GIF_Splash14.enabled=true
	PuPEvent 23
End Sub



Sub pDMD_GIF_Splash11_Timer
	pDMD_GIF_Splash11.enabled = false
	puPlayer.LabelSet pDMD,"Circlebumper","Gif\\blank2.gif",1,"{'mt':2,'color':111111, 'width': 20, 'height': 20.2916, 'anigif': 150 ,}"
	PuPEvent 26
End Sub

Sub pDMD_GIF_Splash12_Timer
	pDMD_GIF_Splash12.enabled = false
	puPlayer.LabelSet pDMD,"Featherbumper","Gif\\blank2.gif",1,"{'mt':2,'color':111111, 'width': 20, 'height':20.2916, 'anigif': 150 ,}"
	PuPEvent 27
End Sub

Sub pDMD_GIF_Splash13_Timer
	pDMD_GIF_Splash13.enabled = false
	puPlayer.LabelSet pDMD,"Zofobumper","Gif\\ blank3 .gif",1,"{'mt':2,'color':111111, 'width': 20, 'height':20.2916, 'anigif': 150 ,}"
	PuPEvent 30
End Sub

Sub pDMD_GIF_Splash14_Timer
	pDMD_GIF_Splash14.enabled = false
	puPlayer.LabelSet pDMD,"Hexbumper","Gif\\ blank4 .gif",1,"{'mt':2,'color':111111, 'width': 20, 'height':20.2916, 'anigif': 150 ,}"
	PuPEvent 29
End Sub

Sub GameoverDelay
	GameOver.interval=0
	GameOver.Enabled=True
End Sub

Sub GameOver_Timer
	GameOver.Enabled=false
	pDMDGameOver
End Sub


'*************************
'PinUPPlayer
'**************************

Sub PinUPInit
Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")
PuPlayer.B2SInit "",CGameName
end Sub

Sub PuPEvent(EventNum)
if hasPUP=false then Exit Sub

PuPlayer.B2SData "D"&EventNum,1  'send event to puppack driver
   
End Sub



