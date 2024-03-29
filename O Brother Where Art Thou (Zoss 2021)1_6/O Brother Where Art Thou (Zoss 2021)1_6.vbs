'****************************************************************
'
'			O Brother Where Art Thou (Zoss 2021)
'				   Game Design by Onevox
'			    	Script by Scottacus
'            Physics by Bord      DOF by Arngrim                     
'						   v1.0.2
'					     August 2021
'
'       Special thanks to Thalamus for beta testing and improving multiball, plus JFR1, mlager for testing,
'       and post-release commenters to point out errors/improvements.
'
'       A tribute to the 2000 movie by the Coen Brothers and Touchstone Pictures, a brilliant allegorical comedy
'       loosely based on Homer's Odyssey. 
'
'		DOF config
'		101 Left Flipper, 102 Right Flipper
'		103 Left sling, 104 Right sling
'		105-107 Bumpers
'       108-109 Slingshot flashers
'       110-112 Bumper flashers
'       113 Drop Targets
'		406-407 Special Targets L/R
'		408-409 Center Targets L/R
'		410-413 Top Side Lane Rollovers L/R
'		414-415 Targets Mid L/R	
'		417-420 Mid L/R
'		421-422 Out Lanes L/R
'		423-424 In Lanes L/R
'		425 Top Kicker
'		426 Drain, 425 Ball Release
'	 	427 Cedit Light
'		428 Knocker
'		429 Knocker and Kicker Strobe
'		430 Spinner1
'		431 Spinner2 
'		450 Ball In Shooter Lane
'		441 Chime1-10s, 442 Chime2-100s, 443 Chime3-1000s
'
'		Code Flow
'									 EndGame 
'										^														
'		Start Game -> New Game -> Check Continue -> Release Ball -> Drain -> Score Bouns -> Advance Player -> Next Ball
'										^																		  |
'									EndGame = True <---------------------------------------------------------------

'	Ball Control Subroutine developed by: rothbauerw
'		Press "c" during play to activate, the arrow keys control the ball
'**********************************************************************************************************************

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

Const cGameName = "OBWAT"
Const cOptions = "OBWAT_2021.txt"
Const HSFileName="O Brother Where Art Thou (Zoss 2021)"

'***************************************************************************************
'Set this variable to 1 to save a PinballY High Score file to your Tables Folder
'this will let the Pinball Y front end display the high scores when searching for tables
'0 = No PinballY High Scores, 1 = Save PinballY High Scores 
Const cPinballY = 2
'***************************************************************************************

Dim coloredBalls
'***********************************Colored Balls**************************************************
'Comment the next line to make the game play with standard balls o/w BOT(0-2) = Green, Red, Blue
coloredBalls = True
'**************************************************************************************************

Dim balls
Dim replays
Dim maxPlayers
Dim players
Dim player
Dim credit
Dim score(6)
Dim hScore(6)
Dim sReel(4)
Dim state
Dim tilt
Dim matchNumber
Dim i,j, f, ii, Object, obj, Light, x, y, z
Dim freePlay 
Dim ballsize,BallMass
ballSize = 50
ballMass = (Ballsize^3)/125000
Dim BIP : BIP = 0
Dim options
Dim chime
Dim onBumper
Dim pfOption
Dim musicOn
Dim musicVolume


'Desktop reel pans,  Valid values: "Lpan", "Mpan" and "Rpan"
Const dtcrReel = "Rpan"
Const dts1Reel = "Lpan"
Const dts2Reel = "Rpan"
Const dts3Reel = "Rpan"
Const dts4Reel = "Rpan"

'Backglass reel pans,  Valid values: "Lpan", "Mpan" and "Rpan"
Const bgcrReel = "Mpan"
Const bgs1Reel = "Lpan"
Const bgs2Reel = "Rpan"
Const bgs3Reel = "Lpan"
Const bgs4Reel = "Rpan"

Sub Table1_init
	LoadEM
	maxPlayers = 4

	For x = 1 to maxPlayers
		Set sReel(x) = EVAL("scoreReel" & x)
	Next

	Player=1
	LoadHighScore

	ballsInPlay = 0

	If highScore(0) = "" Then highScore(0) = 50000
	If highScore(1) = "" Then highScore(1) = 45000
	If highScore(2) = "" Then highScore(2) = 40000
	If highScore(3) = "" Then highScore(3) = 35000
	If HighScore(4) = "" Then highScore(4) = 30000	
	If matchNumber = "" Then matchNumber = 4
	If ShowDT = True Then pfOption = 1
	If pfOption = "" Then pfOption = 1
	If initial(0,1) = "" Then
		initial(0,1) = 19: initial(0,2) = 5: initial(0,3) = 13
		initial(1,1) = 1: initial(1,2) = 1: initial(1,3) = 1
		initial(2,1) = 2: initial(2,2) = 2: initial(2,3) = 2
		initial(3,1) = 3: initial(3,2) = 3: initial(3,3) = 3
		initial(4,1) = 4: initial(4,2) = 4: initial(4,3) = 4
	End If
	If credit = "" Then credit = 0
	If freePlay = "" Then freePlay = 1
	If balls = "" Then balls = 5
	If chime = "" Then chime = 0
	If onBumper = "" Then onBumper = 1
	If musicOn = "" Then musicOn = 1
	If musicVolume = "" Then musicVolume = .5


	replaySettings

	firstBallOut = 0
	updatePostIt
	dynamicUpdatePostIt.enabled = 1
	TiltReel.setValue(1)
	CreditReel.setvalue(credit)

	If ShowDT = True Then
		For each object in backdropstuff 
		Object.visible = 1 	
		Next
	End If

	If ShowDt = False Then
		For each object in backdropstuff 
		Object.visible = 0 	
		Next
	End If

	ballShadowUpdate.enabled = True

	MatchReel.setValue(MatchNumber) 'Need to set to change if 1 point table

	tilt = False
	state = False
	gameState



'	InstructCard.image = "InstructReplay" & balls
'***********Trough Ball Creation
'	Drain.CreateSizedBallWithMass Ballsize/2, BallMass
'	Kicker1.CreateSizedBallWithMass Ballsize/2, BallMass
'	Kicker1.kick 0, 0
'	Kicker2.CreateSizedBallWithMass Ballsize/2, BallMass
'	Kicker2.kick 0, 0

End Sub

'***********KeyCodes
Dim enableInitialEntry, firstBallOut
Sub Table1_KeyDown(ByVal keycode)

	If enableInitialEntry = True Then enterIntitals(keycode)
   
	If keycode = addCreditKey Then
		playFieldSound "coinin",0,Drain,1 
		addCredit = 1
		DOF 427, DOFOn
		scoreMotor5.enabled = 1
    End If

    If keycode = startGameKey and capturedBalls < 1 Then
		If enableInitialEntry = False and operatormenu = 0 and backGlassOn = 1 Then
			If freePlay = 1 and players < 4 and firstBallOut = 0 Then startGame
			If freePlay = 0 and credit > 0 and players < 4 and firstBallOut = 0 Then
				credit = credit - 1
				If showDT = False Then PlayReelSound "Reel5", bgcrReel Else PlayReelSound "Reel5", dtcrReel
				creditReel.setvalue(credit)	
				If credit < 1 Then 
					DOF 427, DOFOff
				End If
				If B2SOn Then 
					If freeplay = 0 Then controller.B2SSetCredits credit
					If freePlay = 0 and credit < 1 Then DOF 427, DOFOff
				End If
				startGame
			End If
		End If
	End If
	  
	If keycode = PlungerKey Then
		plunger.PullBack
		playFieldSound "plungerpull", 0, plunger, 1
	End If

  If tilt = False and state = True Then
	If keycode = leftFlipperKey and contball = 0 Then
		LFPress = 1
		lf.fire
		playFieldSound SoundFX("FlipUpL",DOFFlippers), 0, leftFlipper, 1
		DOF 101,DOFOn
		playFieldSound "FlipBuzzL", -1, leftFlipper, 1
		rotateKeysLeft
	End If
    
	If keycode = RightFlipperKey and contball = 0 Then
		RFPress = 1
		rf.fire
		playFieldSound SoundFX("FlipUpR",DOFFlippers), 0, RightFlipper,1
		DOF 102,DOFOn
		playFieldSound "FlipBuzzR", -1, RightFlipper,1
		rotateKeysRight
	End If
    
	If keycode = leftTiltKey Then
		Nudge 90, 2
		checkTilt
	End If
    
	If keycode = rightTiltKey Then
		Nudge 270, 2
		checkTilt
	End If
    
	If keycode = centerTiltKey Then
		Nudge 0, 2
		checkTilt
	End If
  End If

    If keycode = leftFlipperKey and state = False and operatorMenu = 0 and enableInitialEntry = 0 Then
        operatorMenuTimer.Enabled = true
    End If
 
    If keycode = leftFlipperKey and state = False and operatorMenu = 1 Then
		options = options + 1
		If showDt = True Then If options = 4 Then options = 6 'skips non DT options
        If options = 7 Then options = 0
		optionMenu.visible = True
        playFieldSound "target", 0, SoundPointScoreMotor, 1.5
        Select Case (Options)
            Case 0:
                optionMenu.image = "FreeCoin" & freePlay
            Case 1:
                optionMenu.image = balls & "Balls"
			Case 2:
				optionMenu.image = "Music" & musicOn
			Case 3:
				endMusic
				playMusic "OBWAT/chime10.wav", musicVolume
				optionMenu.image = "musicVolume"
				optionMenu1.visible = 1
				If musicVolume < 1 Then
					optionMenu1.image = "vol" & (musicVolume * 10)
				Else
					optionMenu1.image = "vol10"
				End If
 			Case 4:
				If musicOn = 1 Then playMusic "OBWAT/OBWAT1.mp3", MusicVolume
				optionMenu1.image = "DOF"
				optionMenu.image = "Chime" & chime
			Case 5:
				optionMenu.image = "UnderCab"
				optionMenu1.visible = 1
				optionMenu1.image = "Sound" & pfOption
				optionMenu2.visible = 1
				optionMenu2.image = "SoundChange"
				Select Case (pfOption)
					Case 1: speaker1.visible = 1: speaker2.visible = 1: speaker3.visible = 0: speaker4.visible = 0
					Case 2: speaker5.visible = 1: speaker6.visible = 1: speaker1.visible = 0: speaker2.visible = 0
					Case 3: speaker1.visible = 1: speaker2.visible = 1: speaker3.visible = 1: speaker4.visible = 1: speaker5.visible = 0: speaker6.visible = 0
				End Select
			Case 6:
				If musicOn = 1 Then playMusic "OBWAT/OBWAT1.mp3", MusicVolume
				For x = 1 to 6
					EVAL("Speaker" & x).visible = 0
				Next
				optionMenu1.visible = 0
				optionMenu.image = "SaveExit"
				optionMenu2.visible = 0
		End Select
    End If
 
    If keycode = RightFlipperKey and state = False and operatorMenu = 1 Then
        playFieldSound "metalhit2", 0, SoundPointScoreMotor, 0.2
      Select Case (options)
		Case 0:
            If freePlay = 0 Then
                freePlay = 1
                DOF 427, DOFOn
              Else
                freePlay = 0
				If credit < 1 Then 
					DOF 427, DOFOff
				End If
				If credit > 0 Then 
					DOF 427, DOFOn
				End If
            End If
			optionMenu.image = "FreeCoin" & freePlay
        Case 1:
            If balls = 3 Then
                balls = 5
              Else
                balls = 3
            End If
'			InstructCard.image = "InstructReplay" & balls 

            optionMenu.image = balls & "Balls"
		Case 2:
			If musicOn = 0 Then
				musicOn = 1
				PlayMusic"OBWAT/OBWAT1.mp3", MusicVolume
			Else
				musicOn = 0
				endMusic
			End If
			optionMenu.image = "music" & musicOn
		Case 3:
			musicVolume = musicVolume + .1
			If musicVolume > 1 Then musicVolume = .1
			If musicVolume < 1 Then
				optionMenu1.image = "vol" & (musicVolume *10)
			Else
				optionMenu1.image = "vol10"
			End If
			playMusic "OBWAT/chime10.wav", musicVolume
        Case 4:
            If chime = 0 Then
                chime= 1
				DOF 442,DOFPulse
              Else
                chime = 0
				playFieldsound SoundFX("Chime10",DOFChimes), 0, soundPoint13, 1
            End If
			optionMenu.image = "Chime" & chime
		Case 5:
			optionMenu1.visible = 1
			pfOption = pfOption + 1
			If pfOption = 4 Then pfOption = 1
			optionMenu1.image = "Sound" & pfOption

			Select Case (pfOption)
				Case 1: speaker1.visible = 1: speaker2.visible = 1: speaker3.visible = 0: speaker4.visible = 0
				Case 2: speaker5.visible = 1: speaker6.visible = 1: speaker1.visible = 0: speaker2.visible = 0
				Case 3: speaker1.visible = 1: speaker2.visible = 1: speaker3.visible = 1: speaker4.visible = 1: speaker5.visible = 0: speaker6.visible = 0
			End Select
        Case 6:
            operatorMenu = 0
            saveHighScore
			dynamicUpdatePostIt.enabled = 1
			optionMenu.image = "FreeCoin" & freePlay
            optionMenu1.visible = 0
			optionMenu.visible = 0
			optionsMenu.visible = 0	
			replaySettings
		End Select
    End If

	If Keycode = mechanicalTilt Then 
		tilt = True
		PlaySound "SFX_MusicOver"
		TiltReel.setValue(1)
		If B2SOn Then controller.B2SSetTilt 1
		turnOff
	End If

    If keycode = 46 Then' C Key
       stopSound "FlipBuzzLA"
       stopSound "FlipBuzzLB"
       stopSound "FlipBuzzLC"
       stopSound "FlipBuzzLD"
       stopSound "FlipBuzzRA"
       stopSound "FlipBuzzRB"
       stopSound "FlipBuzzRC"
       stopSound "FlipBuzzRD"
        If contball = 1 Then
            contball = 0
          Else
            contball = 1
        End If
    End If
 
    If keycode = 48 Then 'B Key
        If bcboost = 1 Then
            bcboost = bcboostmulti
          Else
            bcboost = 1
        End If
    End If
 
    If keycode = 203 Then cLeft = 1' Left Arrow
 
    If keycode = 200 Then cUp = 1' Up Arrow
 
    If keycode = 208 Then cDown = 1' Down Arrow
 
    If keycode = 205 Then cRight = 1' Right Arrow

    If keycode = 52 Then Zup = 1' Period

'************************Start Of Test Keys****************************
	If keycode = 30 Then  '"a"
		phaseCompleted
		stopSound("SFX_RadioShow")
'		playsound "SFX_Paddling"
	End If
'
	If Keycode = 31 Then  '"s"
		score(1) = 100000
		endGame = 1
		checkContinue
		
'		For x = 1 to 4
'			Score(x) = Int(rnd(1)*10000)
'			Score(x) = Score(x) * 10
'			SReels(x).setvalue(Score(x))
'			If B2SOn Then Controller.B2SSetScorePlayer x, Score(x)
'		Next
	End If
'
'	If keycode = 33 Then  '"f"
'		BallinPlay = 3
'	End If
'************************End Of Test Keys**************************** 
End Sub

dim TestChr1, TestChr2, TestChr3, TestChr

Sub Table1_KeyUp(ByVal keycode)

	If keycode = plungerKey Then
		plunger.Fire
		playFieldSound "PlungerFire", 0, plunger, 1
	End If

    If keycode = leftFlipperKey Then
        operatorMenuTimer.Enabled = False
    End If

   If tilt = False and state = True Then
		If keycode = leftFlipperKey and contball = 0 Then
			lfpress = 0
			LeftFlipper.eosTorqueAngle = EOSA
			LeftFlipper.eosTorque = EOST
			LeftFlipper.RotateToStart
			playFieldSound SoundFX("FlipDownL",DOFFlippers), 0, leftFlipper, 1
			DOF 101,DOFOff
			stopSound "FlipBuzzLA"
			stopSound "FlipBuzzLB"
			stopSound "FlipBuzzLC"
			stopSound "FlipBuzzLD"
		End If
    
		If keycode = RightFlipperKey and contball = 0 Then
			rfpress = 0
			RightFlipper.eosTorqueAngle = EOSA
			RightFlipper.eosTorque = EOST
			RightFlipper.rotateToStart
			playFieldSound SoundFX("FlipDownR",DOFFlippers), 0, RightFlipper, 1
			DOF 102,DOFOff
			stopSound "FlipBuzzRA"
			stopSound "FlipBuzzRB"
			stopSound "FlipBuzzRC"
			stopSound "FlipBuzzRD"
		End If
   End If

    If keycode = 203 then cLeft = 0' Left Arrow
 
    If keycode = 200 then cUp = 0' Up Arrow
 
    If keycode = 208 then cDown = 0' Down Arrow
 
    If keycode = 205 then cRight = 0' Right Arrow

    If keycode = 52 Then Zup = 0' Period
End Sub

'************** Table Boot
Dim backGlassOn
Dim bootCount:bootCount = 0
Sub bootTable_Timer
	bootCount = bootCount + 1
	If bootCount = 1 Then
		For each Light in GI:light.state = 1: Next
		If B2SOn Then
			Controller.B2SSetCredits Credit
			Controller.B2SSetMatch 34, MatchNumber
			Controller.B2SSetGameOver 35,1
			Controller.B2SSetTilt 33,1
			Controller.B2SSetBallInPlay 32,0
			Controller.B2SSetPlayerUp 30,0
		End If

        If Credit > 0 Then DOF 427, DOFOn		
		If FreePlay = 1 Then DOF 427, DOFOn

		For x = 1 to maxPlayers
			If B2SOn Then controller.B2SSetScorePlayer x, score(x)
			sReel(x).setvalue(score(x))
		Next
		GameOverReel.setValue(1)
		If musicOn = 1 Then PlayMusic"OBWAT/OBWAT1.mp3", MusicVolume
		backGlassOn = 1
		me.enabled = False
		LightSeqAttract.Play SeqUpOn,25,100
		LightSeqAttract2.Play SeqClockRightOn,100,100
		LightSequenceObjs.Play SeqUpOn,25,100
		Alternator
	End If
End Sub

'***********Replay Settings
Sub replaySettings
	If balls = 5 Then
		replay(0) = 140000
		replay(1) = 156000
		replay(2) = 174000
	Else
		replay(0) = 86000
		replay(1) = 97000
		replay(2) = 10500
	End If
End Sub

'***********Operator Menu
Dim operatormenu
Sub operatorMenuTimer_Timer
    If optionMenu.visible = 0 Then PlaySound "SFX_OB_SinceWeBeenFolo",0,1,0,0,0,1,0,0
	options = 0
    operatorMenu = 1
	dynamicUpdatePostIt.enabled = 0
	updatePostIt
	options = 0
    optionsMenu.visible = True
    optionMenu.visible = True
optionMenu.image = "FreeCoin" & freePlay
End Sub

'***********Start Game
Dim ballInPlay, bonusScore
Sub startGame
	DOF 124, DOFOn
	stopSound "SFX_Paddling"
	stopSound "SFX_RadioShow"
	If state = False Then
		PlaySound"SFX_WhoElected",0,1,0,0,0,1,0,0
		ballInPlay = 1
		If B2SOn Then 
			controller.B2SSetCredits credit
			controller.B2SSetBallinPlay 32, ballInPlay
			controller.B2SSetCanPlay 31, 1			
			controller.B2SSetPlayerup 30, 1
			controller.B2SSetGameOver 0
		End If
		UP1.setValue(1)
		BIPReel.setValue(ballInPlay)
		dynamicUpdatePostIt.enabled = 0
		updatePostIt
		tilt = False
		state = True
		gameState
		players = 1
		GameOverReel.setValue (0)
		CanPlayReel.setValue(1)
		for x = 1 to maxPlayers
			score(x) = (score(x) mod 100000)
		Next
		newGame
		If musicOn Then generateSongNumber
	Else If state = True and players < maxPlayers and ballinPlay = 1 Then
		players = players + 1
		creditReel.setvalue(credit)
		PlaySound "SFX_WhoElected" & players
		If B2SOn Then controller.B2SSetCredits credit
		If B2sOn Then controller.B2SSetCanPlay 31, players
		CanPlayReel.setValue(players)
		End If
	End If 
End Sub

'*********New Game
Dim endGame
Sub newGame
	player = 1
    endGame = 0
	radioDelay.enabled = 0
	radioEnd.enabled = 0
	If B2SOn Then 
		controller.B2SSetShootAgain 36,0
	End If
	SamePlayerShootsAgain.state = 0
	For Each obj in bumpers: obj.hasHitEvent = 1: Next
	gameState
	mode(1)
	LightSeqAttract.StopPlay
	LightSeqAttract2.StopPlay
	LightSequenceObjs.StopPlay
	FOPLight.state = 1
	activePhase = 1
	playModeSound = 0
	capturedBalls = 0
	For Each obj in CharacterLights: obj.state = 2: Next
	resetTable
	LockBallsLight.state = 1
	GateWall.isdropped = 1
	For x = 1 to 4
		For y = 1 to 5
			Keys(x,y) = 0
		Next
	Next
	resetReel.enabled = True
End Sub

'**********Check if Game Should Continue
Dim relBall, rep(4)
Sub checkContinue
	If endGame = 1 Then
		DOF 124, DOFOff
		radioDelay.enabled = 1
		turnOff
		match
		state = False
		BIPReel.setValue(0)
		For Each obj in Up: obj.setValue(0): Next
		gameState
		DynamicUpdatePostIt.enabled = 1
		firstBallOut = 0
		CanPlayReel.setValue(0)
		players = 0
		For x = 1 to 4
			rep(x) = 0
			repAwarded(x) = 0
		Next
		saveHighScore
		For Each obj in Bumperlights: obj.state = 0: Next
		For Each obj in bumpers: obj.hasHitEvent = 0: Next
		For Each obj in JourneyLights: obj.state = 0: Next
		For Each obj in ObjectLights: obj.state = 0: Next
		LockBallsLight.state = 0
		GateWall.isdropped = 1
		Kicker1.Kick 160, 12
		Kicker2.kick 230, 15
		Kicker3.Kick 10, 20
		ballsInPlay = ballsInPlay + capturedBalls
		kicker.enabled = 1
		songtimer.enabled = 0
		skedaddleKicker.enabled = 1
		LightSeqAttract.Play SeqUpOn,25,100
		LightSeqAttract2.Play SeqClockRightOn,100,100
		LightSequenceObjs.Play SeqUpOn,25,100
		If B2SOn Then 
			controller.B2SSetGameOver 35,1
			controller.B2SSetballinplay 32, 0
			controller.B2SSetPlayerUp 30, 0
			controller.B2SSetcanplay 31, 0
		End If
		BIPReel.setValue(0)
	Else
		relBall = 1		'variable to tell score motor routine that this is a ball release run
		scoreMotor5.enabled = 1
		bonusScore = activePhase - 1
		BIPReel.setValue(ballinPlay)
		resetTable
  End If
End Sub

'***************Drain and Release Ball
Sub drain_Hit()
	playFieldSound "Drain", 0, drain, 0.001
	repAwarded(Player) = 0
	drain.DestroyBall	'this is used for multiball tables to avoid timing issues with balls moving in the trough
	ballsInPlay = ballsInPlay - 1
	If endGame = 1 Then capturedBalls = capturedBalls - 1
	If Multiball = 1 and ballsInplay = 1 Then 
		phaseCompleted
		MultiballLight.state = 0
		for each obj in MultiBallGroup: obj.state = 1: Next
	End If
	If ballSaveOn = 1 Then 
		releaseBall
	Elseif ballsInPlay = 0 and ballSaveOn = 0 and endGame = 0 Then
		scoreBonus
	End If
End Sub

Dim BallsInPlay
Sub ReleaseBall	
	If ballREnabled = 0 Then
		BallRelease.CreateSizedBallWithMass Ballsize/2, BallMass
		BallRelease.kick 70,20
		ballsInPlay = ballsInPlay + 1
		playFieldSound SoundFX("FastKickIntoLaunchLane",DOFContactors), 0, Drain, 0.5
		DOF 425,DOFPulse
		If ballSaveOn = 0 Then ballSaveOn = 1: ballSave.enabled = 1: SamePlayerShootsAgain.state = 1
	End If
	BIPReel.setValue(ballInPlay)
	If B2SOn Then controller.B2SSetBallinPlay 32, BallinPlay

End Sub

'**********Check if Scoring Bonus is True
Dim bonusFlag
Sub scoreBonus
	endMusic()
	bonusFlag = 1
	If activePhase < 14 Then EVAL("bonus" & activePhase).state = 0
	scoreMotor.enabled = 1
	tb.text = "ap = " & activePhase
	If bonusScore = 0 And SamePlayerShootsAgain.state = 1 And scoreMotorLoop = 0 Then resetTable: releaseBall 
	If bonusScore = 0 And SamePlayerShootsAgain.state = 0 And scoreMotorLoop = 0 Then 
		If activePhase > 13 Then 
			endGame = 1
			checkContinue
		Else
			advancePlayers
		End If
	End If
End Sub


'**********Advance Players
Sub advancePlayers
	BonusFlag = 0
	If players = 1 or player = players Then 
		player = 1
		EVAL("UP" & players).SetValue(0)
		Up1.SetValue(1)
	Else
		player = player + 1
		EVAL ("Up" & (player - 1)).setValue(0)
		EVAL ("Up" & player).setValue(1)
	End If
	If B2SOn Then controller.B2SSetPlayerup 30, player
	nextBall
End Sub

'**********Next Ball
Sub nextBall
    If Tilt = True Then
		For Each obj in bumpers: obj.hasHitEvent = 1: Next
		tilt = False
		tiltReel.setValue(0)
		If B2SOn Then
			controller.B2SSetTilt 33,0
			controller.B2SSetData 1, 1
		End If
    End If
	resetTable
	If Player = 1 then ballinPlay = ballinPlay + 1
'	If player = 4 then BallinPlay = 5 'used for match testing

	If ballinPlay > balls then
		endGame = 1
		checkContinue
	Else
		If musicOn Then generateSongNumber
		PlaySound "SFX_TightSpot",0,1,0,0,0,1,0,0
		If state = True Then checkContinue
	End If
End Sub

'************Game State Check
	dim xx	

Sub gameState
	If state = False Then
		GameOverReel.setValue(1)
		If B2SOn then controller.B2SSetGameOver 35,1
	Else 
		GameOverReel.setValue(0)
		MatchReel.setValue(0)
		TiltReel.setValue(0)
		If B2SOn Then 
			controller.B2SSetTilt 33,0
			controller.B2SSetMatch 34,0
			controller.B2SSetGameOver 35,0
		End If
	End If
End Sub	

'*************Ball in Launch Lane on Plunger Tip
Dim ballREnabled, relGateHit
Sub ballHome_hit
	ballREnabled = 1
	DOF 450, DOFOn
	relGateHit = 0
	Set controlBall = ActiveBall
    contBallInPlay = True
End Sub

'*************Ball off of Plunger Tip
Sub ballHome_unhit
	DOF 450, DOFOff
End Sub

'******* for ball control script
Sub EndControl_Hit()                
    contBallinPlay = False
End Sub

'************Check if Ball Out of Launch Lane
Dim ballInLane
Sub ballsInPlay_hit
	If ballREnabled = 1 Then
		If B2SOn Then controller.B2SSetShootAgain 36,0
		ballREnabled = 0
		ballInLane = False
	End If
	firstBallOut = 1
End Sub

'**********Reset Table
Sub resetTable
	BIPReel.setValue(BallinPlay)
	keySum = 0
	bonafide = 0
	bonafideTotal = 0
	bonafidePhase = 0
	WatchLight.state = 0
	For Each obj in BonaFideLights: obj.state = 0: Next
	DTRaise 1: DTRaise 2: DTRaise 3: DTRaise 4
	bonusMult = 1
	For each obj in MultiplierLights: obj.state = 0: Next
	For each obj in BumperLights: obj.state = 0: Next
	DapperDanLight.state = 0
	FOPLight.state = 1
	For each obj in OutlaneLights: obj.state = 0: Next
	
	If activePhase > 1 Then
		For x = 1 to (activePhase - 1)
			EVAL("bonus" & x).state = 1
		Next
	End If
	EVAL("bonus" & activePhase).state = 2
	For x = 1 to 5
		Keys(player,x) = 0 
	Next
	For Each obj in KeyLights: obj.state = 0: Next
	For Each obj in lockLights: obj.state = 0: Next
End Sub

'************** Treasure Phases
Dim activePhase
Sub mode(phase)
	Select Case phase
		Case 1: multiballReady
				bonus1.state = 2
		Case 2: HandcarLight.state = 2: bonus2.state = 2 
		Case 3: bonus3.state = 2: CarLight.state = 2
		Case 4: bonus4.state = 2: WaterLight.state = 2
		Case 5: bonus5.state = 2: CarLight.state = 2: GuitarLight.state = 2
		Case 6: bonus6.state = 2: GuitarLight.state = 2: MicLight.state = 2
		Case 7: bonus7.state = 2: MachineGunLight.state = 2: CarLight.state = 2: GeorgeLight.state = 2
		Case 8: bonus8.state = 2: MoonshineLight.state = 2: FrogLight.state = 2: For Each obj in SirenLights: obj.state = 2: Next
		Case 9: bonus9.state = 2: FistsLight.state = 2: VernonLight.state = 2
		Case 10: bonus10.state = 2: AnvilLight.state = 2: FrogLight.state = 2
		Case 11: bonus11.state = 2: BroomLight.state = 2: StokesLight.state = 2: BeardLight.state = 2
		Case 12: bonus12.state = 2: BeardLight.state = 2: FlourLight.state = 2: NooseLight.state = 2
		Case 13: bonus13.state = 2: CowLight.state = 2: WaterLight.state = 2: RingLight.state = 2
	End Select
End Sub

Dim playModeSound
Sub modeCheck
	For Each obj in ObjectLights
		If obj.state = 2 Then playModeSound = playModeSound + 1
	Next
	Select Case activePhase  
		Case 2: If HandcarLight.state = 1 Then multiballReady
		Case 3: If CarLight.state = 1 Then multiballReady
		Case 4: If WaterLight.state = 1 Then multiballReady
		Case 5: If CarLight.state = 1 and GuitarLight.state = 1 Then multiballReady
		Case 6: If GuitarLight.state = 1 and MicLight.state = 1 Then multiballReady
		Case 7: If MachineGunLight.state = 1 and CarLight.state = 1 and GeorgeLight.state = 1 Then multiballReady
		Case 8: If  MoonshineLight.state = 1 and FrogLight.state = 1  and Siren1Light.state = 1 and Siren2Light.state = 1 and Siren3Light.state = 1 Then multiballReady
 		Case 9: If FistsLight.state = 1 and VernonLight.state = 1 Then multiballReady
		Case 10: If AnvilLight.state = 1 and FrogLight.state = 1 Then multiballReady
		Case 11: If BroomLight.state = 1 and StokesLight.state = 1 and BeardLight.state = 1 Then multiballReady
		Case 12: If BeardLight.state = 1 and FlourLight.state = 1 and NooseLight.state = 1 Then multiballReady
		Case 13: If CowLight.state = 1 and WaterLight.state = 1 and RingLight.state = 1 Then multiballReady
	End Select
End Sub

Sub phaseCompleted
	playModeSound = 0
	Select Case activePhase
		Case 1: bonus1.state = 1
		Case 2: bonus2.state = 1
		Case 3: bonus3.state = 1
		Case 4: bonus4.state = 1
		Case 5: bonus5.state = 1
		Case 6: bonus6.state = 1
		Case 7: bonus7.state = 1
		Case 8: bonus8.state = 1
		Case 9: bonus9.state = 1
		Case 10: bonus10.state = 1
		Case 11: bonus11.state = 1
		Case 12: bonus12.state = 1
		Case 13: bonus13.state = 1
	End Select
	multiball = 0
	bonusScore = activePhase
	activePhase = activePhase + 1
	tb.text = "AP = " & activePhase
	For Each obj in standupLights: obj.state = 0: Next
	For Each obj in ObjectLights: obj.state = 0: Next
	For Each obj in CharacterLights: obj.state = 0: Next
	mode(activePhase)
End Sub

Sub multiballReady
	For Each obj in CharacterLights: obj.state = 2: Next
	LockBallsLight.state = 1
End Sub

'************** Bumpers Hit
Sub bumpers_Hit(Index)
	If tilt = False Then 
		If EVAL("BumperLight" & (Index + 1)).state = 1 Then
			addscore 100
		Else
			addScore 10
		End If
		DOF (105 + Index), DOFPulse
        DOF (110 + Index), DOFPulse
	End If
End Sub

Dim Alt
Sub Alternator
	Alt = Alt + 1
	If Alt > 1 Then Alt = 0
	If Alt = 1 Then
		VictrolaLightR.state = 1
		VictrolaLightL.state = 0
	Else
		VictrolaLightR.state = 0
		VictrolaLightL.state = 1
	End If
End Sub

'************** StandUps Hit
Dim standupHit
Sub standUps_Hit(Index)
    DOF 114 + Index, DOFPulse
	If tilt = False Then
		Select Case (Index)
			Case 0: addScore 500 
					If StokesLight.state = 2 Then 
						StokesLight.state = 1: modeCheck: If playModeSound = 0 Then playObjectiveSound
					ElseIf StokesLight.state = 0 Then
						flashLight Index,1
					End If
			Case 1: addScore 500
					If VernonLight.state = 2 Then 
						VernonLight.state = 1: modeCheck: If playModeSound = 0 Then playObjectiveSound
					Elseif VernonLight.state = 0 Then 
						flashLight Index,1
					End If
			Case 2: addScore 10
					For each obj in bumperLights: obj.state = 0: Next
					If FOPLight.state = 0 Then PlaySound "SFX_DontCarry",0,1,0,0,0,1,0,0
					DapperDanLight.state = 0
					FOPLight.state = 1
			Case 3: addScore 1000 
					For each obj in bumperLights: obj.state = 1: Next
					If DapperDanLight.state = 0 Then PlaySound "SFX_DapperDan",0,1,0,0,0,1,0,0
					DapperDanLight.state = 1
					FOPLight.state = 0
			Case 4: addScore 300: Siren1Light.state = 1: modeCheck: If playModeSound = 0 Then playObjectiveSound	
			Case 5: addScore 300: Siren2Light.state = 1: modeCheck: If playModeSound = 0 Then playObjectiveSound
			Case 6: addScore 300: Siren3Light.state = 1: modeCheck: If playModeSound = 0 Then playObjectiveSound
			Case 7: addScore 500
					If GuitarLight.state = 2 Then 
						GuitarLight.state = 1 
						modeCheck
						If playModeSound = 0 Then
							Select Case activePhase
								Case 5: playObjectiveSound
								Case 6: playObjectiveSound
							End Select
						End If
					ElseIf GuitarLight.state = 0 Then
						flashLight Index,1
					End If
			Case 8: If WatchLight.state = 2 Then 
						WatchLight.state = 1: addScore 5000
						PlaySound "SFX_DOLightFingers",0,1,0,0,0,1,0,0
					ElseIf WatchLight.state = 0 Then
						flashLight Index,1
						PlaySound "SFX_CountOnYou",0,1,0,0,0,1,0,0
					End If
			Case 9: addScore 500
					If GeorgeLight.state = 2 Then
						GeorgeLight.state = 1:
						modeCheck
						If playModeSound = 0 Then playObjectiveSound
					ElseIf GeorgeLight.state = 2 Then
						flashLight Index,1
					End If
		End Select
			playModeSound = 0
		If Siren1Light.state = 1 and Siren2Light.state = 1 and Siren3Light.state = 1 and activePhase <> 8 Then
			For Each obj in OutlaneLights: obj.state = 1: Next	
			For Each obj in SirenLights: obj.state = 0: Next
		End If
	End If
End Sub

Dim currentIndex
Sub flashLight(Index, onOff)
	If Index < 10 Then currentIndex = Index	
	If onOff = 1 Then
		standupLights(currentIndex).state = 1
	Else
		standupLights(currentIndex).state = 0
	End If
	flashTimer.enabled = 1
End Sub

Sub flashTimer_timer
	flashLight 11,0
	flashTimer.enabled = 0
End Sub

'***********Rotate Spinner
'Dim angle 
'Sub spinnerTimer_Timer
'	angle = (sin (spinner.CurrentAngle))
'	spinner_Mesh.RotX = spinner.CurrentAngle
 '   spinnerRod.TransZ = -sin( (spinner.CurrentAngle+180) * (2*3.14/360)) * 5
'    spinnerRod.TransX = (sin( (spinner.CurrentAngle- 90) * (2*3.14/360)) * -5)
'End Sub

'************** 10's Rubbers
Sub TensRubbers_Hit
	addScore 10
End Sub

'*************** Triggers     
Sub triggers_hit(index)
	If tilt = False Then
		Select Case (index)
			Case 0: 
		End Select
	End If
End Sub

'************** Button Animation
'Dim button
'Sub rollOverAnimation_Timer
'	button = button + 1
'	Select Case Button
'		Case 1: WiggleGateOpenButton.transz = -2
'		Case 2: WiggleGateOpenButton.transz = 0
'		Case 3: WiggleGateOpenButton.transz = 1
'		Case 4: WiggleGateOpenButton.transz = 1	
'				button = 0
'				rollOverAnimation.enabled = 0
'	End Select
'End Sub

'*************** Targets
Dim bonafide, bonafideTotal, bonafidePhase, bonafideMultiplier
Sub dropTargets_hit(index)
	If resetDropsTimer.enabled = 1 Then exit Sub
	If tilt = False Then
		Select Case (index)
			Case 0: If bonafidePhase = 0 Then BLight.state = 1: else: FLight.state = 1
			Case 1: If bonafidePhase = 0 Then OLight.state = 1: else: ILight.state = 1
			Case 2: If bonafidePhase = 0 Then NLight.state = 1: else: DLight.state = 1
			Case 3: If bonafidePhase = 0 Then ALight.state = 1: else: ELight.state = 1
		End Select
		For each obj in bona
			If obj.state = 1 and bonafidePhase = 0 Then bonafideTotal = bonafideTotal + 1 
		Next
		For Each obj in fide
			If obj.state = 1 and bonafidePhase = 1 Then bonafideTotal = bonafideTotal + 1
		Next
		If bonafideTotal = 4 Then
			resetDropsTimer.enabled = 1
		End If
		If BLight.state = 1 and OLight.state = 1 and bonafidePhase= 0 Then WatchLight.state = 2
		If FLight.state = 1 and ILight.state = 1 Then WatchLight.state = 2
		bonafideTotal = 0
		addScore 100
	End If
End Sub

Sub resetDropsTimer_Timer
	DTRaise 1: DTRaise 2: DTRaise 3: DTRaise 4
	WatchLight.state = 0
	If tilt = False Then
		If bonafidePhase = 0 and bonafide < 1 Then
			addScore 1000
			PlaySound "SFX_BonaFide",0,1,0,0,0,1,0,0
		ElseIf bonafidePhase = 0 and bonafide > 0 Then
			addScore 1000
			PlaySound "SFX_Paterfamilias",0,1,0,0,0,1,0,0
		Else
			addScore 1000
			For Each obj in bona: obj.state = 0: Next
			for Each obj in fide: obj.state = 0: Next
			bonafide = bonafide + 1
			If bonafide > 2 Then bonafide = 2
			Select Case bonafide	
				Case 1: PlaySound "SFX_BonaFide2",0,1,0,0,0,1,0,0
				Case 2: PlaySound "SFX_Paterfamilias",0,1,0,0,0,1,0,0: bonafide = 2
			End Select
			If keySum > 0 Then 
				bonafideMultiplier = 2
			Else
				bonafideMultiplier = 0
			End If
			EVAL("MultiLight" & (bonafide + bonafideMultiplier)).state = 1
			If bonafide > 1 Then EVAL("MultiLight" & (bonafide + bonafideMultiplier - 1)).state = 0
		End If
		bonafidePhase = bonafidePhase + 1
		If bonafidePhase > 1 Then bonafidePhase = 0
		bonusMultiplier
	End If
	resetDropsTimer.enabled = 0
End Sub

Dim bonusMult
Sub bonusMultiplier
	If bonafide > 0 Then 
		bonusMult = (bonafide + 1)
		If keySum > 0 Then bonusMult = bonusMult * 2
	End If
End Sub

Sub dtDropTrigger_hit
	DTDrop 1: DTDrop 2
End Sub

Sub dtDropTrigger_unhit
	If skedaddleKicker.enabled = 0 Then dtRaiser.enabled = 1
End Sub

Sub dtRaiser_timer
	If BLight.state = 0 and bonafidePhase = 0 or FLight.state = 0 and bonafidePhase = 1 Then DTRaise 1
	If OLight.state = 0 and bonafidePhase = 0 or ILight.state = 0 and bonafidePhase = 1 Then DTRaise 2
	dtRaiser.enabled = 0
End Sub

'************************ Ramps
Sub wireRampTrigger_Hit
	If HandcarLight.state = 2 Then 
		HandcarLight.state = 1
		modeCheck
		If playModeSound = 0 Then playObjectiveSound
	ElseIf MachineGunLight.state = 2 Then 
		MachineGunLight.state = 1
		modeCheck
		If playModeSound = 0 Then playObjectiveSound
	ElseIf FlourLight.state = 2 Then 
		FlourLight.state = 1
		modeCheck
		If playModeSound = 0 Then playObjectiveSound
	Else:
		PlaySound "SFX_Train3",0,1,0,0,0,1,0,0
	End If
	playModeSound = 0
End Sub

Sub frogRampTrigger_Hit
	If MicLight.state = 2 Then
		MicLight.state = 1 
		modeCheck
		If playModeSound = 0 Then playObjectiveSound
	ElseIf MoonshineLight.state = 2 and FrogLight.state = 1 Then 
		MoonshineLight.state = 1
		modeCheck
		If playModeSound = 0 Then playObjectiveSound
	ElseIf FrogLight.state = 2 Then 
		FrogLight.state = 1
		modeCheck
		Select Case activePhase
			Case 8: If playModeSound = 0 Then playObjectiveSound
			Case 10: If playModeSound = 0 Then playObjectiveSound
		End Select
	Else:
		PlaySound "SFXFrog",0,1,0,0,0,1,0,0
	End If
	playModeSound = 0
End Sub

Sub readyForMB
	For Each obj in CharacterLights: obj.state = 2: Next
	LockBallsLight.state = 1
End Sub

Sub rollOvers_Hit(Index)
	DOF 125 + Index, DOFPulse
	If tilt = False Then
		Select Case Index
			Case 0: If LeftOutlaneLight.state = 1 Then 
						addscore 1000 
					Else: 
						addScore 100
					End If
					Playsound "SFX_NoSense",0,1,0,0,0,1,0,0
			Case 1: addScore 300: Keys(player, 5) = 1: KeyLight5.state = 1: keyCheck:
			Case 2: If AnvilLight.state = 2 Then 
						AnvilLight.state = 1
						modeCheck
						If playModeSound = 0 Then playObjectiveSound
					ElseIf CarLight.state = 2 Then 
						CarLight.state = 1
						modeCheck
						If playModeSound = 0 Then 
							Select Case activePhase
								Case 3: playObjectiveSound
								Case 5: playObjectiveSound
								Case 7: playObjectiveSound
							End Select
						End If
					ElseIf BeardLight.state = 2 Then 
						BeardLight.state = 1
						modeCheck
						If playModeSound = 0 Then 
							Select Case activePhase
								Case 11: playObjectiveSound
								Case 12: playObjectiveSound
							End Select
						End If
					Else:
						PlaySound "SFX_Car",0,1,0,0,0,1,0,0
					End If
					playModeSound = 0
			Case 3: addscore 300: Keys(player, 1) = 1: KeyLight1.state = 1: keyCheck
			Case 4: addscore 300: Keys(player, 2) = 1: KeyLight2.state = 1: keyCheck
			Case 5: addscore 300: Keys(player, 3) = 1: KeyLight3.state = 1: keyCheck
			Case 6: addscore 300: Keys(player, 4) = 1: KeyLight4.state = 1: keyCheck
			Case 7: If RightOutlaneLight.state = 1 Then 
						addscore 1000 
					Else: 
						addScore 100
					End If
					PlaySound "SFX_OB_SamHill",0,1,0,0,0,1,0,0
		End Select
	End If
	wireNumber = index	
	wireAnimation.enabled = 1
End Sub

Sub starRollOvers_Hit(Index)
	Select Case Index
		Case 0: If LeftStarLight.state = 1 Then
					addScore 500
				Else
					addScore 50
				End If
		Case 1: If CowLight.state = 2 Then 
					CowLight.state = 1
					modeCheck
					If playModeSound = 0 Then playObjectiveSound
				Else
					PlaySound "SFX_CowMoo",0,1,0,0,0,1,0,0
				End If
		Case 2: If BroomLight.state = 2 Then 
					BroomLight.state = 1
					modeCheck
					If playModeSound = 0 Then playObjectiveSound
				ElseIf FistsLight.state = 2 Then 
					FistsLight.state = 1
					modeCheck
					If playModeSound = 0 Then playObjectiveSound
				ElseIf NooseLight.state = 2 Then 
					NooseLight.state = 1
					modeCheck
					If playModeSound = 0 Then playObjectiveSound
				Else
					PlaySound "SFX_Dogs3",0,1,0,0,0,1,0,0
				End If
	End Select
	playModeSound = 0
End Sub

'************** Wire RollOver Animation
Dim wire, wireNumber
Sub wireAnimation_Timer
	wire = wire + 1
	Select Case wire
		Case 1: EVAL ("wire" & wireNumber + 1).transz = -10
		Case 2: EVAL ("wire" & wireNumber + 1).transz = -4
		Case 3: EVAL ("wire" & wireNumber + 1).transz = -1
		Case 4: EVAL ("wire" & wireNumber + 1).transz = 0	
				wire = 0
				wireAnimation.enabled = 0
	End Select
End Sub

Dim Keys(4,6), keyTotal, keySum 
Sub keyCheck
	For Each obj in KeyLights
		If obj.state = 1 Then keyTotal = (keyTotal + 1) 
	Next
	If keyTotal = 5 Then
		PlaySound "SFX_Chain",0,1,0,0,0,1,0,0
		keySum = keySum + 1
		If keySum > 3 Then keySum = 3
		For Each obj in KeyLights: obj.state = 0: Next
		For x = 1 to 5
			Keys(player,x) = 0
		Next
		EVAL("LockLight" & keySum).state = 1
		addScore (1000 * keySum)
		bonafideMultiplier = 2
		If bonafide > 0 Then
			EVAL("MultiLight" & (bonafide + bonafideMultiplier)).state = 1
			EVAL("MultiLight" & bonafide).state = 0
		End If
		bonusMultiplier
		If keySum = 3 Then SamePlayerShootsAgain.state = 1
	End If
	keyTotal = 0
End Sub

Dim tempKeys(4,5)
Sub rotateKeysLeft
	For x = 1 To 5
		tempKeys(player, x) = Keys(player, x)
	Next
	For x = 1 to 5
		If tempKeys(player, x) = 1 Then Keys(player, x - 1) = 1
		If tempKeys(player, x) = 0 Then Keys(player, x - 1) = 0
	Next
	Keys(player, 5) = Keys(player, 0)
	updateKeys
End Sub

Sub rotateKeysRight
	For x = 1 To 5
		tempKeys(player, x) = Keys(player, x)
	Next
	For x = 1 to 5
		If tempKeys(player, x) = 1 Then Keys(player, x + 1) = 1
		If tempKeys(player, x) = 0 Then Keys(player, x + 1) = 0
	Next
	Keys(player, 1) = Keys(player, 6)
	updateKeys
End Sub

Sub updateKeys
	For x = 1 to 5
		If Keys(player,x) = 1 Then
			EVAL("KeyLight" & x).state = 1
		Else
			EVAL("KeyLight" & x).state = 0
		End If
	Next
End Sub

'************ Kickers
Dim kickStep, multiball, capturedBalls
Sub Kickers_Hit(Index)
	playFieldSound "SaucerIn", 0, EVAL("kicker" & Index + 1), 1
	scoreMotorLoop = 0
	kickStep = 0
	If tilt = False Then 
		If Index < 3 Then
			addScore 3000
		End If
	End If
	If Index = 2 Then GateWall.isdropped = 0
	If Index = 3 Then 
		If multiballLight.state = 2 Then	
			addScore 5000
			PlaySound "SFX_CashRegister2"
		Else
			addScore 1000
		End If
		skedaddleKicker.enabled = 1
		Exit Sub
	End If
	If LockBallsLight.state = 0 Then 
		kicker.enabled = 1
	Else
		BallsInPlay = BallsInPlay - 1
		capturedBalls = capturedBalls + 1
		If BallsInPlay = 0 and capturedBalls < 3 Then 
			ReleaseBall
			EVAL("character" & Index).state = 1
		Else
			For Each obj in CharacterLights: obj.state = 0: Next	
			LockBallsLight.state = 0
			For Each obj in ObjectLights: obj.state = 0: Next
'			For Each obj in standupLights: obj.state = 1: Next
			capturedBalls = 0
			BallsInPlay = 3
			multiball = 1
			MultiballLight.state = 2
			for each obj in MultiballGroup: obj.state = 2: Next
			playExitSound
			kicker.enabled = 1
		End If
	End If
End Sub

Sub kicker_timer
    Select Case kickStep
        Case 7: Kicker1.Kick 160, 12
				Kicker2.kick 230, 15
				Kicker3.Kick 10, 20
                DOF 432, DOFPulse
				playModeSound = 0
				GateWall.isdropped = 1
				PlayFieldSound SoundFX("SaucerKick",DOFContactors),0, kicker1, 1			
				For Each obj in KickArm: obj.ObjRotX=12: Next
        Case 8: For Each obj in KickArm: obj.ObjRotX = -45: Next
        Case 9: For Each obj in KickArm: obj.ObjRotX = -45: Next
        Case 10: For Each obj in KickArm: obj.ObjRotX = 24: Next
        Case 11: For Each obj in KickArm: obj.ObjRotX = 12: Next
        Case 12: For Each obj in KickArm: obj.ObjRotX = 0:: Next
				 kicker.Enabled = 0
				 kickStep = 0
    End Select
    kickStep = kickStep + 1
End Sub

Sub skedaddleKicker_Timer
    Select Case kickStep
		Case 1: DTDrop 1: DTDrop 2: DTDrop 3: DTDrop 4
				If RingLight.state = 2 Then 
					RingLight.state = 1 
					modeCheck
					If playModeSound = 0 Then playObjectiveSound
				Else:
					If endGame = 0 and ballsInPlay = 1 Then PlaySound "SFX_Skedaddle",0,1,0,0,0,1,0,0
				End If
				playModeSound = 0
        Case 7: Kicker4.kick 240, 20
				PlayFieldSound SoundFXDOF("SaucerKick",432,DOFPulse,DOFContactors),0, kicker4, 1
                DOF 429, DOFPulse			
				Pkickarm4.ObjRotX=12
        Case 8: Pkickarm4.ObjRotX = -45
        Case 9: Pkickarm4.ObjRotX = -45
        Case 10: Pkickarm4.ObjRotX = 24
        Case 11: Pkickarm4.ObjRotX = 12
        Case 12: Pkickarm4.ObjRotX = 0
				 skedaddleKicker.Enabled = 0
				 kickStep = 0
				 If BLight.state = 0 and bonafidePhase = 0 or FLight.state = 0 and bonafidePhase = 1 Then DTRaise 1
				 If OLight.state = 0 and bonafidePhase = 0 or ILight.state = 0 and bonafidePhase = 1 Then DTRaise 2
				 If NLight.state = 0 and bonafidePhase = 0 or DLight.state = 0 and bonafidePhase = 1 Then DTRaise 3
				 If ALight.state = 0 and bonafidePhase = 0 or ELight.state = 0 and bonafidePhase = 1 Then DTRaise 4
    End Select
    kickStep = kickStep + 1
End Sub

Sub playExitSound  'Start Multiball
    DOF 433, DOFPulse
	soundPlayed = 0
	Select Case activePhase
		Case 1: PlaySound "SFX_Escape"
		Case 2: PlaySound "SFX_HitchRide2"
		Case 3: PlaySound "SFX_FleeBarn"
		Case 4: PlaySound "SFX_GetBaptized"
		Case 5: PlaySound "SFX_SingIntoCan"
		Case 6: PlaySound "SFX_YonderCan"
		Case 7: PlaySound "SFX_JackingUp"
		Case 8: PlaySound "SFX_SeducedA"
		Case 9: PlaySound "SFX_Fight"
		Case 10: PlaySound "SFX_ToadB"
		Case 11: PlaySound "SFX_CrashRally"
		Case 12: PlaySound "SFX_Pardoned"
		Case 13: PlaySound "SFX_Repose"
	End Select
End Sub

Dim soundPlayed
soundPlayed = 0
Sub playObjectiveSound   'Have All Objects
	If soundPlayed = 0 Then
		Select Case activePhase
			Case 2: PlaySound "SFX_HitchRide1"
			Case 3: PlaySound "SFX_Coppers"
			Case 4: PlaySound "SFX_Gopher"
			Case 5: PlaySound "SFX_PickUpTommy"
			Case 6: PlaySound "SFX_RecordSong"
			Case 7: PlaySound "SFX_George"
			Case 8: PlaySound "SFX_SeducedB"
			Case 9: PlaySound "SFX_Vernon"
			Case 10: PlaySound "SFX_FreePete"
			Case 11: PlaySound "SFX_Homer"
			Case 12: PlaySound "SFX_ThankSoggy"
			'Case 13: PlaySoung "SFXNoTreasure"
		End Select
	End If
	soundPlayed = 1
End Sub

''**********Sling Shot Animations
'' Rstep and Lstep  are the variables that increment the animation
''****************
Dim lStep, rStep, slingTotal
Sub rightSlingShot_Slingshot
	playfieldSound SoundFXDOF("SlingShot",109,DOFPulse,DOFContactors), 0, SoundPoint13, 1
	DOF 104, DOFPulse
    If VictrolaLightR.state = 1 Then	
		addscore 100
	Else	
		addscore 10
	End If
    rSling0.Visible = 0
    rSling1.Visible = 1
    sling1.Rotx = 10
    rStep = 0
	slingTotal = slingTotal + 1
	If slingTotal mod 20 = 0 Then 
'		PlaySound "SFX_CashRegister"
		PlaySound "SFX_FatContract",0,1,0,0,0,1,0,0
		addScore 1000
	End If
    rightSlingShot.TimerEnabled = 1
End Sub

Sub rightSlingShot_Timer
    Select Case rStep
        Case 3:rSLing1.Visible = 0:rSLing2.Visible = 1:sling1.Rotx = 0
        Case 4:rSLing2.Visible = 0:rSLing0.Visible = 1:rightSlingShot.TimerEnabled = 0
    End Select
    rStep = rStep + 1
End Sub
'
Sub leftSlingShot_Slingshot
 	playfieldSound SoundFXDOF("SlingShot",103,DOFPulse,DOFContactors), 0, SoundPoint12, 1
	DOF 108, DOFPulse
    If VictrolaLightL.state = 1 Then	
		addscore 100
	Else	
		addscore 10
	End If
    lSling0.Visible = 0
    lSling1.Visible = 1
    sling2.Rotx = 10
    lStep = 0
	slingTotal = slingTotal + 1
	If slingTotal mod 20 = 0 Then 
'		PlaySound "SFX_CashRegister"
		PlaySound "SFX_FatContract",0,1,0,0,0,1,0,0
		addScore 1000
	End If
    leftSlingShot.TimerEnabled = 1
End Sub

Sub leftSlingShot_Timer
    Select Case lStep
        Case 3:lSLing1.Visible = 0:lSLing2.Visible = 1:sling2.Rotx = 0
        Case 4:lSLing2.Visible = 0:lSLing0.Visible = 1:leftSlingShot.TimerEnabled = 0
    End Select
    lStep = lStep + 1
End Sub

Sub waterSling_Slingshot
	addScore 100
	If WaterLight.state = 2 Then 
		WaterLight.state = 1
		modeCheck
		If playModeSound = 0 Then 
			Select Case activePhase
				Case 4: playObjectiveSound
				Case 13: playObjectiveSound
			End Select
		End If
		playModeSound = 0
	End If
End Sub

'*************** Spinners
Sub spinners_Spin(Index)
	playFieldSound "spinner", 0, Spinner1, 1
	If tilt = False Then 
		addscore 100
		DOF (430 + Index), DOFPulse
	End If
End Sub

Dim ballSaveOn, ballSaveCount
'**************Ball Save 
Sub ballSave_Timer
	ballSaveCount = ballSaveCount + 1
	If ballSaveCount = 8 Then SamePlayerShootsAgain.state = 2
	If ballSaveCount = 10 Then: SamePlayerShootsAgain.state = 0: ballSaveOn = 0: ballSaveCount = 0: ballSave.enabled = 0
End Sub

'**************Special
Sub special
	AddCredit = 1
	ScoreMotor5.enabled = 1
	playsound SoundFXDOF("Knocker",428,DOFPulse,DOFKnocker)
    DOF 429, DOFPulse
End Sub

'***************Delay to start radio sfx
Sub radioDelay_Timer
	If activePhase = 14 Then
		PlaySound "EMGameEnd",0,1,0,0,0,1,0,0
	Else
		PlaySound "SFX_RadioShow",0,1,0,0,0,1,0,0
	End If
 	sortScores
	checkHighScores
	radioDelay.enabled = 0
	radioEnd.enabled = 1
End Sub

Sub radioEnd_Timer
	If musicOn = 1 Then PlayMusic"OBWAT/OBWAT1.mp3", MusicVolume
	radioEnd.Enabled = 0
End Sub

'***************Score Motor Run one full rotation
Dim scoreMotorCount, addCredit
Sub scoremotor5_Timer
	scoreMotorCount = scoreMotorCount + 1
	playFieldSound "ScoreMotorSingleFire", 0, SoundPointScoreMotor, 0. 'need to set location of score motor under the PF
	If scoremotorCount = 5 Then
		If relBall = 1 Then
			relBall = 0
			releaseBall
		End If
		If addCredit = 1 Then
			credit = credit + 1
			If showDT = False Then PlayReelSound "Reel5", bgcrReel Else PlayReelSound "Reel5", dtcrReel
			DOF 427, DOFOn	
			If credit > 15 then credit = 15
			CreditReel.setValue (credit)
			If B2SOn Then 
				controller.B2SSetCredits credit
			End If
            If credit > 0 Then DOF 427, DOFOn
			addCredit = 0
		End If
		scoreMotorCount = 0
		scoreMotor5.enabled = 0
	End If	
End Sub

'**************Score Motor Routine
Dim bellRing, scoreMotorLoop, kickerUp, KickBonus, motorOn
Sub ScoreMotor_timer
	scoreMotorLoop = scoreMotorLoop + 1
	If scoreMotorLoop < 6 Then playFieldSound "ScoreMotorSingleFire", 0, SoundPointScoreMotor, 0.2
	motorOn = 1

' BonusFlag lets the sub know that the bonus score is being paid out
' The bonus is paid out on the 2nd and 4th positions of the score motor, the 6th position is the index reel notch	
	If bonusFlag = 1 Then
		Select Case scoreMotorLoop
			Case 1:	
			Case 2: If bonusScore > 0 Then
						If tilt = False Then totalUp 1000 * bonusMult
						If bonusScore < 14 Then EVAL("bonus" & BonusScore).State =  0
						bonusScore = bonusScore - 1
					End If
			Case 3: 
			Case 4: If bonusScore > 0 Then
						If tilt = False Then totalUp 1000 * bonusMult
						EVAL("bonus" & BonusScore).State =  0
						bonusScore = bonusScore - 1
					End If
			Case 5: 
			Case 6: scoreMotorLoop = 0
					If bonusScore < 1 Then
						scoreBonus
						motorOn = 0
						scoreMotor.enabled = 0
					End If
		End Select
	Else
		scoreMotorLoop = 0
		scoremotor.enabled = 0
		Exit Sub
	End If
End Sub

'***************Scoring Routine
Dim point, point100
Sub addScore(points) 
	reelDone(player) = 0
	If tilt = False Then
		If points > 9 and points <100 Then
			totalUp(points)
			If chime = 0 Then
				playFieldSound SoundFX("Chime10",DOFChimes), 0, soundPoint13, 1
			Else 
				DOF 442,DOFPulse
			End If
			Exit Sub
		End If

		If points > 99 and points < 1000 Then
'			Number Matching, decrement the match unit for each 100 point score
			point100 =  (points / 100)
			If matchNumber >= point100 Then 		
				matchNumber = matchNumber - point100
			Else 
				point100 = point100 - matchNumber
				matchNumber = 10 - point100
			End If	
			Alternator
			totalUp(points)
			If chime = 0 Then
				playFieldSound SoundFX("Chime100",DOFChimes), 0, soundPoint13, 1
			Else 
				DOF 443,DOFPulse
			End If
			Exit Sub
		End If

		If Points > 999 Then  
			totalUp(points)
			If chime = 0 Then
				playFieldSound SoundFX("Chime100",DOFChimes), 0, soundPoint13, 1
			Else 
				DOF 443,DOFPulse
			End If
		End If

	End If
End Sub

Dim replayX,  replay(7), repAwarded(5)
Sub totalUp(points)
	If B2SOn and showDT = False Then
		If Player = 1 Then PlayReelSound "Reel1", bgs1Reel
		If Player = 2 Then PlayReelSound "Reel2", bgs2Reel
		If Player = 3 Then PlayReelSound "Reel3", bgs3Reel
		If Player = 4 Then PlayReelSound "Reel4", bgs4Reel
	End If
	
	If showDT = True Then
		If Player = 1 Then PlayReelSound "Reel1", dts1Reel
		If Player = 2 Then PlayReelSound "Reel2", dts2Reel
		If Player = 3 Then PlayReelSound "Reel3", dts3Reel
		If Player = 4 Then PlayReelSound "Reel4", dts4Reel
	End If

	If bonusFlag = 1 Then
		If chime = 0 Then
			playFieldSound  SoundFX("chime1000",DOFChimes), 0, SoundPoint13, 1
		Else 
			DOF 443,DOFPulse
		End If
	End If

	score(Player) = score(player) + points
	sReel(Player).addvalue(points)


	If B2SOn Then controller.B2SSetScorePlayer player, score(player)
		
	If score(player) > replay(rep(player)) or score(player) = replay(rep(player)) Then
		If rep(player) > 3 Then Exit Sub
		AddCredit = 1
		ScoreMotor5.enabled = 1
		rep(player) = rep(player) + 1
		playsound SoundFXDOF("knocker",428,DOFPulse,DOFKnocker)
        DOF 429, DOFPulse 
	End If
End Sub


'***************Tilt
Dim tiltSens, tiltPenalty 
'**** Set tiltPenalty; 0 = loose current ball / 1 = end game
tiltPenalty = 0

Sub checkTilt
	If tilttimer.enabled = True Then 
		tiltSens = tiltSens + 1
		If tiltSens = 3 Then
		tilt = True
		PlaySound "SFX_MusicOver",0,1,0,0,0,1,0,0
		TiltReel.setValue(1)
       	If B2SOn Then controller.B2SSetTilt 33,1
       	If B2SOn Then controller.B2SSetdata 1, 0
		turnOff
	 End If
	Else
	 tiltSens = 0
	 tilttimer.enabled = True
	End If
End Sub

Sub tilttimer_Timer()
	tilttimer.enabled = False
End Sub

'***************Match
Sub match
'	matchNumber = ((int(rnd(1)*10)) * 10) + 10
	If matchNumber = 0 Then matchNumber = 10
	MatchReel.setValue(matchNumber) 'need to change if 1 point table

	If B2SOn Then controller.B2SSetMatch 34, matchNumber 

	For i = 1 to players
		If (matchNumber * 1) = (score(i) mod 10) Then 'need to set this to match 10's or 100's
			addCredit = 1
			scoreMotor5.enabled = 1
			playsound SoundFXDOF("Knocker",428,DOFPulse,DOFKnocker)
            DOF 429, DOFPulse
	    End If
    Next
End Sub

'************Music Playing Routines
Dim songTrack(10), songPlace, songNumber
Sub generateSongNumber
    songNumber = INT(10 * RND(1) )
	checkSongNumber
End Sub

Sub checkSongNumber
	For x = 0 to 9
		If songTrack(x) = songNumber Then 
			generateSongNumber
			Exit Sub
		End If
	Next
	songPlace = songPlace + 1
	songTrack(songPlace) = songNumber
'	tb.text = "sn1 = " & songTrack(1) & " sn2 = " & songTrack(2) & " sn3 = " & songTrack(3) & " sn4 = " & songTrack(4) & " sn5 = " & songTrack(5) & " sn6 = " & songTrack(6) & " sn7 = " & songTrack(7) & " sn8 = " & songTrack(8) & " sn9 = " & songTrack(9)
	songPlayer(songNumber)
	If songPlace = 9 Then 
		songPlace = 0
		For x = 0 to 9
			songTrack(x) = ""
		Next
	End If
End Sub


Sub songPlayer(track)
    Select Case track
		Case 0: PlayMusic "OBWAT/OBWAT2.mp3", MusicVolume: songLength = 25144: songTimer.enabled = 1
		Case 1: PlayMusic "OBWAT/OBWAT3.mp3", MusicVolume: songLength = 21233: songTimer.enabled = 1
		Case 2: PlayMusic "OBWAT/OBWAT4.mp3", MusicVolume: songLength = 20625: songTimer.enabled = 1
		Case 3: PlayMusic "OBWAT/OBWAT5.mp3", MusicVolume: songLength = 23315: songTimer.enabled = 1
		Case 4: PlayMusic "OBWAT/OBWAT6.mp3", MusicVolume: songLength = 13562: songTimer.enabled = 1
		Case 5: PlayMusic "OBWAT/OBWAT7.mp3", MusicVolume: songLength = 15272: songTimer.enabled = 1
		Case 6: PlayMusic "OBWAT/OBWAT8.mp3", MusicVolume: songLength = 13603: songTimer.enabled = 1
		Case 7: PlayMusic "OBWAT/OBWAT9.mp3", MusicVolume: songLength = 25781: songTimer.enabled = 1
		Case 8: PlayMusic "OBWAT/OBWAT11.mp3", MusicVolume: songLength = 9281: songTimer.enabled = 1
		Case 9: PlayMusic "OBWAT/OBWAT13.mp3", MusicVolume: songLength = 18389: songTimer.enabled = 1
    End Select
End Sub

Dim songLength, songLoop
Sub songTimer_Timer()
	songLength = songLength - 1
	TB1.text = songLength
	If songLength = 0 Then
		songTimer.enabled = 0
		generateSongNumber
	End If
End  Sub


'************Reset Reels

'This Sub looks at each individual digit in each players score and sets them in an array RScore.  If the value is >0 and <9
'then the players score is increased by one times the position value of that digit (ie 1 * 1000 for the 1000's digit)
'If the value of the digit is 9 then it subtracts 9 times the postion value of that digit (ie 9*100 for the 100's digit)
'so that the score is not rolled over and the next digit in line gets incremented as well (ie 9 in the 10's positon gets 
'incremented so the 100's position rolls up by one as well since 90 -> 100).  Lastly the RScore array values get incremented
'by one to get ready for the next pass.

Dim rScore(4,5), resetLoop, test, playerTest, resetFlag, reelFlag, reelStop, reelDone(4), SJWreelDone(4)
Sub countUp	
	For playerTest = 1 to 4
		For test = 0 to 4
			rScore(playerTest,test) = Int(score(playerTest)/10^test) mod 10
		Next
	Next 

	For playerTest = 1 to 4
		For x = 0 to 4
			If rscore(playerTest, x) > 0 And rscore(playerTest, x) < 9 Then score(playerTest) = score(playerTest) + 10^x
			If rScore(playerTest, x) = 9 Then score(playerTest) = score(playerTest) - (9 * 10^x)	
			If rScore(playerTest, x) > 0 Then rScore(playerTest, x) = rScore(playerTest, x) + 1
			If rScore(playerTest, x) = 10 Then rScore(playerTest, x) = 0
		Next 
	Next

	If score(1) = 0 and score(2) = 0 and score(3) = 0 and score(4) = 0  Then 
		reelFlag = 1
		For i = 1 to maxPlayers
			score(i) = 0
			rep(i) = 0
			repAwarded(i) = 0
		Next
	End If
End Sub

'This Sub sets each B2S reel or Desdktop reels to their new values and then plays the score motor sound each time and the
'reel sounds only if the reels are being stepped

Sub updateReels
	For playerTest = 1 to 4
		If B2SOn and showDT = False and reelDone(playerTest) = 0 and reelStop = 0 Then
			controller.B2SSetScorePlayer playerTest, score(playerTest)
			If reelDone(1) = 0 and playerTest = 1 Then PlayReelSound "Reel1", bgs1Reel
			If reelDone(2) = 0 and playerTest = 2 Then PlayReelSound "Reel2", bgs2Reel
			If reelDone(3) = 0 and playerTest = 3 Then PlayReelSound "Reel3", bgs3Reel
			If reelDone(4) = 0 and playerTest = 4 Then PlayReelSound "Reel4", bgs4Reel
			If score(playerTest) = 0 Then reelDone(playerTest) = 1
		End If

		If showDT = True and reelDone(playerTest) = 0 and reelStop = 0 Then 
			sReel(playerTest).setvalue (score(playerTest))
			If reelDone(1) = 0 and playerTest = 1 Then playReelSound "Reel1", dts1Reel
			If reelDone(2) = 0 and playerTest = 2 Then playReelSound "Reel2", dts2Reel
			If reelDone(3) = 0 and playerTest = 3 Then playReelsound "Reel3", dts3Reel
			If reelDone(4) = 0 and playerTest = 4 Then playReelSound "Reel4", dts4Reel
			If score(playerTest) = 0 Then reelDone(playerTest) = 1
		End If

	Next
	playfieldSound "scoreMotorSingleFire", 0, soundPointScoreMotor, 0.2
	If reelStop = 0 Then playsound "reel"
	If reelFlag = 1 Then reelStop = 1

End Sub

'This Timer runs a loop that calls the CountUp and UpdateReels routines to step the reels up five times and Then
'check to see if they are all at zero during a two loop pause and then step them the rest of the way to zero

Dim testFlag
Sub resetReel_Timer
	For x = 1 to 4
		score(x) = (score(x) Mod 100000)
	Next
	resetLoop = resetLoop + 1
	If resetLoop = 1 and score(1) = 0 and score(2) = 0 and score(3) = 0 and score(4) = 0 Then
		resetLoop = 0
		If testFlag = 0 Then releaseBall
		testFlag = 0
		resetReel.enabled = 0
		Exit Sub
	End If
	Select Case resetLoop
		Case 1: countUp: updateReels
		Case 2: countUp: updateReels
		Case 3: countUp: updateReels
		Case 4: countUp: updateReels
		Case 5: countUp: updateReels
		Case 6: If reelStop = 1 Then 
					resetLoop = 0
					reelFlag = 0
					reelStop = 0
					If testFlag = 0 Then releaseBall
					testFlag = 0
					resetReel.enabled = 0
					Exit Sub
				End If

		Case 7:
		Case 8: countUp: updateReels
		Case 9: countUp: updateReels
		Case 10: countUp: updateReels
		Case 11: countUp: updateReels
		Case 12: countUp: updateReels: 
			resetLoop = 0
			reelFlag = 0
			reelStop = 0
			If testFlag = 0 Then releaseBall
			testFlag = 0
			resetReel.enabled = 0
			Exit Sub 	
	End Select
End Sub

'************************************************Post It Note Section**************************************************************************
'***************Static Post It Note Update
Dim  hsY, shift, scoreMil, score100K, score10K, scoreK, score100, score10, scoreUnit
Dim hsInitial0, hsInitial1, hsInitial2
Dim hsArray: hsArray = Array("HS_0","HS_1","HS_2","HS_3","HS_4","HS_5","HS_6","HS_7","HS_8","HS_9","HS_Space","HS_Comma")
Dim hsiArray: hsIArray = Array("HSi_0","HSi_1","HSi_2","HSi_3","HSi_4","HSi_5","HSi_6","HSi_7","HSi_8","HSi_9","HSi_10","HSi_11","HSi_12","HSi_13","HSi_14","HSi_15","HSi_16","HSi_17","HSi_18","HSi_19","HSi_20","HSi_21","HSi_22","HSi_23","HSi_24","HSi_25","HSi_26")

Sub updatePostIt
	scoreMil = Int(highScore(0)/1000000)
	score100K = Int( (highScore(0) - (scoreMil*1000000) ) / 100000)
	score10K = Int( (highScore(0) - (scoreMil*1000000) - (score100K*100000) ) / 10000)														
	scoreK = Int( (highScore(0) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) ) / 1000)										
	score100 = Int( (highScore(0) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) ) / 100)						
	score10 = Int( (highScore(0) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) - (score100*100) ) / 10)			
	scoreUnit = (highScore(0) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) - (score100*100) - (score10*10) ) 

	pScore6.image = hsArray(scoreMil):If highScore(0) < 1000000 Then pScore6.image = hsArray(10)
	pScore5.image = hsArray(score100K):If highScore(0) < 100000 Then pScore5.image = hsArray(10)
	pScore4.image = hsArray(score10K):If highScore(0) < 10000 Then pScore4.image = hsArray(10)
	pScore3.image = hsArray(scoreK):If highScore(0) < 1000 Then pScore3.image = hsArray(10)
	pScore2.image = hsArray(score100):If highScore(0) < 100 Then pScore2.image = hsArray(10)
	pScore1.image = hsArray(score10):If highScore(0) < 10 Then pScore1.image = hsArray(10)
	pScore0.image = hsArray(scoreUnit):If highScore(0) < 1 Then pScore0.image = hsArray(10)
	If highScore(0) < 1000 Then
		PComma.image = hsArray(10)
	Else
		pComma.image = hsArray(11)
	End If
	If highScore(0) < 1000000 Then
		pComma1.image = hsArray(10)
	Else
		pComma1.image = hsArray(11)
	End If
	If highScore(0) > 999999 Then shift = 0 :pComma.transx = 0
	If highScore(0) < 1000000 Then shift = 1:pComma.transx = -10
	If highScore(0) < 100000 Then shift = 2:pComma.transx = -20
	If highScore(0) < 10000 Then shift = 3:pComma.transx = -30
	For hsY = 0 to 6
		EVAL("Pscore" & hsY).transx = (-10 * shift)
	Next
	initial1.image = hsIArray(initial(0,1))
	initial2.image = hsIArray(initial(0,2))
	initial3.image = hsIArray(initial(0,3))
End Sub

'***************Show Current Score
Sub showScore
	scoreMil = Int(highScore(activeScore(flag))/1000000)
	score100K = Int( (highScore(activeScore(flag)) - (scoreMil*1000000) ) / 100000)
	score10K = Int( (highScore(activeScore(flag)) - (scoreMil*1000000) - (score100K*100000) ) / 10000)														
	scoreK = Int( (highScore(activeScore(flag)) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) ) / 1000)										
	score100 = Int( (highScore(activeScore(flag)) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) ) / 100)						
	score10 = Int( (highScore(activeScore(flag)) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) - (score100*100) ) / 10)			
	scoreUnit = (highScore(activeScore(flag)) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) - (score100*100) - (score10*10) ) 

	pScore6.image = hsArray(scoreMil):If highScore(activeScore(flag)) < 1000000 Then pScore6.image = hsArray(10)
	pScore5.image = hsArray(score100K):If highScore(activeScore(flag)) < 100000 Then pScore5.image = hsArray(10)
	pScore4.image = hsArray(score10K):If highScore(activeScore(flag)) < 10000 Then pScore4.image = hsArray(10)
	pScore3.image = hsArray(scoreK):If highScore(activeScore(flag)) < 1000 Then pScore3.image = hsArray(10)
	pScore2.image = hsArray(score100):If highScore(activeScore(flag)) < 100 Then pScore2.image = hsArray(10)
	pScore1.image = hsArray(score10):If highScore(activeScore(flag)) < 10 Then pScore1.image = hsArray(10)
	pScore0.image = hsArray(scoreUnit):If highScore(activeScore(flag)) < 1 Then pScore0.image = hsArray(10)
	If highScore(activeScore(flag)) < 1000 Then
		pComma.image = hsArray(10)
	Else
		pComma.image = hsArray(11)
	End If
	If highScore(activeScore(flag)) < 1000000 Then
		pComma1.image = hsArray(10)
	Else
		pComma1.image = hsArray(11)
	End If
	If highScore(flag) > 999999 Then shift = 0 :pComma.transx = 0
	If highScore(activeScore(flag)) < 1000000 Then shift = 1:pComma.transx = -10
	If highScore(activeScore(flag)) < 100000 Then shift = 2:pComma.transx = -20
	If highScore(activeScore(flag)) < 10000 Then shift = 3:pComma.transx = -30
	For HSy = 0 to 6
		EVAL("Pscore" & hsY).transx = (-10 * shift)
	Next
	initial1.image = hsIArray(initial(activeScore(flag),1))
	initial2.image = hsIArray(initial(activeScore(flag),2))
	initial3.image = hsIArray(initial(activeScore(flag),3))
End Sub

'***************Dynamic Post It Note Update
Dim scoreUpdate, dHSx
Sub dynamicUpdatePostIt_Timer
	scoreMil = Int(highScore(scoreUpdate)/1000000)
	score100K = Int( (highScore(ScoreUpdate) - (scoreMil*1000000) ) / 100000)
	score10K = Int( (highScore(scoreUpdate) - (ScoreMil*1000000) - (Score100K*100000) ) / 10000)														
	scoreK = Int( (highScore(scoreUpdate) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) ) / 1000)										
	score100 = Int( (highScore(ScoreUpdate) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) ) / 100)						
	score10 = Int( (highScore(ScoreUpdate) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) - (score100*100) ) / 10)			
	scoreUnit = (highScore(ScoreUpdate) - (scoreMil*1000000) - (score100K*100000) - (score10K*10000) - (scoreK*1000) - (score100*100) - (score10*10) ) 

	pScore6.image = hsArray(ScoreMil):If highScore(scoreUpdate) < 1000000 Then pScore6.image = hsArray(10)
	pScore5.image = hsArray(Score100K):If highScore(scoreUpdate) < 100000 Then pScore5.image = hsArray(10)
	pScore4.image = hsArray(Score10K):If highScore(scoreUpdate) < 10000 Then pScore4.image = hsArray(10)
	pScore3.image = hsArray(ScoreK):If highScore(scoreUpdate) < 1000 Then pScore3.image = hsArray(10)
	pScore2.image = hsArray(Score100):If highScore(scoreUpdate) < 100 Then pScore2.image = hsArray(10)
	pScore1.image = hsArray(Score10):If highScore(scoreUpdate) < 10 Then pScore1.image = hsArray(10)
	pScore0.image = hsArray(ScoreUnit):If highScore(scoreUpdate) < 1 Then pScore0.image = hsArray(10)
	If highScore(scoreUpdate) < 1000 Then
		pComma.image = hsArray(10)
	Else
		pComma.image = hsArray(11)
	End If
	If highScore(scoreUpdate) < 1000000 Then
		pComma1.image = hsArray(10)
	Else
		pComma1.image = hsArray(11)
	End If
	If highScore(scoreUpdate) > 999999 Then shift = 0 :pComma.transx = 0
	If highScore(scoreUpdate) < 1000000 Then shift = 1:pComma.transx = -10
	If highScore(scoreUpdate) < 100000 Then shift = 2:pComma.transx = -20
	If highScore(scoreUpdate) < 10000 Then shift = 3:pComma.transx = -30
	For dHSx = 0 to 6
		EVAL("Pscore" & dHSx).transx = (-10 * shift)
	Next
	initial1.image = hsIArray(initial(scoreUpdate,1))
	initial2.image = hsIArray(initial(scoreUpdate,2))
	initial3.image = hsIArray(initial(scoreUpdate,3))
	scoreUpdate = scoreUpdate + 1
	If scoreUpdate = 5 then scoreUpdate = 0
End Sub

'***************Bubble Sort
Dim tempScore(2), tempPos(3), position(5)
Dim bSx, bSy
'Scores are sorted high to low with Position being the player's number
Sub sortScores
	For bSx = 1 to 4
		position(bSx) = bSx
	Next
	For bSx = 1 to 4
		For bSy = 1 to 3
			If score(bSy) < score(bSy+1) Then	
				tempScore(1) = score(bSy+1)
				tempPos(1) = position(bSy+1)
				score(bSy+1) = score(bSy)
				score(bSy) = tempScore(1)
				position(bSy+1) = position(BSy)
				position(bSy) = tempPos(1)
			End If
		Next
	Next
End Sub

'*************Check for High Scores

Dim highScore(5), activeScore(5), hs, chX, chY, chZ, chIX, tempI(4), tempI2(4), flag, hsI, hsX
'goes through the 5 high scores one at a time and compares them to the player's scores high to 
'if a player's score is higher it marks that postion with ActiveScore(x) and moves all of the other
'	high scores down by one along with the high score's player initials
'	also clears the new high score's initials for entry later
Sub checkHighScores
	flag = 0
	For hs = 1 to maxPlayers     							'look at all 5 saved high scores
		For chY = 0 to 4   					    	'look at 4 player scores
			If score(hs) > highScore(chY) Then
				flag = flag + 1						'flag to show how many high scores needs replacing 
				tempScore(1) = highScore(chY)
				highScore(chY) = score(hs)
				activeScore(hs) = chY				'ActiveScore(x) is the high score being modified with x=1 the largest and x=4 the smallest
				For chIX = 1 to 3					'set initals to blank and make temporary initials = to intials being modifed so they can move down one high score
					tempI(chIX) = initial(chY,chIX)
					initial(chY,chIX) = 0
				Next
				
				If chY < 4 Then						'check if not on lowest high score for overflow error prevention
					For chZ = chY+1 to 4			'set as high score one more than score being modifed (CHy+1)
						tempScore(2) = highScore(chZ)	'set a temporaray high score for the high score one higher than the one being modified 
						highScore(chZ) = tempScore(1)	'set this score to the one being moved
						tempScore(1) = tempScore(2)		'reassign TempScore(1) to the next higher high score for the next go around
						For chIX = 1 to 3
							tempI2(chIX) = initial(chZ,chIX)	'make a new set of temporary initials
						Next
						For chIX = 1 to 3
							initial(chZ,chIX) = tempI(chIX)		'set the initials to the set being moved
							tempI(chIX) = tempI2(chIX)			'reassign the initials for the next go around
						Next
					Next
				End If
				chY = 4								'if this loop was accessed set CHy to 4 to get out of the loop
			End If
		Next
	Next
'	Goto Initial Entry
		hsI = 1			'go to the first initial for entry
		hsX = 1			'make the displayed inital be "A"
		If flag > 0 Then	'Flag 0 when all scores are updated so leave subroutine and reset variables
			showScore
			PlayerEntry.visible = 1
			PlayerEntry.image = "Player" & position(Flag)
			initial(activeScore(flag),1) = 1	'make first inital "A"
			For chY = 2 to 3
				initial(activeScore(flag),chY) = 0	'set other two to " "
			Next
			For chY = 1 to 3
				EVAL("Initial" & chY).image = hsIArray(initial(activeScore(flag),chY))		'display the initals on the tape
			Next
			initialTimer1.enabled = 1		'flash the first initial
			dynamicUpdatePostIt.enabled = 0		'stop the scrolling intials timer
			stopSound("SFX_RadioShow")
			if activePhase < 14 Then playsound "SFX_Paddling"
			'DOF 428, DOFPulse
			enableInitialEntry = True
		End If
End Sub

'************Enter Initials Keycode Subroutine
Dim initial(6,5), initialsDone
Sub enterIntitals(keycode)
		If keyCode = leftFlipperKey Then 
			hsX = hsX - 1						'HSx is the inital to be displayed A-Z plus " "
			If hsX < 0 Then hsX = 26
			If hsI < 4 Then EVAL("Initial" & hsI).image = hsIArray(hsX)		'HSi is which of the three intials is being modified
			playSound "metalhit_thin"
		End If
		If keycode = RightFlipperKey Then
			hsX = hsX + 1
			If hsX > 26 Then hsX = 0
			If hsI < 4 Then EVAL("Initial"& hsI).image = hsIArray(hsX)
			playSound "metalhit_thin"
		End If
		If keycode = startGameKey and initialsDone = 0 Then
			If hsI < 3 Then									'if not on the last initial move on to the next intial
				EVAL("Initial" & hsI).image = hsIArray(hsX)	'display the initial
				initial(activeScore(flag), hsI) = hsX		'save the inital
				playSound "metalhit_medium"
				EVAL("InitialTimer" & hsI).enabled = 0		'turn that inital's timer off
				EVAL("Initial" & hsI).visible = 1			'make the initial not flash but be turn on
				initial(activeScore(flag),hsI + 1) = hsX	'move to the next initial and make it the same as the last initial
				EVAL("Initial" & hsI +1).image = hsIArray(hsX)	'display this intial
'				y = 1
				EVAL("InitialTimer" & hsI + 1).enabled = 1	'make the new intial flash
				hsI = hsI + 1								'increment HSi
			Else										'if on the last initial then get ready yo exit the subroutine 
				initial3.visible = 1					'make the intial visible
				playSound "metalhit_medium"
				initialTimer3.enabled = 0				'shut off the flashing
				initial(activeScore(flag),3) = hsX		'set last initial
				initialEntry							'exit subroutine
			End If
		End If
End Sub

'************Update Initials and see if more scores need to be updated
Dim eIX
Sub initialEntry
	playsound SoundFXDOF("Chime10",441,DOFPulse,DOFChimes)
	flag = flag - 1
'	TextBox2.text = Flag
	hsI = 1
	If flag < 0 Then flag = 0: Exit Sub
	If flag = 0 Then 					'exit high score entry mode and reset variables
		initialsDone = 1				'prevents changes in intials while the highScoreDelay timer waits to finish
		players = 0
		For eIX = 1 to 4
			activeScore(eIX) = 0
			position(eIX) = 0
		Next
		For eIX = 1 to 3
			EVAL("InitialTimer" & eIX).enabled = 0
		Next
		playerEntry.visible = 0
		scoreUpdate = 0						'go to the highest score
		updatePostIt						'display that score
		highScoreDelay.enabled = 1
	Else
		showScore
		playerEntry.image = "Player" & position(flag)
'		TextBox3.text = ActiveScore(Flag) 	'tells which high score is being entered
'		TextBox2.text = Flag
'		TextBox1.text =  Position(Flag) 	'tells which player is entering values
		initial(activeScore(flag),1) = 1	'set the first initial to "A"
		For chY = 2 to 3
			initial(activeScore(flag),chY) = 0	'set the other two to " "
		Next
		For chY = 1 to 3
			EVAL("Initial" & chY).image = hsIArray(initial(activeScore(flag),chY))	'display the intials
		Next
		hsX = 1							'go to the letter "A"
		initialTimer1.enabled = 1		'flash the first intial
	End If
End Sub

'************Delay to prevent start button push for last initial from starting game Update
Sub highScoreDelay_timer
	highScoreDelay.enabled = 0
	enableInitialEntry = False
	initialsDone = 0
	saveHighScore
	For eIX = 1 to 3
		EVAL("InitialTimer" & eIX).enabled = 0
	Next
	dynamicUpdatePostIt.enabled = 1		'turn scrolling high score back on
End Sub

'************Flash Initials Timers
Sub initialTimer1_Timer
	y = y + 1
	If y > 1 Then y = 0
	If y = 0 Then 
		initial1.visible = 1
	Else
		initial1.visible = 0	
	End If
End Sub

Sub initialTimer2_Timer
	y = y + 1
	If y > 1 Then y = 0
	If y = 0 Then 
		initial2.visible = 1
	Else
		initial2.visible = 0	
	End If
End Sub

Sub initialTimer3_Timer
	y = y + 1
	If y > 1 Then y = 0
	If y = 0 Then 
		initial3.visible = 1
	Else
		initial3.visible = 0	
	End If
End Sub

'*************Load Scores
Sub loadHighScore
	Dim fileObj
	Dim scoreFile
	Dim temp(40)
	Dim textStr

	dim hiInitTemp(3)
	dim hiInit(5)

    Set fileObj = CreateObject("Scripting.FileSystemObject")
	If Not fileObj.FolderExists(UserDirectory) Then 
		Exit Sub
	End If
	If Not fileObj.FileExists(UserDirectory & cOptions) Then
		Exit Sub
	End If
	Set scoreFile = fileObj.GetFile(UserDirectory & cOptions)
	Set textStr = scoreFile.OpenAsTextStream(1,0)
		If (textStr.AtEndOfStream = True) Then
			Exit Sub
		End If

		For x = 1 to 33
			temp(x) = textStr.readLine
		Next
		TextStr.Close	
		For x = 0 to 4
			highScore(x) = cdbl (temp(x+1))
		Next
		For x = 0 to 4
			hiInit(x) = (temp(x + 6))
		Next
		i = 10
		For x = 0 to 4
			For y = 1 to 3
				i = i + 1
				initial(x,y) = cdbl (temp(i))
			Next
		Next
		credit = cdbl (temp(26))
		freePlay = cdbl (temp(27))
		balls = cdbl (temp(28))
		matchNumber = cdbl (temp(29))
		chime = cdbl (temp(30))
		pfOption = cdbl (temp(31))
		musicOn = cdbl (temp(32))
		musicVolume = cdbl (temp(33))
		Set scoreFile = Nothing
	    Set fileObj = Nothing
End Sub

'************Save Scores
Sub saveHighScore
Dim hiInit(5)
Dim hiInitTemp(5)
Dim FolderPath
	For x = 0 to 4
		For y = 1 to 3
			hiInitTemp(y) = chr(initial(x,y) + 64)
		Next
		hiInit(x) = hiInitTemp(1) + hiInitTemp(2) + hiInitTemp(3)
	Next
	Dim fileObj
	Dim scoreFile
	Set fileObj = createObject("Scripting.FileSystemObject")
	If Not fileObj.folderExists(userDirectory) Then 
		Exit Sub
	End If
	Set scoreFile = fileObj.createTextFile(userDirectory & cOptions,True)

		For x = 0 to 4
			scoreFile.writeLine highScore(x)
		Next
		For x = 0 to 4
			scoreFile.writeLine hiInit(x)
		Next
		For x = 0 to 4
			For y = 1 to 3
				scoreFile.writeLine initial(x,y)
			Next
		Next
		scoreFile.WriteLine credit
		scorefile.writeline freePlay
		scoreFile.WriteLine balls
		scoreFile.WriteLine matchNumber
		scoreFile.WriteLine chime
		scoreFile.WriteLine pfOption
		scoreFile.WriteLine musicOn
		scoreFile.WriteLine musicVolume
		scoreFile.Close
	Set scoreFile = Nothing
	Set fileObj = Nothing

'This section of code writes a file in the User Folder of VisualPinball that contains the High Score data for PinballY.
'PinballY can read this data and display the high scores on the DMD during game selection mode in PinballY.

	Set FileObj = CreateObject("Scripting.FileSystemObject")

	If cPinballY = 0 Then Exit Sub

	If Not FileObj.FolderExists(UserDirectory) Then 
		Exit Sub
	End If

	FolderPath = FileObj.GetParentFolderName(UserDirectory)

	If cPinballY = 1 Then
		Set ScoreFile = FileObj.CreateTextFile(FolderPath & "/Tables/" & hsFileName & ".PinballYHighScores",True)
	Else
		Set ScoreFile = FileObj.CreateTextFile(UserDirectory & hsFileName & ".PinballYHighScores",True)	
	End If

	For x = 0 to 4 
		ScoreFile.WriteLine HighScore(x) 
		ScoreFile.WriteLine HiInit(x)
	Next
	ScoreFile.Close
	Set ScoreFile = Nothing
	Set FileObj = Nothing

End Sub

'************Shut Down and De-Energize on Tilt
Sub turnOff	

  	leftFlipper.RotateToStart
	rightFlipper.RotateToStart
	stopSound "flipBuzzLA"
	stopSound "flipBuzzLB"
	stopSound "flipBuzzLC"
	stopSound "flipBuzzLD"
	stopSound "flipBuzzRA"
	stopSound "flipBuzzRB"
	stopSound "flipBuzzRC"
	stopSound "flipBuzzRD"
	DOF 101, DOFOff
	DOF 102, DOFOff
End Sub 

'*****************************************************Supporting Code Written By Others*************************************



' Roth's drop target routine
'************************************************************************************************
'These are the hit subs for the DT walls that start things off and send the number of the wall hit to the DTHit sub
Sub DW001_Hit : DTHit 01 : DOF 113, DOFPulse : End Sub
Sub DW002_Hit : DTHit 02 : DOF 113, DOFPulse : End Sub
Sub DW003_Hit : DTHit 03 : DOF 113, DOFPulse : End Sub
Sub DW004_Hit : DTHit 04 : DOF 113, DOFPulse : End Sub

' sub to raise all DTs at once

Sub ResetDrops
	DTRaise 1: DTRaise 2: DTRaise 3: DTRaise 4
	For x = 1 to 4
		targetsDown(x) = 0
	Next
End Sub

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110             'in milliseconds
Const DTDropUpSpeed = 40            'in milliseconds
Const DTDropUnits = 44              'VP units primitive drops
Const DTDropUpUnits = 10            'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8                 'max degrees primitive rotates when hit
Const DTDropDelay = 20              'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40             'time in milliseconds before target drops back to normal up position after the solendoid fires to raise the target
Const DTBrickVel = 30               'velocity at which the target will brick, set to '0' to disable brick
Const DTEnableBrick = 1             'Set to 0 to disable bricking, 1 to enable bricking
Const DTDropSound = "DTDrop"        'Drop Target Drop sound
Const DTResetSound = "DTReset"      'Drop Target reset sound
Const DTHitSound = "Target_Hit_1"   'Drop Target Hit sound
Const DTMass = 0.2                  'Mass of the Drop Target (between 0 and 1), higher values provide more resistance

'******************************************************
'                                DROP TARGETS FUNCTIONS
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

'An array of objects for each DT of (primary wall, secondary wall, primitive, switch, animate variable)
Dim DT001, DT002, DT003, DT004
Set DT001 = (new DropTarget)(DW001, DOW001, drop001, 01, 0, false)
Set DT002 = (new DropTarget)(DW002, DOW002, drop002, 02, 0, false)
Set DT003 = (new DropTarget)(DW003, DOW003, drop003, 03, 0, false)
Set DT004 = (new DropTarget)(DW004, DOW004, drop004, 04, 0, false)


'An array of DT arrays
Dim DTArray
DTArray = Array(DT001, DT002, DT003, DT004)

' This function looks over the DTArray and polls the ID the target hit (ie 06) and returns its position in the array (ie 0)
Function DTArrayID(switch)
    Dim i
    For i = 0 to uBound(DTArray) 
		If DTArray(i).sw = switch Then DTArrayID = i: Exit Function 
    Next
End Function' This function looks over the DTArray and pulls the ID the target hit (ie 06))

Sub DTRaise(switch)
    Dim i
    i = DTArrayID(switch)
    DTArray(i).animate = -1 'this sets the last variable in the DT array to -1 from 0 to raise DT
    DoDTAnim
End Sub

Sub DTDrop(switch)
    Dim i
    i = DTArrayID(switch)
    DTArray(i).animate = 1 'this sets the last variable in the DT array to 1 from 0
    DoDTAnim
End Sub

Sub DTHit(switch)
    Dim i
    i = DTArrayID(switch) ' this sets i to be the position of the DT in the array DTArray

    DTArray(i).animate =  DTCheckBrick(Activeball, DTArray(i).prim) ' this sets the animate value (-1 raise, 1&4 drop, 0 do nothing, 3 bend backwards, 2 BRICK

    If DTArray(i).animate = 1 or DTArray(i).animate = 3 or DTArray(i).animate = 4 Then ' if the value from brick checking is not 2 then apply ball physics
	DTBallPhysics Activeball, DTArray(i).prim.rotz, DTMass
    End If
    DoDTAnim
End Sub

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

Sub DTAnim_Timer()  ' 10 ms timer
    DoDTAnim
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
    'debug.print "brick " & perpvel & " : " & paravel & " : " & perpvelafter & " : " & paravelafter

    If perpvel > 0 and perpvelafter <= 0 Then
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

	DTCheckBrick = 1
End Function

'**************************************How this all works********************************************
'This uses walls to register the hits and primitives that get dropped as well as an offsetwall which is a wall that is set behind the main wall.
'1) The wallTarget, offsetWallTarget, primitive, target number and animation value of 0 are put into arrays called DT006 - DT010
'2) A master array called DTArray contains all of the DT0xx arrays
'3) If a wall is hit then the position in the array of the wall hit is determined
'4) The animation value is calculated by DTCheckBrick
'5) If the animation value is 1,3 or 4 then DTBallPhysics applies velocity corrections to the active ball
'6) the animation sub is called and it passes each element of the DT array to the DTAnimate function
'  - if the animate value is 0 do nothing
'  - if the animate value is 1 or 4 then bend the dt back make the front wall not collidable and turn on the offset wall's collide and 
'    when the elapsed drop target delay time has passed change the animate value to 2
'  - if the animate value is two then drop the target and once it is dropped turn off the collide for the offset wall
'  - if the animate value is 3 then BRICK  bend the prim back and the forth but no drop 
'  - if the anumate value is -1 then raise the drop target past its resting point and drop back down and if a ball is over it kick the ball up in the air
'****************************************************************************************************

Sub DoDTAnim()
        Dim i
        For i = 0 to Ubound(DTArray)
	    DTArray(i).animate = DTAnimate(DTArray(i).primary, DTArray(i).secondary, DTArray(i).prim, DTArray(i).sw, DTArray(i).animate)
        Next
End Sub

' This is the function that animates the DT drop and raise
Function DTAnimate(primary, secondary, prim, switch,  animate)
        dim transz
        Dim animtime, rangle
        rangle = prim.rotz * 3.1416 / 180 ' number of radians

        DTAnimate = animate

        if animate = 0  Then  ' no action to be taken
                primary.uservalue = 0  ' primary.uservalue is used to keep track of gameTime
                DTAnimate = 0
                Exit Function
        Elseif primary.uservalue = 0 then 
                primary.uservalue = gametime ' sets primary.uservalue to game time for calculating how much time has elapsed
        end if

        animtime = gametime - primary.uservalue 'variable for elapsed time

        If (animate = 1 or animate = 4) and animtime < DTDropDelay Then 'if the time elapse is less than time for the dt to start to drop after impact
                primary.collidable = 0 'primary wall is not collidable
                If animate = 1 then secondary.collidable = 1 else secondary.collidable = 0 'animate 1 turns on offset wall collide and 4 turns offest collide off
                prim.rotx = DTMaxBend * cos(rangle) ' bend the primitive back the max value
                prim.roty = DTMaxBend * sin(rangle)
                DTAnimate = animate
                Exit Function
        elseif (animate = 1 or animate = 4) and animtime > DTDropDelay Then ' if the drop time has passed then
                primary.collidable = 0 ' primary wall is not collidable
                If animate = 1 then secondary.collidable = 1 else secondary.collidable = 0 'animate 1 turns on offset wall collide and 4 turns offest collide off
                prim.rotx = DTMaxBend * cos(rangle) ' bend the primitive back the max value
                prim.roty = DTMaxBend * sin(rangle)
                animate = 2 '**** sets animate to 2 for dropping the DT
                playFieldSound "DTDrop", 0, prim, 1  
        End If

        if animate = 2 Then ' DT drop time
                transz = (animtime - DTDropDelay)/DTDropSpeed *  DTDropUnits * -1
                if prim.transz > -DTDropUnits  Then ' if not fully dropped then transz
                        prim.transz = transz
                end if

                prim.rotx = DTMaxBend * cos(rangle)/2
                prim.roty = DTMaxBend * sin(rangle)/2

                if prim.transz <= -DTDropUnits Then  ' if fully dropped then 
                        prim.transz = -DTDropUnits
                        secondary.collidable = 0 ' turn off collide for secondary wall now the rubber behind can be hit
                        'controller.Switch(Switch) = 1
                        primary.uservalue = 0 ' reset the time keeping value
                        DTAnimate = 0 ' turn off animation
                        Exit Function
                Else
                        DTAnimate = 2
                        Exit Function
                end If 
        End If

		'*** animate 3 is a brick!
        If animate = 3 and animtime < DTDropDelay Then ' if elapsed time is less than the drop time
                primary.collidable = 0 'turn off primary collide
                secondary.collidable = 1 'turn on secondary collide
                prim.rotx = DTMaxBend * cos(rangle) 'rotate back
                prim.roty = DTMaxBend * sin(rangle)
        elseif animate = 3 and animtime > DTDropDelay Then
                primary.collidable = 1 'turn on the primary collide
                secondary.collidable = 0 'turn off secondary collide
                prim.rotx = 0 'rotate back to start
                prim.roty = 0
                primary.uservalue = 0
                DTAnimate = 0
                Exit Function
        End If

        if animate = -1 Then ' If the value is -1 raise the DT past its resting point
                transz = (1 - (animtime)/DTDropUpSpeed) *  DTDropUnits * -1
                If prim.transz = -DTDropUnits Then
                        Dim BOT, b
                        BOT = GetBalls

                        For b = 0 to UBound(BOT) ' if a ball is over a DT that is rising, pop it up in the air with a vel of 20
                                If InRect(BOT(b).x,BOT(b).y,prim.x-25,prim.y-10,prim.x+25, prim.y-10,prim.x+25,prim.y+25,prim.x -25,prim.y+25) Then
                                        BOT(b).velz = 20
                                End If
                        Next
                End If

                if prim.transz < 0 Then
                        prim.transz = transz
                elseif transz > 0 then
                        prim.transz = transz
                end if

                if prim.transz > DTDropUpUnits then 'If the dt is at the top of its rise
                        DTAnimate = -2  ' set the dt animate to -2
                        prim.rotx = 0  'remove the rotation
                        prim.roty = 0
                        primary.uservalue = gametime
                end if
                primary.collidable = 0
                secondary.collidable = 1
                'controller.Switch(Switch) = 0

        End If

        if animate = -2 and animtime > DTRaiseDelay Then ' if the value is -2 then drop back down to resting height
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
'                DROP TARGET
'                SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for drop targets
Function Atn2(dy, dx)
        dim pi
        pi = 4*Atn(1)

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

' Used for drop targets
Function Distance(ax,ay,bx,by)
        Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function









'*****************************************
'			BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

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

'This table has a ball shadow that projects through the shooter lane wall and needs to be turned off
Sub ballshadowOff_hit
	BallShadow(0).visible = 0
	BallShadowUpdate.enabled = 0
End Sub

Sub ballShadowOff_unhit
	BallShadowUpdate.enabled = 1
End Sub

'************************************************************************
'                         Ball Control - 3 Axis
'************************************************************************

Dim Cup, Cdown, Cleft, Cright, Zup, contball, contballinplay, ControlBall, bcboost
Dim bcvel, bcyveloffset, bcboostmulti
 
bcboost = 1 'Do Not Change - default setting
bcvel = 4 'Controls the speed of the ball movement
bcyveloffset = -.014 'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
bcboostmulti = 3 'Boost multiplier to ball veloctiy (toggled with the B key)

Sub BallControl_Timer()
    If Contball and ContBallInPlay then
        If Cright = 1 Then
            ControlBall.velx = bcvel * bcboost
          ElseIf Cleft = 1 Then
            ControlBall.velx = -bcvel * bcboost
          Else
            ControlBall.velx = 0
        End If
        If Cup = 1 Then
            ControlBall.vely = -bcvel * bcboost
          ElseIf Cdown = 1 Then
            ControlBall.vely = bcvel * bcboost
          Else
            ControlBall.vely = bcyveloffset
        End If
        If Zup = 1 Then
            ControlBall.velz = bcvel * bcboost
		Else
			ControlBall.velz = -bcvel * bcboost
        End If
    End If
End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function AudioPan(TableObj)	'Calculates the pan for a TableObj based on the X position on the table. "table1" is the name of the table.  New AudioPan algorithm for accurate stereo pan positioning.
    Dim tmp
    If PFOption=1 Then tmp = TableObj.x * 2 / table1.width-1
	If PFOption=2 Then tmp = TableObj.y * 2 / table1.height-1
	If tmp < 0 Then
		AudioPan = -((0.8745898957*(ABS(tmp)^12.78313661)) + (0.1264569796*(ABS(tmp)^1.000771219)))
	Else
		AudioPan = (0.8745898957*(ABS(tmp)^12.78313661)) + (0.1264569796*(ABS(tmp)^1.000771219))
	End If
End Function

Function xGain(TableObj)
'xGain algorithm calculates a PlaySound Volume parameter multiplier to provide a Constant Power "pan".
'PFOption=1:  xGain = 1 at PF Left, xGain = 0.32931 (-3dB for PlaySound's volume parameter) at PF Center and xGain = 1 at PF Right.  Used for Left & Right stereo PF Speakers.
'PFOption=2:  xGain = 1 at PF Top, xGain = 0.32931 (-3dB for PlaySound's volume parameter) at PF Center and xGain = 1 at PF Bottom.  Used for Top & Bottom stereo PF Speakers.
	Dim tmp, PI
    If PFOption=1 Then tmp = TableObj.x * 2 / table1.width-1
	If PFOption=2 Then tmp = TableObj.y * 2 / table1.height-1
	PI = 4 * ATN(1)
	If tmp < 0 Then
	xGain = 0.3293074856*EXP(-0.9652695455*tmp^3 - 2.452909811*tmp^2 - 2.597701999*tmp)
	Else
	xGain = 0.3293074856*EXP(-0.9652695455*-tmp^3 - 2.452909811*-tmp^2 - 2.597701999*-tmp)
	End If
'	TB1.text = "xGain=" & Round(xGain,4)
End Function

Function XVol(tableobj)
'XVol algorithm calculates a PlaySound Volume parameter multiplier for a tableobj based on its X table position to provide a Constant Power "pan".
'XVol = 1 at PF Left, XVol = 0.32931 (-3dB for PlaySound's volume parameter) at PF Center and XVol = 0 at PF Right
Dim tmpx
	If PFOption = 3 Then
		tmpx = tableobj.x * 2 / table1.width-1
		XVol = 0.3293074856*EXP(-0.9652695455*tmpx^3 - 2.452909811*tmpx^2 - 2.597701999*tmpx)
	End If
'	TB1.text = "xVol=" & Round(xVol,4)
End Function

Function YVol(tableobj)
'YVol algorithm calculates a PlaySound Volume parameter multiplier for a tableobj based on its Y table position to provide a Constant Power "fade".
'YVol = 1 at PF Top, YVol = 0.32931 (-3dB for PlaySound's volume parameter) at PF Center and YVol = 0 at PF Bottom
Dim tmpy
	If PFOption = 3 Then
		tmpy = tableobj.y * 2 / table1.height-1
		YVol = 0.3293074856*EXP(-0.9652695455*tmpy^3 - 2.452909811*tmpy^2 - 2.597701999*tmpy)
	End If
'	TB2.text = "yVol=" & Round(yVol,4)
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 500)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

'******************************************************
'      JP's VP10 Rolling Sounds - Modified by Whirlwind
'******************************************************

'******************************************
' Explanation of the rolling sound routine
'******************************************

' ball rolling sounds are played based on the ball speed and position
' the routine checks first for deleted balls and stops the rolling sound.
' The For loop goes through all the balls on the table and checks for the ball speed and 
' if the ball is on the table (height lower than 30) then then it plays the sound
' otherwise the sound is stopped.

'New algorithms added to make sounds for TopArch Hits, Arch Rolls, ball bounces and glass hits. 
'For stereo, xGain is a Playsound volume multiplier that provides a Constant Power pan.
'For quad, multiple PlaySound commands are launched together that are panned and faded to their maximum extents where PlaySound's PAN and FADE have the least error.
'XVol and YVol are Playsound volume multipliers that provide a Constant Power "pan" and "fade".
'Subtracting XVol or YVol from 1 yields an inverse response.

Const tnob = 5 ' total number of balls

'Change GHT, GHB and PFL values based upon the real pinball table dimensions.  Values are used by the GlassHit code.
Const GHT = 6	'Glass height in inches at top of real playfield
Const GHB = 6	'Glass height in inches at bottom of real playfield
Const PFL = 40	'Real playfield length in inches

ReDim rolling(tnob)
InitRolling

ReDim ArchRolling(tnob)
InitArchRolling

Dim ArchHit
'Sub TopArch_Hit
'	ArchHit = 1
'	ArchTimer.Enabled = True
'End Sub

Sub LowerArch_Hit
	ArchHit = 1
	ArchTimer.Enabled = True
End Sub

Dim archCount
'The ArchHit sound is played and.the ArchTimer is enabled upon the first hit of the arch by the ball 
'ArchTimer is enabled for 15ms after which ArchTimer2 is enabled for 2 seconds
'While ArchTimer2 is enabled it "locks-out" the ArchHit sound from being played again until after 2 seconds have elapsed
Sub ArchTimer_Timer
	archCount = archCount + 1
	If archCount = 1 Then
		archCount = 0
		ArchTimer.enabled = False
		If ArchTimer2.enabled = False Then ArchTimer2.enabled = True
	End If
End Sub

Dim archCount2
Sub ArchTimer2_Timer
	archCount2 = archCount2 + 1
	If archCount2 = 1 Then
		archCount2 = 0
		ArchTimer2.enabled = False
	End If
End Sub

Sub NotOnArch_Hit
	ArchHit = 0
End Sub

Sub NotOnArch2_Hit
	ArchHit = 0
End Sub

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
	Next
End Sub

Sub InitArchRolling
	Dim i
	For i = 0 to tnob
		ArchRolling(i) = False
	Next
End Sub

Sub RollingTimer_Timer()
	Dim BOT, b, paSub
	BOT = GetBalls
	paSub=45000	'Playsound pitch adder for subway/ramp rolling ball sound

'TB.text="BOT(b).Z  " & formatnumber(BOT(b).Z,1)
'TB.text="BOT(b).VelZ  " & formatnumber(BOT(b).VelZ,1)
'TB.text="GLASS  " & formatnumber((BOT(b).Y * ((GHT-GHB)/PFL)) + (GHB*50/1.0625) - (BallSize/2),1)
'TB.text = "ArchTimer.enabled=" & ArchTimer.enabled & "    ArchTimer2.enabled=" & ArchTimer2.enabled & "    ArchHit=" & ArchHit

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
		rolling(b) = False
		StopSound("BallrollingA" & b)
		StopSound("BallrollingB" & b)
		StopSound("BallrollingC" & b)
		StopSound("BallrollingD" & b)
	Next

	' exit the sub if no balls on the table
	If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball
	For b = 0 to UBound(BOT)
'	TB.text = "Ball0.z=" & (BOT(0).z)	'Use to verify ball.z (currently -3) when in Adrian's new saucer primitives.


'	Ball Rolling sounds
'**********************
'	Ball=50 units=1.0625".  One unit = 0.02125"  Ball.z is ball center.
'	A ball in Adrian's saucer has a Z of -3.  Use <-5 for subway sounds.
	If PFOption = 1 or PFOption = 2 Then
		If BallVel(BOT(b)) > 1 AND BOT(b).z > 10 and BOT(b).z <26 Then	'Ball on playfield
			rolling(b) = True
			PlaySound("BallrollingA" & b), -1, Vol(BOT(b)) * 0.2 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		ElseIf BallVel(BOT(b)) > 1 AND BOT(b).z > 26 Then	'Ball on Ramp
			PlaySound("BallrollingA" & b), -1, Vol(BOT(b)) * 0.2 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b))+paSub, 1, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.			
		ElseIf rolling(b) = True Then
			StopSound("BallrollingA" & b)
			rolling(b) = False
		End If
	End If
	
	If PFOption = 3 Then
		If BallVel(BOT(b)) > 1 AND BOT(b).z > 10 and BOT(b).z < 26 Then	'Ball on playfield
			rolling(b) = True
			PlaySound("BallrollingA" & b), -1, Vol(BOT(b)) * 0.2 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Left PF Speaker
			PlaySound("BallrollingB" & b), -1, Vol(BOT(b)) * 0.2 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Right PF Speaker
			PlaySound("BallrollingC" & b), -1, Vol(BOT(b)) * 0.2 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Left PF Speaker
			PlaySound("BallrollingD" & b), -1, Vol(BOT(b)) * 0.2 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Right PF Speaker
		ElseIf BallVel(BOT(b)) > 1 AND BOT(b).z > 26 Then	'Ball on ramp
			PlaySound("BallrollingA" & b), -1, Vol(BOT(b)) * 0.2 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b))+paSub, 1, 0, -1	'Top Left PF Speaker
			PlaySound("BallrollingB" & b), -1, Vol(BOT(b)) * 0.2 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b))+paSub, 1, 0, -1	'Top Right PF Speaker
			PlaySound("BallrollingC" & b), -1, Vol(BOT(b)) * 0.2 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b))+paSub, 1, 0,  1	'Bottom Left PF Speaker
			PlaySound("BallrollingD" & b), -1, Vol(BOT(b)) * 0.2 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b))+paSub, 1, 0,  1	'Bottom Right PF Speaker
		ElseIf rolling(b) = True Then
			StopSound("BallrollingA" & b)		'Top Left PF Speaker
			StopSound("BallrollingB" & b)		'Top Right PF Speaker
			StopSound("BallrollingC" & b)		'Bottom Left PF Speaker
			StopSound("BallrollingD" & b)		'Bottom Right PF Speaker
			rolling(b) = False
		End If
	End If

'	Arch Hit and Arch Rolling sounds
'***********************************
	If PFOption = 1 or PFOption = 2 Then
		If BallVel(BOT(b)) > 1 And ArchHit =1 Then
			If ArchTimer2.enabled = 0 Then
				PlaySound("ArchHit" & b), 0, (BallVel(BOT(b))/32)^5 * xGain(BOT(b)), AudioPan(BOT(b)), 0, (BallVel(BOT(b))/40)^7, 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
			End If	
			ArchRolling(b) = True
			PlaySound("ArchRollA" & b), -1, (BallVel(BOT(b))/40)^5 * xGain(BOT(b)), AudioPan(BOT(b)), 0, (BallVel(BOT(b))/40)^7, 1, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		Else
			If ArchRolling(b) = True Then
			StopSound("ArchRollA" & b)
			ArchRolling(b) = False
			End If
		End If
'	If ArchTimer2.enabled = 0 And ArchHit = 1 Then TB.text = "ArchHit vol=" & round((BallVel(BOT(b))/32)^5 * 1 * xGain(BOT(b)),4)	'Keep below 1.
	End If
	
	If PFOption = 3 Then
		If BallVel(BOT(b)) > 1 And ArchHit =1 Then
			If ArchTimer2.enabled = 0 Then
				PlaySound("ArchHit" & b),   0, (BallVel(BOT(b))/32)^5 *    XVol(BOT(b))  *     YVol(BOT(b)),  -1, 0, (BallVel(BOT(b))/40)^5, 0, 0, -1	'Top Left PF Speaker
				PlaySound("ArchHit" & b),   0, (BallVel(BOT(b))/32)^5 * (1-XVol(BOT(b))) *     YVol(BOT(b)),   1, 0, (BallVel(BOT(b))/40)^5, 0, 0, -1	'Top Right PF Speaker
				PlaySound("ArchHit" & b),   0, (BallVel(BOT(b))/32)^5 *    XVol(BOT(b))  *  (1-YVol(BOT(b))), -1, 0, (BallVel(BOT(b))/40)^5, 0, 0,  1	'Bottom Left PF Speaker
				PlaySound("ArchHit" & b),   0, (BallVel(BOT(b))/32)^5 * (1-XVol(BOT(b))) *  (1-YVol(BOT(b))),  1, 0, (BallVel(BOT(b))/40)^5, 0, 0,  1	'Bottom Right PF Speaker
			End If
			ArchRolling(b) = True
			PlaySound("ArchRollA" & b), -1, (BallVel(BOT(b))/40)^5 *    XVol(BOT(b))  *     YVol(BOT(b)),  -1, 0, (BallVel(BOT(b))/40)^5, 1, 0, -1	'Top Left PF Speaker
			PlaySound("ArchRollB" & b), -1, (BallVel(BOT(b))/40)^5 * (1-XVol(BOT(b))) *     YVol(BOT(b)),   1, 0, (BallVel(BOT(b))/40)^5, 1, 0, -1	'Top Right PF Speaker
			PlaySound("ArchRollC" & b), -1, (BallVel(BOT(b))/40)^5 *    XVol(BOT(b))  *  (1-YVol(BOT(b))), -1, 0, (BallVel(BOT(b))/40)^5, 1, 0,  1	'Bottom Left PF Speaker
			PlaySound("ArchRollD" & b), -1, (BallVel(BOT(b))/40)^5 * (1-XVol(BOT(b))) *  (1-YVol(BOT(b))),  1, 0, (BallVel(BOT(b))/40)^5, 1, 0,  1	'Bottom Right PF Speaker
		Else
			If ArchRolling(b) = True Then
			StopSound("ArchRollA" & b)	'Top Left PF Speaker
			StopSound("ArchRollB" & b)	'Top Right PF Speaker
			StopSound("ArchRollC" & b)	'Bottom Left PF Speaker
			StopSound("ArchRollD" & b)	'Bottom Right PF Speaker
			ArchRolling(b) = False
			End If
		End If
	End If

'	Ball drop sounds
'*******************
'Four intensities of ball bounce sound files ranging from 1 to 4 bounces.  The number of bounces increases as the ball's downward Z velocity increases.
'A BOT(b).VelZ < -2 eliminates nuisance ball bounce sounds.

	If PFOption = 1 or PFOption = 2 Then
		If BOT(b).VelZ > -4 And BOT(b).VelZ < -2 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop1" & b, 0, ABS(BOT(b).VelZ)/600 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		ElseIf BOT(b).VelZ > -8 And BOT(b).VelZ < -4 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop2" & b, 0, ABS(BOT(b).VelZ)/600 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		ElseIf BOT(b).VelZ > -12 And BOT(b).VelZ < -8 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop3" & b, 0, ABS(BOT(b).VelZ)/600 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		ElseIf BOT(b).VelZ < -12 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop4" & b, 0, ABS(BOT(b).VelZ)/600 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		End If
	End If

	If PFOption = 3 Then
		If BOT(b).VelZ > -4 And BOT(b).VelZ < -2 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop1" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Left PF Speaker
			PlaySound "BallDrop1" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Right PF Speaker
			PlaySound "BallDrop1" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Left PF Speaker
			PlaySound "BallDrop1" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Right PF Speaker
		ElseIf BOT(b).VelZ > -8 And BOT(b).VelZ < -4 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop2" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Left PF Speaker
			PlaySound "BallDrop2" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Right PF Speaker
			PlaySound "BallDrop2" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Left PF Speaker
			PlaySound "BallDrop2" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Right PF Speaker
		ElseIf BOT(b).VelZ > -12 And BOT(b).VelZ < -8 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop3" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Left PF Speaker
			PlaySound "BallDrop3" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Right PF Speaker
			PlaySound "BallDrop3" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Left PF Speaker
			PlaySound "BallDrop3" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Right PF Speaker
		ElseIf BOT(b).VelZ < -12 And BOT(b).Z > 24 And BallinPlay => 1 Then
			PlaySound "BallDrop4" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Left PF Speaker
			PlaySound "BallDrop4" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b)), 1, 0, -1	'Top Right PF Speaker
			PlaySound "BallDrop4" & b, 0, ABS(BOT(b).VelZ)/600 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Left PF Speaker
			PlaySound "BallDrop4" & b, 0, ABS(BOT(b).VelZ)/600 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b)), 1, 0,  1	'Bottom Right PF Speaker
		End If
	End If
'TB.text="BOT(b).VelZ  " & round(BOT(b).VelZ,1)

'	Glass hit sounds
'*******************
'	Ball=50 units=1.0625".  Ball.z is ball center.  Balls are physically limited by Top Glass Height.  Max ball.z is 25 units below Top Glass Height.
'	To ensure ball can go high enough to trigger glass hit, make Table Options/Dimensions & Slope/Top Glass Height equal to (GHT*50/1.0625) + 5

	If PFOption = 1 or PFOption = 2 Then
		If BOT(b).Z > (BOT(b).Y * ((GHT-GHB)/PFL)) + (GHB*50/1.0625) - BallSize/2 And BallinPlay => 1 Then
			PlaySound "GlassHit" & b, 0, ABS(BOT(b).VelZ)/30 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		End If
	End If

	If PFOption = 3 Then
		If BOT(b).Z > (BOT(b).Y * ((GHT-GHB)/PFL)) + (GHB*50/1.0625) - Ballsize/2 And BallinPlay => 1 Then
			PlaySound "GlassHit" & b, 0, ABS(BOT(b).VelZ)/30 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b)), 0, 0, -1	'Top Left PF Speaker
			PlaySound "GlassHit" & b, 0, ABS(BOT(b).VelZ)/30 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b)), 0, 0, -1	'Top Right PF Speaker
			PlaySound "GlassHit" & b, 0, ABS(BOT(b).VelZ)/30 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b)), 0, 0,  1	'Bottom Left PF Speaker
			PlaySound "GlassHit" & b, 0, ABS(BOT(b).VelZ)/30 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b)), 0, 0,  1	'Bottom Right PF Speaker
		End If
	End If
	Next
End Sub

'*************Hit Sound Routines
'Eliminated the Hit Subs extra velocity criteria since the PlayFieldSoundAB command already incorporates the ball?s velocity.

Sub aRubberPins_Hit(idx)
	PlayFieldSoundAB "pinhit_low", 0, 1
End Sub

Sub aTargets_Hit(idx)
	PlayFieldSoundAB "target", 0, 1
End Sub

Sub aMetalsThin_Hit(idx)
	PlayFieldSoundAB "metalhit_thin", 0, 1
End Sub

Sub aMetalsMedium_Hit(idx)
	PlayFieldSoundAB "metalhit_medium", 0, 1
End Sub

Sub aMetals2_Hit(idx)
	PlayFieldSoundAB "metalhit2", 0, 1
End Sub

Sub aGates_Hit(idx)
	PlayFieldSoundAB "gate4", 0, 1
End Sub

Sub aRubberBands_Hit(idx)
	If BallinPlay > 0 Then	'Eliminates the thump of Trough Ball Creation balls hitting walls 9 and 14 during table initiation
	PlayFieldSoundAB "rubber2", 0, 1
	End If
End Sub

Sub RubberWheel_hit
	PlayFieldSoundAB "rubber_hit_2", 0, 1
End Sub

Sub aPosts_Hit(idx)
	PlayFieldSoundAB "rubber2", 0, 1
End Sub

Sub aWoods_Hit(idx)
	PlayFieldSoundAB "wood", 0, 1
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlayFieldSoundAB "flip_hit_1", 0, 1
		Case 2 : PlayFieldSoundAB "flip_hit_2", 0, 1
		Case 3 : PlayFieldSoundAB "flip_hit_3", 0, 1
	End Select
End Sub

Sub ApronWall_Hit
	Dim Volume
	If ActiveBall.vely < 0 Then	Volume = abs(ActiveBall.vely) / 1 Else Volume = ActiveBall.vely / 30	'The first bounce is -vely subsequent bounces are +vely
'		TextBox1.text = "Volume = " & Volume
		If ActiveBall.z > 24 Then
			If PFOption = 1 Or PFOption = 2 Then
				PlaySound "ApronHit", 0, Volume * xGain(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
			End If
			If PFOption = 3 Then
				PlaySound "ApronHit", 0, Volume *    XVol(ActiveBall)  *    YVol(ActiveBall),  -1, 0, Pitch(ActiveBall), 0, 0, -1	'Top Left PF Speaker
				PlaySound "ApronHit", 0, Volume * (1-XVol(ActiveBall)) *    YVol(ActiveBall),   1, 0, Pitch(ActiveBall), 0, 0, -1	'Top Right PF Speaker
				PlaySound "ApronHit", 0, Volume *    XVol(ActiveBall)  * (1-YVol(ActiveBall)), -1, 0, Pitch(ActiveBall), 0, 0,  1	'Bottom Left PF Speaker
				PlaySound "ApronHit", 0, Volume * (1-XVol(ActiveBall)) * (1-YVol(ActiveBall)),  1, 0, Pitch(ActiveBall), 0, 0,  1	'Bottom Right PF Speaker
			End If
		End If
End Sub

'**********************
' Ball Collision Sound
'**********************

'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine.

'New algorithm for BallBallCollision
'For stereo, xGain is a Playsound volume multiplier that provides a Constant Power pan.
'For quad, multiple PlaySound commands are launched together that are panned and faded to their maximum extents where PlaySound's PAN and FADE have the least error.
'XVol and YVol are Playsound volume multipliers that provide a Constant Power "pan" and "fade".
'Subtracting XVol or YVol from 1 yields an inverse response.

Sub OnBallBallCollision(ball1, ball2, velocity)
	If PFOption = 1 or PFOption = 2 Then
		PlaySound "BBcollide", 0, (Csng(velocity) ^2 / 2000) * xGain(ball1), AudioPan(ball1), 0, Pitch(ball1), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
	End If
	If PFOption = 3 Then
		PlaySound "BBcollide", 0, (Csng(velocity) ^2 / 2000) *    XVol(ball1)  *    YVol(ball1),  -1, 0, Pitch(ball1), 0, 0, -1	'Top Left Playfield Speaker
		PlaySound "BBcollide", 0, (Csng(velocity) ^2 / 2000) * (1-XVol(ball1)) *    YVol(ball1),   1, 0, Pitch(ball1), 0, 0, -1	'Top Right Playfield Speaker
		PlaySound "BBcollide", 0, (Csng(velocity) ^2 / 2000) *    XVol(ball1)  * (1-YVol(ball1)), -1, 0, Pitch(ball1), 0, 0,  1	'Bottom Left Playfield Speaker
		PlaySound "BBcollide", 0, (Csng(velocity) ^2 / 2000) * (1-XVol(ball1)) * (1-YVol(ball1)),  1, 0, Pitch(ball1), 0, 0,  1	'Bottom Right Playfield Speaker
	End If
End Sub

Sub PlayFieldSound (SoundName, Looper, TableObject, VolMult)
'Plays the sound of a table object at the table object's coordinates.
'For stereo, xGain is a Playsound volume multiplier that provides a Constant Power pan.
'For quad, multiple PlaySound commands are launched together that are panned and faded to their maximum extents where PlaySound's PAN and FADE have the least error.
'XVol and YVol are Playsound volume multipliers that provide a Constant Power "pan" and "fade".
'Subtracting XVol or YVol from 1 yields an inverse response.

	If PFOption = 1 Or PFOption = 2 Then
		If Looper = -1 Then
			PlaySound SoundName&"A", Looper, VolMult * xGain(TableObject), AudioPan(TableObject), 0, 0, 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		End If
		If Looper = 0 Then
			PlaySound SoundName, Looper, VolMult * xGain(TableObject), AudioPan(TableObject), 0, 0, 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
		End If
	End If
	If PFOption = 3 Then
		If Looper = -1 Then
			PlaySound SoundName&"A", Looper, VolMult *    XVol(TableObject)  *    YVol(TableObject),  -1, 0, 0, 0, 0, -1	'Top Left PF Speaker
			PlaySound SoundName&"B", Looper, VolMult * (1-XVol(TableObject)) *    YVol(TableObject),   1, 0, 0, 0, 0, -1	'Top Right PF Speaker
			PlaySound SoundName&"C", Looper, VolMult *    XVol(TableObject)  * (1-YVol(TableObject)), -1, 0, 0, 0, 0,  1	'Bottom Left PF Speaker
			PlaySound SoundName&"D", Looper, VolMult * (1-XVol(TableObject)) * (1-YVol(TableObject)),  1, 0, 0, 0, 0,  1	'Bottom Right PF Speaker
		End If
		If Looper = 0 Then
			PlaySound SoundName, Looper, VolMult *    XVol(TableObject)  *    YVol(TableObject),  -1, 0, 0, 0, 0, -1	'Top Left PF Speaker
			PlaySound SoundName, Looper, VolMult * (1-XVol(TableObject)) *    YVol(TableObject),   1, 0, 0, 0, 0, -1	'Top Right PF Speaker
			PlaySound SoundName, Looper, VolMult *    XVol(TableObject)  * (1-YVol(TableObject)), -1, 0, 0, 0, 0,  1	'Bottom Left PF Speaker
			PlaySound SoundName, Looper, VolMult * (1-XVol(TableObject)) * (1-YVol(TableObject)),  1, 0, 0, 0, 0,  1	'Bottom Right PF Speaker
		End If
	End If
End Sub

Sub PlayFieldSoundAB (SoundName, Looper, VolMult)
'Plays the sound of a table object at the Active Ball's location.
'For stereo, xGain is a Playsound volume multiplier that provides a Constant Power pan.
'For quad, multiple PlaySound commands are launched together that are panned and faded to their maximum extents where PlaySound's PAN and FADE have the least error.
'XVol and YVol are Playsound volume multipliers that provide a Constant Power "pan" and "fade".
'Subtracting XVol or YVol from 1 yields an inverse response.

	If PFOption = 1 Or PFOption = 2 Then
		PlaySound SoundName, Looper, VolMult * Vol(ActiveBall) * xGain(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.
	End If
	If PFOption = 3 Then
		PlaySound SoundName, Looper, VolMult * Vol(ActiveBall) *    XVol(ActiveBall)  *    YVol(ActiveBall),  -1, 0, Pitch(ActiveBall), 0, 0, -1	'Top Left PF Speaker
		PlaySound SoundName, Looper, VolMult * Vol(ActiveBall) * (1-XVol(ActiveBall)) *    YVol(ActiveBall),   1, 0, Pitch(ActiveBall), 0, 0, -1	'Top Right PF Speaker
		PlaySound SoundName, Looper, VolMult * Vol(ActiveBall) *    XVol(ActiveBall)  * (1-YVol(ActiveBall)), -1, 0, Pitch(ActiveBall), 0, 0,  1	'Bottom Left PF Speaker
		PlaySound SoundName, Looper, VolMult * Vol(ActiveBall) * (1-XVol(ActiveBall)) * (1-YVol(ActiveBall)),  1, 0, Pitch(ActiveBall), 0, 0,  1	'Bottom Right PF Speaker
	End If
End Sub

Sub PlayReelSound (SoundName, Pan)
Dim ReelVolAdj
ReelVolAdj = 0.2
'Provides a Constant Power Pan for the backglass reel sound volume to match the playfield's Constant Power Pan response
	If showDT = False Then	'-3dB for desktop mode
		If Pan = "Lpan" Then PlaySound SoundName, 0, ReelVolAdj * 1.00, -0.12, 0, 0, 0, 1, 0	'Panned 3/4 Left at 0dB * ReelVolAdj
		If Pan = "Mpan" Then PlaySound SoundName, 0, ReelVolAdj * 0.33,  0.00, 0, 0, 0, 1, 0	'Panned Middle at -3dB * ReelVolAdj
		If Pan = "Rpan" Then PlaySound SoundName, 0, ReelVolAdj * 1.00,  0.12, 0, 0, 0, 1, 0	'Panned 3/4 Right at 0dB * ReelVolAdj
	Else
		If Pan = "Lpan" Then PlaySound SoundName, 0, ReelVolAdj * 0.33, -0.12, 0, 0, 0, 1, 0	'Panned 3/4 Left at -3dB * ReelVolAdj
		If Pan = "Mpan" Then PlaySound SoundName, 0, ReelVolAdj * 0.11,  0.00, 0, 0, 0, 1, 0	'Panned Middle at -6dB * ReelVolAdj
		If Pan = "Rpan" Then PlaySound SoundName, 0, ReelVolAdj * 0.33,  0.12, 0, 0, 0, 1, 0	'Panned 3/4 Right at -3dB * ReelVolAdj
	End If
End Sub

'******************************************************
'				FLIPPER AND RUBBER CORRECTION
'******************************************************
dim LFPress, RFPress, EOST, EOSA, EOSTnew, EOSaNew
dim FStrength, FRampUp, fElasticity, EOSRampUp, SOSRampUp
dim RFEndAngle, LFEndAngle, LFCount, RFCount, LiveCatch

LFEndAngle = Leftflipper.EndAngle
RFEndAngle = RightFlipper.EndAngle

EOST = leftflipper.eosTorque   			'End of Swing Torque
EOSA = leftflipper.eosTorqueAngle		'End of Swing Torque Angle
fStrength = LeftFlipper.strength		'Flipper Strength
fRampUp = LeftFlipper.RampUp			'Flipper Ramp Up
fElasticity = LeftFlipper.elasticity	'Flipper Elasticity
EOStNew = 1.0 		'new Flipper Torque 
EOSaNew = 0.2		'new FLipper Tprque Angle
EOSRampUp = 1.5		'new EOS Ramp Up weaker at EOS because of the weaker holding coil
SOSRampUp = 8.5 	'new SOS Ramp Up strong at start because of the stronger starting coil
LiveCatch = 8		'variable to check elapsed time from 

'********Need to have a flipper timer to check for these values

Sub flipperTimer_Timer
    Dim BOT, b
    BOT = GetBalls
	Lflip.ObjRotZ = LeftFlipper.currentangle
	Rflip.ObjRotZ = RightFlipper.currentangle
'	For b = 0 to UBound(BOT)
'		BOT(0).color = vbgreen
'		If b > 0 then BOT(1).color = vbred
'		If b > 1 Then BOT(2).color = vbblue
'		tb.text = "x = " & BOT(0).x
'		tb1.text = "y = " & BOT(0).y
'		tb.text = formatnumber(BOT(b).AngMomY,1)
'		tb.text = "AngMomX = " & formatnumber(BOT(b).AngMomX,1)
'		tb1.text = "AngMomY = " & formatnumber(BOT(b).AngMomY,1)
'		tb2.text = "AngMMomZ = " & formatnumber(BOT(b).AngMomZ,1)

'	Next

'	lFlip.rotz = leftflipper.CurrentAngle -121 'silver metal flipper obj
'	lFlipR.rotz = leftflipper.CurrentAngle -121 
'	rFlip.rotz = RightFlipper.CurrentAngle +121
'	rFlipR.rotz = RightFlipper.CurrentAngle +121


	FlipperLSh.RotZ = LeftFlipper.CurrentAngle
	FlipperRSh.RotZ = RightFlipper.CurrentAngle



	'--------------Flipper Tricks Section
	'What this code does is swing the flipper fast and make the flipper soft near its EOS to enable live catches.  It resets back to the base Table
	'settings once the flipper reaches the end of swing.  The code also makes the flipper starting ramp up high to simulate the stronger starting
	'coil strength and weaker at its EOS to simulate the weaker hold coil.

	If LeftFlipper.CurrentAngle = LeftFlipper.EndAngle and LFPress = 1 Then 	'If the flipper is fully swung and the flipper button is pressed then...
		LeftFlipper.eosTorqueAngle = EOSaNew	'sets flipper EOS Torque Angle to .2 
		LeftFlipper.eosTorque = EOStNew			'sets flipper EOS Torque to 1
		LeftFlipper.RampUp = EOSRampUp			'sets flipper ramp up to 1.5
		If LFCount = 0 Then LFCount = GameTime	'sets the variable LFCount = to the elapsed game time
		If GameTime - LFCount < LiveCatch Then	'if less than 8ms have elasped then we are in a "Live Catch" scenario
			LeftFlipper.Elasticity = 0.1		'sets flipper elasticity WAY DOWN to allow Live Catches
			If LeftFlipper.EndAngle <> LFEndAngle Then LeftFlipper.EndAngle = LFEndAngle	'Keep the flipper at its EOS and don't let it deflect
		Else	
			LeftFlipper.Elasticity = fElasticity	'reset flipper elasticity to the base table setting
		End If
	Elseif LeftFlipper.CurrentAngle > LeftFlipper.startangle - 0.05  Then 	'If the flipper has started its swing, make it swing fast to nearly the end...
		LeftFlipper.RampUp = SOSRampUp				'set flipper Ramp Up high
		LeftFlipper.EndAngle = LFEndAngle - 3		'swing to within 3 degrees of EOS
		LeftFlipper.Elasticity = fElasticity		'Set the elasticity to the base table elasticity
		LFCount = 0
	Elseif LeftFlipper.CurrentAngle > LeftFlipper.EndAngle + 0.01 Then  'If the flipper has swung past it's end of swing then...
		LeftFlipper.eosTorque = EOST			'set the flipper EOS Torque back to the base table setting
		LeftFlipper.eosTorqueAngle = EOSA		'set the flipper EOS Torque Angle back to the base table setting
		LeftFlipper.RampUp = fRampUp			'set the flipper Ramp Up back to the base table setting
		LeftFlipper.Elasticity = fElasticity	'set the flipper Elasticity back to the base table setting
	End If

	If RightFlipper.CurrentAngle = RightFlipper.EndAngle and RFPress = 1 Then
		RightFlipper.eosTorqueAngle = EOSaNew
		RightFlipper.eosTorque = EOStNew
		RightFlipper.RampUp = EOSRampUp
		If RFCount = 0 Then RFCount = GameTime
		If GameTime - RFCount < LiveCatch Then
			RightFlipper.Elasticity = 0.1
			If RightFlipper.EndAngle <> RFEndAngle Then RightFlipper.EndAngle = RFEndAngle
		Else
			RightFlipper.Elasticity = fElasticity
		End If
	Elseif RightFlipper.CurrentAngle < RightFlipper.StartAngle + 0.05 Then
		RightFlipper.RampUp = SOSRampUp 
		RightFlipper.EndAngle = RFEndAngle + 3
		RightFlipper.Elasticity = fElasticity
		RFCount = 0 
	Elseif RightFlipper.CurrentAngle < RightFlipper.EndAngle - 0.01 Then 
		RightFlipper.eosTorque = EOST
		RightFlipper.eosTorqueAngle = EOSA
		RightFlipper.RampUp = fRampUp
		RightFlipper.Elasticity = fElasticity
	End If

End Sub

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity


InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		'safety coefficient (diminishes polarity correction only)
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1

		x.enabled = True
		x.TimeDelay = 69    '*****Important, this variable is an offset for the speed that the ball travels down the table to determine if the flippers have been fired 
							'This is needed because the corrections to ball trajectory should only applied if the flippers have been fired and the ball is in the trigger zones.
							'FlipAT is set to GameTime when the ball enters the flipper trigger zones and if GameTime is less than FlipAT + this time delay then changes to velocity
							'and trajectory are applied.  If the flipper is fired before the ball enters the trigger zone then with this delay added to FlipAT the changes
							'to tragectory and velocity will not be applied.  Also if the flipper is in the final 20 degrees changes to ball values will also not be applied.
							'"Faster" tables will need a smaller value while "slower" tables will need a larger value to give the ball more time to get to the flipper. 		
							'If this value is not set high enough the Flipper Velocity and Polarity corrections will NEVER be applied.
	Next

	'rf.report "Polarity"
	AddPt "Polarity", 0, 0, -2.7
	AddPt "Polarity", 1, 0.16, -2.7	
	AddPt "Polarity", 2, 0.33, -2.7
	AddPt "Polarity", 3, 0.37, -2.7	'4.2
	AddPt "Polarity", 4, 0.41, -2.7
	AddPt "Polarity", 5, 0.45, -2.7 '4.2
	AddPt "Polarity", 6, 0.576,-2.7
	AddPt "Polarity", 7, 0.66, -1.8'-2.1896
	AddPt "Polarity", 8, 0.743, -0.5
	AddPt "Polarity", 9, 0.81, -0.5
	AddPt "Polarity", 10, 0.88, 0

	'"Velocity" Profile
	addpt "Velocity", 0, 0, 	1
	addpt "Velocity", 1, 0.16, 1.06
	addpt "Velocity", 2, 0.41, 	1.05
	addpt "Velocity", 3, 0.53, 	1'0.982
	addpt "Velocity", 4, 0.702, 0.968
	addpt "Velocity", 5, 0.95,  0.968
	addpt "Velocity", 6, 1.03, 	0.945

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


Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() :  LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub

'Methods:
'.TimeDelay - Delay before trigger shuts off automatically. Default = 80 (ms)
'.AddPoint - "Polarity", "Velocity", "Ycoef" coordinate points. Use one of these 3 strings, keep coordinates sequential. x = %position on the flipper, y = output
'.Object - set to flipper reference. Optional.
'.StartPoint - set start point coord. Unnecessary, if .object is used.

'Called with flipper - 
'ProcessBalls - catches ball data. 
' - OR - 
'.Fire - fires flipper.rotatetoend automatically + processballs. Requires .Object to be set to the flipper.

'***************This is flipperPolarity's addPoint Sub
Class FlipperPolarity
	Public Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart, FlipperEnd, LR, PartialFlipCoef
	Private Balls(20), balldata(20)
	
	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut

	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True: TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new spoofBall: next  
	End Sub
	
	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
	Public Property Let StartPoint(aInput) : if IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end if : End Property
	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
	Public Property Let EndPoint(aInput) : if IsObject(aInput) then FlipperEnd = aInput.x else FlipperEnd = aInput : end if : End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property
	
	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select

	End Sub 

'********Triggered by a ball hitting the flipper trigger area	
	Public Sub AddBall(aBall) : dim x : 
		for x = 0 to uBound(balls)  
			if IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end if  
		Next   
	End Sub

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

'*********Used to rotate flipper since this is removed from the key down for the flippers	
	Public Sub Fire()
		Flipper.RotateToEnd
		processballs
	End Sub

	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then balldata(x).Data = balls(x)
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))  '% of flipper swing
		PartialFlipCoef = abs(PartialFlipCoef-1)
		if abs(Flipper.CurrentAngle - Flipper.EndAngle) < 20 Then 'last 20 degrees of swing is not dealt with
			PartialFlipCoef = 0
		End If
'		tb.text = FlipAT
	End Sub

'***********gameTime is a global variable of how long the game has progressed in ms
'***********This function lets the table know if the flipper has been fired
	Private Function FlipperOn() 
'		TB.text = gameTime & ":" & (FlipAT + TimeDelay) ' ******MOVE TB into view WHEN THIS FLIPPER FUNCTIONALITY IS ADDED TO A NEW TABLE TO CHECK IF THE TIME DELAY IS LONG ENOUGH*****
		if gameTime < FlipAt + TimeDelay then FlipperOn = True 
	End Function	'Timer shutoff for polaritycorrect 
	
'***********This is turned on when a ball leaves the flipper trigger area
	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then 'don't run this if the flippers are at rest
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1
			dim teststr : teststr = "Cutoff"
			tmp = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
			if tmp < 0.1 then 'if real ball position is behind flipper, exit Sub to prevent stucks	'Disabled 1.03, I think it's the Mesh that's causing stucks, not this
			end if

			'y safety Exit
			if aBall.VelY > -8 then 'if ball going down then remove the ball
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

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
'				tb.text = "Vel corr"
				Dim VelCoef
				if IsEmpty(BallData(idx).id) and aBall.VelY < -12 then 'if tip hit with no collected data, do vel correction anyway
					if PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1) > 1.1 then 'adjust plz
						VelCoef = LinearEnvelope(5, VelocityIn, VelocityOut)
						if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
						if Enabled then aBall.Velx = aBall.Velx*VelCoef'VelCoef
						if Enabled then aBall.Vely = aBall.Vely*VelCoef'VelCoef
					end if
				Else
		 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
					if Enabled then aBall.Velx = aBall.Velx*VelCoef
					if Enabled then aBall.Vely = aBall.Vely*VelCoef
				end if
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
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
				Set a(aCount) = aArray(x) 'Set creates an object in VB
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

'**********Takes in more than one array and passes them to ShuffleArray
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

'**********Calculate ball speed as hypotenuse of velX/velY triangle
Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

'**********Calculates the value of Y for an input x using the slope intercept equation
Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

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

'****************************************************************************
'PHYSICS DAMPENERS

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR


Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
End Sub

'*********This sets up the rubbers:
dim RubbersD  
Set RubbersD = new Dampener  'Makes a Dampener Class Object 	
RubbersD.name = "Rubbers"

'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
RubbersD.addpoint 0, 0, 0.935 '0.96	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.935 '0.96
RubbersD.addpoint 2, 5.76, 0.942 '0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener	'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.CopyCoef RubbersD, 0.85

'**********Class for dampener section of nfozzy's code
Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold 	'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): End Sub 

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
	End Sub

	public sub Dampen(aBall)
		if threshold then if BallSpeed(aBall) < threshold then exit sub end if end if
		dim RealCOR, DesiredCOR, str, coef
'               Uses the LinearEnvelope function to calculate the correction based upon where it's value sits in relation
	
'               to the addpoint parameters set above.  Basically interpolates values between set points in a linear fashion
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		
'                Uses the function BallSpeed's value at the point of impact/the active ball's velocity which is constantly being updated	
'               RealCor is always less than 1 
		RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)

'               Divides the desired CoR by the real COR to make a multiplier to correct velocity in x and y
		coef = desiredcor / realcor 
		
'		TB.text = coef

'               Applies the coef to x and y velocities
		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
	End Sub

'***********This Sub sets the values for Sleeves (or any other future objects) to 85% (or whatever is passed in) of Posts
	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub
End Class

'*****************************Generates cor.ballVel for dampener
Sub RDampen_Timer() ' 1 ms timer always on
	CoR.Update
End Sub

'*********CoR is Coefficient of Restitution defined as "how much of the kinetic energy remains for the objects 
'to rebound from one another vs. how much is lost as heat, or work done deforming the objects 
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

'********Interpolates the value for areas between the low and upper bounds sent to it
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	'clamp 2.0
	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
End Function

Sub Table1_Exit
	If B2sOn Then controller.stop
	saveHighScore
End Sub

