'****************************************************************
'
'				 Heat Wave (Williams 1964)	
'							v1.0				  
'				     Script by Scottacus
'					     June 2021
'
'	Basic DOF config
'		220 Left Flipper, 221 Right Flipper
'		203 Left sling, 204 Right sling
'		205 Bumper1,  206 Bumper2, 207 Bumper3 , 208 Bumper4
'		210 - 213 Top Rollovers
'		214 - 217 Side Rollovers
'		218 - 219 Top Targets
'		230 Middle Target
'		109 Launch Ball
'		124 Drain, 125 Ball Release
'	 	127 credit light
'		128 Knocker, 129 Knocker and Kicker Strobe 
'		160 Ball In Shooter Lane
'		153 Chime1-10s, 154 Chime2-100s, 155 Chime3-1000s
'
'		Code Flow									  
'																						
'		Start Game -> Check Continue ->  BallDrain - 
'							^						|							                                                                               
'						EndGame = False <-----------
'***********************************************************************************************************************

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

Const cGameName = "HeatWave_1964"
Const cOptions = "HeatWave_1964.txt"
Const hsFileName = "Heat Wave (Williams 1964)"

'***************************************************************************************
'Set this variable to 1 to save a PinballY High Score file to your Tables Folder
'this will let the Pinball Y front end display the high scores when searching for tables
'0 = No PinballY High Scores, 1 = Save PinballY High Scores 
Const cPinballY = 2
'***************************************************************************************

Dim balls
Dim replays
Dim maxPlayers
Dim players
Dim player
Dim credit
Dim score(6)
Dim hScore(6)
Dim sReel
Dim state
Dim tilt
Dim matchNumber
Dim i,j, f, ii, Object, Light, x, y, z
Dim freePlay 
Dim ballsize,BallMass
Dim hsInitial0, hsInitial1, hsInitial2
Dim hsArray: hsArray = Array("HS_0","HS_1","HS_2","HS_3","HS_4","HS_5","HS_6","HS_7","HS_8","HS_9","HS_Space","HS_Comma")
Dim hsiArray: hsIArray = Array("HSi_0","HSi_1","HSi_2","HSi_3","HSi_4","HSi_5","HSi_6","HSi_7","HSi_8","HSi_9","HSi_10","HSi_11","HSi_12","HSi_13","HSi_14","HSi_15","HSi_16","HSi_17","HSi_18","HSi_19","HSi_20","HSi_21","HSi_22","HSi_23","HSi_24","HSi_25","HSi_26")
ballSize = 50
ballMass = 1
Dim options
Dim replayEB
Dim chime
Dim onBumper
Dim pfOption
Dim Temperature


'Desktop reel pans,  Valid values: "Lpan", "Mpan" and "Rpan"
Const dtcrReel = "Rpan"
Const dts1Reel = "Rpan"
Const dts2Reel = "Rpan"
Const dts3Reel = "Rpan"
Const dts4Reel = "Rpan"
Const dtsjwReel = "Rpan"

'Backglass reel pans,  Valid values: "Lpan", "Mpan" and "Rpan"
Const bgcrReel = "Rpan"
Const bgs1Reel = "Lpan"
Const bgs2Reel = "Rpan"
Const bgs3Reel = "Rpan"
Const bgs4Reel = "Rpan"
Const bgsjwReel = "Rpan"

Sub Table1_init
	LoadEM
	maxPlayers = 1
	Set sReel = scoreReel
	player = 1
	loadHighScore
	If highScore(0)="" Then highScore(0)=1200
	If highScore(1)="" Then highScore(1)=1000
	If highScore(2)="" Then highScore(2)=800
	If highScore(3)="" Then highScore(3)=600
	If highScore(4)="" Then highScore(4)=400
	If matchNumber = "" Then matchNumber = 4
	If showDT = True Then pfOption = 1
	If pfOption = "" Then pfOption = 1	
	score(1) = 0
	If initial(0,1) = "" Then
		initial(0,1) = 2: initial(0,2) = 18: initial(0,3) = 4
		initial(1,1) = 1: initial(1,2) = 1: initial(1,3) = 1
		initial(2,1) = 2: initial(2,2) = 2: initial(2,3) = 2
		initial(3,1) = 3: initial(3,2) = 3: initial(3,3) = 3
		initial(4,1) = 4: initial(4,2) = 4: initial(4,3) = 4
	End If
	If credit = "" Then credit = 0
	If freePlay = "" Then freePlay = 1
	If balls = "" Then balls = 5
	If chime = "" Then chime = 0
	replaySettings

	If FreePlay = 1 Then
		If B2SOn Then DOF 127, DOFOn
	End If

	contball = 0
	reelStop = 0

	For x = 1 to balls
		EVAL("Kicker" & x).createball
		EVAL("Kicker" & x).enabled = False
	Next

	For Each x in leftTargets
		x.isdropped = 1
	Next

	For Each x in rightTargets
		x.isdropped = 1
	Next
	
	LeftTarget1.isdropped = 0
	RightTarget1.isDropped = 0
	tStep = 1
	pRot = -26
	
	firstBallOut = 0
	updatePostIt
	dynamicUpdatePostIt.enabled = 1
	tiltReel.SetValue(1)
	creditReel.setvalue(Credit)

	ballShadowUpdate.enabled = True

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

	tilt = False
	state = False
	gameState

	
End Sub

'****************KeyCodes
Dim enableInitialEntry, firstBallOut
Sub Table1_KeyDown(ByVal keycode)

	If enableInitialEntry = True Then enterInitials(keycode)
   
	If keycode = addCreditKey Then
		playFieldSound "coinin",0,Drain5,1 
		AddCredit = 1
		ScoreMotor5.enabled = 1
    End If

    If keycode = startGameKey Then
		If enableInitialEntry = False and operatormenu = 0 and backGlassOn = 1 Then
			If freePlay = 1 and state = 0 Then startGame
			If freePlay = 0 and credit > 0 and players = 0 and firstBallOut = 0 Then
				credit = credit - 1
				If showDT = False Then playReelSound "Reel5", bgcrReel Else playReelSound "Reel5", dtcrReel
				creditReel.setvalue(credit)	
				If B2SOn Then 
					If freePlay = 0 Then controller.B2SSetCredits credit
					If freePlay = 0 and credit < 1 Then DOF 127, DOFOff
				End If
				startGame
			End If
		End If
	End If

	If Keycode = startGameKey and contBall = 0 and lifter = 0 and starter = 1 and state = True and enableInitialEntry = False Then
		ballLiftTimer.enabled = 1
	End If

	If Keycode = rightMagnaSave and contBall = 0 and lifter = 0 and state = True Then
		ballLiftTimer.enabled = 1
	End If
	  
	If keycode = PlungerKey Then
		plunger.PullBack
		playFieldSound "plungerpull", 0, plunger, 1
	End If

  If tilt = False and state = True Then
	If keycode = leftFlipperKey and contball = 0 Then
		LFPress = 1
		lf.fire
		playFieldSound "FlipUpL", 0, leftFlipper, 1
		If B2SOn Then DOF 101,DOFOn
		playFieldSound "FlipBuzzL", -1, leftFlipper, 1
	End If
    
	If keycode = rightFlipperKey and contball = 0 Then
		RFPress = 1
		rf.fire
		playFieldSound "FlipUpR", 0, rightFlipper,1
		If B2SOn Then DOF 102,DOFOn
		playFieldSound "FlipBuzzR", -1, rightFlipper,1
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
  End if

	If Keycode = mechanicalTilt Then 
		tilt = True
		tiltReel.SetValue(1)
		If B2SOn Then controller.B2SSetTilt 1
		turnOff
	End If

    If keycode = leftFlipperKey and state = False and operatorMenu = 0 and enableInitialEntry =  False Then
        operatorMenuTimer.Enabled = true
    end if
 
    If keycode = leftFlipperKey and state = False and operatorMenu = 1 Then
		options = options + 1
		If ShowDt = True Then If options = 2 Then options = 4
        If options =  6 Then options = 0
		optionMenu.visible = True
        playFieldSound "target", 0, SoundPointScoreMotor, 1.5
        Select Case (Options)
            Case 0:
                optionMenu.image = "FreeCoin" & freePlay
            Case 1:
                optionMenu.image = balls & "Balls"
			Case 2:
				optionMenu1.image = "DOF"
				optionMenu.image = "Chime" & chime
			Case 3:
				optionMenu.image = "UnderCab"
				optionMenu1.image = "Sound" & pfOption
				optionMenu2.visible = 1
				optionMenu2.image = "SoundChange"
				Select Case (pfOption)
					Case 1: speaker1.visible = 1: speaker2.visible = 1: speaker3.visible = 0: speaker4.visible = 0
					Case 2: speaker5.visible = 1: speaker6.visible = 1: speaker1.visible = 0: speaker2.visible = 0
					Case 3: speaker1.visible = 1: speaker2.visible = 1: speaker3.visible = 1: speaker4.visible = 1: speaker5.visible = 0: speaker6.visible = 0
				End Select

			Case 4:
				For x = 1 to 6
					EVAL("Speaker" & x).visible = 0
				Next
				optionMenu1.visible = 0
				optionMenu.image = "SaveExit"
				optionMenu2.visible = 0
        End Select
    End If
 
    If keycode = rightFlipperKey and state = False and operatorMenu = 1 Then
      playFieldSound "metalhit2", 0, Speaker5, 1.5
      Select Case (Options)
		Case 0:
            If freePlay = 0 Then
                freePlay = 1
              Else
                freePlay = 0
            End If
            OptionMenu.image= "FreeCoin" & freePlay
			If freePlay = 0 Then
				If credit > 0 and B2SOn Then DOF 127, DOFOn
				If credit < 1 and B2SOn Then DOF 127, DOFOff
			Else
				If B2SOn Then DOF 127, DOFOn
			End If
        Case 1:
            If balls = 3 Then
				For x = 4 to 5 
					EVAL("Kicker" & x).enabled = 1
					EVAL("Kicker" & x).createball
					EVAL("Kicker" & x).enabled = 0
				Next
                balls = 5
	'			instructCard.image = "InstructionCard1"
              Else
                balls = 3
				For x = 4 to 5
					EVAL("Drain" & x).destroyball
				Next
	'			instructCard.image = "InstructionCard0"
            End If
            optionMenu.image = balls & "Balls"
		Case 2:
            If chime = 0 Then
                chime= 1
				If B2SOn Then DOF 155,DOFPulse
              Else
                chime = 0
				playsound "Bell10"
            End If
			optionMenu.image = "Chime" & chime		
		Case 3:
			optionMenu1.visible = 1
			pfOption = pfOption + 1
			If pfOption = 4 Then pfOption = 1
			optionMenu1.image = "Sound" & pfOption
			Select Case (pfOption)
				Case 1: speaker1.visible = 1: speaker2.visible = 1: speaker3.visible = 0: speaker4.visible = 0
				Case 2: speaker5.visible = 1: speaker6.visible = 1: speaker1.visible = 0: speaker2.visible = 0
				Case 3: speaker1.visible = 1: speaker2.visible = 1: speaker3.visible = 1: speaker4.visible = 1: speaker5.visible = 0: speaker6.visible = 0
			End Select
        Case 4:
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

    If keycode = 46 Then' C Key
       stopSound "FlipBuzzLA"
       stopSound "FlipBuzzLB"
       stopSound "FlipBuzzLC"
       stopSound "FlipBuzzLD"
       stopSound "FlipBuzzRA"
       stopSound "FlipBuzzRB"
       stopSound "FlipBuzzRC"
       stopSound "FlipBuzzRD"
        If contBall = 1 Then
            contBall = 0
          Else
            contBall = 1
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
	If keycode= 30 Then 
		EVAL("match" & matchNumber).setValue (0)	
		matchNumber = matchNumber + 1
		If matchNumber > 10 Then matchNumber = 1
		If b2SOn then controller.b2SSetmatch 34, matchNumber
		EVAL("match" & matchNumber).setValue (1)

	End If

'
'************************End Of Test Keys****************************
End Sub

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
			LFPress = 0
			leftFlipper.RotateToStart
			playFieldSound "FlipDownL", 0, leftFlipper, 1
			If B2SOn Then DOF 220,DOFOff
			stopSound "FlipBuzzLA"
			stopSound "FlipBuzzLB"
			stopSound "FlipBuzzLC"
			stopSound "FlipBuzzLD"
		End If
    
		If keycode = rightFlipperKey and contball = 0 Then
			RFPress = 0
			rightFlipper.rotateToStart
			playFieldSound "FlipDownR", 0, rightFlipper, 1
			If B2SOn Then DOF 221,DOFOff
			stopSound "FlipBuzzRA"
			stopSound "FlipBuzzRB"
			stopSound "FlipBuzzRC"
			stopSound "FlipBuzzRD"
		End If
   End If

	If keycode = rightMagnaSave Then playFieldSound "BallLiftDown", 0, Plunger, .7


    If keycode = 203 Then cLeft = 0' Left Arrow
 
    If keycode = 200 Then cUp = 0' Up Arrow
 
    If keycode = 208 Then cDown = 0' Down Arrow
 
    If keycode = 205 Then cRight = 0' Right Arrow

	If keycode = 52 Then Zup = 0' Period

End Sub

'************** Table Boot
Dim backGlassOn
Dim bootCount:bootCount = 0
Sub bootTable_Timer
	score(1) = 0
	bootCount = bootCount + 1
	If bootCount = 1 Then
		playFieldSound "poweron",0, Plunger, 1
		EVAL("match" & matchNumber).setValue (1)
		If B2SOn Then
			Controller.B2SSetData 80,1
			Controller.B2SSetData 81,10
			Controller.B2SSetCredits Credit
			Controller.B2SSetMatch 34, MatchNumber
			Controller.B2SSetGameOver 35,1
			Controller.B2SSetTilt 33,1
			If Credit > 0 Then DOF 127, DOFOn		
			If FreePlay = 1 Then DOF 127, DOFOn
			For x = 10 to 15
				controller.B2SSetData x, 1
			Next
		End If
		Alternator
		EMReelSun.SetValue (1)
		EMReelWilliams.SetValue (1)
		For Each x in TempRollOvers
			x.state = 1
		Next
		ResetAB
		backGlassOn = 1
		bootCount = 0
		bootTable.enabled = False
	End If
End Sub

Dim replayValue
Sub replaySettings
	If balls = 3 Then
		replay(1) = 1000
		replay(2) = 1200
		replay(3) = 1400
		replay(4) = 1600
		topReplay = 3
	Else
		replay(1) = 1500
		replay(2) = 1600
		replay(3) = 1700
		topReplay = 2
	End If
End Sub

'***********Operator Menu
Dim operatormenu

Sub operatorMenuTimer_Timer
	options = 0
    operatorMenu = 1
	dynamicUpdatePostIt.enabled = 0
	updatePostIt
	options = 0
    playFieldSound "target", 0, SoundPointScoreMotor, 1.5
    optionsMenu.visible = True
    optionMenu.visible = True
	optionMenu.image = "FreeCoin" & freePlay
End Sub

'***********Timer to delay lift of first ball until one ball is destroyed on game start
Dim starter, starterCount
Sub gameStart_Timer
	starterCount = starterCount + 1
	If starterCount = 2 Then	
		starter = 1
		starterCount = 0
		gameStart.enabled = 0	
	End If
End Sub

'***********Start Game
Dim ballInPlay, bonusScore
Sub startGame
	If state = False Then
		ballInPlay = 0
		If B2SOn Then 
			controller.B2SSetCredits credit
			controller.B2SSetGameOver 0
			controller.B2SSetTilt 33, 0
			controller.B2SSetMatch 34, 0
			controller.B2SSetBallInPlay 32, 0
			controller.B2SSetData 81, 1
			For x = 51 to 77
				controller.B2SSetData x,0
			Next
		End If
		gameOverReel.SetValue(0)
		tiltReel.SetValue(0)
		dynamicUpdatePostIt.enabled = 0
		updatePostIt
		tilt = False
		state = True
		players = 1
		BallOut = 0 
		lastBall = False
		drainGate.collidable = False
		Temperature = 0
		resetTemp
		bgThermometer.setValue (0)
		TargetTimer.enabled = 1
		
		For x = 1 to 5
			EVAL("Bumper" & x).hashitevent = 1
		Next


		newGame

	End If
End Sub

'*********New Game
Dim endGame, roundHS
Sub newGame
	player = 1
    endGame = 0
	roundHS = 0
	gameState
	For x = 1 to 4
		EVAL("Bumper" & x).hasHitEvent = 1
	Next
	EVAL("match" & matchNumber).setValue (0)
	resetReel.enabled = True
	gameStart.enabled = 1
	bgThermometer.setValue (0)
	For x = 2 to 6
		EVAL("tReel" & x).setValue (0)
	Next
	treel1.setValue (1)
End Sub

'*************Check for Continuing Game
Sub checkContinue
	If endGame = 1 Then
		turnOff
		starter = 0
		stopSound "FlipBuzzLA"
		stopSound "FlipBuzzLB"
		stopSound "FlipBuzzLC"
		stopSound "FlipBuzzLD"
		stopSound "FlipBuzzRA"
		stopSound "FlipBuzzRB"
		stopSound "FlipBuzzRC"
		stopSound "FlipBuzzRD"
		match
		state = False
'		BIPReel.setvalue(0)
		gameState
		dynamicUpdatePostIt.enabled = 1
		sortScores
		checkHighScores
		firstBallOut = 0		
		players = 0
		For x = 1 to 4
			ReplayValue = 0
			RepAwarded(x) = 0
		Next
		saveHighScore
		TargetTimer.enabled = 0
		If B2SOn Then 
			Controller.B2SSetGameOver 35,1
			If credit > 0 Then DOF 127, DOFOn
			If freePlay = 1 Then DOF 127, DOFOn
		End If
	End If
' NOTE because this is a ball lift table there is no Else to eject the next ball
End Sub

'*******************Ball Drained
Dim lastBall
Sub ballDrained
	flag100 = 0: flag10 = 0:  flag1 = 0
	ballDrain = ballDrain + 1
	If ballDrain = balls Then endGame = 1
	checkContinue
End Sub

'***************Drain and Release Ball
Dim ballOut, D1, D2, D3, D4, D5

Sub drain1_Hit
	D1 = 1
	If ballDrain = 1 Then archHit = 0
	resetTarget
End Sub

Sub drain1_UnHit
	D1 = 0
End Sub

Sub drain2_Hit
	If ballDrain = 2 Then archHit = 0
	D2 = 1
	resetTarget
End Sub

Sub drain2_UnHit
	D2 = 0
End Sub

Sub drain3_Hit
	If ballDrain = 3 Then archHit = 0
	D3 = 1
	resetTarget
End Sub

Sub drain3_UnHit
	D3 = 0
End Sub

Sub drain4_Hit
	If ballDrain = 4 Then archHit = 0
	D4 = 1
	resetTarget
End Sub

Sub drain4_UnHit
	D4 = 0
End Sub

Sub drain5_Hit
		If ballDrain = 5 Then archHit = 0
	D5 = 1
	ballOut = ballOut + 1
	scoreMotorLoop = 0
	repAwarded(player) = 0
	leftSlingShot.Collidable = 1
	rightSlingShot.Collidable = 1
	For x= 1 to 4
		EVAL("Bumper" & x).hasHitEvent = 1
	Next
	Tilt = False	
	If B2SOn Then Controller.B2SSetTilt 33,0
	TiltReel.SetValue(0)
	TargetReset = 0
End Sub

Sub drain5_UnHit
	D5 = 0
End Sub

Sub drain6_Hit
	ballDrained
End Sub

Dim ballDrain
'************Game State Check
Sub gameState
	If state = 0 Then
'		For each light in GIlights:light.state = 0: Next
		gameOverReel.SetValue(1)
		If B2SOn Then 
			controller.B2SSetGameOver 35, 1
			controller.B2SSetBallInPlay 32, 0
		End If
		stopSound "FlipBuzzLA"
		stopSound "FlipBuzzLB"
		stopSound "FlipBuzzLC"
		stopSound "FlipBuzzLD"
		stopSound "FlipBuzzRA"
		stopSound "FlipBuzzRB"
		stopSound "FlipBuzzRC"
		stopSound "FlipBuzzRD"
		ballDrain = 0
		bumpersOff
	Else 
'		For each Light in GIlights:Light.state = 1: Next
		For x= 1 to 5
			EVAL("Bumper" & x).hasHitEvent = 1
		Next
		tilt = False
		gameOverReel.SetValue(0)
		tiltReel.SetValue(0)
		If B2SOn then 
			controller.B2SSetTilt 33,0
			controller.B2SSetMatch 34,0
			controller.B2SSetGameOver 35,0
		End If
	End If
End Sub	

'*************Ball in Launch Lane on Plunger Tip
Dim ballREnabled
Sub ballHome_hit
	ArchHit = 0
	BallREnabled = 1
	If B2SOn then DOF 160, DOFOn
	Set ControlBall = ActiveBall   
    contBallInPlay = True
End Sub

'*************Ball off of Plunger Tip
Sub ballHome_unhit
	If B2SOn Then DOF 160, DOFOff
End Sub

'******* for ball control script
Sub endControl_Hit()              
    contBallInPlay = False
End Sub

'************Check if Ball Out of Launch Lane
Sub ballsInPlay_hit
	If BallREnabled = 1 Then
		BallREnabled = 0
		BallInLane = False
	End If
	FirstBallOut = 1
	DrainGate.collidable = 1
End Sub

'************** Reset Lights For New Game
Sub resetAB
	lightA.state = 1
	LightB.state = 1
End Sub

Sub bumpersOff
	bumper1Light.state = 0
	bumper1Light001.state = 0
	bumper2Light.state = 0
	bumper2Light001.state = 0
	bumper3Light.state = 0
	bumper3Light001.state = 0
	bumper4Light.state = 0
	bumper4Light001.state = 0
	bumper5Light.state = 0
	bumper5Light001.state = 0
End Sub

Sub resetTemp
	For Each x in TempLights
		x.state = 0
	Next
	SpecialL.state = 0
	SpecialR.state = 0
End Sub

Sub resetTarget
	DTRaise 01
	lightA.state = 1
	LightB.state = 1
	TargetReset = 0
End Sub

'**************Temperature Subroutine
Dim tScore
Sub tempUp
	If Tilt = False Then
		Temperature = Temperature + 1
		If Temperature > 25 Then Temperature = 25
		TB1.text = "Temperature = " & Temperature
		tScore = Temperature\5
		EVAL("tReel" & (tScore + 1)).setValue (1)
		If  tScore > 0 Then EVAL("tReel" & tScore).setValue (0)
		bgThermometer.setValue (Temperature)
		If B2SOn Then 
			controller.B2SSetData (51 + Temperature), 1
			controller.B2SSetData (81 + tScore), 1
		End If
		For x = 0 to tScore
			EVAL("lTemp" & x).state = 1
		Next
	If Temperature > 20 Then
		bumper1Light.state = 1
		bumper1Light001.state = 1
		bumper2Light.state = 1
		bumper2Light001.state = 1
		bumper4Light.state = 1
		bumper4Light001.state = 1
		bumper5Light.state = 1
		bumper5Light001.state = 1	
	ElseIf Temperature mod 4 = 0 Then
		bumper1Light.state = 1
		bumper1Light001.state = 1
		bumper4Light.state = 1
		bumper4Light001.state = 1
	Elseif (Temperature + 2) mod 4 = 0 Then
		bumper2Light.state = 1
		bumper2Light001.state = 1
		bumper5Light.state = 1
		bumper5Light001.state = 1
	Else
		bumpersOff
	End If

	If Temperature >14 Then
		Bumper3Light.state = 1
		Bumper3Light001.state = 1
	End If

	If Temperature = 25 Then
		SpecialL.state = 1
		SpecialR.state = 1
	End If

	EVAL("lTemp" & tScore).state = 1
	TB2.text = "Temp Light = " & tScore

	End If
End Sub

'************** Bumpers
Sub bumpers_hit(Index)
	If Tilt = False Then
		Select Case (Index + 1)
			Case 1: playFieldSound "PopBump", 0, Bumper1, 1
					If B2SOn Then DOF 105, DOFPulse	
					If bumper1Light.state = 0 Then
						addScore 1
					Else		
						addScore 10
					End If
			Case 2: playFieldSound "PopBump", 0, Bumper2, 1
					If B2SOn Then DOF 106, DOFPulse	
					If bumper2Light.state = 0 Then
						addScore 1
					Else		
						addScore 10
					End If
			Case 3: playFieldSound "PopBump", 0, Bumper3, 1
					If B2SOn Then DOF 107, DOFPulse	
					If bumper3Light.state = 0 Then
						addScore 10
					Else		
						addScore 100
					End If
			Case 4: playFieldSound "PopBump", 0, Bumper4, 1
					If B2SOn Then DOF 108, DOFPulse	
					If bumper4Light.state = 0 Then
						addScore 1
					Else		
						addScore 10
					End If
			Case 5: playFieldSound "PopBump", 0, Bumper5, 1
					If B2SOn Then DOF 109, DOFPulse	
					If bumper5Light.state = 0 Then
						addScore 1
					Else		
						addScore 10
					End If
		End Select
	End If
End Sub


'************** Leaf Switches
Sub phys_leafs_hit()
	If Tilt = False Then
		addScore 1
	End If
End Sub

'************** Targets
Sub MainTarget_Hit()
'	MainTarget.collidable = 0
	DTHit 01
'	drop.visible = 0
	If Tilt = False Then
		If Temperature < 25 Then
			addScore (((temperature\5) + 1) * 100)
		Else
			special
		End If
	End If	
End Sub

Dim tStep, stepDirection, pRot
Sub targetTimer_Timer
	If stepDirection = 0 Then
		EVAL("leftTarget" & tStep).isDropped = 1
		EVAL("leftTarget" & tStep + 1).isDropped = 0
		EVAL("rightTarget" & tStep).isDropped = 1
		EVAL("rightTarget" & tStep + 1).isDropped = 0
		targetRight.rotY = tStep + pRot
		targetLeft.rotY = tStep + pRot
		tStep = tStep + 1
		if tStep > 25 Then pRot = -25
		If tStep = 50 Then stepDirection = 1
	Else
		tStep = tStep - 1
		if tStep < 26 Then pRot = -26
		If tStep = 1 Then stepDirection = 0
		EVAL("leftTarget" & tStep).isDropped = 0
		EVAL("leftTarget" & tStep + 1).isDropped = 1
		EVAL("rightTarget" & tStep).isDropped = 0
		EVAL("rightTarget" & tStep + 1).isDropped = 1
		targetRight.rotY = tStep + pRot
		targetLeft.rotY = tStep + pRot
	End If
End Sub

Dim TargetReset, side
Sub rightTargets_Hit(Index)
	lightB.state = 0
	side = 2
	targetBend.enabled = 1
	If lightA.state = 0 and targetReset = 0 Then 
		DTRaise 01
		targetReset = 1
	End If
	TempUP
End Sub

Sub leftTargets_hit(Index)
	lightA.state = 0
	side = 1
	targetBend.enabled = 1
	If lightB.state = 0 and targetReset = 0 Then 
		DTRaise 01
		targetReset = 1
	End If
	TempUp
End Sub

Dim tBend
Sub targetBend_timer
	tBend = tBend + 1
	If tBend = 1 and side = 1 Then
		tb.text = "3"
		targetLeft.rotx = 3
	ElseIf tBend = 1 and side = 2 Then
		targetRight.rotx = 3
	ElseIf tBend = 2 and side = 1 Then
		tb.text = "0"
		targetLeft.rotx = 0
		tBend = 0
		targetBend.enabled = 0
	Else
		targetRight.rotx = 0
		tBend = 0
		targetBend.enabled = 0
	End If
End Sub

'************** Slings
Dim lStep
Sub leftSlingShot_Slingshot
	playfieldSound "sling_l1", 0, SoundPoint12, 1
	If B2SOn Then DOF 203,DOFPulse
	lsling.Visible = 0
	lsling1.Visible = 1
    leftSling.Rotx = 27
	lStep = 0
	leftSlingShot.TimerEnabled = 1
	If Tilt = False Then	
		If Kicker10Left.state = 0 Then
			addScore 1
		Else
			addScore 10
		End If
	End If
End Sub

Sub leftSlingShot_Timer
    Select Case lStep
        Case 3: leftSling.Rotx = 10
				lsling1.Visible = 0
				lsling2.visible=1
        Case 4: leftSling.Rotx = 0
				lsling2.Visible = 0
				lsling.visible = 1
        Case 5: lsling2.Visible = 0
				lsling.visible = 1
				leftSlingShot.timerEnabled = 0
    End Select
    lStep = lStep + 1
End Sub

Dim rStep
Sub rightSlingShot_Slingshot
	playfieldSound "sling_r1", 0, SoundPoint13, 1
	If B2SOn Then DOF 204,DOFPulse
	rsling.Visible = 0
	rsling1.Visible = 1
	rightSling.Rotx = 27
	rStep = 0
	rightSlingShot.TimerEnabled = 1
	If Tilt = False Then	
		If Kicker10Right.state = 0 Then
			addScore 1
		Else
			addScore 10
		End If
	End If
End Sub

Sub rightSlingShot_Timer
    Select Case rStep
        Case 3: rightSling.Rotx = 10
				rSling1.Visible = 0
				rSling2.visible = 1
        Case 4: rightSling.Rotx = 0
				rSling2.Visible = 0
				rSling.visible=1
        Case 5: rSling.Visible = 0
				rSling.visible = 1
				rightSlingShot.timerEnabled = 0
    End Select
    rStep = rStep + 1
End Sub

'*************** Triggers 
Sub leftOutLane_hit
	If Tilt = False Then
		If SpecialL.state = 1 Then special
		If lOutlane.state = 1 Then 
			addScore ((tScore + 1) * 100)
		Else	
			addScore 100
		End If
	End If
End Sub    

Sub rightOutLane_hit
	If Tilt = False Then 
		If SpecialR.state = 1 Then special
		If rOutlane.state = 1 Then 
			addScore ((tScore + 1) * 100)
		Else	
			addScore 100
		End If
	End If
End Sub

Sub triggers_Hit(Index)
	If Tilt = False Then
		addScore 10
		tempUp
	End If
End Sub

Dim launched
Sub ballLaunchedTrigger_hit
	launched = launched + 1
	If CLng(launched) Mod 2 > 0 Then 
		If B2SOn Then DOF 161 ,DOFPulse
	End If
	If B2SOn Then DOF 127, DOFOff
End Sub

'**************Alternator Unit
Dim alternate
Sub alternator
	alternate = alternate + 1
	If alternate > 1 Then alternate = 0
	If alternate = 1 Then
		rOutlane.state = 1
		Kicker10Left.state = 1
		Kicker10Right.state = 0
		lOutlane.state = 0
	Else
		rOutlane.state = 0
		Kicker10Right.state = 1
		lOutlane.state = 1
		Kicker10Left.state = 0
	End If
End Sub

'**************Special
Sub special
	addCredit = 1
	scoreMotor5.enabled = 1
	playsound SoundFXDOF("Knocker",128,DOFPulse,DOFKnocker)
	If B2SOn Then DOF 151, DOFPulse
End Sub

Dim ballInLane
'************Check if Ball Out of Launch Lane
Sub ballsInPlay_hit
	If ballREnabled = 1 Then
		ballREnabled = 0
		ballInLane = False
	End If
	firstBallOut = 1
	drainGate.collidable = 1
End Sub

'**************Destroy Balls to Start New Game
Sub ballKiller_Hit()
	ballKiller.destroyball
End Sub


'***********Ball Lift Speed Limiter to Prevent Loss of Balls
Dim lifter
lifter = 0
Sub ballLiftTimer_Timer
	lifter = lifter + 1
	If lifter = 1 Then
		If ballinPlay < balls Then
			playFieldSound "BallLift" , 0, Plunger, .7 
			ballLift.CreateBall
			ballLift.kick 0,88+(Int(RND*5)),1.56
			ballinPlay = ballinPlay + 1 
			If ballInPlay = balls and ballOut = 0 Then 

			End If
		End If
	End If
	If lifter = 2 Then 
		lifter = 0
		ballLiftTimer.enabled = 0
	End If
End Sub



'***************Score Motor Run one full rotation
Dim scoreMotorCount, addCredit
Sub scoremotor5_Timer
	scoreMotorCount = scoreMotorCount + 1
	If scoremotorCount = 5 Then
		If addCredit = 1 Then
			credit = credit + 1
			If showDT = False Then PlayReelSound "Reel5", bgcrReel Else PlayReelSound "Reel5", dtcrReel
			If B2SOn Then DOF 199, DOFOn	
			If credit > 15 then credit = 15
			CreditReel.setValue (credit)
			If B2SOn Then 
				controller.B2SSetCredits credit
				If credit > 0 Then DOF 127, DOFOn
			End If
			addCredit = 0
		End If
		scoreMotorCount = 0
		scoreMotor5.enabled = 0
	End If	
End Sub

'****************Score Motor Run Timer
Dim bellRing, scoreMotorLoop, motorOn
Sub scoreMotor_timer
	scoreMotorLoop = scoreMotorLoop + 1
	motorOn = 1

	If scoreMotorLoop < 6 Then playFieldSound "ScoreMotorSingleFire", 0, SoundPointScoreMotor, .3
		tb.text = "sml=" & scoreMotorLoop
'  These Flags are passed by scores with multiple of 10, 100 or 1000
	If flag1 = 1 or flag10 = 1 or flag100 = 1 or flag1000 = 1 Then

		Select Case scoreMotorLoop
			Case 1: totalUp point
			Case 2: totalUp point
			Case 3: If bellRing > 2 Then totalUp point
			Case 4: If bellRing > 3 Then totalUp point
			Case 5: If bellRing > 4 Then totalUp point
			Case 6: scoreMotorLoop = 0 
					flag1 = 0
					flag10 = 0 
					flag100 = 0
					flag1000 = 0
					motorOn = 0
					scoremotor.enabled = 0
		End Select
	End If
End Sub

'***************Scoring Routine
Dim flag1, flag10, flag100, flag1000, point, point1, ones
Sub addScore(points) 
	If tilt = False Then

		If points < 9 Then
'			Number Matching, decrement the match unit for each 1 point score roll over from 10 to 1
			matchNumber = matchNumber - 1
			If matchNumber < 1 Then matchNumber = 10
			bellRing = (points)
			ones = ((Score(1) + points) Mod 10)


			If bellRing = 1 Then totalUp(1): alternator
			If bellRing > 1 Then point = 1: flag1 = 1: If motorOn = 0 Then scoreMotor.enabled = 1
			Exit Sub
		End If		

		If points > 9 and points <100 Then
			bellRing = (points / 10)
			If bellRing > 1 Then point = 10: flag10 = 1: If motorOn = 0 Then scoreMotor.enabled = 1
			If bellRing = 1 Then 
				totalUp(10)
				If chime = 0 Then
					playSound "Bell10"
				Else 
					If B2SOn Then DOF 153,DOFPulse
				End If
			End If
			Exit Sub
		End If

		If points > 99 and points < 1000 Then 
			bellRing = (points / 100)
			If bellRing > 1 Then point = 100: flag100 = 1: If motorOn = 0 Then scoreMotor.enabled = 1
			If bellRing = 1 Then 
				totalUp(100)
				If chime = 0 Then
					playSound "Bell100"
				Else 
					If B2SOn Then DOF 154,DOFPulse
				End If
			End If
			Exit Sub
		End If

	End If
End Sub

Dim replayX,  replay(7), repAwarded(5), topReplay
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

	If flag1 = 1 Then alternator

	If flag10 = 1 Then
		If chime = 0 Then
			playSound "Bell10"
		Else 
			If B2SOn Then DOF 153,DOFPulse
		End If
	End If

	If flag100 = 1 Then
		If chime = 0 Then
			playSound "Bell100"
		Else 
			If B2SOn Then DOF 154,DOFPulse
		End If
	End If

	score(player) = score(player) + points
	sReel.addValue(points)

	If B2SOn Then controller.B2SSetScorePlayer player, score(player)
		
	For replayX = rep(player) + 1 to topReplay
		If score(player) => replay(replayX) Then
			addCredit = 1
			rep(player) = rep(Player) + 1
			playsound soundFXDOF("Knocker",128,DOFPulse,DOFKnocker) 
		End If
	Next
End Sub



'***************Tilt
Dim tiltSens, tiltPenalty 
'**** Set tiltPenalty; 0 = loose current ball / 1 = end game
tiltPenalty = 1

Sub checkTilt
	If tilttimer.enabled = True Then 
		tiltSens = tiltSens + 1
		If tiltSens = 3 Then
		tilt = True
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

'***************Match Gottlieb Style
Dim matchDisplay
Sub match
	If matchNumber = (score(1) mod 10) or matchNumber - 10 = (score(1) mod 10) Then 
		addCredit = 1
		scoreMotor5.enabled = 1
		playsound SoundFXDOF("Knocker",128,DOFPulse,DOFKnocker)
		If B2SOn Then DOF 151, DOFPulse
	End If
	EVAL("match" & matchNumber).setValue (1)
	If B2SOn Then controller.B2SSetMatch 34, matchNumber
End Sub

'***************************************************Reset Reels Section*******************************************************

'This Sub looks at each individual digit in each players score and sets them in an array RScore.  If the value is >0 and <9
'then the players score is increased by one times the position value of that digit (ie 1 * 1000 for the 1000's digit)
'If the value of the digit is 9 then it subtracts 9 times the postion value of that digit (ie 9*100 for the 100's digit)
'so that the score is not rolled over and the next digit in line gets incremented as well (ie 9 in the 10's positon gets 
'incremented so the 100's position rolls up by one as well since 90 -> 100).  Lastly the RScore array values get incremented
'by one to get ready for the next pass.

Dim rScore(4,5), resetLoop, test, playerTest, resetFlag, reelFlag, reelStop, rep(4)
Sub countUp
	For playerTest = 1 to maxPlayers
		For test = 0 to 4
			rScore(playerTest,test) = Int(score(playerTest)/10^test) mod 10
		Next
	Next
	
	For playerTest = 1 to maxPlayers
		For x = 0 to 4
			If rscore(playerTest, x) > 0 And rscore(playerTest, x) < 9 Then score(playerTest) = score(playerTest) + 10^x
			If rScore(playerTest, x) = 9 Then score(playerTest) = score(playerTest) - (9 * 10^x)	
			If rScore(playerTest, x) > 0 Then rScore(playerTest, x) = rScore(playerTest, x) + 1
			If rScore(playerTest, x) = 10 Then rScore(playerTest, x) = 0
		Next 
	Next
	If score(1) = 0 and score(2) = 0 and score(3) = 0 and score(4) = 0 Then 
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
	For playerTest = 1 to maxPlayers
		If showDT = False Then 
			controller.B2SSetScorePlayer playerTest, score(playerTest)
		Else
			sReel.setvalue (score(playerTest))
		End If
	Next

	playfieldSound "ScoreMotorSingleFire", 0, SoundPointScoreMotor, .3

	If reelStop = 0 Then 
		If showDT = False Then 
			PlayReelSound "Reel1", bgs1Reel
		Else 
			PlayReelSound "Reel1", dts1Reel
		End If
	End If

	If reelFlag = 1 Then reelStop = 1

End Sub

'This Timer runs a loop that calls the CountUp and UpdateReels routines to step the reels up five times and Then
'check to see if they are all at zero during a two loop pause and then step them the rest of the way to zero

Dim testFlag
Sub resetReel_Timer
	resetLoop = resetLoop + 1
	If resetLoop = 1 and score(1) = 0 Then
		resetLoop = 0
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
			testFlag = 0
			resetReel.enabled = 0
			Exit Sub 	
	End Select
End Sub

'************************************************Post It Note Section**************************************************************************
'***************Static Post It Note Update
Dim  hsY, shift, scoreMil, score100K, score10K, scoreK, score100, score10, scoreUnit
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
	For hs = 1 to maxPlayers     					'look at 4 player scores		
		For chY = 0 to 4   					    	'look at all 5 saved high scores
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
'			playerEntry.visible = 1
'			playerEntry.image = "Player" & position(Flag)
'			TextBox3.text = ActiveScore(Flag) 'tells which high score is being entered
'			TextBox2.text = Flag
'			TextBox1.text =  Position(Flag) 'tells which player is entering values
			initial(activeScore(flag),1) = 1	'make first inital "A"
			For chY = 2 to 3
				initial(activeScore(flag),chY) = 0	'set other two to " "
			Next
			For chY = 1 to 3
				EVAL("Initial" & chY).image = hsIArray(initial(activeScore(flag),chY))		'display the initals on the tape
			Next
			initialTimer1.enabled = 1		'flash the first initial
			dynamicUpdatePostIt.enabled = 0		'stop the scrolling intials timer
			playsound SoundFXDOF("Knocker",128,DOFPulse,DOFKnocker)
			enableInitialEntry = True
		End If
End Sub


'************Enter Initials Keycode Subroutine
Dim initial(6,5), initialsDone
Sub enterInitials(keycode)
		If keyCode = leftFlipperKey Then 
			hsX = hsX - 1						'HSx is the inital to be displayed A-Z plus " "
			If hsX < 0 Then hsX = 26
			If hsI < 4 Then EVAL("Initial" & hsI).image = hsIArray(hsX)		'HSi is which of the three intials is being modified
			playSound "metalhit_thin"
		End If
		If keycode = rightFlipperKey Then
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
'				enableInitialEntry = False
'				highScoreDelay.enabled = 1
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
	playsound SoundFXDOF("Chime100",141,DOFPulse,DOFChimes)
	flag = flag - 1
	hsI = 1
	If flag < 0 Then flag = 0: Exit Sub
	If flag = 0 Then 					'exit high score entry mode and reset variables
		initialsDone = 1
		players = 0
		For eIX = 1 to 4
			activeScore(eIX) = 0
			position(eIX) = 0
		Next
		For eIX = 1 to 3
			EVAL("InitialTimer" & eIX).enabled = 0
		Next
'		playerEntry.visible = 0
		scoreUpdate = 0						'go to the highest score
		updatePostIt						'display that score
		highScoreDelay.enabled = 1
	Else
		showScore
'		playerEntry.image = "Player" & position(flag)
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
Dim hsCount
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

'**************************************************File Writing Section******************************************************

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

		For x = 1 to 32
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
		score(1) = cdbl (temp(31))
		pfOption = cdbl (temp(32))
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
		scoreFile.WriteLine score(1)
		scoreFile.WriteLine pfOption
		scoreFile.Close
	Set scoreFile = Nothing
	Set fileObj = Nothing

'This section of code writes a file in the Table Folder of VisualPinball that contains the High Score data for PinballY.
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

Sub turnOff
	For x = 1 to 3
		EVAL("Bumper" & x).hasHitEvent = 0
	Next
	If tiltPenalty = 1 then ballInPlay = Balls
  	leftFlipper.RotateToStart
	stopSound "FlipBuzzLA"
	stopSound "FlipBuzzLB"
	stopSound "FlipBuzzLC"
	stopSound "FlipBuzzLD"
	stopSound "FlipBuzzRA"
	stopSound "FlipBuzzRB"
	stopSound "FlipBuzzRC"
	stopSound "FlipBuzzRD"
	If B2SOn Then DOF 101, DOFOff
	If B2SOn Then DOF 111, DOFOff
	rightFlipper.RotateToStart
	If B2SOn Then DOF 102, DOFOff
	If B2SOn Then DOF 112, DOFOff	
End Sub 

'*****************************************************Supporting Code Written By Others*************************************

'*****************************Roth's drop target routine********************************
'How this all works
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

'These are the hit subs for the DT walls that start things off and send the number of the wall hit to the DTHit sub
'Sub WallTarget006_Hit : DTHit 06 : End Sub
'Sub WallTarget007_Hit : DTHit 07 : End Sub
'Sub WallTarget008_Hit : DTHit 08 : End Sub
'Sub WallTarget009_Hit : DTHit 09 : End Sub
'Sub WallTarget010_Hit : DTHit 10 : End Sub

' sub to raise all DTs at once
'Sub ResetDropsRoth(enabled)
'	if enabled then
'		DTRaise 06
'       DTRaise 07
'       DTRaise 08
'       DTRaise 09
'       DTRaise 10
'       PlaySoundAt SoundFX(DTResetSound,DOFDropTargets), vca_Target008
'   end if
'End Sub

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
Const DTHitSound = "Target"   'Drop Target Hit sound
Const DTMass = 0.2                  'Mass of the Drop Target (between 0 and 1), higher values provide more resistance

'An array of objects for each DT of (primary wall, secondary wall, primitive, switch, animate variable)
Dim DT001
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

Set DT001 = (new DropTarget)(MainTarget, MainTargetOffset, drop, 01, 0, false)

'An array of DT arrays
Dim DTArray
DTArray = Array(DT001)

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
	If DTEnableBrick = 1 and  perpvel > DTBrickVel and DTBrickVel <> 0  Then '  and cdist < 8
	    DTCheckBrick = 3
        Else
            DTCheckBrick = 1
        End If
    ElseIf perpvel > 0 and ((paravel > 0 and paravelafter > 0) or (paravel < 0 and paravelafter <  0)) Then
        DTCheckBrick = 4
    Else 
        DTCheckBrick = 0
    End If

	DTCheckBrick = 1
End Function

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
BallShadow = Array (BallShadow001,BallShadow002,BallShadow003,BallShadow004,BallShadow005)

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

'************************************************************************
'                         Ball Control - 3 Axis
'************************************************************************

Dim Cup, Cdown, Cleft, Cright, Zup, contball, contballinplay, ControlBall, bcboost
Dim bcvel, bcyveloffset, bcboostmulti
 
bcboost = 1 'Do Not Change - default setting
bcvel = 4 'Controls the speed of the ball movement
bcyveloffset = -0.014 'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
bcboostmulti = 3 'Boost multiplier to ball veloctiy (toggled with the B key)

Sub BallControl_Timer()
    If Contball and ContBallInPlay then
        If Cright = 1 Then
            ControlBall.velx = bcvel*bcboost
          ElseIf Cleft = 1 Then
            ControlBall.velx = -bcvel*bcboost
          Else
            ControlBall.velx=0
        End If
        If Cup = 1 Then
            ControlBall.vely = -bcvel*bcboost
          ElseIf Cdown = 1 Then
            ControlBall.vely = bcvel*bcboost
          Else
            ControlBall.vely = bcyveloffset
        End If
        If Zup = 1 Then
            ControlBall.velz = bcvel*bcboost
		Else
			ControlBall.velz = -bcvel*bcboost
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
    Vol = Csng(BallVel(ball) ^2 / 2000)
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
'Subtracting XVol or YVol from 1 yeilds an inverse response.

Const tnob = 5 ' total number of balls

'Change GHT, GHB and PFL values based upon the real pinball table dimensions.  Values are used by the GlassHit code.
Const GHT = 2	'Glass height in inches at top of real playfield
Const GHB = 2	'Glass height in inches at bottom of real playfield
Const PFL = 40	'Real playfield length in inches

ReDim rolling(tnob)
InitRolling

ReDim ArchRolling(tnob)
InitArchRolling

Dim ArchHit
Sub TopArch_Hit
	ArchHit = 1
	ArchTimer.Enabled = True
End Sub

Dim archCount
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
	paSub=35000	'Playsound pitch adder for subway rolling ball sound

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
		ElseIf BallVel(BOT(b)) > 1 AND BOT(b).z < -5 Then	'Ball on subway
			PlaySound("BallrollingA" & b), -1, Vol(BOT(b)) * 0.2 * xGain(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b))+paSub, 1, 0, 0	'Left & Right stereo or Top & Bottom stereo PF Speakers.			
		ElseIf rolling(b) = True OR BallVel(BOT(b)) < 0.1 AND BOT(b).z < -5 Then
'		ElseIf rolling(b) = True Then
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
		ElseIf BallVel(BOT(b)) > 1 AND BOT(b).z < -5 Then	'Ball on subway
			PlaySound("BallrollingA" & b), -1, Vol(BOT(b)) * 0.2 *    XVol(BOT(b))  *     YVol(BOT(b)), -1, 0, Pitch(BOT(b))+paSub, 1, 0, -1	'Top Left PF Speaker
			PlaySound("BallrollingB" & b), -1, Vol(BOT(b)) * 0.2 * (1-XVol(BOT(b))) *     YVol(BOT(b)),  1, 0, Pitch(BOT(b))+paSub, 1, 0, -1	'Top Right PF Speaker
			PlaySound("BallrollingC" & b), -1, Vol(BOT(b)) * 0.2 *    XVol(BOT(b))  * (1-YVol(BOT(b))), -1, 0, Pitch(BOT(b))+paSub, 1, 0,  1	'Bottom Left PF Speaker
			PlaySound("BallrollingD" & b), -1, Vol(BOT(b)) * 0.2 * (1-XVol(BOT(b))) * (1-YVol(BOT(b))),  1, 0, Pitch(BOT(b))+paSub, 1, 0,  1	'Bottom Right PF Speaker
		ElseIf rolling(b) = True OR BallVel(BOT(b)) < 0.1 AND BOT(b).z < -5 Then
'		ElseIf rolling(b) = True Then
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
'The Hit Subs use PlayFieldSoundAB that incorporates the balls velocity.

Sub aRubberPins_Hit(idx)
	PlayFieldSoundAB "pinhit_low", 0, 1
End Sub

Sub aRubberPosts_Hit(idx)
	PlayFieldSoundAB "pinhit_low", 0, 1
End Sub

Sub aMushroom_Hit(idx)
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
	If BallinPlay > 0 Then
	PlayFieldSoundAB "rubber2", 0, 0.1
	End If
End Sub

Sub RubberWheel_hit
	PlayFieldSoundAB "rubber_hit_2", 0, 0.5
End sub

Sub aPosts_Hit(idx)
	PlayFieldSoundAB "rubber2", 0, 1
End Sub

Sub aWoods_Hit(idx)
	PlayFieldSoundAB "Wood", 0, 1
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

    Sub ApronWalls_Hit
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

'New algorithm for OnBallBallCollision
'For stereo, xGain is a Playsound volume multiplier that provides a Constant Power pan.
'For quad, multiple PlaySound commands are launched together that are panned and faded to their maximum extents where PlaySound's PAN and FADE have the least error.
'XVol and YVol are Playsound volume multipliers that provide a Constant Power "pan" and "fade".
'Subtracting XVol or YVol from 1 yeilds an inverse response.

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
'Subtracting XVol or YVol from 1 yeilds an inverse response.

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
'Subtracting XVol or YVol from 1 yeilds an inverse response.

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

dim LFPress, RFPress, EOST, EOSA, EOSTnew, EOSAnew
dim FStrength, Frampup, FElasticity, EOSRampup, SOSRampup
dim RFEndAngle, LFEndAngle, LFCount, RFCount, LiveCatch

LFEndAngle = Leftflipper.EndAngle
RFEndAngle = RightFlipper.EndAngle

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

'********Need to have a flipper timer to check for these values
Sub flipperTimer_Timer
	Pgate.rotx = Gate0.CurrentAngle

	lFlip.rotz = leftflipper.currentangle 
	rFlip.rotz = rightflipper.currentangle 

	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle

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
		if abs(Flipper.currentAngle - Flipper.EndAngle) < 20 Then 'last 20 degrees of swing is not dealt with
			PartialFlipCoef = 0
		End If
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
'						tb.text = "In"
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
	If B2SOn Then controller.stop
	saveHighScore
End Sub


'***************Shooter Lane Gate Animation
Sub PGateTimer_Timer
Pgate.rotz = Gate0.CurrentAngle * .6
End Sub

'***********Rotate Flipper Shadow
Sub FlipperShadowUpdate_Timer
	FlipperLSh.RotZ = LeftFlipper.CurrentAngle
	FlipperRSh.RotZ = RightFlipper.CurrentAngle
End Sub