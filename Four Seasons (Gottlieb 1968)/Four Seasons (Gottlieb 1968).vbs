'*
'*        Gottlieb's Four Seasons (1968)
'*        
'*
'*

option explicit
Randomize
ExecuteGlobal GetTextFile("core.vbs")

On Error Resume Next
ExecuteGlobal GetTextFile("Controller.vbs")
If Err Then MsgBox "Unable to open Controller.vbs. Ensure that it is in the scripts folder."
On Error Goto 0

Const cGameName = "FourSeasons_1968"

Const ShadowFlippersOn = true
Const ShadowBallOn = true

Const ShadowConfigFile = false




Dim Controller	' B2S
Dim B2SScore	' B2S Score Displayed

Const HSFileName="FourSeasons_68VPX.txt"
Const B2STableName="FourSeasons_1968"
Const LMEMTableConfig="LMEMTables.txt"
Const LMEMShadowConfig="LMEMShadows.txt"
Dim EnableBallShadow
Dim EnableFlipperShadow

'* this value adjusts score motor behavior - 0 allows you to continue scoring while the score motor is running - 1 sets score motor to behave more like a real EM
Const ScoreMotorAdjustment=1

'* this is a debug setting to use an older scoring routine vs a newer score routine - don't change this value
Const ScoreAdditionAdjustment=1

Dim B2SOn		'True/False if want backglass

dim ScoreChecker
dim CheckAllScores
dim sortscores(4)
dim sortplayers(4)
Dim B2SFrameCounter
Dim BackglassBallFlagColor
Dim TextStr,TextStr2,TiltEndsGame
Dim i
Dim obj
Dim Points210counter
Dim Points500counter
Dim Points1000counter
Dim Points2000counter
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

Dim BonusBooster
Dim BonusBoosterCounter
Dim BonusCounter
Dim HoleCounter

Dim Ones
Dim Tens
Dim Hundreds
Dim Thousands

Dim Player
Dim Players

Dim ChimesOn

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
Dim Replay1Table(15)
Dim Replay2Table(15)
Dim Replay3Table(15)
Dim Replay4Table(15)
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

Dim LStep, LStep2, RStep, xx

Dim ReelCounter
Dim BallCounter
Dim BallReelAddStart(12)
Dim BallReelAddFrames(12)
Dim BallReelDropStart(12)
Dim BallReelDropFrames(12)

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
Dim tKickerTCount,BumperSetting

Dim SpinPos,SpinPos2,Spin,Spin2,Count,Count1,Count2,Count3,Reset,VelX,VelY,BallSpeed,LitSpinner,SpinArrow,TargetValue

Spin= Array("4","4","5","5","1","1","2","2","3","3","4","4","5","5","1","1","2","2","3","3")
Spin2=Array("100","100","10","10","100","100","10","10","100","100","10","10","100","100","999","999","100","100","10","10")

SpinArrow=Array(9,27,45,63,81,99,117,135,153,171,189,207,225,243,261,279,297,315,333,351)

Sub Table1_Init()

	Count=0
    Count1=0
    Count2=0
	Count3=0
    Reset=0
	ZeroToNineUnit=Int(Rnd*10)

	SpinPos=Int(Rnd*20)
	For X=0 To 19
		Spinner(X).IsDropped=True
		Spinner(X+20).IsDropped=True
		Spinner(X+40).enabled=False
	Next
	Spinner(SpinPos).IsDropped=False
	Spinner(SpinPos+20).IsDropped=False
	Spinner(SpinPos+40).enabled=True
	ArrowPrimitive.ObjRotZ=SpinArrow(SpinPos)

	SpinPos2=Int(Rnd*20)
	For X=0 To 19
		Spinner2(X).IsDropped=True
		Spinner2(X+20).IsDropped=True
		Spinner2(X+40).enabled=False
	Next
	Spinner2(SpinPos2).IsDropped=False
	Spinner2(SpinPos2+20).IsDropped=False
	Spinner2(SpinPos2+40).enabled=True
	ArrowPrimitive1.ObjRotZ=SpinArrow(SpinPos2)
	if Spin2(SpinPos2) <> 999 then
		TargetValue = Spin(SpinPos) * Spin2(SpinPos2)
	else 
		TargetValue = 500
	end if
	LoadEM
	LoadLMEMConfig2

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


	KickLight1.state=0
	KickLight2.state=0
	KickLight3.state=0
	KickLight4.state=0
	KickLight5.state=0
	KickLight6.state=0
	KickLight7.state=0

	ChimesOn=1
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
	TiltEndsGame=1
	AlternatingRelay=1
	SpecialLightOption=2
	BackglassBallFlagColor=1
	loadhs
	if HighScore=0 then HighScore=5000


	TableTilted=false
	TiltReel.SetValue(1)
	Match=int(Rnd*10)
	MatchReel.SetValue((Match)+1)

'	CanPlayReel.SetValue(0)
'	GameOverReel.SetValue(1)

	For each obj in PlayerHuds
		obj.SetValue(0)
	next
	For each obj in PlayerScores
		obj.ResetToZero
	next

	for each obj in bottgate
		obj.isdropped=true
    next

	for each obj in Bonus
		obj.state=0
	next
	for each obj in TargetSpecialLights
		obj.state=0
	next
	for each obj in TargetAdvanceLights
		obj.state=0
	next
	for each obj in RolloverAdvanceLights
		obj.state=0
	next

	For each obj in NumberLights
		obj.state=0
	next

	Replay1=Replay1Table(ReplayLevel)
	Replay2=Replay2Table(ReplayLevel)
	Replay3=Replay3Table(ReplayLevel)
	Replay4=Replay4Table(ReplayLevel)


	BonusCounter=0
	HoleCounter=0


	InstructCard.image="IC"+FormatNumber(BallsPerGame,0)+"0"

	RefreshReplayCard

	CurrentFrame=0
	
	Bumper1Light.state=0
	Bumper2Light.state=0
	Bumper3Light.state=0
	



	AdvanceLightCounter=0
	
	Players=0
	RotatorTemp=1
	InProgress=false
	TargetLightsOn=false

	ScoreText.text=HighScore


	If B2SOn Then

		if Match=0 then
			Controller.B2SSetMatch 10
		else
			Controller.B2SSetMatch Match
		end if
		Controller.B2SSetScoreRolloverPlayer1 0
		Controller.B2SSetScoreRolloverPlayer2 0
		Controller.B2SSetScoreRolloverPlayer3 0
		Controller.B2SSetScoreRolloverPlayer4 0

		
		Controller.B2SSetTilt 1
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
	bump3=1
	bump4=1
	InitPauser5.enabled=true
	If Credits > 0 Then DOF 109, 1
End Sub

Sub Table1_exit()
	savehs
	SaveLMEMConfig
	SaveLMEMConfig2
	If B2SOn Then Controller.Stop
end sub

Sub TimerR_timer
	if B2SOn then
		if raceRflag=1 then
			Controller.B2SSetData 11,0
			raceRflag=0
		else
			Controller.B2SSetData 11,1
			raceRflag=1
		end if
	end if
end sub

Sub TimerA_timer
	if B2SOn then
		if raceAflag=1 then
			Controller.B2SSetData 12,0
			raceAflag=0
		else
			Controller.B2SSetData 12,1
			raceAflag=1
		end if
	end if
end sub

Sub TimerC_timer
	if B2SOn then
		if raceCflag=1 then
			Controller.B2SSetData 13,0
			raceCflag=0
		else
			Controller.B2SSetData 13,1
			raceCflag=1
		end if
	end if
end sub

Sub TimerE_timer
	if B2SOn then
		if raceEflag=1 then
			Controller.B2SSetData 14,0
			raceEflag=0
		else
			Controller.B2SSetData 14,1
			raceEflag=1
		end if
	end if
end sub


Sub Table1_KeyDown(ByVal keycode)

	' GNMOD
	if EnteringInitials then
		CollectInitials(keycode)
		exit sub
	end if


	if EnteringOptions then
		CollectOptions(keycode)
		exit sub
	end if



	If keycode = PlungerKey Then
		Plunger.PullBack
		PlungerPulled = 1
	End If

	if keycode = LeftFlipperKey and InProgress = false then
		OperatorMenuTimer.Enabled = true
	end if
	' END GNMOD

	If keycode = LeftFlipperKey and InProgress=true and TableTilted=false Then
		LeftFlipper.RotateToEnd
		PlaySound "buzzL",-1
		PlaySound "FlipperUp"
		DOF 101, 1
	End If
    
	If keycode = RightFlipperKey  and InProgress=true and TableTilted=false Then
		RightFlipper.RotateToEnd

		PlaySound "FlipperUp"
		PlaySound "buzz",-1
		DOF 102, 1
	End If
    
	If keycode = LeftTiltKey Then
		Nudge 90, 2
		TiltIt
	End If
    
	If keycode = RightTiltKey Then
		Nudge 270, 2
		TiltIt
	End If
    
	If keycode = CenterTiltKey Then
		Nudge 0, 2
		TiltIt
	End If

	If keycode = MechanicalTilt Then
		TiltCount=2
		TiltIt
	End If

	If keycode = AddCreditKey or keycode = 4 then
		If B2SOn Then
			'Controller.B2SSetScorePlayer6 HighScore
			
		End If

		playsound "coinin"
		AddSpecial2
	end if

   if keycode = 5 then 
		playsound "coinin"
		AddSpecial2
		keycode= StartGameKey
	end if

	if keycode=StartGameKey and Credits>0 and InProgress=true and Players<4 and OperatorMenu=0 and Score(1)=0 and BallInPlay<2 then
		Credits=Credits-1
		If Credits < 1 Then DOF 109, 0
		CreditsReel.SetValue(Credits)
		Players=Players+1
		CanPlayReel.SetValue(Players)
		playsound "click"
		If B2SOn Then
			Controller.B2SSetCanPlay Players
			
			Controller.B2SSetCredits Credits
		End If
	end if

	if keycode=StartGameKey and Credits>0 and InProgress=false and Players=0 and EnteringOptions = 0 then
'GNMOD
		OperatorMenuTimer.Enabled = false
'END GNMOD
		Credits=Credits-1
		If Credits < 1 Then DOF 109, 0
		CreditsReel.SetValue(Credits)
		Players=1
'		CanPlayReel.SetValue(Players)
		MatchReel.SetValue(0)

		Player=1
		playsound "startup_norm"
		TempPlayerUp=Player
'		PlayerUpRotator.enabled=true
		rst=0
		BallInPlay=1
		InProgress=true
		resettimer.enabled=true
		BonusMultiplier=1
		TimerR.enabled=0
		TimerA.enabled=0
		TimerC.enabled=0
		TimerE.enabled=0
		If B2SOn Then
			Controller.B2SSetTilt 0
			Controller.B2SSetGameOver 0
			Controller.B2SSetMatch 0
			Controller.B2SSetCredits Credits
			
			Controller.B2SSetCanPlay 1
			Controller.B2SSetPlayerUp 1
			Controller.B2SSetBallInPlay BallInPlay
			Controller.B2SSetScoreRolloverPlayer1 0

			Controller.B2SSetData 11,0
			Controller.B2SSetData 12,0
			Controller.B2SSetData 13,0
			Controller.B2SSetData 14,0
			Controller.B2SSetData 81,1
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
				obj.SetValue(0)
			next
			For each obj in PlayerHUDScores
				obj.state=0
			next
			PlayerHuds(Player-1).SetValue(1)
			PlayerHUDScores(Player-1).state=1
			PlayerScores(Player-1).Visible=0
			PlayerScoresOn(Player-1).Visible=1
		end If
'		GameOverReel.SetValue(0)


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
		
		PlaySound"plungerrelease"
		Plunger.Fire
	End If

	if keycode = LeftFlipperKey then
		OperatorMenuTimer.Enabled = false
	end if

	' END GNMOD   
    
    
	If keycode = LeftFlipperKey and InProgress=true and TableTilted=false Then
		LeftFlipper.RotateToStart
		PlaySound "FlipperDown"
		StopSound "buzzL"
		DOF 101, 0
	End If
    
	If keycode = RightFlipperKey and InProgress=true and TableTilted=false Then
		RightFlipper.RotateToStart
        StopSound "buzz"
		PlaySound "FlipperDown"
		DOF 102, 0
	End If

End Sub



Sub Drain_Hit()
	Drain.DestroyBall
	PlaySound "fx_drain"
	DOF 108, 2
	Pause4Bonustimer.enabled=true
		
End Sub

Sub Trigger0_Unhit()
	DOF 112, 2		
End Sub

Sub Pause4Bonustimer_timer
	Pause4Bonustimer.enabled=0
	NextBallDelay.enabled=true
	
End Sub

'***********************
'     Flipper Logos
'***********************

Sub UpdateFlipperLogos_Timer
    LFLogo.RotZ = LeftFlipper.CurrentAngle
    RFlogo.RotZ = RightFlipper.CurrentAngle
    LFLogo1.RotZ = LeftFlipper.CurrentAngle
    RFlogo1.RotZ = RightFlipper.CurrentAngle

	PGate.Rotz = (Gate.CurrentAngle*.75) + 25
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
End Sub

'***********************
' slingshots
'

Sub RightSlingShot_Slingshot
    PlaySound "right_slingshot", 0, 1, 0.05, 0.05
	DOF 104, 2
    RSling0.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	AddScore 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSling0.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    PlaySound "left_slingshot",0,1,-0.05,0.05
	DOF 103, 2
    LSling0.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	AddScore 1
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
		Select Case idx
			Case 0,1: AddScore(1)
			Case 2,3,4,5,6,7: AddScore(10)
		end Select
	end if
end Sub

'***********************************
' Stationary Targets
'***********************************

Sub Target1_hit
	if TableTilted=false then
		if SpinnerSpecialLight.state=1 then
			SetMotor(TargetValue)
		else
			SetMotor(5)
		end if
	end if
end sub

sub Target2_hit
	if TableTilted=false then
		if SpinnerSpecialLight.state=1 then
			SetMotor(TargetValue)
		else
			SetMotor(5)
		end if
	end if

end sub

'***********************************
' Bumpers
'***********************************


Sub Bumper1_Hit
	If TableTilted=false then

		PlaySound "bumper1"
		DOF 105, 2
		bump1 = 1
		If Bumper1Light.state=1 then
			AddScore(10)
		else
			AddScore(1)
		end if
		
		
    end if
    
End Sub

Sub Bumper2_Hit
	If TableTilted=false then

		PlaySound "bumper1"
		DOF 106, 2
		bump2 = 1
		If Bumper2Light.state=1 then
			AddScore(100)
		else
			AddScore(10)
		end if
		
    end if
    
End Sub

Sub Bumper3_Hit
	If TableTilted=false then

		PlaySound "bumper1"
		DOF 107, 2
		bump3 = 1
		If Bumper3Light.state=1 then
			AddScore(10)
		else
			AddScore(1)
		end if
		
    end if
    
End Sub

Sub LightYellowBumpers

	
	BumpersOn

End Sub

Sub LightGreenBumpers


	
	BumpersOn
end sub

'****************************************************
' KICKERS
'****************************************************

sub Kicker1_Hit()
	Dim speedx,speedy,finalspeed
	tKickerTCount=0
	speedx=activeball.velx
	speedy=activeball.vely
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	if TableTilted=true then
		tKickerTCount=0
		Kicker1.Timerenabled=1
		exit sub
	end if
	if finalspeed>10 then
		Kicker1.Kick 0,0
		activeball.velx=speedx
		activeball.vely=speedy
	else
'		mHole.MagnetOn=1
		KickerHolder1.enabled=1
	end if
	
end sub

Sub KickerHolder1_timer
	If MotorRunning=0 then
		KickerHolder1.enabled=0
		Kicker1.timerinterval=(500)
		Kicker1.timerenabled=1
		tKickerTCount=0
		SetMotor(50)
		UpperLight1.state=1
		UpperLight5.state=1
		KickLight1.state=0
	end if
end sub

Sub Kicker1_Timer()
	tKickerTCount=tKickerTCount+1
	select case tKickerTCount
	case 1:
	'	mHole.MagnetOn=0
		Pkickarm1.rotz=15
		Playsound "saucer"
		DOF 111, 2
		Kicker1.kick 165,13
	case 2:
		Kicker1.timerenabled=0
		Pkickarm1.rotz=0
	end Select
	
end sub


sub Kicker2_Hit()
	Dim speedx,speedy,finalspeed
	tKickerTCount=0
	speedx=activeball.velx
	speedy=activeball.vely
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	if TableTilted=true then
		tKickerTCount=0
		Kicker2.Timerenabled=1
		exit sub
	end if
	if finalspeed>10 then
		Kicker2.Kick 0,0
		activeball.velx=speedx
		activeball.vely=speedy
	else
'		mHole2.MagnetOn=1
		KickerHolder2.enabled=1
	end if
	
end sub

Sub KickerHolder2_timer
	If MotorRunning=0 then
		KickerHolder2.enabled=0
		Kicker2.timerinterval=(500)
		Kicker2.timerenabled=1
		tKickerTCount=0
		SetMotor(50)
		UpperLight2.state=1
		UpperLight6.state=1
		KickLight2.state=0
	end if
end sub

Sub Kicker2_Timer()
	tKickerTCount=tKickerTCount+1
	select case tKickerTCount
	case 1:
	'	mHole.MagnetOn=0
		Pkickarm2.rotz=15
		Playsound "saucer"
		DOF 112, 2
		Kicker2.kick 195,13
	case 2:
		Kicker2.timerenabled=0
		Pkickarm2.rotz=0
	end Select

end sub

sub Kicker3_Hit()
	Dim speedx,speedy,finalspeed
	tKickerTCount=0
	speedx=activeball.velx
	speedy=activeball.vely
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	if TableTilted=true then
		tKickerTCount=0
		Kicker3.Timerenabled=1
		exit sub
	end if
	if finalspeed>10 then
		Kicker3.Kick 0,0
		activeball.velx=speedx
		activeball.vely=speedy
	else
'		mHole3.MagnetOn=1
		KickerHolder3.enabled=1
	end if
	
end sub

Sub KickerHolder3_timer
	If MotorRunning=0 then
		KickerHolder3.enabled=0
		Kicker3.timerinterval=(500)
		Kicker3.timerenabled=1
		tKickerTCount=0
		SetMotor(50)
		Bumper1Light.state=1
		Bumper3Light.state=1
		SpinnerSpecialLight.state=1
		KickLight3.state=0
	end if
end sub

Sub Kicker3_Timer()
	tKickerTCount=tKickerTCount+1
	select case tKickerTCount
	case 1:
	'	mHole.MagnetOn=0
		Pkickarm3.rotz=15
		Playsound "saucer"
		DOF 112, 2
		Kicker3.kick 170,13
	case 2:
		Kicker3.timerenabled=0
		Pkickarm3.rotz=0
	end Select
	
end sub

sub Kicker4_Hit()
	Dim speedx,speedy,finalspeed
	tKickerTCount=0
	speedx=activeball.velx
	speedy=activeball.vely
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	if TableTilted=true then
		tKickerTCount=0
		Kicker4.Timerenabled=1
		exit sub
	end if
	if finalspeed>10 then
		Kicker4.Kick 0,0
		activeball.velx=speedx
		activeball.vely=speedy
	else
'		mHole4.MagnetOn=1
		KickerHolder4.enabled=1
	end if
	
end sub

Sub KickerHolder4_timer
	If MotorRunning=0 then
		KickerHolder4.enabled=0
		Kicker4.timerinterval=(500)
		Kicker4.timerenabled=1
		tKickerTCount=0
		SetMotor(50)
		UpperLight3.state=1
		UpperLight7.state=1
		KickLight4.state=0
	end if
end sub

Sub Kicker4_Timer()

	tKickerTCount=tKickerTCount+1
	select case tKickerTCount
	case 1:
	'	mHole.MagnetOn=0
		Pkickarm4.rotz=15
		Playsound "saucer"
		DOF 112, 2
		Kicker4.kick 190,13
	case 2:
		Kicker4.timerenabled=0
		Pkickarm4.rotz=0
	end Select
	
end sub

sub Kicker5_Hit()
	Dim speedx,speedy,finalspeed
	tKickerTCount=0
	speedx=activeball.velx
	speedy=activeball.vely
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	if TableTilted=true then
		tKickerTCount=0
		Kicker5.Timerenabled=1
		exit sub
	end if
	if finalspeed>10 then
		Kicker5.Kick 0,0
		activeball.velx=speedx
		activeball.vely=speedy
	else
'		mHole5.MagnetOn=1
		KickerHolder5.enabled=1
	end if
	
end sub

Sub KickerHolder5_timer
	If MotorRunning=0 then
		KickerHolder5.enabled=0
		Kicker5.timerinterval=(500)
		Kicker5.timerenabled=1
		tKickerTCount=0
		SetMotor(50)
		UpperLight4.state=1
		UpperLight8.state=1
		KickLight5.state=0
	end if
end sub

Sub Kicker5_Timer()
	tKickerTCount=tKickerTCount+1
	select case tKickerTCount
	case 1:
	'	mHole.MagnetOn=0
		Pkickarm5.rotz=15
		Playsound "saucer"
		DOF 113, 2
		Kicker5.kick 190,13
	case 2:
		Kicker5.timerenabled=0
		Pkickarm5.rotz=0
	end Select
end sub
		
sub Kicker6_Hit()
	Dim speedx,speedy,finalspeed
	tKickerTCount=0
	speedx=activeball.velx
	speedy=activeball.vely
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	if TableTilted=true then
		tKickerTCount=0
		Kicker6.Timerenabled=1
		exit sub
	end if
	if finalspeed>10 then
		Kicker6.Kick 0,0
		activeball.velx=speedx
		activeball.vely=speedy
	else
'		mHole6.MagnetOn=1
		KickerHolder6.enabled=1
	end if
	
end sub

Sub KickerHolder6_timer
	If MotorRunning=0 then
		KickerHolder6.enabled=0
		Kicker6.timerinterval=(500)
		Kicker6.timerenabled=1
		tKickerTCount=0
		SetMotor(TargetValue)
		If KickLight6.state=1 then
			SpecialLight9.state=1
		end if
	end if
end sub

Sub Kicker6_Timer()
	tKickerTCount=tKickerTCount+1
	select case tKickerTCount
	case 1:
	'	mHole.MagnetOn=0
		Pkickarm6.rotz=15
		Playsound "saucer"
		DOF 113, 2
		Kicker6.kick 110,15
	case 2:
		Kicker6.timerenabled=0
		Pkickarm6.rotz=0
	end Select
end sub

sub Kicker7_Hit()
	Dim speedx,speedy,finalspeed
	tKickerTCount=0
	speedx=activeball.velx
	speedy=activeball.vely
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	if TableTilted=true then
		tKickerTCount=0
		Kicker7.Timerenabled=1
		exit sub
	end if
	if finalspeed>10 then
		Kicker7.Kick 0,0
		activeball.velx=speedx
		activeball.vely=speedy
	else
'		mHole5.MagnetOn=1
		KickerHolder7.enabled=1
	end if
	
end sub

Sub KickerHolder7_timer
	If MotorRunning=0 then
		KickerHolder7.enabled=0
		Kicker7.timerinterval=(500)
		Kicker7.timerenabled=1
		tKickerTCount=0
		SetMotor(TargetValue)
		If KickLight6.state=1 then
			SpecialLight9.state=1
		end if
	end if
end sub

Sub Kicker7_Timer()
	tKickerTCount=tKickerTCount+1
	select case tKickerTCount
	case 1:
	'	mHole.MagnetOn=0
		Pkickarm7.rotz=15
		Playsound "saucer"
		DOF 113, 2
		Kicker7.kick 240,15
	case 2:
		Kicker7.timerenabled=0
		Pkickarm7.rotz=0
	end Select
end sub

'************************************
'  Rollover lanes
'************************************

Sub TriggerCollection_Hit(idx)
	If TableTilted=false then
		DOF 200 + idx, 2
		If NumberLights(idx).state=1 then
			SetMotor(TargetValue)
		else
			SetMotor(5)
		end if
	end if	
end Sub


'**************************************
' arrow target
'**************************************

Sub Spinner_Hit(Idx)
	Dim TempPos
	
	If Idx>39 Then
		If Idx>59 Then
			VelX=activeball.VelX
			VelY=activeball.VelY
		End If
		Exit Sub
	End If
	If Idx>19 then
		TempPos=Idx-20
	else
		TempPos=Idx
	end if
	BallSpeed=Int(Abs(activeball.VelX*5))+(Abs(activeball.VelY*5))
	If BallSpeed<14 Then Exit Sub
	If Idx<20 Then
		SpinPos=SpinPos+1
		DOF 113, 2
		If SpinPos=20 Then SpinPos=0
		Spinner(Idx).IsDropped=True
		Spinner(Idx+20).IsDropped=True
		Spinner(Idx+40).enabled=False
'		SpinnerPrims(TempPos).TopVisible=false
	Else
		SpinPos=SpinPos-1
		DOF 113, 2
		If SpinPos=-1 Then SpinPos=19
		Spinner(Idx-20).IsDropped=True
		Spinner(Idx).IsDropped=True
		Spinner(Idx+20).enabled=False
'		SpinnerPrims(TempPos).TopVisible=false
	End If
	Spinner(SpinPos).IsDropped=False
	Spinner(SpinPos+20).IsDropped=False
	Spinner(SpinPos+40).enabled=True
	ArrowPrimitive.ObjRotZ=SpinArrow(SpinPos)
	VelX=VelX*0.8
	VelY=VelY*0.8
	activeball.VelX=VelX
	activeball.VelY=VelY
	Count3=0
	if Spin2(SpinPos2) <> 999 then
		TargetValue = Spin(SpinPos) * Spin2(SpinPos2)
	else 
		TargetValue = 500
	end if

End Sub


Sub Spinner2_Hit(Idx)
	Dim TempPos
	
	If Idx>39 Then
		If Idx>59 Then
			VelX=activeball.VelX
			VelY=activeball.VelY
		End If
		Exit Sub
	End If
	If Idx>19 then
		TempPos=Idx-20
	else
		TempPos=Idx
	end if
	BallSpeed=Int(Abs(activeball.VelX*5))+(Abs(activeball.VelY*5))
	If BallSpeed<14 Then Exit Sub
	If Idx<20 Then
		SpinPos2=SpinPos2+1
		DOF 113, 2
		If SpinPos2=20 Then SpinPos2=0
		Spinner2(Idx).IsDropped=True
		Spinner2(Idx+20).IsDropped=True
		Spinner2(Idx+40).enabled=False
'		SpinnerPrims(TempPos).TopVisible=false
	Else
		SpinPos2=SpinPos2-1
		DOF 113, 2
		If SpinPos2=-1 Then SpinPos2=19
		Spinner2(Idx-20).IsDropped=True
		Spinner2(Idx).IsDropped=True
		Spinner2(Idx+20).enabled=False
'		SpinnerPrims(TempPos).TopVisible=false
	End If
	Spinner2(SpinPos2).IsDropped=False
	Spinner2(SpinPos2+20).IsDropped=False
	Spinner2(SpinPos2+40).enabled=True
	ArrowPrimitive1.ObjRotZ=SpinArrow(SpinPos2)
	VelX=VelX*0.8
	VelY=VelY*0.8
	activeball.VelX=VelX
	activeball.VelY=VelY
	Count3=0
	if Spin2(SpinPos2) <> 999 then
		TargetValue = Spin(SpinPos) * Spin2(SpinPos2)
	else 
		TargetValue = 500
	end if

End Sub


Sub CheckSpecial()

End Sub

'**************************************


Sub AddSpecial()
	PlaySound"knocker"
	DOF 110, 2
	Credits=Credits+1
	DOF 109, 1
	if Credits>15 then Credits=15
	If B2SOn Then
		Controller.B2SSetCredits Credits
	End If
	CreditsReel.SetValue(Credits)
End Sub

Sub AddSpecial2()
	PlaySound"click"
	Credits=Credits+1
	DOF 109, 1
	if Credits>15 then Credits=15
	If B2SOn Then
		Controller.B2SSetCredits Credits
	End If
	CreditsReel.SetValue(Credits)
End Sub

Sub LightYellowBumper
	Bumper2Light.state=0
	Select Case ZeroToNineUnit
		case 1,5,9:
			

		case 0,2,6:
			If BallsPerGame=3 then
				Bumper2Light.state=1
			end if
		case 3:
			If BumperSetting > 0 then
				Bumper2Light.state=1
			end if
		case 7:
			If BumperSetting > 1 then
				Bumper2Light.state=1
			end if

		case 4,8:
			Bumper2Light.state=1
	end select
	If ZeroToNineUnit=1 then
		If AlternatingRelay=1 then
			KickLight6.state=1
			KickLight7.state=0
		else
			KickLight6.state=0
			KickLight7.state=1
		end if
	else
		KickLight6.state=0
		KickLight7.state=0
	end if

end sub

Sub AdvanceZeroToNine
	ZeroToNineUnit=ZeroToNineUnit+1
	if ZeroToNineUnit>9 then
		ZeroToNineUnit=0
	end if
	LightYellowBumper
end sub

Sub ToggleAlternatingRelay
	if AlternatingRelay=1 then
		AlternatingRelay=2
	else
		AlternatingRelay=1
	end if

	
end sub

Sub ToggleLitSpinnerNumber

end sub



Sub ResetBallDrops
	If SpecialLight9.state=0 then
		For each obj in NumberLights
			obj.state=0
		next
		KickLight1.state=1
		KickLight2.state=1
		KickLight3.state=1
		KickLight4.state=1
		KickLight5.state=1
		Bumper1Light.state=0
		Bumper3Light.state=0
		SpinnerSpecialLight.state=0
	else
		SpecialLight9.state=0
	end if

	HoleCounter=0
	
End Sub


Sub LightsOut
	for each obj in Bonus
		obj.state=0
	next
	for each obj in HorseshoeLights
		obj.state=0
	next
	BonusCounter=0
	HoleCounter=0
	Bumper1Light.state=0
	Bumper2Light.state=0


end sub

Sub ResetBalls()

	TempMultiCounter=BallsPerGame-BallInPlay

	ResetBallDrops
	BonusMultiplier=1
	TableTilted=false
	TiltReel.SetValue(0)
	If B2Son then
		Controller.B2SSetTilt 0
		If BallInPlay=BallsPerGame then
			Controller.B2SSetData 79,1
		else
			Controller.B2SSetData 79,0
		end if
	end if
	PlasticsOn
	'CreateBallID BallRelease
	Ballrelease.CreateSizedBall 25
    Ballrelease.Kick 40,7
	DOF 111, 2
'	BallInPlayReel.SetValue(BallInPlay)
	InstructCard.image="IC"+FormatNumber(BallsPerGame,0)+FormatNumber(BallInPlay,0)

	

End Sub





sub resettimer_timer
    rst=rst+1
	If rst=1 then
		PlayerUpRotator.enabled=true
	end if
	if rst=7 then
		PlayerUpRotator.enabled=true
	end if

	if rst>10 and rst<21 then
		ResetReelsToZero(1)
	end if
	if rst>20 and rst<31 then
		ResetReelsToZero(2)
	end if
  
    if rst=32 then
    playsound "StartBall1"
    end if
    if rst=35 then
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

	end if
	If reelzeroflag=2 then
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
		Replay4Paid(i)=false
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
	for each obj in NumberLights
		obj.state=1
	next
	for each obj in AllRolloverSpecials
		obj.state=0
	next
	SpinnerSpecialLight.state=0

	BonusCounter=0
	BallCounter=0
	TargetLeftFlag=1
	TargetCenterFlag=1
	TargetRightFlag=1
	TargetSequenceComplete=0
'	IncreaseBonus
'	ToggleBumper
	ResetBalls
End sub

sub nextball
	If SpecialLight9.state=0 then
		Player=Player+1
	end if
	If Player>Players Then
		BallInPlay=BallInPlay+1
		If BallInPlay>BallsPerGame then
			PlaySound("MotorLeer")
			InProgress=false
			
			If B2SOn Then
				Controller.B2SSetGameOver 1
				Controller.B2SSetPlayerUp 0
				Controller.B2SSetBallInPlay 0
				Controller.B2SSetCanPlay 0
				Controller.B2SSetData 81,0
				Controller.B2SSetData 82,0
				Controller.B2SSetData 83,0
				Controller.B2SSetData 84,0
				Controller.B2SSetData 79,0
			End If
			For each obj in PlayerHuds
				obj.SetValue(0)
			next
			For each obj in PlayerHUDScores
				obj.state=0
			next
			If Table1.ShowDT = True then
				For each obj in PlayerScores
					obj.visible=1
				Next
				For each obj in PlayerScoresOn
					obj.visible=0
				Next
			end If
'			GameOverReel.SetValue(1)
			InstructCard.image="IC"+FormatNumber(BallsPerGame,0)+"0"

'			BallInPlayReel.SetValue(0)
'			CanPlayReel.SetValue(0)
			LeftFlipper.RotateToStart
			RightFlipper.RotateToStart
			LightsOut
			BumpersOff
			PlasticsOff
			checkmatch
			CheckHighScore
			Players=0
			TimerR.enabled=1
			TimerA.enabled=1
			TimerC.enabled=1
			TimerE.enabled=1
			HighScoreTimer.interval=100
			HighScoreTimer.enabled=True
		Else
			Player=1
			If B2SOn Then
				Controller.B2SSetPlayerUp Player
				Controller.B2SSetBallInPlay BallInPlay

			End If
			PlaySound("RotateThruPlayers")
			TempPlayerUp=Player
			PlayerUpRotator.enabled=true
			PlayStartBall.enabled=true
			For each obj in PlayerHuds
				obj.SetValue(0)
			next
			For each obj in PlayerHUDScores
				obj.state=0
			next
			If Table1.ShowDT = True then
				For each obj in PlayerScores
					obj.visible=1
				Next
				For each obj in PlayerScoresOn
					obj.visible=0
				Next
				PlayerHuds(Player-1).SetValue(1)
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
			Controller.B2SSetData 81,0
			Controller.B2SSetData 82,0
			Controller.B2SSetData 83,0
			Controller.B2SSetData 84,0
			Controller.B2SSetData 80+Player,1
		End If
		PlaySound("RotateThruPlayers")
		TempPlayerUp=Player
'		PlayerUpRotator.enabled=true
'		PlayStartBall.enabled=true
		For each obj in PlayerHuds
			obj.SetValue(0)
		next
		For each obj in PlayerHUDScores
			obj.state=0
		next
		If Table1.ShowDT = True then
			For each obj in PlayerScores
					obj.visible=1
			Next
			For each obj in PlayerScoresOn
					obj.visible=0
			Next
			PlayerHuds(Player-1).SetValue(1)
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
	if TableTilted=true and TiltEndsGame=1 then
		exit sub
	end if
	tempmatch=Int(Rnd*10)
	Match=tempmatch
	MatchReel.SetValue(tempmatch+1)
	
	If B2SOn Then
		If Match = 0 Then
			Controller.B2SSetMatch 10
		Else
			Controller.B2SSetMatch Match
		End If
	End if
	for i = 1 to Players
		if Match=(Score(i) mod 10) then
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
			TiltReel.SetValue(1)
			If TiltEndsGame=1 then
				BallInPlay=BallsPerGame
				If B2SOn Then
					Controller.B2SSetGameOver 1
					Controller.B2SSetPlayerUp 0
					Controller.B2SSetBallInPlay 0
					Controller.B2SSetCanPlay 0
				End If
				For each obj in PlayerHuds
					obj.SetValue(0)
				next
	'			GameOverReel.SetValue(1)
				InstructCard.image="IC"+FormatNumber(BallsPerGame,0)+"0"

			end if
			PlasticsOff
			BumpersOff

			LeftFlipper.RotateToStart
			RightFlipper.RotateToStart
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
	PlaySound("StartBall2-5")
end sub

Sub PlayerUpRotator_timer()
		If RotatorTemp<5 then
			TempPlayerUp=TempPlayerUp+1
			If TempPlayerUp>4 then
				TempPlayerUp=1
			end if
			For each obj in PlayerHuds
				obj.SetValue(0)
			next
			For each obj in PlayerHUDScores
				obj.state=0
			next
			If Table1.ShowDT = True then
				For each obj in PlayerScores
					obj.visible=1
				Next
				For each obj in PlayerScoresOn
					obj.visible=0
				Next
				PlayerHuds(TempPlayerUp-1).SetValue(1)
				PlayerHUDScores(TempPlayerUp-1).state=1
				PlayerScores(TempPlayerUp-1).visible=0
				PlayerScoresOn(TempPlayerUp-1).visible=1
			end If
			If B2SOn Then
				Controller.B2SSetPlayerUp TempPlayerUp
				Controller.B2SSetData 81,0
				Controller.B2SSetData 82,0
				Controller.B2SSetData 83,0
				Controller.B2SSetData 84,0
				Controller.B2SSetData 80+TempPlayerUp,1
			End If

		else
			if B2SOn then
				Controller.B2SSetPlayerUp Player
				Controller.B2SSetData 81,0
				Controller.B2SSetData 82,0
				Controller.B2SSetData 83,0
				Controller.B2SSetData 84,0
				Controller.B2SSetData 80+Player,1
			end if
			PlayerUpRotator.enabled=false
			RotatorTemp=1
			For each obj in PlayerHuds
				obj.SetValue(0)
			next
			For each obj in PlayerHUDScores
				obj.state=0
			next
			If Table1.ShowDT = True then
				For each obj in PlayerScores
					obj.visible=1
				Next
				For each obj in PlayerScoresOn
					obj.visible=0
				Next
				PlayerHuds(Player-1).SetValue(1)
				PlayerHUDScores(Player-1).state=1
				PlayerScores(Player-1).visible=0
				PlayerScoresOn(Player-1).visible=1
			end If
		end if
		RotatorTemp=RotatorTemp+1

	
	
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

	if EnableBallShadow=true then
		tempBS=1
	else
		tempBS=0
	end if
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
		EnableBallShadow = ShadowBallOn
		BallShadowUpdate.enabled = ShadowBallOn
		EnableFlipperShadow = ShadowFlippersOn
		FlipperLSh.visible = ShadowFlippersOn
		FlipperRSh.visible = ShadowFlippersOn
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
	if tempBS=0 then
		EnableBallShadow=false
		BallShadowUpdate.enabled=false
	else
		EnableBallShadow=true
	end if
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
		scorefile.writeline TiltEndsGame
		scorefile.writeline SpecialLightOption
		scorefile.writeline ReplayLevel
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
		TiltEndsGame=cdbl(temp4)
		SpecialLightOption=cdbl(temp5)
		ReplayLevel=cdbl(temp6)
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

sub InitPauser5_timer
		
		DisplayHighScore
		CreditsReel.SetValue(Credits)
		InitPauser5.enabled=false
end sub

Sub DisplayHighScore

	

end sub

sub BumpersOff
	
	Bumper1Light.visible=0
	
	Bumper2Light.visible=0
	
	Bumper3Light.visible=0
	
end sub

sub BumpersOn
	Bumper1Light.visible=1
	
	Bumper2Light.visible=1
	
	Bumper3Light.visible=1

end sub

Sub PlasticsOn

	For each obj in Flashers
		obj.visible=1
	next

		
end sub

Sub PlasticsOff

	For each obj in Flashers
		obj.visible=0
	next

end sub

Sub SetupReplayTables

	Replay1Table(1)=3800
	Replay1Table(2)=5900
	Replay1Table(3)=3800
	Replay1Table(4)=3900
	Replay1Table(5)=4100
	Replay1Table(6)=4400
	Replay1Table(7)=4600
	Replay1Table(8)=5200
	Replay1Table(9)=5300
	Replay1Table(10)=5500
	Replay1Table(11)=5800
	Replay1Table(12)=6000
	Replay1Table(13)=6300
	Replay1Table(14)=6600
	Replay1Table(15)=999000

	Replay2Table(1)=4400
	Replay2Table(2)=6500
	Replay2Table(3)=4400
	Replay2Table(4)=4500
	Replay2Table(5)=4700
	Replay2Table(6)=5000
	Replay2Table(7)=5200
	Replay2Table(8)=5800
	Replay2Table(9)=5900
	Replay2Table(10)=6100
	Replay2Table(11)=6400
	Replay2Table(12)=6600
	Replay2Table(13)=6900
	Replay2Table(14)=7200
	Replay2Table(15)=999000

	Replay3Table(1)=5000
	Replay3Table(2)=7100
	Replay3Table(3)=5000
	Replay3Table(4)=5100
	Replay3Table(5)=5300
	Replay3Table(6)=5600
	Replay3Table(7)=5800
	Replay3Table(8)=6400
	Replay3Table(9)=6500
	Replay3Table(10)=6700
	Replay3Table(11)=7000
	Replay3Table(12)=7200
	Replay3Table(13)=7500
	Replay3Table(14)=7800
	Replay3Table(15)=999000

	Replay4Table(1)=5600
	Replay4Table(2)=7700
	Replay4Table(3)=5600
	Replay4Table(4)=5700
	Replay4Table(5)=5900
	Replay4Table(6)=6200
	Replay4Table(7)=6400
	Replay4Table(8)=7000
	Replay4Table(9)=7100
	Replay4Table(10)=7300
	Replay4Table(11)=7600
	Replay4Table(12)=7800
	Replay4Table(13)=8100
	Replay4Table(14)=8400
	Replay4Table(15)=999000

	ReplayTableMax=2


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

			Case 1:
				AddScore(1)
				MotorRunning=0
				BumpersOn
				
			Case 2:
				MotorMode=1
				MotorPosition=2
				BumpersOff
			Case 3:
				MotorMode=1
				MotorPosition=3
				BumpersOff
			Case 4:
				MotorMode=1
				MotorPosition=4
				BumpersOff
			Case 5:
				MotorMode=1
				MotorPosition=5
				BumpersOff

			Case 10:
				AddScore(10)
				MotorRunning=0
				BumpersOn
				
			Case 20:
				MotorMode=10
				MotorPosition=2
				BumpersOff
			Case 30:
				MotorMode=10
				MotorPosition=3
				BumpersOff
			Case 40:
				MotorMode=10
				MotorPosition=4
				BumpersOff
			Case 50:
				MotorMode=10
				MotorPosition=5
				BumpersOff
			Case 100:
				AddScore(100)
				MotorRunning=0
				BumpersOn
			Case 200:
				MotorMode=100
				MotorPosition=2
				BumpersOff
			Case 300:
				MotorMode=100
				MotorPosition=3
				BumpersOff
			Case 400:
				MotorMode=100
				MotorPosition=4
				BumpersOff
			Case 500:
				MotorMode=100
				MotorPosition=5
				BumpersOff
			Case 1000:
				AddScore(1000)
				MotorRunning=0
				BumpersOn
			Case 2000:
				MotorMode=1000
				MotorPosition=2
				BumpersOff
			Case 3000:
				MotorMode=1000
				MotorPosition=3
				BumpersOff
			Case 4000:
				MotorMode=1000
				MotorPosition=4
				BumpersOff
			Case 5000:
				MotorMode=1000
				MotorPosition=5
				BumpersOff
		End Select
	End If
End Sub

Sub AddScoreTimer_Timer
	Dim tempscore
	
	
	If MotorRunning<>1 And InProgress=true then
		if queuedscore>=5000 then
			tempscore=5000
			queuedscore=queuedscore-5000
			SetMotor2(5000)
			exit sub
		end if
		if queuedscore>=4000 then
			tempscore=4000
			queuedscore=queuedscore-4000
			SetMotor2(4000)
			exit sub
		end if
				
		if queuedscore>=3000 then
			tempscore=3000
			queuedscore=queuedscore-3000
			SetMotor2(3000)
			exit sub
		end if
			
		if queuedscore>=2000 then
			tempscore=2000
			queuedscore=queuedscore-2000
			SetMotor2(2000)
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
		if queuedscore>=400 then
			tempscore=400
			queuedscore=queuedscore-400
			SetMotor2(400)
			exit sub
		end if
		if queuedscore>=300 then
			tempscore=300
			queuedscore=queuedscore-300
			SetMotor2(300)
			exit sub
		end if
		if queuedscore>=200 then
			tempscore=200
			queuedscore=queuedscore-200
			SetMotor2(200)
			exit sub
		end if
		if queuedscore>=100 then
			tempscore=100
			queuedscore=queuedscore-100
			SetMotor2(100)
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
		if queuedscore>=30 then
			tempscore=30
			queuedscore=queuedscore-30
			SetMotor2(30)
			exit sub
		end if
		if queuedscore>=20 then
			tempscore=20
			queuedscore=queuedscore-20
			SetMotor2(20)
			exit sub
		end if
		if queuedscore>=10 then
			tempscore=10
			queuedscore=queuedscore-10
			SetMotor2(10)
			exit sub
		end if

		if queuedscore>=5 then
			tempscore=5
			queuedscore=queuedscore-5
			SetMotor2(5)
			exit sub
		end if
		if queuedscore>=4 then
			tempscore=4
			queuedscore=queuedscore-4
			SetMotor2(4)
			exit sub
		end if
		if queuedscore>=3 then
			tempscore=3
			queuedscore=queuedscore-3
			SetMotor2(3)
			exit sub
		end if
		if queuedscore>=2 then
			tempscore=2
			queuedscore=queuedscore-2
			SetMotor2(2)
			exit sub
		end if
		if queuedscore>=1 then
			tempscore=1
			queuedscore=queuedscore-1
			SetMotor2(1)
			exit sub
		end if
	End If


end Sub

Sub ScoreMotorTimer_Timer
	If MotorPosition > 0 Then
		Select Case MotorPosition
			Case 5,4,3,2:
				If MotorMode=1000 Then 
					AddScore(1000)
				end if
				if MotorMode=100 then
					AddScore(100)
				End If
				if MotorMode=10 then
					AddScore(10)
				End if
				if MotorMode=1 then
					AddScore(1)
				End if
				MotorPosition=MotorPosition-1
			Case 1:
				If MotorMode=1000 Then 
					AddScore(1000)
				end if
				If MotorMode=100 then 
					AddScore(100)
				End If
				if MotorMode=10 then
					AddScore(10)
				End if
				if MotorMode=1 then
					AddScore(1)
				End if
				MotorPosition=0:MotorRunning=0:BumpersOn
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
		Case 1:
			PlayChime(10)
			Score(Player)=Score(Player)+1
			If BallsPerGame=3 then
				FireAlternatingRelay
			end if
		Case 10:
			PlayChime(100)
			Score(Player)=Score(Player)+10
'			debugscore=debugscore+10
			If BallsPerGame=3 then
				FireAlternatingRelay
			end if
		Case 100:
			PlayChime(1000)
			Score(Player)=Score(Player)+100
'			debugscore=debugscore+100
			If BallsPerGame=5 then
				FireAlternatingRelay
			end if
		Case 1000:
			PlayChime(1000)
			Score(Player)=Score(Player)+1000
'			debugscore=debugscore+1000
	End Select
	PlayerScores(Player-1).AddValue(x)
	PlayerScoresOn(Player-1).AddValue(x)
	If ScoreDisplay(Player)<100000 then
		ScoreDisplay(Player)=Score(Player)
	Else
		Score100K(Player)=Int(Score(Player)/100000)
		ScoreDisplay(Player)=Score(Player)-100000
	End If
	if Score(Player)=>100000 then
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
'	ScoreText.text=debugscore
End Sub

Sub AddScore2(x)
	Dim OldScore, NewScore, OldTestScore, NewTestScore
    OldScore = Score(Player)

	Select Case x
        Case 1:
            Score(Player)=Score(Player)+1
		Case 10:
			Score(Player)=Score(Player)+10
		Case 100:
			Score(Player)=Score(Player)+100
		Case 1000:
			Score(Player)=Score(Player)+1000
	End Select
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
		NewTestScore = NewTestScore - 100000
		OldTestScore = OldTestScore - 100000
	Loop While NewTestScore > 0

    OldScore = int(OldScore / 1)	' divide by 10 for games with fixed 0 in 1s position, by 1 for games with real 1s digits
    NewScore = int(NewScore / 1)	' divide by 10 for games with fixed 0 in 1s position, by 1 for games with real 1s digits
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore&", OldScore Mod 10="&OldScore Mod 10 & ", NewScore % 10="&NewScore Mod 10)

    if (OldScore Mod 10 <> NewScore Mod 10) then
		PlayChime(10)
		
    end if

    OldScore = int(OldScore / 10)
    NewScore = int(NewScore / 10)
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore)
    if (OldScore Mod 10 <> NewScore Mod 10) then
		PlayChime(10)
		AdvanceZeroToNine
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

	If B2SOn Then
		Controller.B2SSetScorePlayer Player, Score(Player)
	End If
'	EMReel1.SetValue Score(Player)
	PlayerScores(Player-1).AddValue(x)
	PlayerScoresOn(Player-1).AddValue(x)
End Sub



Sub PlayChime(x)
	if ChimesOn=0 then
		Select Case x
			Case 10
				If LastChime10=1 Then
					PlaySound SoundFXDOF("SpinACard_1_10_Point_Bell",141,DOFPulse,DOFChimes)
					LastChime10=0
				Else
					PlaySound SoundFXDOF("SpinACard_1_10_Point_Bell",141,DOFPulse,DOFChimes)
					LastChime10=1
				End If
			Case 100
				If LastChime100=1 Then
					PlaySound SoundFXDOF("SpinACard_100_Point_Bell",142,DOFPulse,DOFChimes)
					LastChime100=0
				Else
					PlaySound SoundFXDOF("SpinACard_100_Point_Bell",142,DOFPulse,DOFChimes)
					LastChime100=1
				End If
		
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

'*********************************************************************
'                 Positional Sound Playback Functions
'*********************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
	PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub


'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / table1.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / table1.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
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

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 5 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingSoundTimer_Timer()
    Dim BOT, b
    BOT = GetBalls

	' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub



'*****************************************
'	ninuzzu's	BALL SHADOW
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
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 6
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 6
        End If
        ballShadow(b).Y = BOT(b).Y + 12
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub

'*****************************************
'	Object sounds
'*****************************************

Sub Plastics_Hit (idx)
	PlaySound "woodhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
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
HSScore(1) = 7500
HSScore(2) = 7000
HSScore(3) = 6000
HSScore(4) = 5500
HSScore(5) = 5000

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

	for xfor = StartHSArray(LineNo) to EndHSArray(LineNo)
		Eval("HS"&xfor).image = GetHSChar(String, Index)
		Index = Index + 1
	next

End Sub

Sub NewHighScore(NewScore, PlayNum)
	if NewScore > HSScore(5) then
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
		PlaySound "DropTargetDropped"
	elseif keycode = RightFlipperKey Then
		' advance to next character
		AlphaStringPos = AlphaStringPos + 1
		if AlphaStringPos > len(AlphaString) or (AlphaStringPos = len(AlphaString) and InitialString = "") then
			' Skip the backspace if there are no characters to backspace over
			AlphaStringPos = 1
		end if
		SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
		PlaySound "DropTargetDropped"
	elseif keycode = StartGameKey or keycode = PlungerKey Then
		SelectedChar = MID(AlphaString, AlphaStringPos, 1)
		if SelectedChar = "_" then
			InitialString = InitialString & " "
			PlaySound("Ding10")
		elseif SelectedChar = "<" then
			InitialString = MID(InitialString, 1, len(InitialString) - 1)
			if len(InitialString) = 0 then
				' If there are no more characters to back over, don't leave the < displayed
				AlphaStringPos = 1
			end if
			PlaySound("Ding100")
		else
			InitialString = InitialString & SelectedChar
			PlaySound("Ding10")
		end if
		if len(InitialString) < 3 then
			SetHSLine 3, InitialString & SelectedChar
		End If
	End If
	if len(InitialString) = 3 then
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
				PlaySound("Ding1000")
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
Const OptionLinesToMark="111000011"
Const OptionLine1="" 'do not use this line
Const OptionLine2="" 'do not use this line
Const OptionLine3="" 'do not use this line
Const OptionLine4=""
Const OptionLine5=""
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
				if Replay3Table(ReplayLevel)=999000 then
					tempstring = FormatNumber(Replay1Table(ReplayLevel),0) + "/" + FormatNumber(Replay2Table(ReplayLevel),0)
				elseif Replay4Table(ReplayLevel)=999000 then
					tempstring = FormatNumber(Replay1Table(ReplayLevel),0) + "/" + FormatNumber(Replay2Table(ReplayLevel),0) + "/" + FormatNumber(Replay3Table(ReplayLevel),0)
				else
					tempstring = FormatNumber(Replay1Table(ReplayLevel),0) + "/" + FormatNumber(Replay2Table(ReplayLevel),0) + "/" + FormatNumber(Replay3Table(ReplayLevel),0) + "/" + FormatNumber(Replay4Table(ReplayLevel),0)
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
		PlaySound "DropTargetDropped"
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
		PlaySound "DropTargetDropped"
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
		elseif CurrentOption = 4 then
			if TiltEndsGame=0 then
				TiltEndsGame=1
			else
				TiltEndsGame=0
			end if
			DisplayAllOptions
		
			
		elseif CurrentOption = 8 or CurrentOption = 9 then
				if OptionCHS=1 then
					HSScore(1) = 7500	
					HSScore(2) = 7000
					HSScore(3) = 6000
					HSScore(4) = 5500
					HSScore(5) = 5000

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
				InstructCard.image="IC"+FormatNumber(BallsPerGame,0)+"0"
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

