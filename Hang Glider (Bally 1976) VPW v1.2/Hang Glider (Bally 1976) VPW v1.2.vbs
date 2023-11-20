'*		Bally's Hang Glider (1976)
'*		https://www.ipdb.org/machine.cgi?id=1112
'*		Table primary scripted by Loserman76
'*		New Playfield by updon
'
'VPW Hang Glider Team
'********************
'Project Lead - Benji

'VPW presents Hang Glider by Bally, In honor and memoriam of Jeff Whitehead, aka loserman76, who passed on Nov 16th 2021.
'loserman76 made the original VPX version of Hang Glider and was a longstanding champion of EM machines in virtual pinball (and prolifically so, making literally hundreds of EM virtual tables), and although none of us knew him personally he was often seen in the forums offering his expert help and advice, and was happy for us to start work on this some months ago. He will be missed and our thoughts are with his family.

'VPW change log
'001-004 - Benji - Intitial import of baked geometry and low rez images. Added nfozzzy physics and fleep code (most not assigned yet). 
'005 - apophis - added RTX BS
'006 - Benji - adjusted flipper mesh height. Adjust transparency of plastic over center kicker,changed background color to black, added holes in playfield mesh and added hole meshes
'007 - gtxjoe - scripted spinner and standup prims. added shot tester (W E R Y U I P A S F G H J K).  Standup 002-225 and DropTgt need new pivot point. Drops not scripted yet
'008 - Benji - Changed pivot of all drops and targets 'one more time'...
'009 - gtxjoe - Added roth drop target scripting.  Drop targets work but sometimes hard to drop.  Not sure why.
'011 - Benji - Playfield hole texture adjusted, apron mesh/render fixed/moved VPX GI deleted. DL changed to 1 on all imported/baked prims
'012 - gtxjoe - Worked on drop target animation.  Works but target primitives need to be rotated 180 degrees.
'013 - benji - targets rotated 180 degrees vn_kicker mesh added and vn_KickerArm needs to be animated. Baked sling rubber meshes imported to layer 5, need to be hooked up
'014 - gtxjoe - vn_KickerArm and sling rubber meshes animated. There is temp "vn_KickerArmTEST" to show the kicker arm animation - can be deleted after reviewing
'016 - benji - New half-rez baked render set with adjusted moodier lighting. Added Rubbers collection for fleep hit sounds. Hooked up fleep slingshot shounds. Added more visible playfield holes. Removed old non collidable slings
'				Imported baked GI bulbs as separate mesh, settting DL to 1. Moved them down a bit. Adjusted center kicker plywood hole mesh and texture. Adjusted Apron mesh and texture.
'017 - Benji - more fiddling with baked lighting. added Credit light mesh on hapron  with on/off image that needs to be hooked up.
'018 - gtxjoe - Scripted Credit Light, Gottlieb chime sounds and Drop target reset sound
'019 - Benji - adjusted flipper mesh height from playfield. tweaked some render things
'020 - scottacus - flipper tricks and the DT code.  If you part out posts those could be dampened.
'021 - apophis - Added GI lights back in on layer 9, set all halo heights to -1. Updated RTX BS implementation.
'022 - apophis - Updated RTX BS so that shadows don't disappear when ball over playfield mesh hole. 
'023 - apophis - Updated ambient shadow so that it becomes less dark when close to RTX GI light sources 
'024 - Sixtoe - Fixed object & graphics conflicts, tweaked rollovers, minor adjustments
'025 - apophis - added drop target shadow functionality
'026 - apophis - fixed drop target shadow heights. Added rubberizer, target bouncer, and coil ramp up options. Fixed a lot of Fleep sound implementation issues. Updated flipper tricks scripts. Tweaked some physics parameters.
'027 - leojreimroc - Added VR Room.  Added Buzzer sound for "Over the Top".  Slightly enlarge Apron.
'028 - leojreimroc - Added bar and Fancy Minimal Room
'029 - leojreimroc - Removed Bar Room.  Disabled Bumpers and Slingshots during tilt.  Slightly adjusted VR Cabinet size.
'030 - Sixtoe - Religned primitives, added missing rubber post, realigned all targets, realigned rubber band targets, adjust centre vuk hole, expanded rubber slings, realigned drop target shadows
'031 - Sixtoe - fixed stupid script mistake, removed redundant images
'032 - Wylte - ShadowConfigFile disabled (may require deletion of HangGlider_76VPX HS file, sorry), Ballsize and Ballmass constants added, Dynamic Shadows updated, GILight002 & 003 added (still no vpx lights behind dt's), 
'033 - Sixtoe - Added missing GI lights and bulb primitives, added top of inlane rounded top after ball stopped balancing on it!,
'1.0 Release
'1.1 - Sixtoe - Updated DOF by Outhere, Updated playfield by BrandonLaw, reduced image file sizes so table smaller in general.
'1.2 - Sixtoe - Fixed desktop reel showing on cabinet & vr version

'DOF UPDATED BY OUTHERE
'101 Left Flipper
'102 Right Flipper
'103 droptargetreset
'104 vn_Kicker
'105 Spinner
'121 Slingshot Left
'122 Slingshot Right
'124 Bumper Left
'125 Bumper Center
'123 Bumper Right
'128 Ball Release
'141 Chimes
'142 Chimes 


option explicit
Randomize


Const Ballsize = 50
Const Ballmass = 1

ExecuteGlobal GetTextFile("core.vbs")

On Error Resume Next
ExecuteGlobal GetTextFile("Controller.vbs")
If Err Then MsgBox "Unable to open Controller.vbs. Ensure that it is in the scripts folder."
On Error Goto 0

Const cGameName = "HangGlider_1976"

Const TestMode = 0		'Set to 0 to disable.  1 to enable

'********************
'Options
'********************

'----- VR Room -----
Const VRRoom = 0					' 0 = off, 1 = Minimal Room, 2 = Fancy Minimal Room (Room by b4ast1)

'----- General Sound Options -----
Const VolumeDial = 0.8				' Recommended values should be no greater than 1.

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's)
									'2 = flasher image shadow, but it moves like ninuzzu's
'----- Phsyics Mods -----
Const RubberizerEnabled = 1			'0 = normal flip rubber, 1 = more lively rubber for flips, 2 = a different rubberizer
Const FlipperCoilRampupMode = 1   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)
Const TargetBouncerEnabled = 2 		'0 = normal standup targets, 1 = bouncy targets, 2 = orig TargetBouncer
Const TargetBouncerFactor = 1.1 	'Level of bounces. Recommmended value of 0.7 when TargetBouncerEnabled=1, and 1.1 when TargetBouncerEnabled=2







'*******************************************
'  Constants and Global Variables
'*******************************************

Dim Controller	' B2S
Dim B2SScore	' B2S Score Displayed
Const HSFileName="HangGlider_76VPX.txt"
Const B2STableName="HangGlider_1976"
'Const LMEMTableConfig="LMEMTables.txt"
'Const LMEMShadowConfig="LMEMShadows.txt"
'Dim EnableBallShadow
'Dim EnableFlipperShadow

Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height

'* this value adjusts score motor behavior - 0 allows you to continue scoring while the score motor is running - 1 sets score motor to behave more like a real EM
Const ScoreMotorAdjustment=1

'* this is a debug setting to use an older scoring routine vs a newer score routine - don't change this value
Const ScoreAdditionAdjustment=1

dim ScoreChecker
dim CheckAllScores
dim sortscores(4)
dim sortplayers(4)
Dim B2SOn
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
Dim CardLightOption
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
dim race5flag

Dim LStep, LStep2, RStep, xx

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

Dim ChimesOn

Dim TempLightTracker

Dim TargetLeftFlag
Dim TargetCenterFlag
Dim TargetRightFlag

Dim SpecialLightsFlag

Dim AlternatingRelay

Dim ZeroToNineUnit

Dim StarTriggerValue

Dim Kicker1Hold,Kicker2Hold, Kicker3Hold, Kicker4Hold, Kicker5Hold
Dim KickerHold1,KickerHold2,KickerHold3,KickerHold4,KickerHold5

Dim mHole,mHole2

Dim TargetSequenceCompleted,SpecialLightCounter,HighComplete,LowComplete
Dim ArrowTargetCounter1, ArrowTargetCounter2, ArrowTargetCounter3, ArrowTargetCounter4, tKickerTCount

Dim SpinPos,Spin,Count,Count1,Count2,Count3,Reset,VelX,VelY,LitSpinner,SpecialOption, dooralreadyopen, bgpos, GoldBonusCounter, ScoreMotorStepper

Dim Object

'***********Rotate Spinner

Dim Angle

Sub SpinnerTimer_Timer
	SpinnerPlate.Rotx = Spinner1.CurrentAngle
'	Angle = (sin (sw5.CurrentAngle-180))
'	SpinnerRod.TransX = sin( (sw5.CurrentAngle+180) * (2*PI/360)) * 12
'	SpinnerRod.TransZ = sin( (sw5.CurrentAngle- 90) * (2*PI/360)) * 3.5
'	Dim SpinnerRadius: SpinnerRadius=7
End Sub


Spin=Array("5","6","7","8","9","10","J","Q","K","A")

Dim ReplayBalls1, ReplayBalls2
ReplayBalls1=Array(0,80000,120000)
ReplayBalls2=Array(0,100000,120000)

Sub Table1_Init()
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


	LoadEM
'	LoadLMEMConfig2
	
	Count=0
    Count1=0
    Count2=0
	Count3=0
    Reset=0
	ZeroToNineUnit=Int(Rnd*10)
	Kicker1Hold=0

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
	TiltEndsGame=0
	ChimesOn=1
	SpecialOption=10
	AlternatingRelay=1
	CardLightOption=1
	BackglassBallFlagColor=1
	loadhs
	if HighScore=0 then HighScore=5000


	TableTilted=false
	
	Match=int(Rnd*10)
	MatchReel.SetValue((Match)+1)

	SpinPos=int(Rnd*10)

'	CanPlayReel.SetValue(0)
	GameOverReel.SetValue(1)
	BallInPlayReel.SetValue(0)
	TiltReel.SetValue(1)

	For each obj in PlayerHuds
		obj.state=0
	next
	For each obj in PlayerScores
		obj.ResetToZero
	next
	for each obj in ShadowDT
		obj.visible=True
	Next


 	dooralreadyopen=0


	Replay1=Replay1Table(ReplayLevel)
	Replay2=Replay2Table(ReplayLevel)
	Replay3=Replay3Table(ReplayLevel)
	Replay4=Replay4Table(ReplayLevel)


	BonusCounter=0
	HoleCounter=0


	InstructCard.image="IC"
	
	RefreshReplayCard

	CurrentFrame=0
	
	Bumper1Light.state=0


	AdvanceLightCounter=0

	If VRRoom > 0 Then
		for each Object in VRBGGameOver : object.visible = 1 : next
		for each Object in VRBGTilt : object.visible = 1 : next
		FlasherMatch

		cred =reels(4, 0)
		reels(4, 0) = 0
		SetDrum -1,0,  0
		SetReel 0,-1,  Credits
		reels(4, 0) = Credits	
	End If

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

		'Controller.B2SSetScore 3,HighScore
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
	If B2SOn then
		Controller.B2SSetData 60+SpinPos,1
	end if

	If Credits > 0 Then DOF 129, DOFOn:vn_CreditLight.Image = "b_CreditLightOn" 'CreditLight.state=1
End Sub

Sub Table1_exit()
	savehs
'	SaveLMEMConfig
'	SaveLMEMConfig2
	If B2SOn Then Controller.Stop
end sub

Sub BlinkingLogo_timer
	LogoReel.SetValue(int((Rnd * 100) / 25))
end sub


Sub TimerTitle1_timer
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

Sub TimerTitle2_timer
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

Sub TimerTitle3_timer
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

Sub TimerTitle4_timer
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

Sub TimerTitle5_timer
	if B2SOn then
		if race5flag=1 then
			Controller.B2SSetData 15,0
			race5flag=0
		else
			Controller.B2SSetData 15,1
			race5flag=1
		end if
	end if
end sub


Sub Table1_KeyDown(ByVal keycode)

	TestTableKeyDownCheck keycode

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
		If VRRoom > 0 Then
			TimerVRPlunger.Enabled = True
			TimerVRPlunger2.Enabled = False
		End If
	End If

'********************

'	if keycode = LeftFlipperKey and InProgress = false then
'		OperatorMenuTimer.Enabled = true
'	end if
	' END GNMOD


'	If keycode = LeftFlipperKey and InProgress=true and TableTilted=false Then
'		LeftFlipper.RotateToEnd
'		PlaySound "buzzL",-1
'		PlaySound SoundFXDOF("FlipperUp",101,DOFOn, DOFFlippers)
'	End If
   
'	If keycode = RightFlipperKey  and InProgress=true and TableTilted=false Then
'		RightFlipper.RotateToEnd

'		PlaySound SoundFXDOF("FlipperUp",102,DOFOn, DOFFlippers)
'		PlaySound "buzz",-1
		
'	End If

'**********************

Const ReflipAngle = 20

	if keycode = LeftFlipperKey and InProgress = false then
		OperatorMenuTimer.Enabled = true
	end if
	' END GNMOD

	If keycode = LeftFlipperKey and InProgress=true and TableTilted=false Then
		LFPress = 1
		lf.fire
    
		LeftFlipper.RotateToEnd

		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If

		DOF 101, DOFON
		PlaySoundAtVolLoops "buzzL",LeftFlipper,0.05,-1
	End If
    
	If keycode = RightFlipperKey  and InProgress=true and TableTilted=false Then
		RFPress = 1
		rf.fire		
		RightFlipper.RotateToEnd

		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If

		DOF 102, DOFON
		PlaySoundAtVolLoops "buzz",LeftFlipper,0.05,-1	
	End If


'*******************
    
	If keycode = LeftTiltKey Then
		Nudge 90, 2
		TiltIt
		SoundNudgeLeft
	End If
    
	If keycode = RightTiltKey Then
		Nudge 270, 2
		TiltIt
		SoundNudgeRight
	End If
    
	If keycode = CenterTiltKey Then
		Nudge 0, 2
		TiltIt
		SoundNudgeCenter
	End If

	If keycode = MechanicalTilt Then
		TiltCount=2
		TiltIt
	End If


	If keycode = AddCreditKey or keycode = 4 then
		If B2SOn Then
			'Controller.B2SSetScorePlayer6 HighScore
			
		End If

		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select

		AddSpecial2
	end if

   if keycode = 5 then 
		AddSpecial2
		keycode= StartGameKey
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	end if


   if keycode = StartGameKey and Credits>0 and InProgress=true and Players>0 and Players<4 and BallInPlay<2 then
		Credits=Credits-1
		If Credits <1 Then DOF 129, DOFOff:vn_CreditLight.Image = "b_CreditLightOff" ':CreditLight.state=0
		CreditsReel.SetValue(Credits)
		EMReel7.SetValue(Credits)
		Players=Players+1
		for each obj in CanPlayLights
			obj.state=0
		next
		CanPlayLights(Players-1).state=1
		If VRRoom > 0 Then
			FlasherPlayers
			cred =reels(4, 0)
			reels(4, 0) = 0
			SetDrum -1,0,  0
			SetReel 0,-1,  Credits
			reels(4, 0) = Credits
		End If
		
		SoundStartButton
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
		If Credits < 1 Then DOF 129, DOFOff:vn_CreditLight.Image = "b_CreditLightOff" ':CreditLight.state=0
		CreditsReel.SetValue(Credits)
		Players=1
		for each obj in CanPlayLights
			obj.state=0
		next
		CanPlayLights(Players-1).state=1
		MatchReel.SetValue(0)
		Player=1
		RolloverReel1.SetValue(0)
		RolloverReel2.SetValue(0)
		RolloverReel3.SetValue(0)
		RolloverReel4.SetValue(0)
		playsound "startup_norm"
		TempPlayerUp=Player
'		PlayerUpRotator.enabled=true
		GameOverReel.SetValue(0)
		rst=0
		BallInPlay=1
		InProgress=true
		resettimer.enabled=true
		BonusMultiplier=1
		TiltOff

		If VRRoom > 0 Then
			for each Object in VRBGTilt : object.visible = 0 : next
			for each Object in VRBGGameOver : object.visible = 0 : next
			for each Object in VRBGMatch : object.visible = 0 : next
			for each Object in VRBGShootAgain : object.visible = 0 : Next
			for each Object in VRBG100kAll : object.visible = 0 : next
			FlasherPlayers
			FlasherCurrentPlayer
			cred =reels(4, 0)
			reels(4, 0) = 0
			SetDrum -1,0,  0
			SetReel 0,-1,  Credits
			reels(4, 0) = Credits
		End If


		If B2SOn Then
			Controller.B2SSetTilt 0
			Controller.B2SSetGameOver 0
			Controller.B2SSetMatch 0
			Controller.B2SSetCredits Credits
			'Controller.B2SSetScore 3,HighScore
			Controller.B2SSetCanPlay 1
			Controller.B2SSetPlayerUp 1
			'Controller.B2SSetBallInPlay BallInPlay
			Controller.B2SSetScoreRolloverPlayer1 0
			Controller.B2SSetScoreRolloverPlayer2 0
			Controller.B2SSetScoreRolloverPlayer3 0
			Controller.B2SSetScoreRolloverPlayer4 0
			Controller.B2SSetData 99,1
			Controller.B2SSetData 11,0
			Controller.B2SSetData 12,0
			Controller.B2SSetData 13,0
			Controller.B2SSetData 14,0
			Controller.B2SSetData 15,0
		End If
		If Table1.ShowDT = True then
			For each obj in PlayerScores
'				obj.ResetToZero
				obj.Visible=true
			next


			For each obj in PlayerHuds
				obj.state=0
			next

			PlayerHuds(Player-1).state=1
			
		end If
	end if

	If VRRoom > 0 Then
		If keycode = LeftFlipperKey Then 
			Primary_flipper_button_left.X = 2109.804 + 2
		End If
		If keycode = RightFlipperKey Then
			Primary_flipper_button_right.X = 2104.841 - 2
		End If
		If keycode = StartGameKey Then
			Start_button.y = -245.1216 - 1
		End If
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

    
End Sub

Sub Table1_KeyUp(ByVal keycode)

	TestTableKeyUpCheck keycode

	' GNMOD
	if EnteringInitials then
		exit sub
	end if

	If keycode = PlungerKey Then
		
		if PlungerPulled = 0 then
			exit sub
		end if
		
		SoundPlungerPull
		Plunger.Fire
	End If

	If VRRoom > 0 Then
		If keycode = PlungerKey Then
			TimerVRPlunger.Enabled = False
			TimerVRPlunger2.Enabled = True
		End If
	End If

	if keycode = LeftFlipperKey then
		OperatorMenuTimer.Enabled = false
	end if

	' END GNMOD  

	If keycode = LeftFlipperKey and InProgress=true and TableTilted=false Then
   DOF 101, DOFOff
		LeftFlipper.RotateToStart
		StopSound "buzzL"
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
		
		
	End If
    
	If keycode = RightFlipperKey and InProgress=true and TableTilted=false Then
   DOF 102, DOFOff
		RightFlipper.RotateToStart
        StopSound "buzz"
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
		
	End If

	If VRRoom > 0 Then
		If keycode = LeftFlipperKey Then 
			Primary_flipper_button_left.X = 2109.804
		End If
		If keycode = RightFlipperKey Then
			Primary_flipper_button_right.X = 2104.841
		End If
		If keycode = StartGameKey Then
			Start_button.y = -245.1216
		End If
	End If

    If keycode = 203 then cLeft = 0' Left Arrow
 
    If keycode = 200 then cUp = 0' Up Arrow
 
    If keycode = 208 then cDown = 0' Down Arrow
 
    If keycode = 205 then cRight = 0' Right Arrow

    If keycode = 52 Then Zup = 0' Period

End Sub



Sub Drain_Hit()
	Drain.DestroyBall
	RandomSoundDrain Drain
	DOF 137, DOFPulse
	Pause4Bonustimer.enabled=true
		
End Sub

Sub Trigger1_Unhit()
	DOF 138, DOFPulse
End Sub

Sub Pause4Bonustimer_timer
	Pause4Bonustimer.enabled=0
	ScoreGoldBonus
	
End Sub

'***********************
'     Flipper Logos
'***********************

Sub UpdateFlipperLogos_Timer
	LFlip.RotZ = LeftFlipper.CurrentAngle
	RFlip.RotZ = RightFlipper.CurrentAngle
'	LFlip1.RotZ = LeftFlipper.CurrentAngle
'	RFlip1.RotZ = RightFlipper.CurrentAngle
	PGate.Rotz = (Gate.CurrentAngle*.75) + 25
	PGate1.Rotz = (Gate002.CurrentAngle*.75) + 25
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle

	SpinnerPlate.RotX = -Spinner1.currentangle
End Sub

'***********************
'     Flipper Collide
'***********************

Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
	if RubberizerEnabled = 1 then Rubberizer(parm)
	if RubberizerEnabled = 2 then Rubberizer2(parm)
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
	if RubberizerEnabled = 1 then Rubberizer(parm)
	if RubberizerEnabled = 2 then Rubberizer2(parm)
End Sub




'*******************************************
'  VPW Rubberizer by Iaakki
'*******************************************

' iaakki Rubberizer
sub Rubberizer(parm)
	if parm < 10 And parm > 2 And Abs(activeball.angmomz) < 10 then
		'debug.print "parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = activeball.angmomz * 1.2
		activeball.vely = activeball.vely * 1.2
		'debug.print ">> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	Elseif parm <= 2 and parm > 0.2 and activeball.vely < 0 Then
		'debug.print "* parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = activeball.angmomz * -1.1
		activeball.vely = activeball.vely * 1.4
		'debug.print "**** >> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	end if
end sub

' apophis Rubberizer
sub Rubberizer2(parm)
	if parm < 10 And parm > 2 And Abs(activeball.angmomz) < 10 then
		'debug.print "parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = -activeball.angmomz * 2
		activeball.vely = activeball.vely * 1.2
		'debug.print ">> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	Elseif parm <= 2 and parm > 0.2 and activeball.vely < 0 Then
		'debug.print "* parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = -activeball.angmomz * 0.5
		activeball.vely = activeball.vely * (1.2 + rnd(1)/3 )
		'debug.print "**** >> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	end if
end sub

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


'***********************
' slingshots
'

Sub RightSlingShot_Slingshot
	If TableTilted = False Then
		RandomSoundSlingshotRight Sling1
		DOF 122, DOFPulse
	'    RSling0.Visible = 0
	'    RSling1.Visible = 1
		vn_RSling0.Visible = 0
		vn_RSling1.Visible = 1
		sling1.TransZ = -20
		RStep = 0
		RightSlingShot.TimerEnabled = 1
		AddScore 10
	End If
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:vn_RSLing1.Visible = 0:vn_RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:vn_RSLing2.Visible = 0:vn_RSling0.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0    
'        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
'        Case 4:RSLing2.Visible = 0:RSling0.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
	End Select
    RStep = RStep + 1
End Sub


Sub LeftSlingShot_Slingshot
	If TableTilted = False Then
		RandomSoundSlingshotLeft Sling2
		DOF 121, DOFPulse
	'    LSling0.Visible = 0
	'    LSling1.Visible = 1
		vn_LSling0.Visible = 0
		vn_LSling1.Visible = 1
		sling2.TransZ = -20
		LStep = 0
		LeftSlingShot.TimerEnabled = 1
		AddScore 10
	End If
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
'        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
'        Case 4:LSLing2.Visible = 0:LSLing0.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
        Case 3:vn_LSLing1.Visible = 0:vn_LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:vn_LSLing2.Visible = 0:vn_LSLing0.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0    
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
		DOF 124, DOFPulse
		bump1 = 1
		If Bumper1Light.state=1 then
			AddScore(100)
		else
			AddScore(100)
		end if
		
    end if  
End Sub

Sub Bumper2_Hit
	If TableTilted=false then

		RandomSoundBumperMiddle Bumper2
		DOF 123, DOFPulse
		bump1 = 1
		If Bumper2Light.state=1 then
			AddScore(100)
		else
			AddScore(100)
		end if	
		
    end if
    
End Sub

Sub Bumper3_Hit
	If TableTilted=false then

		RandomSoundBumperBottom Bumper3
		DOF 125, DOFPulse
		bump1 = 1
		If Bumper3Light.state=1 then
			AddScore(10)
		else
			AddScore(10)
		end if
	end if

End Sub



'************************************
'  Rollover lanes
'************************************

Sub TriggerCollection_Hit(idx)
	DOF 110 + idx, DOFPulse
	If TableTilted=false then
		
		
		Select Case idx
			case 0:
				AddScore(1000)
				If ExtraBall001.state=1 then 
					AddSpecial
				end if
			case 1:
				SetMotor(500)
				IncreaseGoldBonus

			case 2:
				SetMotor(500)
				IncreaseGoldBonus

			case 3:
				AddScore(1000)
				If ExtraBall004.state=1 then 
					AddSpecial
				end if
			case 4:
				SetMotor(500)
				IncreaseGoldBonus
				If RightAlleyLight.state=1 then 
					DoubleBonus.state=1
					SpinnerLight.state=1
				end if
		end select
		
	end if
end sub
'************************************
'  Buttons
'************************************
Sub Buttons_hit(idx)
	ButtonPrims(idx).z=-2.2
	if TableTilted=false then
		if ButtonLights(idx).state=1 then
			SetMotor(500)
			IncreaseGoldBonus
		end if
	end if
end sub
Sub Buttons_unhit(idx)
	ButtonPrims(idx).z=.5
end sub


'Sub Trigger001_Unhit
'	Button001.z=.5
'end sub
'
'Sub Trigger002_Unhit
'	Button002.z=.5
'end sub
'
'Sub Trigger003_Unhit
'	Button003.z=.5
'end sub
'
'Sub Trigger004_Unhit
'	Button004.z=.5
'end sub
'
'Sub Trigger005_Unhit
'	Button005.z=.5
'end sub
'
'Sub Trigger006_Unhit
'	Button006.z=.5
'end sub
'
'Sub Trigger007_Unhit
'	Button007.z=.5
'end sub
'
'Sub Trigger008_Unhit
'	Button008.z=.5
'end sub
'
'Sub Trigger009_Unhit
'	Button009.z=.5
'end sub
'
'Sub Trigger010_Unhit
'	Button010.z=.5
'end sub


						
'**************************************
Sub Postup

end Sub

Sub Postdown
end Sub		

'****************************************************



' Targets
'**************************************
Sub TargetCollection_hit(idx)
	if TableTilted=false then
		DOF 130+idx,DOFPulse
		
		Select case idx
			case 0,1,2,3,4:
				if BonusTargetLights(idx).state=1 then
					SetMotor(500)
					IncreaseGoldBonus
				else
					AddScore(100)
				end if
			case 5:
				SetMotor(500)
'				Target006.IsDropped=true
'				playsound "droptargetdropped"
				Cards001.state=1
				CheckCardLights
			case 6:
				SetMotor(500)
'				Target007.IsDropped=true
'				playsound "droptargetdropped"
				Cards002.state=1
				CheckCardLights
			case 7:
				SetMotor(500)
'				Target008.IsDropped=true
'				playsound "droptargetdropped"
				Cards003.state=1
				CheckCardLights
			case 8:
				SetMotor(500)
'				Target009.IsDropped=true
'				playsound "droptargetdropped"
				Cards004.state=1
				CheckCardLights
			case 9:
				SetMotor(500)
'				Target010.IsDropped=true
'				playsound "droptargetdropped"
				Cards005.state=1
				CheckCardLights
		end select
		
	end if
end sub


sub CheckCardLights
	If Cards001.state=1 AND Cards002.state=1 AND Cards003.state=1 AND Cards004.state=1 AND Cards005.state=1 then
		If KickerLight1.state=0 AND KickerLight2.state=0 AND ShootAgain.state=0 then
			KickerLight1.state=1
			
			DisplayAltRelay
		elseif ShootAgain.state=1 OR KickerLight1.state=1 then
			KickerLight1.state=0
			KickerLight2.state=1
		end if
	end if
end sub


Dim Target001Step	
Sub Target001_Hit
	vca_Target001.RotX = 5
	Target001Step = 0
	Target001.TimerEnabled = True
	'PlaySoundAt SoundFX("target",DOFTargets),Target001
End Sub			
Sub Target001_timer()
	Select Case Target001Step
		Case 1:vca_Target001.RotX = 3
        Case 2:vca_Target001.RotX = -2
        Case 3:vca_Target001.RotX = 1
        Case 4:vca_Target001.RotX = 0: Target001.TimerEnabled = False: Target001Step = 0
     End Select
	Target001Step = Target001Step + 1
End Sub

Dim Target002Step	
Sub Target002_Hit
	vca_Target002.RotX = 5
	Target002Step = 0
	Target002.TimerEnabled = True
	'PlaySoundAt SoundFX("target",DOFTargets),Target002
End Sub			
Sub Target002_timer()
	Select Case Target002Step
		Case 1:vca_Target002.RotX = 3
        Case 2:vca_Target002.RotX = -2
        Case 3:vca_Target002.RotX = 1
        Case 4:vca_Target002.RotX = 0: Target002.TimerEnabled = False: Target002Step = 0
     End Select
	Target002Step = Target002Step + 1
End Sub

Dim Target003Step	
Sub Target003_Hit
	vca_Target003.RotX = 5
	Target003Step = 0
	Target003.TimerEnabled = True
	'PlaySoundAt SoundFX("target",DOFTargets),Target003
End Sub			
Sub Target003_timer()
	Select Case Target003Step
		Case 1:vca_Target003.RotX = 3
        Case 2:vca_Target003.RotX = -2
        Case 3:vca_Target003.RotX = 1
        Case 4:vca_Target003.RotX = 0: Target003.TimerEnabled = False: Target003Step = 0
     End Select
	Target003Step = Target003Step + 1
End Sub

Dim Target004Step	
Sub Target004_Hit
	vca_Target004.RotX = -5
	Target004Step = 0
	Target004.TimerEnabled = True
	'PlaySoundAt SoundFX("target",DOFTargets),Target004
End Sub			
Sub Target004_timer()
	Select Case Target004Step
		Case 1:vca_Target004.RotX = -3
        Case 2:vca_Target004.RotX = 2
        Case 3:vca_Target004.RotX = -1
        Case 4:vca_Target004.RotX = 0: Target004.TimerEnabled = False: Target004Step = 0
     End Select
	Target004Step = Target004Step + 1
End Sub

Dim Target005Step	
Sub Target005_Hit
	vca_Target005.RotX = -5
	Target005Step = 0
	Target005.TimerEnabled = True
	'PlaySoundAt SoundFX("target",DOFTargets),Target005
End Sub			
Sub Target005_timer()
	Select Case Target005Step
		Case 1:vca_Target005.RotX = -3
        Case 2:vca_Target005.RotX = 2
        Case 3:vca_Target005.RotX = -1
        Case 4:vca_Target005.RotX = 0: Target005.TimerEnabled = False: Target005Step = 0
     End Select
	Target005Step = Target005Step + 1
End Sub



'******************************************
' Drop Targets

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

Dim DT006, DT007, DT008, DT009, DT010
Set DT006 = (new DropTarget)(WallTarget006, WallTarget006offset, vca_Target006, 06, 0, false)
Set DT007 = (new DropTarget)(WallTarget007, WallTarget007offset, vca_Target007, 07, 0, false)
Set DT008 = (new DropTarget)(WallTarget008, WallTarget008offset, vca_Target008, 08, 0, false)
Set DT009 = (new DropTarget)(WallTarget009, WallTarget009offset, vca_Target009, 09, 0, false)
Set DT010 = (new DropTarget)(WallTarget010, WallTarget010offset, vca_Target010, 10, 0, false)

Dim DTArray
DTArray = Array(DT006, DT007, DT008, DT009, DT010)

Sub WallTarget006_Hit : DTHit 06 : ShadowDT(0).visible=False : End Sub
Sub WallTarget007_Hit : DTHit 07 : ShadowDT(1).visible=False : End Sub
Sub WallTarget008_Hit : DTHit 08 : ShadowDT(2).visible=False : End Sub
Sub WallTarget009_Hit : DTHit 09 : ShadowDT(3).visible=False : End Sub
Sub WallTarget010_Hit : DTHit 10 : ShadowDT(4).visible=False : End Sub


Sub ResetDropsRoth(enabled)
     if enabled then
          DTRaise 06
          DTRaise 07
          DTRaise 08
          DTRaise 09
          DTRaise 10

          PlaySoundAt SoundFX(DTResetSound,DOFDropTargets), vca_Target008

	      for each obj in ShadowDT
			obj.visible=True
		  Next

     end if
End Sub


'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110                         'in milliseconds
Const DTDropUpSpeed = 40                         'in milliseconds
Const DTDropUnits = 44                         'VP units primitive drops
Const DTDropUpUnits = 10                         'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8                                 'max degrees primitive rotates when hit
Const DTDropDelay = 20                         'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40                         'time in milliseconds before target drops back to normal up position after the solendoid fires to raise the target
Const DTBrickVel = 30                                'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 1                        'Set to 0 to disable bricking, 1 to enable bricking
'Const DTHitSound = "targethit"        'Drop Target Hit sound
Const DTDropSound = "DropTarget_Down" 'Drop Target Drop sound
Const DTResetSound = "DropTarget_Up" 'Drop Target reset sound
Const DTHitSound = "Target_Hit_1"        'Drop Target Hit sound


Const DTMass = 0.2                                'Mass of the Drop Target (between 0 and 1), higher values provide more resistance

'******************************************************
'                                DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
        Dim i
        i = DTArrayID(switch)

'        PlaySoundAtVol  DTHitSound, Activeball, Vol(Activeball)*22.5
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

'Add Timer name DTAnim to editor to handle drop target animations
DTAnim.interval = 10
DTAnim.enabled = True

Sub DTAnim_Timer()
        DoDTAnim
End Sub

'Check if target is hit on it's face or sides and whether a 'brick' occurred
Function DTCheckBrick(aBall, dtprim) 
        dim bangle, bangleafter, rangle, rangle2, Xintersect, Yintersect, cdist, perpvel, perpvelafter, paravel, paravelafter
        rangle = (dtprim.rotz - 90) * 3.1416 / 180
        rangle2 = dtprim.rotz * 3.1416 / 180
'        bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
        bangleafter = Atn2(aBall.vely,aball.velx)

        Xintersect = (aBall.y - dtprim.y - tan(bangle) * aball.x + tan(rangle2) * dtprim.x) / (tan(rangle2) - tan(bangle))
        Yintersect = tan(rangle2) * Xintersect + (dtprim.y - tan(rangle2) * dtprim.x)

        cdist = Distance(dtprim.x, dtprim.y, Xintersect, Yintersect)

        perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
        paravel = cor.BallVel(aball.id) * sin(bangle-rangle)

        perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 
        paravelafter = BallSpeed(aBall) * sin(bangleafter - rangle)
		'debug.print "brick " & perpvel & " : " & paravel & " : " & perpvelafter & " : " & paravelafter

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

DTCheckBrick = 1

End Function




Sub DoDTAnim()
        Dim i
        For i=0 to Ubound(DTArray)
                DTArray(i).animate = DTAnimate(DTArray(i).primary,DTArray(i).secondary,DTArray(i).prim,DTArray(i).sw,DTArray(i).animate)
        Next
End Sub

Function DTAnimate(primary, secondary, prim, switch,  animate)
        dim transz
        Dim animtime, rangle
        rangle = prim.rotz * 3.1416 / 180

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
                PlaySoundAt SoundFX(DTDropSound,DOFDropTargets),prim
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
                        'controller.Switch(Switch) = 1
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

                if prim.transz > DTDropUpUnits then 
                        DTAnimate = -2
                        prim.rotx = 0
                        prim.roty = 0
                        primary.uservalue = gametime
                end if
                primary.collidable = 0
                secondary.collidable = 1
                'controller.Switch(Switch) = 0

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
'                DROP TARGET
'                SUPPORTING FUNCTIONS 
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

' Used for drop targets
Function Distance(ax,ay,bx,by)
        Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function


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

'******* for ball control script
Sub EndControl_Hit()                
    contBallinPlay = False
End Sub

'******************************************
' Spinners


Sub Spinner1_Spin()
	PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, Spinner1
	If TableTilted=false and MotorRunning=0 then
			DOF 105, 2
		if SpinnerLight.state=1 then
			AddScore(1000)
		else
			AddScore(100)
		end if
		
	end if

end Sub

'****************************************************
' Kickers

sub Kicker1_Hit()
	SoundSaucerLock
	Dim speedx,speedy,finalspeed
	speedx=activeball.velx
	speedy=activeball.vely
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	if TableTilted=true then
		tKickerTCount=0
		Kicker1.Timerenabled=1
		exit sub
	end if
	KickerHolder1.enabled=1
	
end sub

Sub KickerHolder1_timer
	If MotorRunning<>0 then EXIT SUB
	KickerHolder1.enabled=0
	Kicker1.timerenabled=1
	tKickerTCount=0
end sub

Sub Kicker1_Timer()
	tKickerTCount=tKickerTCount+1
	if tKickerTCount>7 then tKickerTCount=0
	select case tKickerTCount
		case 1:
			AddScore(100)
			If Cards001.state=1 then
				IncreaseGoldBonus
			end if
		case 2:
			AddScore(100)
			If Cards002.state=1 then
				IncreaseGoldBonus
			end if
		case 3:
			AddScore(100)
			If Cards003.state=1 then
				IncreaseGoldBonus
			end if
		case 4:
			AddScore(100)
			If Cards004.state=1 then
				IncreaseGoldBonus
			end if

		case 5:
			AddScore(100)
			If Cards005.state=1 then
				IncreaseGoldBonus
			end if
			If KickerLight1.state=1 then
				KickerLight1.state=0
				ShootAgain.state=1
				ShootAgainReel.SetValue(1)
				If VRRoom > 0 Then
					for each Object in VRBGShootAgain : object.visible = 1 : Next
				End If
				If B2SOn Then Controller.B2SSetShootAgain 1
				ResetDrops
			end if
			If KickerLight2.state=1 then
				AddSpecial
				KickerLight2.state=0
				ResetDrops
				DisplayAltRelay
			end if
		case 6:
'			Pkickarm1.rotz=15
			vn_KickerArm.roty = -15
			vn_KickerArmTEST.roty = -15
			'Playsound SoundFXDOF("saucer",138,DOFPulse,DOFContactors)
			DOF 104, 2
			Kicker1.kick 195,15
		case 7:
		Kicker1.timerenabled=0
'		Pkickarm1.rotz=0
		vn_KickerArm.roty = 0
		vn_KickerArmTEST.roty = 0
	end Select
	
end sub



'**************************************


Sub AddSpecial()
	Select Case SpecialOption
		case 0:
			PlaySound"knocker"
			DOF 140, DOFPulse
			Credits=Credits+1
			:vn_CreditLight.Image = "b_CreditLightOn" 'CreditLight.state=1
			DOF 129, DOFOn
			if Credits>15 then Credits=15
			If VRRoom > 0 Then
				cred =reels(4, 0)
				reels(4, 0) = 0
				SetDrum -1,0,  0

				SetReel 0,-1,  Credits
				reels(4, 0) = Credits
			End If
			If B2SOn Then
				Controller.B2SSetCredits Credits
			End If
			CreditsReel.SetValue(Credits)
		case 1:
			PlaySound"knocker"
			DOF 140, DOFPulse
			AddExtraBall
		case 2:
			SetMotor(50000)
	end select

End Sub

Sub AddSpecial2()
	PlaySound"click"
	Credits=Credits+1
	:vn_CreditLight.Image = "b_CreditLightOn" 'CreditLight.state=1
	DOF 129, DOFOn
	if Credits>15 then Credits=15
	If VRRoom > 0 Then
		cred =reels(4, 0)
		reels(4, 0) = 0
		SetDrum -1,0,  0

		SetReel 0,-1,  Credits
		reels(4, 0) = Credits
	End If
	If B2SOn Then
		Controller.B2SSetCredits Credits
	End If
	CreditsReel.SetValue(Credits)
End Sub

Sub AddExtraBall
	If BallInPlay=9 then exit sub
	BallInPlay=BallInPlay+1
	BallInPlayReel.SetValue(BallInPlay)
	If VRRoom > 0 Then
		FlasherBalls
	End If
	If B2SOn then Controller.B2SSetBallInPlay BallInPlay

end sub

Sub ToggleAlternatingRelay
	if AlternatingRelay=1 then
		AlternatingRelay=0
	else
		AlternatingRelay=1
	end if
	DisplayAltRelay
end sub

Sub AdvanceZeroToNine
	ZeroToNineUnit=ZeroToNineUnit+1
	if ZeroToNineUnit>9 then
		ZeroToNineUnit=0
	end if
	DisplayAltRelay
end sub

Sub DisplayAltRelay
	ExtraBall001.state=0

	ExtraBall004.state=0
	if AlternatingRelay=1 then
		if GoldBonusCounter=15 then ExtraBall001.state=1
		
	else
		if GoldBonusCounter=15 then ExtraBall004.state=1
		
	end if
	RightAlleyLight.state=0
	Select case CardLightOption
		case 0:
			if ZeroToNineUnit=0 OR ZeroToNineUnit=5 then
				RightAlleyLight.state=1
			end if
		case 1:
			if ZeroToNineUnit=0 OR ZeroToNineUnit=5 or ZeroToNineUnit=7 then
				RightAlleyLight.state=1
			end if
		case 2:
			if ZeroToNineUnit=0 OR ZeroToNineUnit=5 or ZeroToNineUnit=7 OR ZeroToNineUnit=2 OR ZeroToNineUnit=8 then
				RightAlleyLight.state=1
			end if
	end select
end sub	

sub LightBumpers


end sub

Sub Trigger0_Hit
	Set controlBall = ActiveBall
    contBallInPlay = True
	tb.Text = "hit"
End Sub

Sub CloseGateTrigger_Hit()

	if dooralreadyopen=1 then
		closeg.enabled=true

	end if
End Sub


 sub openg_timer

		openg.enabled=false
	

 end sub 

sub closeg_timer
 		closeg.enabled=false
end sub   

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
	For each obj in ButtonLights
		obj.state=0
	next
	For each obj in BonusTargetLights
		obj.state=0
	next
	Select case GoldBonusCounter
		case 1,6,11:
			ButtonLights(1).state=1
			ButtonLights(7).state=1
			BonusTargetLights(1).state=1
		case 2,7,12:
			ButtonLights(0).state=1
			ButtonLights(6).state=1
			BonusTargetLights(2).state=1		
		case 3,8,13:
			ButtonLights(2).state=1
			ButtonLights(4).state=1
			BonusTargetLights(4).state=1
		case 4,9,14:
			ButtonLights(3).state=1
			ButtonLights(8).state=1
			BonusTargetLights(3).state=1		
		case 5,10,15:
			ButtonLights(5).state=1
			ButtonLights(9).state=1
			BonusTargetLights(0).state=1		
	
	end select
	DisplayAltRelay
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
					For each obj in ButtonLights
						obj.state=0
					next
					For each obj in BonusTargetLights
						obj.state=0
					next
					Select case GoldBonusCounter
						case 1,6,11:
							ButtonLights(1).state=1
							ButtonLights(7).state=1
							BonusTargetLights(1).state=1
						case 2,7,12:
							ButtonLights(0).state=1
							ButtonLights(6).state=1
							BonusTargetLights(2).state=1		
						case 3,8,13:
							ButtonLights(2).state=1
							ButtonLights(4).state=1
							BonusTargetLights(4).state=1
						case 4,9,14:
							ButtonLights(3).state=1
							ButtonLights(8).state=1
							BonusTargetLights(3).state=1		
						case 5,10,15:
							ButtonLights(5).state=1
							ButtonLights(9).state=1
							BonusTargetLights(0).state=1		
	
					end select
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
					For each obj in ButtonLights
						obj.state=0
					next
					For each obj in BonusTargetLights
						obj.state=0
					next
					Select case GoldBonusCounter
						case 1,6,11:
							ButtonLights(1).state=1
							ButtonLights(7).state=1
							BonusTargetLights(1).state=1
						case 2,7,12:
							ButtonLights(0).state=1
							ButtonLights(6).state=1
							BonusTargetLights(2).state=1		
						case 3,8,13:
							ButtonLights(2).state=1
							ButtonLights(4).state=1
							BonusTargetLights(4).state=1
						case 4,9,14:
							ButtonLights(3).state=1
							ButtonLights(8).state=1
							BonusTargetLights(3).state=1		
						case 5,10,15:
							ButtonLights(5).state=1
							ButtonLights(9).state=1
							BonusTargetLights(0).state=1		
	
					end select
				case 5:
					PlaySound"BallyClunk"

				case 6:


				case 7:


			end select
			ScoreMotorStepper=ScoreMotorStepper+1
			If ScoreMotorStepper>7 then
				ScoreMotorStepper=0
			end if
		end if
	end if
end sub			

Sub ResetDrops
	For each obj in CardsOff
		obj.state=0
	next
	for each obj in CardsOn
		obj.state=1
	next
  DOF 103, DOFPulse
'	playsound "droptargetreset"
'	Target006.IsDropped=false
'	Target007.IsDropped=false
'	Target008.IsDropped=false
'	Target009.IsDropped=false
'	Target010.IsDropped=false	
	ResetDropsRoth True
end sub

Sub ResetBallDrops

	HoleCounter=0
	
End Sub


Sub LightsOut

	BonusCounter=0
	HoleCounter=0



	StopSound "buzz"
	StopSound "buzzL"

end sub

Sub ResetBalls()

	TempMultiCounter=BallsPerGame-BallInPlay
	SpinnerLight.state=0
	For each obj in CardsOff
		obj.state=0
	next
	for each obj in CardsOn
		obj.state=1
	next
	KickerLight1.state=0
	KickerLight2.state=0
	DisplayAltRelay
	ResetBallDrops
	BonusMultiplier=1
	TableTilted=false
	TiltOff
	TiltReel.SetValue(0)
	If VRRoom > 0 Then
		for each Object in VRBGTilt : object.visible = 0 : next
	End If
	If B2Son then
		Controller.B2SSetTilt 0
	end if
	BumpersOn
	Bumper1Light.state=1
	Bumper2Light.state=1
	Bumper3Light.state=1
	if dooralreadyopen=1 then
		closeg.enabled=true

	end if
	Postdown
	IncreaseGoldBonus
	DoubleBonus.state=0
	ResetDrops

	PlasticsOn
	'CreateBallID BallRelease
	Ballrelease.CreateSizedBall 25
    Ballrelease.Kick 40,7
	RandomSoundBallRelease Ballrelease
	DOF 128, DOFPulse
	BallInPlayReel.SetValue(BallInPlay)
	InstructCard.image="IC"
	
	

End Sub





sub resettimer_timer
    rst=rst+1
	if rst>1 and rst<12 then
		ResetReelsToZero(1)
		ResetReelsToZero(2)
	end if

    if rst=16 then
    playsound "StartBall1"
    end if
    if rst=18 then
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
		If VRRoom > 0 Then
			EMMODE = 1
			UpdateVRReels 0,0 ,score(0), 0, -1,-1,-1,-1,-1
			UpdateVRReels 1,1 ,score(1), 0, -1,-1,-1,-1,-1
			UpdateVRReels 2,2 ,score(2), 0, -1,-1,-1,-1,-1
			UpdateVRReels 3,3 ,score(3), 0, -1,-1,-1,-1,-1
			UpdateVRReels 4,4 ,score(4), 0, -1,-1,-1,-1,-1
			EMMODE = 0 ' restore EM mode	
		End If
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
		If VRRoom > 0 Then
			EMMODE = 1
			UpdateVRReels 0,0 ,score(0), 0, -1,-1,-1,-1,-1
			UpdateVRReels 1,1 ,score(1), 0, -1,-1,-1,-1,-1
			UpdateVRReels 2,2 ,score(2), 0, -1,-1,-1,-1,-1
			UpdateVRReels 3,3 ,score(3), 0, -1,-1,-1,-1,-1
			UpdateVRReels 4,4 ,score(4), 0, -1,-1,-1,-1,-1
			EMMODE = 0 ' restore EM mode	
		End If
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
	TiltOff

	If VRRoom > 0 Then
		for each Object in VRBGTilt : object.visible = 0 : next
		for each Object in VRBGGameOver : object.visible = 0 : next
		for each Object in VRBGMatch : object.visible = 0 : next
		for each Object in VRBG100kAll : object.visible = 0 : next
		FlasherBalls
		UpdateVRReels  0 ,0 ,Score(0), 0, 0,0,0,0,0
		UpdateVRReels  1 ,1 ,Score(0), 0, 0,0,0,0,0
		UpdateVRReels  2 ,2 ,Score(0), 0, 0,0,0,0,0
		UpdateVRReels  3 ,3 ,Score(0), 0, 0,0,0,0,0
	End If
	OTT1 = 0
	OTT2 = 0
	OTT3 = 0
	OTT4 = 0
	
	If B2SOn Then
		Controller.B2SSetTilt 0
		Controller.B2SSetGameOver 0
		Controller.B2SSetMatch 0
'		Controller.B2SSetScorePlayer1 0
'		Controller.B2SSetScorePlayer2 0
'		Controller.B2SSetScorePlayer3 0
'		Controller.B2SSetScorePlayer4 0
		Controller.B2SSetBallInPlay BallInPlay
		Controller.B2SSetData 81,1
		Controller.B2SSetData 82,0
		Controller.B2SSetData 83,0
		Controller.B2SSetData 84,0
	End if
	for each obj in CardsOn
		obj.state=1
	next
	for each obj in CardsOff
		obj.state=0
	next
	
	
	Bumper1Light.state=0
	Bumper2Light.state=0
	Bumper3Light.state=0

	BumpersOn
	GoldBonusCounter=0
	LowComplete=false
	HighComplete=false

	ResetBalls
End sub

sub nextball
	If ShootAgain.state=1 then
		ShootAgain.state=0
		ShootAgainReel.SetValue(0)
		If VRRoom > 0 Then
			for each Object in VRBGShootAgain : object.visible = 0 : next
		End If
		If B2SOn then Controller.B2SSetShootAgain 0
	else
		Player=Player+1
	end if
	If Player>Players Then
		BallInPlay=BallInPlay+1
		If BallInPlay>BallsPerGame then
			PlaySound("MotorLeer")
			InProgress=false

			If VRRoom > 0 Then
				for each Object in VRBGGameOver : object.visible = 1 : next
				for each Object in VRBGPlayers : object.visible = 0 : next
				FlasherBalls
				FlasherMatch
			End If
			
			If B2SOn Then
				Controller.B2SSetGameOver 1
				Controller.B2SSetPlayerUp 0
				Controller.B2SSetBallInPlay 0
				Controller.B2SSetCanPlay 0
				Controller.B2SSetData 99,0
				Controller.B2SSetData 81,0
				Controller.B2SSetData 82,0
				Controller.B2SSetData 83,0
				Controller.B2SSetData 84,0
			End If
			For each obj in PlayerHuds
				obj.state=0
			next
			If Table1.ShowDT = True then
				For each obj in PlayerScores
					obj.visible=1
				Next

			end If
			GameOverReel.SetValue(1)
			InstructCard.image="IC"

			BallInPlayReel.SetValue(0)
'			CanPlayReel.SetValue(0)
			LeftFlipper.RotateToStart
			RightFlipper.RotateToStart
			LightsOut
			BumpersOff
			PlasticsOff
			checkmatch
			CheckHighScore
			Players=0
			HighScoreTimer.interval=100
			HighScoreTimer.enabled=True
			TimerTitle1.enabled=1
			TimerTitle2.enabled=1
			TimerTitle3.enabled=1
			TimerTitle4.enabled=1

		Else
			Player=1
			If VRRoom > 0 Then
				FlasherBalls
				FlasherCurrentPlayer
				for each Object in VRBGShootAgain : object.visible = 0 : next
			End If
			If B2SOn Then
				Controller.B2SSetPlayerUp Player
				Controller.B2SSetBallInPlay BallInPlay
				Controller.B2SSetData 81,1
				Controller.B2SSetData 82,0
				Controller.B2SSetData 83,0
				Controller.B2SSetData 84,0
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

				PlayerHuds(Player-1).state=1
				
			end If

			ResetBalls
		End If
	Else 
		If VRRoom > 0 Then
			FlasherBalls
			FlasherCurrentPlayer
			for each Object in VRBGShootAgain : object.visible = 0 : next
		End If
		If B2SOn Then
			Controller.B2SSetPlayerUp Player
			Controller.B2SSetBallInPlay BallInPlay
			Controller.B2SSetData 81,0
			Controller.B2SSetData 82,0
			Controller.B2SSetData 83,0
			Controller.B2SSetData 84,0
			Controller.B2SSetData 80+Player,1
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

			PlayerHuds(Player-1).state=1
			
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
	Match=tempmatch*10
	MatchReel.SetValue(tempmatch+1)

	If VRRoom > 0 Then
		FlasherMatch
	End If
	
	If B2SOn Then
		If Match = 0 Then
			Controller.B2SSetMatch 100
		Else
			Controller.B2SSetMatch Match
		End If
	End if
	for i = 1 to Players
		if Match=(Score(i) mod 100) then
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
			If TiltEndsGame=1 then
				BallInPlay=BallsPerGame
				If VRRoom > 0 Then
					for each Object in VRBGGameOver : object.visible = 1 : next
					for each Object in VRBGPlayers : object.visible = 0 : next
					FlasherBalls
				End If
				If B2SOn Then
					Controller.B2SSetGameOver 1
					Controller.B2SSetPlayerUp 0
					Controller.B2SSetBallInPlay 0
					Controller.B2SSetCanPlay 0
				End If
				For each obj in PlayerHuds
					obj.state=0
				next
				GameOverReel.SetValue(1)
				InstructCard.image="IC"
			end if
			TiltOn
			PlasticsOff
			BumpersOff
			Postdown
			if dooralreadyopen=1 then
				closeg.enabled=true
			end if
			LeftFlipper.RotateToStart
			RightFlipper.RotateToStart
			StopSound "buzz"
			StopSound "buzzL"
			TiltReel.SetValue(1)
			If VRRoom > 0 Then
				for each Object in VRBGTilt : object.visible = 1 : next
			End If
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
		ScoreFile.WriteLine 1
		ScoreFile.WriteLine Credits
		scorefile.writeline BallsPerGame
		scorefile.writeline 0
		scorefile.writeline CardLightOption
		scorefile.writeline 0
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

'sub SaveLMEMConfig
'	Dim FileObj
'	Dim LMConfig
'	dim temp1
'	dim tempb2s
'	tempb2s=0
'	if B2SOn=true then
'		tempb2s=1
'	else
'		tempb2s=0
'	end if
'	Set FileObj=CreateObject("Scripting.FileSystemObject")
'	If Not FileObj.FolderExists(UserDirectory) then 
'		Exit Sub
'	End if
'	Set LMConfig=FileObj.CreateTextFile(UserDirectory & LMEMTableConfig,True)
'	LMConfig.WriteLine tempb2s
'	LMConfig.Close
'	Set LMConfig=Nothing
'	Set FileObj=Nothing
'
'end Sub

'sub LoadLMEMConfig
'	Dim FileObj
'	Dim LMConfig
'	dim tempC
'	dim tempb2s
'
'    Set FileObj=CreateObject("Scripting.FileSystemObject")
'	If Not FileObj.FolderExists(UserDirectory) then 
'		Exit Sub
'	End if
'	If Not FileObj.FileExists(UserDirectory & LMEMTableConfig) then
'		Exit Sub
'	End if
'	Set LMConfig=FileObj.GetFile(UserDirectory & LMEMTableConfig)
'	Set TextStr2=LMConfig.OpenAsTextStream(1,0)
'	If (TextStr2.AtEndOfStream=True) then
'		Exit Sub
'	End if
'	tempC=TextStr2.ReadLine
'	TextStr2.Close
'	tempb2s=cdbl(tempC)
'	if tempb2s=0 then
'		B2SOn=false
'	else
'		B2SOn=true
'	end if
'	Set LMConfig=Nothing
'	Set FileObj=Nothing
'end sub

'sub SaveLMEMConfig2
'	If ShadowConfigFile=false then exit sub
'	Dim FileObj
'	Dim LMConfig2
'	dim temp1
'	dim temp2
'	dim tempBS
'	dim tempFS

'	if EnableBallShadow=true then
'		tempBS=1
'	else
'		tempBS=0
'	end if
'	if EnableFlipperShadow=true then
'		tempFS=1
'	else
'		tempFS=0
'	end if
'
'	Set FileObj=CreateObject("Scripting.FileSystemObject")
'	If Not FileObj.FolderExists(UserDirectory) then 
'		Exit Sub
'	End if
'	Set LMConfig2=FileObj.CreateTextFile(UserDirectory & LMEMShadowConfig,True)
'	LMConfig2.WriteLine tempBS
'	LMConfig2.WriteLine tempFS
'	LMConfig2.Close
'	Set LMConfig2=Nothing
'	Set FileObj=Nothing
'
'end Sub

'sub LoadLMEMConfig2
'	If ShadowConfigFile=false then
'		EnableBallShadow = ShadowBallOn
'		BallShadowUpdate.enabled = ShadowBallOn
'		EnableFlipperShadow = ShadowFlippersOn
'		FlipperLSh.visible = ShadowFlippersOn
'		FlipperRSh.visible = ShadowFlippersOn
'		exit sub
'	end if
'	Dim FileObj
'	Dim LMConfig2
'	dim tempC
'	dim tempD
'	dim tempFS
'	dim tempBS
'
'    Set FileObj=CreateObject("Scripting.FileSystemObject")
'	If Not FileObj.FolderExists(UserDirectory) then 
'		Exit Sub
'	End if
'	If Not FileObj.FileExists(UserDirectory & LMEMShadowConfig) then
'		Exit Sub
'	End if
'	Set LMConfig2=FileObj.GetFile(UserDirectory & LMEMShadowConfig)
'	Set TextStr2=LMConfig2.OpenAsTextStream(1,0)
'	If (TextStr2.AtEndOfStream=True) then
'		Exit Sub
'	End if
'	tempC=TextStr2.ReadLine
'	tempD=TextStr2.Readline
'	TextStr2.Close
'	tempBS=cdbl(tempC)
'	tempFS=cdbl(tempD)
'	if tempBS=0 then
'		EnableBallShadow=false
'		BallShadowUpdate.enabled=false
'	else
'		EnableBallShadow=true
'	end if
'	if tempFS=0 then
'		EnableShadow=false
'		FlipperLSh.visible=false
'		FLipperRSh.visible=false
'	else
'		EnableFlipperShadow=true
'	end if
'	Set LMConfig2=Nothing
'	Set FileObj=Nothing
'end sub


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
		temp7=textstr.readline
		temp6=textstr.readline
		HighScore=cdbl(temp1)
		if HighScore=1 then
			
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
		if HighScore=1 then
			Credits=cdbl(temp2)
			BallsPerGame=cdbl(temp3)
			TiltEndsGame=cdbl(temp4)
			CardLightOption=cdbl(temp5)
			SpecialOption=cdbl(temp7)
			ReplayLevel=cdbl(temp6)
		
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
		If B2SOn Then
			'Controller.B2SSetScore 3,HighScore
		End If
		DisplayHighScore
		CreditsReel.SetValue(Credits)
		InitPauser5.enabled=false
end sub



sub BumpersOff
	Bumper1Light.Visible=0
	Bumper2Light.Visible=0
	Bumper3Light.Visible=0
end sub

sub BumpersOn
	Bumper1Light.Visible=1
	Bumper2Light.Visible=1
	Bumper3Light.Visible=1
end sub

Sub TiltOn
	Bumper1.hashitevent=0
	Bumper2.hashitevent=0
	Bumper3.hashitevent=0
	RightSlingShot.collidable=0
	LeftSlingShot.collidable=0
End Sub

Sub TiltOff
	Bumper1.hashitevent=1
	Bumper2.hashitevent=1
	Bumper3.hashitevent=1
	RightSlingShot.collidable=1
	LeftSlingShot.collidable=1
End Sub

Sub PlasticsOn

	For each obj in Flashers
		obj.Visible=1
	next

end sub

Sub PlasticsOff

	For each obj in Flashers
		obj.Visible=0
	next

end sub

Sub SetupReplayTables

	
	Replay1Table(1)=63000
	Replay1Table(2)=84000
	Replay1Table(3)=90000
	Replay1Table(4)=120000
	Replay1Table(5)=82000
	Replay1Table(6)=84000
	Replay1Table(7)=122000
	Replay1Table(8)=124000
	Replay1Table(9)=128000
	Replay1Table(10)=132000
	Replay1Table(11)=71000
	Replay1Table(12)=74000
	Replay1Table(13)=78000
	Replay1Table(14)=999000
	Replay1Table(15)=999000

	
	Replay2Table(1)=125000
	Replay2Table(2)=129000
	Replay2Table(3)=120000
	Replay2Table(4)=170000
	Replay2Table(5)=94000
	Replay2Table(6)=96000
	Replay2Table(7)=140000
	Replay2Table(8)=142000
	Replay2Table(9)=146000
	Replay2Table(10)=150000
	Replay2Table(11)=85000
	Replay2Table(12)=88000
	Replay2Table(13)=92000
	Replay2Table(14)=999000
	Replay2Table(15)=999000

	Replay3Table(1)=999000
	Replay3Table(2)=999000
	Replay3Table(3)=170000
	Replay3Table(4)=999000
	Replay3Table(5)=106000
	Replay3Table(6)=108000
	Replay3Table(7)=158000
	Replay3Table(8)=160000
	Replay3Table(9)=164000
	Replay3Table(10)=168000
	Replay3Table(11)=93000
	Replay3Table(12)=96000
	Replay3Table(13)=999000
	Replay3Table(14)=999000
	Replay3Table(15)=999000

	Replay4Table(1)=999000
	Replay4Table(2)=999000
	Replay4Table(3)=999000
	Replay4Table(4)=999000
	Replay4Table(5)=999000
	Replay4Table(6)=999000
	Replay4Table(7)=999000
	Replay4Table(8)=999000
	Replay4Table(9)=999000
	Replay4Table(10)=999000
	Replay4Table(11)=999000
	Replay4Table(12)=999000
	Replay4Table(13)=999000
	Replay4Table(14)=999000
	Replay4Table(15)=999000

	ReplayTableMax=2


end sub

Sub RefreshReplayCard
	Dim tempst1
	Dim tempst2
	Dim tempst3
	tempst1=FormatNumber(BallsPerGame,0)
	tempst2=FormatNumber(ReplayLevel,0)
	tempst3=FormatNumber(SpecialOption,0)

	BallCard.image = "BC"+tempst1
	Select case SpecialOption
		case 0:
			Replay1=Replay1Table(ReplayLevel)
			Replay2=Replay2Table(ReplayLevel)
			Replay3=Replay3Table(ReplayLevel)
			Replay4=Replay4Table(ReplayLevel)
		case 1:
			Replay1=EVAL("ReplayBalls"&ReplayLevel)(1)
			Replay2=EVAL("ReplayBalls"&ReplayLevel)(2)
			Replay3=999000
			Replay4=999000
		case 2:
			Replay1=999000
			Replay2=999000
			Replay3=999000
			Replay4=999000
	end select
	ReplayCard.image = "SC" + tempst2 

		
	
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
			Case 50:
				MotorMode=10
				MotorPosition=5
				'BumpersOff
				
			Case 100:
				MotorMode=100
				MotorPosition=1
				
			Case 500:
				MotorMode=500
				MotorPosition=5
				'BumpersOff
				LightBumpers
				
			Case 1000:
				MotorMode=1000
				MotorPosition=1
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
				'BumpersOff
			Case 5000:
				MotorMode=1000
				MotorPosition=5
				'BumpersOff
				LightBumpers
			Case 10000:
				MotorMode=10000
				MotorPosition=1
			Case 50000:
				MotorMode=10000
				MotorPosition=5
				'BumpersOff
				LightBumpers
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

	End If


end Sub

Sub ScoreMotorTimer_Timer
	If MotorPosition > 0 Then
		Select Case MotorPosition
			Case 5,4,3,2:
				If MotorMode=10000 Then 
					AddScore(10000)
				end if
				If MotorMode=1000 Then 
					AddScore(1000)
				end if
				if MotorMode=100 OR MotorMode=500 then
					AddScore(100)
				End If
				if MotorMode=10 then
					AddScore(10)
				End if
				MotorPosition=MotorPosition-1
			Case 1:
				If MotorMode=10000 Then 
					AddScore(10000)
				end if
				If MotorMode=1000 Then 
					AddScore(1000)
				end if
				If MotorMode=100 or MotorMode=500 then 
					AddScore(100)
				End If
				if MotorMode=10 then
					AddScore(10)
				End if
				If MotorMode=500 then ToggleAlternatingRelay
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
			
		Case 10:
			PlayChime(10)
			Score(Player)=Score(Player)+10
'			debugscore=debugscore+10
			
		Case 100:
			PlayChime(100)
			Score(Player)=Score(Player)+100
'			debugscore=debugscore+100
			
			
		Case 1000:
			PlayChime(100)
			Score(Player)=Score(Player)+1000
'			debugscore=debugscore+1000
	End Select
	PlayerScores(Player-1).AddValue(x)
	
	If ScoreDisplay(Player)<100000 then
		ScoreDisplay(Player)=Score(Player)
	Else
		Score100K(Player)=Int(Score(Player)/100000)
		ScoreDisplay(Player)=Score(Player)-100000
	End If
	if Score(Player)=>100000 then
		EVAL("RolloverReel"&Player).SetValue(1)
		If VRRoom > 0 Then
			Flasher100k
		End If
			FlasherOverTheTop
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
        Case 1:
            Score(Player)=Score(Player)+1
		Case 10:
			Score(Player)=Score(Player)+10
		Case 100:
			Score(Player)=Score(Player)+100
		Case 1000:
			Score(Player)=Score(Player)+1000
		Case 10000:
			Score(Player)=Score(Player)+10000
	End Select
	NewScore = Score(Player)
	If NewScore>=100000 then
		EVAL("RolloverReel"&Player).SetValue(1)
		If VRRoom > 0 Then
			Flasher100k
		End If
		FlasherOverTheTop
		if B2SOn then 
			if Player=1 then Controller.B2SSetScoreRolloverPlayer1 1
			elseif Player=2 then Controller.B2SSetScoreRolloverPlayer2 1
			elseif Player=3 then Controller.B2SSetScoreRolloverPlayer3 1
			elseif Player=4 then Controller.B2SSetScoreRolloverPlayer4 1
		end if
	end if
	OldTestScore = OldScore
	NewTestScore = NewScore
	Do
		if OldTestScore < Replay1 and NewTestScore >= Replay1 AND Replay1Paid(Player)=false then
			AddSpecial()
			Replay1Paid(Player)=true
			NewTestScore = 0
		Elseif OldTestScore < Replay2 and NewTestScore >= Replay2 AND Replay2Paid(Player)=false then
			AddSpecial()
			Replay2Paid(Player)=true
			NewTestScore = 0
		Elseif OldTestScore < Replay3 and NewTestScore >= Replay3 AND Replay3Paid(Player)=false then
			AddSpecial()
			Replay3Paid(Player)=true
			NewTestScore = 0
		Elseif OldTestScore < Replay4 and NewTestScore >= Replay4 AND Replay4Paid(Player)=false then
			AddSpecial()
			Replay4Paid(Player)=true
			NewTestScore = 0
		End if
		NewTestScore = NewTestScore - 100000
		OldTestScore = OldTestScore - 100000
	Loop While NewTestScore > 0

    OldScore = int(OldScore / 10)	' divide by 10 for games with fixed 0 in 1s position, by 1 for games with real 1s digits
    NewScore = int(NewScore / 10)	' divide by 10 for games with fixed 0 in 1s position, by 1 for games with real 1s digits
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore&", OldScore Mod 10="&OldScore Mod 10 & ", NewScore % 10="&NewScore Mod 10)

    if (OldScore Mod 10 <> NewScore Mod 10) then
		PlayChime(10)
		AdvanceZeroToNine
		
    end if

    OldScore = int(OldScore / 10)
    NewScore = int(NewScore / 10)
	' MsgBox("OldScore="&OldScore&", NewScore="&NewScore)
    if (OldScore Mod 10 <> NewScore Mod 10) then
		PlayChime(10)
		
		
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

	If VRRoom > 0 Then
		EMMODE = 1
		UpdateVRReels 0,0 ,score(0), 0, -1,-1,-1,-1,-1
		UpdateVRReels 1,1 ,score(1), 0, -1,-1,-1,-1,-1
		UpdateVRReels 2,2 ,score(2), 0, -1,-1,-1,-1,-1
		UpdateVRReels 3,3 ,score(3), 0, -1,-1,-1,-1,-1
		UpdateVRReels 4,4 ,score(4), 0, -1,-1,-1,-1,-1
		EMMODE = 0 ' restore EM mode
	End If
	
	else
		Select Case x
			Case 10
				If LastChime10=1 Then
					PlaySound SoundFXDOF("Gottlieb10Points",141,DOFPulse,DOFChimes)
					LastChime10=0
				Else
					PlaySound SoundFXDOF("Gottlieb10Points",141,DOFPulse,DOFChimes)
					LastChime10=1
				End If
			Case 100
				If LastChime100=1 Then
					PlaySound SoundFXDOF("Gottlieb100Points",142,DOFPulse,DOFChimes)
					LastChime100=0
				Else
					PlaySound SoundFXDOF("Gottlieb100Points",142,DOFPulse,DOFChimes)
					LastChime100=1
				End If
			Case 1000
				If LastChime1000=1 Then
					PlaySound SoundFXDOF("Gottlieb1000Points",143,DOFPulse,DOFChimes)
					LastChime1000=0
				Else
					PlaySound SoundFXDOF("Gottlieb1000Points",143,DOFPulse,DOFChimes)
					LastChime1000=1
				End If
		End Select
	If VRRoom > 0 Then
		EMMODE = 1
		UpdateVRReels 0,0 ,score(0), 0, -1,-1,-1,-1,-1
		UpdateVRReels 1,1 ,score(1), 0, -1,-1,-1,-1,-1
		UpdateVRReels 2,2 ,score(2), 0, -1,-1,-1,-1,-1
		UpdateVRReels 3,3 ,score(3), 0, -1,-1,-1,-1,-1
		UpdateVRReels 4,4 ,score(4), 0, -1,-1,-1,-1,-1
	EMMODE = 0 ' restore EM mode
	End If
	end if
End Sub

Sub HideOptions()

end sub




'******************************************************
'****  BALL ROLLING AND DROP SOUNDS
'******************************************************

Const tnob = 5 ' total number of balls
Const lob = 0		'number of locked balls

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
	Dim BOT, b
	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
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
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If

		' "Static" Ball Shadows
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
	Next
End Sub


'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
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
'
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
'****** End Part A:  Table Elements ******


'****** Part B:  Code and Functions ******

' *** Timer sub
' The "DynamicBSUpdate" sub should be called by a timer with an interval of -1 (framerate)
Sub FrameTimer_Timer()
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
End Sub

' *** User Options - Uncomment here or move to top
Const fovY					= -2	'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 10	'Sets minimum as ball moves away from source


' *** This segment goes within the RollingUpdate sub, so that if Ambient...=0 and Dynamic...=0 the entire DynamicBSUpdate sub can be skipped for max performance

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
Dim sourcenames, currentShadowCount
sourcenames = Array ("","","","","","","","","","","","")
currentShadowCount = Array (0,0,0,0,0,0,0,0,0,0,0,0)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!
dim objrtx1(12), objrtx2(12)
dim objBallShadow(12)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadowA5,BallShadowA6,BallShadowA7,BallShadowA8,BallShadowA9,BallShadowA10,BallShadowA11)

DynamicBSInit

sub DynamicBSInit()
	Dim iii

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
end sub


Sub DynamicBSUpdate
	Dim falloff:	falloff = 100			'Max distance to light sources, can be changed if you have a reason
	Dim ShadowOpacity, ShadowOpacity2 
	Dim s, Source, LSd, currentMat, AnotherSource, BOT
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
				For Each Source in DynamicSources
					LSd=DistanceFast((BOT(s).x-Source.x),(BOT(s).y-Source.y))	'Calculating the Linear distance to the Source
					If LSd < falloff and Source.state=1 Then	    			'If the ball is within the falloff range of a light and light is on
						currentShadowCount(s) = currentShadowCount(s) + 1		'Within range of 1 or 2
						if currentShadowCount(s) = 1 Then						'1 dynamic shadow source
							sourcenames(s) = source.name
							currentMat = objrtx1(s).material
							objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
	'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01						'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(Source.x, Source.y, BOT(s).X, BOT(s).Y) + 90
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
Const OptionLinesToMark="111010011"
Const OptionLine1="" 'do not use this line
Const OptionLine2="" 'do not use this line
Const OptionLine3="" 'do not use this line
Const OptionLine4=""
Const OptionLine5="Double Bonus Light Option"
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
				If SpecialOption = 0 then
					if Replay3Table(ReplayLevel)=999000 then
						tempstring = FormatNumber(Replay1Table(ReplayLevel),0) + "/" + FormatNumber(Replay2Table(ReplayLevel),0)
					elseif Replay4Table(ReplayLevel)=999000 then
						tempstring = FormatNumber(Replay1Table(ReplayLevel),0) + "/" + FormatNumber(Replay2Table(ReplayLevel),0) + "/" + FormatNumber(Replay3Table(ReplayLevel),0)
					else
						tempstring = FormatNumber(Replay1Table(ReplayLevel),0) + "/" + FormatNumber(Replay2Table(ReplayLevel),0) + "/" + FormatNumber(Replay3Table(ReplayLevel),0) + "/" + FormatNumber(Replay4Table(ReplayLevel),0)
					end if
				elseif SpecialOption = 1 then
					tempstring = FormatNumber(EVAL("ReplayBalls"&ReplayLevel)(1),0) + "/" + FormatNumber(EVAL("ReplayBalls"&ReplayLevel)(2),0)
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
				Select Case CardLightOption
					Case 0:
						tempstring = "Conservative"
					Case 1:
						tempstring = "Medium"
					Case 2:
						tempstring = "Liberal"
				end select
				
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
			if SpecialOption=0 then
				If ReplayLevel>ReplayTableMax then
					ReplayLevel=1
				end if
			end if
			if SpecialOption=1 then
				If ReplayLevel>2 then
					ReplayLevel=1
				end if
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
			CardLightOption=CardLightOption+1
			if CardLightOption>2 then
				CardLightOption=0
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
				InstructCard.image="IC"
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
	elseif ThisChar = ">" then
		FileName = FileName & "GT"
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
	End If
End Sub


'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************

'
''******************************************************
''				FLIPPER AND RUBBER CORRECTION
''******************************************************
'dim LFPress, RFPress, EOST, EOSA, EOSTnew, EOSaNew
'dim FStrength, FRampUp, fElasticity, EOSRampUp, SOSRampUp
'dim RFEndAngle, LFEndAngle, LFCount, RFCount, LiveCatch
'
'LFEndAngle = Leftflipper.EndAngle
'RFEndAngle = RightFlipper.EndAngle
'
'EOST = leftflipper.eosTorque   			'End of Swing Torque
'EOSA = leftflipper.eosTorqueAngle		'End of Swing Torque Angle
'fStrength = LeftFlipper.strength		'Flipper Strength
'fRampUp = LeftFlipper.RampUp			'Flipper Ramp Up
'fElasticity = LeftFlipper.elasticity	'Flipper Elasticity
'EOStNew = 1.0 		'new Flipper Torque 
'EOSaNew = 0.2		'new FLipper Tprque Angle
'EOSRampUp = 1.5		'new EOS Ramp Up weaker at EOS because of the weaker holding coil
'Select Case FlipperCoilRampupMode 
'	Case 0:
'		SOSRampup = 2.5
'	Case 1:
'		SOSRampup = 6
'	Case 2:
'		SOSRampup = 8.5
'End Select
'LiveCatch = 8		'variable to check elapsed time from 
'
''********Need to have a flipper timer to check for these values
'
'Sub flipperTimer_Timer
'    Dim BOT, b
'    BOT = GetBalls
''	For b = 0 to UBound(BOT)
''		BOT(0).color = vbgreen
''		If b > 0 then BOT(1).color = vbred
''		If b > 1 Then BOT(2).color = vbblue
''		tb.text = "x = " & BOT(0).x
''		tb1.text = "y = " & BOT(0).y
''		tb.text = formatnumber(BOT(b).AngMomY,1)
''		tb.text = "AngMomX = " & formatnumber(BOT(b).AngMomX,1)
''		tb1.text = "AngMomY = " & formatnumber(BOT(b).AngMomY,1)
''		tb2.text = "AngMMomZ = " & formatnumber(BOT(b).AngMomZ,1)
'
''	Next
'
''	lFlip.rotz = leftflipper.CurrentAngle -121 'silver metal flipper obj
''	lFlipR.rotz = leftflipper.CurrentAngle -121 
''	rFlip.rotz = RightFlipper.CurrentAngle +121
''	rFlipR.rotz = RightFlipper.CurrentAngle +121
'
'
'	FlipperLSh.RotZ = LeftFlipper.CurrentAngle
'	FlipperRSh.RotZ = RightFlipper.CurrentAngle
'
'
'
'	'--------------Flipper Tricks Section
'	'What this code does is swing the flipper fast and make the flipper soft near its EOS to enable live catches.  It resets back to the base Table
'	'settings once the flipper reaches the end of swing.  The code also makes the flipper starting ramp up high to simulate the stronger starting
'	'coil strength and weaker at its EOS to simulate the weaker hold coil.
'
'	If LeftFlipper.CurrentAngle = LeftFlipper.EndAngle and LFPress = 1 Then 	'If the flipper is fully swung and the flipper button is pressed then...
'		LeftFlipper.eosTorqueAngle = EOSaNew	'sets flipper EOS Torque Angle to .2 
'		LeftFlipper.eosTorque = EOStNew			'sets flipper EOS Torque to 1
'		LeftFlipper.RampUp = EOSRampUp			'sets flipper ramp up to 1.5
'		If LFCount = 0 Then LFCount = GameTime	'sets the variable LFCount = to the elapsed game time
'		If GameTime - LFCount < LiveCatch Then	'if less than 8ms have elasped then we are in a "Live Catch" scenario
'			LeftFlipper.Elasticity = 0.1		'sets flipper elasticity WAY DOWN to allow Live Catches
'			If LeftFlipper.EndAngle <> LFEndAngle Then LeftFlipper.EndAngle = LFEndAngle	'Keep the flipper at its EOS and don't let it deflect
'		Else	
'			LeftFlipper.Elasticity = fElasticity	'reset flipper elasticity to the base table setting
'		End If
'	Elseif LeftFlipper.CurrentAngle > LeftFlipper.startangle - 0.05  Then 	'If the flipper has started its swing, make it swing fast to nearly the end...
'		LeftFlipper.RampUp = SOSRampUp				'set flipper Ramp Up high
'		LeftFlipper.EndAngle = LFEndAngle - 3		'swing to within 3 degrees of EOS
'		LeftFlipper.Elasticity = fElasticity		'Set the elasticity to the base table elasticity
'		LFCount = 0
'	Elseif LeftFlipper.CurrentAngle > LeftFlipper.EndAngle + 0.01 Then  'If the flipper has swung past it's end of swing then...
'		LeftFlipper.eosTorque = EOST			'set the flipper EOS Torque back to the base table setting
'		LeftFlipper.eosTorqueAngle = EOSA		'set the flipper EOS Torque Angle back to the base table setting
'		LeftFlipper.RampUp = fRampUp			'set the flipper Ramp Up back to the base table setting
'		LeftFlipper.Elasticity = fElasticity	'set the flipper Elasticity back to the base table setting
'	End If
'
'	If RightFlipper.CurrentAngle = RightFlipper.EndAngle and RFPress = 1 Then
'		RightFlipper.eosTorqueAngle = EOSaNew
'		RightFlipper.eosTorque = EOStNew
'		RightFlipper.RampUp = EOSRampUp
'		If RFCount = 0 Then RFCount = GameTime
'		If GameTime - RFCount < LiveCatch Then
'			RightFlipper.Elasticity = 0.1
'			If RightFlipper.EndAngle <> RFEndAngle Then RightFlipper.EndAngle = RFEndAngle
'		Else
'			RightFlipper.Elasticity = fElasticity
'		End If
'	Elseif RightFlipper.CurrentAngle < RightFlipper.StartAngle + 0.05 Then
'		RightFlipper.RampUp = SOSRampUp 
'		RightFlipper.EndAngle = RFEndAngle + 3
'		RightFlipper.Elasticity = fElasticity
'		RFCount = 0 
'	Elseif RightFlipper.CurrentAngle < RightFlipper.EndAngle - 0.01 Then 
'		RightFlipper.eosTorque = EOST
'		RightFlipper.eosTorqueAngle = EOSA
'		RightFlipper.RampUp = fRampUp
'		RightFlipper.Elasticity = fElasticity
'	End If
'
'End Sub
'
'dim LF : Set LF = New FlipperPolarity
'dim RF : Set RF = New FlipperPolarity
'
'
'InitPolarity
'
'Sub InitPolarity()
'	dim x, a : a = Array(LF, RF)
'	for each x in a
'		'safety coefficient (diminishes polarity correction only)
'		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
'		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
'
'		x.enabled = True
'		x.TimeDelay = 69    '*****Important, this variable is an offset for the speed that the ball travels down the table to determine if the flippers have been fired 
'							'This is needed because the corrections to ball trajectory should only applied if the flippers have been fired and the ball is in the trigger zones.
'							'FlipAT is set to GameTime when the ball enters the flipper trigger zones and if GameTime is less than FlipAT + this time delay then changes to velocity
'							'and trajectory are applied.  If the flipper is fired before the ball enters the trigger zone then with this delay added to FlipAT the changes
'							'to tragectory and velocity will not be applied.  Also if the flipper is in the final 20 degrees changes to ball values will also not be applied.
'							'"Faster" tables will need a smaller value while "slower" tables will need a larger value to give the ball more time to get to the flipper. 		
'							'If this value is not set high enough the Flipper Velocity and Polarity corrections will NEVER be applied.
'	Next
'
'	'rf.report "Polarity"
'	AddPt "Polarity", 0, 0, -2.7
'	AddPt "Polarity", 1, 0.16, -2.7	
'	AddPt "Polarity", 2, 0.33, -2.7
'	AddPt "Polarity", 3, 0.37, -2.7	'4.2
'	AddPt "Polarity", 4, 0.41, -2.7
'	AddPt "Polarity", 5, 0.45, -2.7 '4.2
'	AddPt "Polarity", 6, 0.576,-2.7
'	AddPt "Polarity", 7, 0.66, -1.8'-2.1896
'	AddPt "Polarity", 8, 0.743, -0.5
'	AddPt "Polarity", 9, 0.81, -0.5
'	AddPt "Polarity", 10, 0.88, 0
'
'	'"Velocity" Profile
'	addpt "Velocity", 0, 0, 	1
'	addpt "Velocity", 1, 0.16, 1.06
'	addpt "Velocity", 2, 0.41, 	1.05
'	addpt "Velocity", 3, 0.53, 	1'0.982
'	addpt "Velocity", 4, 0.702, 0.968
'	addpt "Velocity", 5, 0.95,  0.968
'	addpt "Velocity", 6, 1.03, 	0.945
'
'	LF.Object = LeftFlipper
'	LF.EndPoint = EndPointLp	'you can use just a coordinate, or an object with a .x property. Using a couple of simple primitive objects
'	RF.Object = RightFlipper
'	RF.EndPoint = EndPointRp
'End Sub
'
'Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
'	dim a : a = Array(LF, RF)
'	dim x : for each x in a
'		x.addpoint aStr, idx, aX, aY
'	Next
'End Sub
'
'
'Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
'Sub TriggerLF_UnHit() :  LF.PolarityCorrect activeball : End Sub
'Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
'Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub
'
''Methods:
''.TimeDelay - Delay before trigger shuts off automatically. Default = 80 (ms)
''.AddPoint - "Polarity", "Velocity", "Ycoef" coordinate points. Use one of these 3 strings, keep coordinates sequential. x = %position on the flipper, y = output
''.Object - set to flipper reference. Optional.
''.StartPoint - set start point coord. Unnecessary, if .object is used.
'
''Called with flipper - 
''ProcessBalls - catches ball data. 
'' - OR - 
''.Fire - fires flipper.rotatetoend automatically + processballs. Requires .Object to be set to the flipper.
'
''***************This is flipperPolarity's addPoint Sub
'Class FlipperPolarity
'	Public Enabled
'	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
'	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
'	private Flipper, FlipperStart, FlipperEnd, LR, PartialFlipCoef
'	Private Balls(20), balldata(20)
'	
'	dim PolarityIn, PolarityOut
'	dim VelocityIn, VelocityOut
'	dim YcoefIn, YcoefOut
'
'	Public Sub Class_Initialize 
'		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
'		Enabled = True: TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new spoofBall: next  
'	End Sub
'	
'	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
'	Public Property Let StartPoint(aInput) : if IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end if : End Property
'	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
'	Public Property Let EndPoint(aInput) : if IsObject(aInput) then FlipperEnd = aInput.x else FlipperEnd = aInput : end if : End Property
'	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property
'	
'	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
'		Select Case aChooseArray
'			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
'			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
'			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
'		End Select
'
'	End Sub 
'
''********Triggered by a ball hitting the flipper trigger area	
'	Public Sub AddBall(aBall) : dim x : 
'		for x = 0 to uBound(balls)  
'			if IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end if  
'		Next   
'	End Sub
'
'	Private Sub RemoveBall(aBall)
'		dim x : for x = 0 to uBound(balls)
'			if TypeName(balls(x) ) = "IBall" then 
'				if aBall.ID = Balls(x).ID Then
'					balls(x) = Empty
'					Balldata(x).Reset
'				End If
'			End If
'		Next
'	End Sub
'
''*********Used to rotate flipper since this is removed from the key down for the flippers	
'	Public Sub Fire()
'		Flipper.RotateToEnd
'		processballs
'	End Sub
'
'	Public Sub ProcessBalls() 'save data of balls in flipper range
'		FlipAt = GameTime
'		dim x : for x = 0 to uBound(balls)
'			if not IsEmpty(balls(x) ) then balldata(x).Data = balls(x)
'		Next
'		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))  '% of flipper swing
'		PartialFlipCoef = abs(PartialFlipCoef-1)
'		if abs(Flipper.CurrentAngle - Flipper.EndAngle) < 20 Then 'last 20 degrees of swing is not dealt with
'			PartialFlipCoef = 0
'		End If
''		tb.text = FlipAT
'	End Sub
'
''***********gameTime is a global variable of how long the game has progressed in ms
''***********This function lets the table know if the flipper has been fired
'	Private Function FlipperOn() 
''		TB.text = gameTime & ":" & (FlipAT + TimeDelay) ' ******MOVE TB into view WHEN THIS FLIPPER FUNCTIONALITY IS ADDED TO A NEW TABLE TO CHECK IF THE TIME DELAY IS LONG ENOUGH*****
'		if gameTime < FlipAt + TimeDelay then FlipperOn = True 
'	End Function	'Timer shutoff for polaritycorrect 
'	
''***********This is turned on when a ball leaves the flipper trigger area
'	Public Sub PolarityCorrect(aBall)
'		if FlipperOn() then 'don't run this if the flippers are at rest
'			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1
'			dim teststr : teststr = "Cutoff"
'			tmp = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
'			if tmp < 0.1 then 'if real ball position is behind flipper, exit Sub to prevent stucks	'Disabled 1.03, I think it's the Mesh that's causing stucks, not this
'			end if
'
'			'y safety Exit
'			if aBall.VelY > -8 then 'if ball going down then remove the ball
'				RemoveBall aBall
'				exit Sub
'			end if
'			'Find balldata. BallPos = % on Flipper
'			for x = 0 to uBound(Balls)
'				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
'					idx = x
'					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
'					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
'				end if
'			Next
'
'			'Velocity correction
'			if not IsEmpty(VelocityIn(0) ) then
''				tb.text = "Vel corr"
'				Dim VelCoef
'				if IsEmpty(BallData(idx).id) and aBall.VelY < -12 then 'if tip hit with no collected data, do vel correction anyway
'					if PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1) > 1.1 then 'adjust plz
'						VelCoef = LinearEnvelope(5, VelocityIn, VelocityOut)
'						if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
'						if Enabled then aBall.Velx = aBall.Velx*VelCoef'VelCoef
'						if Enabled then aBall.Vely = aBall.Vely*VelCoef'VelCoef
'					end if
'				Else
'		 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
'					if Enabled then aBall.Velx = aBall.Velx*VelCoef
'					if Enabled then aBall.Vely = aBall.Vely*VelCoef
'				end if
'			End If
'
'			'Polarity Correction (optional now)
'			if not IsEmpty(PolarityIn(0) ) then
'				If StartPoint > EndPoint then LR = -1	'Reverse polarity if left flipper
'				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
'				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
'			End If
'		End If
'		RemoveBall aBall
'	End Sub
'End Class
'
''================================
''Helper Functions
'
'
'Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
'	dim x, aCount : aCount = 0
'	redim a(uBound(aArray) )
'	for x = 0 to uBound(aArray)	'Shuffle objects in a temp array
'		if not IsEmpty(aArray(x) ) Then
'			if IsObject(aArray(x)) then 
'				Set a(aCount) = aArray(x) 'Set creates an object in VB
'			Else
'				a(aCount) = aArray(x)
'			End If
'			aCount = aCount + 1
'		End If
'	Next
'	if offset < 0 then offset = 0
'	redim aArray(aCount-1+offset)	'Resize original array
'	for x = 0 to aCount-1		'set objects back into original array
'		if IsObject(a(x)) then 
'			Set aArray(x) = a(x)
'		Else
'			aArray(x) = a(x)
'		End If
'	Next
'End Sub
'
''**********Takes in more than one array and passes them to ShuffleArray
'Sub ShuffleArrays(aArray1, aArray2, offset)
'	ShuffleArray aArray1, offset
'	ShuffleArray aArray2, offset
'End Sub
'
''**********Calculate ball speed as hypotenuse of velX/velY triangle
'Function BallSpeed(ball) 'Calculates the ball speed
'    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
'End Function
'
''**********Calculates the value of Y for an input x using the slope intercept equation
'Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
'	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
'	Y = M*x+b
'	PSlope = Y
'End Function
'
'Class spoofball 
'	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius 
'	Public Property Let Data(aBall)
'		With aBall
'			x = .x : y = .y : z = .z : velx = .velx : vely = .vely : velz = .velz
'			id = .ID : mass = .mass : radius = .radius
'		end with
'	End Property
'	Public Sub Reset()
'		x = Empty : y = Empty : z = Empty  : velx = Empty : vely = Empty : velz = Empty 
'		id = Empty : mass = Empty : radius = Empty
'	End Sub
'End Class

'****************************************************************************
'PHYSICS DAMPENERS

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR


Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	TargetBouncer Activeball, 1
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
	TargetBouncer Activeball, 0.7
End Sub

'*********This sets up the rubbers:
dim RubbersD  
Set RubbersD = new Dampener  'Makes a Dampener Class Object 	
RubbersD.name = "Rubbers"

'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 0.96        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.96
RubbersD.addpoint 2, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64        'there's clamping so interpolate up to 56 at least

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
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-L01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-R01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
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
'	If Abs(cor.ballvelx(activeball.id) < 4) and cor.ballvely(activeball.id) > 7 then
'		RandomSoundBottomArchBallGuideHardHit()
'	Else
		RandomSoundBottomArchBallGuide
'	End If
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


'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************





'****************************************************************
' Section; Debug Shot Tester 2.0
' 1.  Raise/Lower outlanes and drain posts by pressing 2 key
' 2.  Capture and Launch ball, Press and hold one of the buttons (W, E, R, Y, U, I, P, A) below to capture ball by flipper.  Release key to shoot ball
' 3.  To change the test shot angles, press and hold a key and use Flipper keys to adjust the shot angle.  Shot angles are saved into the User direction as cgamename.txt
' 4.  Set TestMode = 0 to disable test code.
'
' HOW TO INSTALL: Copy all objects from Layer 2 to table and adjust. Copy the Debug Shot Tester code starting at line 500 to bottom of table script.
'	Add "TestTableKeyDownCheck keycode" to top of Table1_KeyDown sub and add "TestTableKeyUpCheck keycode" to top of Table1_KeyUp sub
'****************************************************************
'Const TestMode = 0		'Set to 0 to disable.  1 to enable
Dim KickerDebugForce: KickerDebugForce = 30

' Enable Disable Outlane and Drain Blocker Wall for debug testing
Dim BLState
debug_BLW1.IsDropped=1:debug_BLP1.Visible=0:debug_BLR1.Visible=0: debug_BLW2.IsDropped=1:debug_BLP2.Visible=0:debug_BLR2.Visible=0: debug_BLW3.IsDropped=1:debug_BLP3.Visible=0:debug_BLR3.Visible=0
Sub BlockerWalls

	BLState = (BLState + 1) Mod 4
'	debug.print "BlockerWalls"
	PlaySound ("Start_Button")
	Select Case BLState:
		Case 0
			debug_BLW1.IsDropped=1:debug_BLP1.Visible=0:debug_BLR1.Visible=0: debug_BLW2.IsDropped=1:debug_BLP2.Visible=0:debug_BLR2.Visible=0: debug_BLW3.IsDropped=1:debug_BLP3.Visible=0:debug_BLR3.Visible=0
		Case 1:
			debug_BLW1.IsDropped=0:debug_BLP1.Visible=1:debug_BLR1.Visible=1: debug_BLW2.IsDropped=0:debug_BLP2.Visible=1:debug_BLR2.Visible=1: debug_BLW3.IsDropped=0:debug_BLP3.Visible=1:debug_BLR3.Visible=1
		Case 2:
			debug_BLW1.IsDropped=0:debug_BLP1.Visible=1:debug_BLR1.Visible=1: debug_BLW2.IsDropped=0:debug_BLP2.Visible=1:debug_BLR2.Visible=1: debug_BLW3.IsDropped=1:debug_BLP3.Visible=0:debug_BLR3.Visible=0
		Case 3:
			debug_BLW1.IsDropped=1:debug_BLP1.Visible=0:debug_BLR1.Visible=0: debug_BLW2.IsDropped=1:debug_BLP2.Visible=0:debug_BLR2.Visible=0: debug_BLW3.IsDropped=0:debug_BLP3.Visible=1:debug_BLR3.Visible=1
	End Select
End Sub


Sub TestTableKeyDownCheck (Keycode)

	'Cycle through Outlane/Centerlane blocking posts
	'-----------------------------------------------
	If Keycode = 3 Then	
		BlockerWalls
	End If

	If TestMode = 1 Then

		'Capture and launch ball:	
		'	Press and hold one of the buttons (W, E, R, T, Y, U, I, P) below to capture ball by flipper.  Release key to shoot ball
		'	In the Keydown sub, set up ball launch angle and force
		'--------------------------------------------------------------------------------------------
		If keycode = 17 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleW	'W key
		If keycode = 18 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleE	'E key
		If keycode = 19 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleR	'R key
		If keycode = 21 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleY	'Y key
		If keycode = 22 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleU	'U key
		If keycode = 23 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleI	'I key
		If keycode = 25 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleP	'P key
		If keycode = 30 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleA	'A key
		If keycode = 31 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleS	'S key
		If keycode = 33 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleF	'F key
		If keycode = 34 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleG	'G key
		If keycode = 35 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleH	'H key
		If keycode = 36 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleJ	'J key
		If keycode = 37 Then KickerDebug.enabled = true : TestKickerVar = TestKickAngleK	'K key

		If KickerDebug.enabled = true Then		'Use Flippers to adjust angle while holding key
			If keycode = leftflipperkey Then
				TestKickAim.Visible = True
				TestKickerVar = TestKickerVar - 1
				'debug.print TestKickerVar
			ElseIf keycode = rightflipperkey Then
				TestKickAim.Visible = True
				TestKickerVar = TestKickerVar + 1
				'debug.print TestKickerVar
			End If
			TestKickAim.ObjRotz = TestKickerVar 
		End If

	End If
End Sub


Sub TestTableKeyUpCheck (Keycode)

	' Capture and launch ball:
	' Press and hold one of the buttons below to capture ball by flipper.  Release to shoot ball. Set up angle and force as needed for each shot.  
	'--------------------------------------------------------------------------------------------
	If TestMode = 1 Then
		If keycode = 17 Then TestKickAngleW = TestKickerVar : KickerDebug.kick TestKickAngleW, KickerDebugForce: KickerDebug.enabled = false		'W key		
		If keycode = 18 Then TestKickAngleE = TestKickerVar : KickerDebug.kick TestKickAngleE, KickerDebugForce: KickerDebug.enabled = false		'E key		
		If keycode = 19 Then TestKickAngleR = TestKickerVar : KickerDebug.kick TestKickAngleR, KickerDebugForce: KickerDebug.enabled = false: 		'R key		
		If keycode = 21 Then TestKickAngleY = TestKickerVar : KickerDebug.kick TestKickAngleY, KickerDebugForce: KickerDebug.enabled = false		'Y key		
		If keycode = 22 Then TestKickAngleU = TestKickerVar : KickerDebug.kick TestKickAngleU, KickerDebugForce: KickerDebug.enabled = false		'U key		
		If keycode = 23 Then TestKickAngleI = TestKickerVar : KickerDebug.kick TestKickAngleI, KickerDebugForce: KickerDebug.enabled = false		'I key		
		If keycode = 25 Then TestKickAngleP = TestKickerVar : KickerDebug.kick TestKickAngleP, KickerDebugForce: KickerDebug.enabled = false		'P key		
		If keycode = 30 Then TestKickAngleA = TestKickerVar : KickerDebug.kick TestKickAngleA, KickerDebugForce: KickerDebug.enabled = false		'A key		
		If keycode = 31 Then TestKickAngleS = TestKickerVar : KickerDebug.kick TestKickAngleS, KickerDebugForce: KickerDebug.enabled = false		'S key		
		If keycode = 33 Then TestKickAngleF = TestKickerVar : KickerDebug.kick TestKickAngleF, KickerDebugForce: KickerDebug.enabled = false		'F key		
		If keycode = 34 Then TestKickAngleG = TestKickerVar : KickerDebug.kick TestKickAngleG, KickerDebugForce: KickerDebug.enabled = false		'G key		
		If keycode = 35 Then TestKickAngleH = TestKickerVar : KickerDebug.kick TestKickAngleH, KickerDebugForce: KickerDebug.enabled = false		'H key		
		If keycode = 36 Then TestKickAngleJ = TestKickerVar : KickerDebug.kick TestKickAngleJ, KickerDebugForce: KickerDebug.enabled = false		'J key		
		If keycode = 37 Then TestKickAngleK = TestKickerVar : KickerDebugBall.X=430: KickerDebugBall.Y=1456: KickerDebugBall.Z = 25: KickerDebug.kick TestKickAngleK, KickerDebugForce: KickerDebug.enabled = false		'K key		

'		EXAMPLE CODE to set up key to cycle through 3 predefined shots
'		If keycode = 17 Then 	'Cycle through all left target shots
'			If TestKickerAngle = -28 then 
'				TestKickerAngle = -24
'			ElseIf TestKickerAngle = -24 Then
'				TestKickerAngle = -19
'			Else
'				TestKickerAngle = -28
'			End If
'			KickerDebug.kick TestKickerAngle, KickerDebugforce: KickerDebug.enabled = false		 	'W key	
'		End If

	End If

	If (KickerDebug.enabled = False and TestKickAim.Visible = True) Then 'Save Angle changes
		TestKickAim.Visible = False
		SaveTestKickAngles
	End If

End Sub

Dim KickerDebugBall
Sub Kickerdebug_Hit ()
	Set KickerDebugBall = ActiveBall
End Sub

Dim TestKickerAngle, TestKickerAngle2, TestKickerVar, TeskKickKey, TestKickForce
Dim TestKickAngleWDefault, TestKickAngleEDefault, TestKickAngleRDefault, TestKickAngleYDefault, TestKickAngleUDefault, TestKickAngleIDefault 
Dim TestKickAnglePDefault, TestKickAngleADefault, TestKickAngleSDefault, TestKickAngleFDefault, TestKickAngleGDefault, TestKickAngleHDefault, TestKickAngleJDefault, TestKickAngleKDefault
Dim TestKickAngleW, TestKickAngleE, TestKickAngleR, TestKickAngleY, TestKickAngleU, TestKickAngleI
Dim TestKickAngleP, TestKickAngleA, TestKickAngleS, TestKickAngleF, TestKickAngleG, TestKickAngleH, TestKickAngleJ, TestKickAngleK
TestKickAngleWDefault = -30
TestKickAngleEDefault = -26
TestKickAngleRDefault = -22
TestKickAngleYDefault = -19
TestKickAngleUDefault = -11
TestKickAngleIDefault = 0
TestKickAnglePDefault = 10
TestKickAngleADefault = 13
TestKickAngleSDefault = 19
TestKickAngleFDefault = 22
TestKickAngleGDefault = 24
TestKickAngleHDefault = 27
TestKickAngleJDefault = 30
TestKickAngleKDefault = 41

If TestMode = 1 Then LoadTestKickAngles

Sub SaveTestKickAngles
	Dim FileObj, OutFile
	Set FileObj = CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then Exit Sub
	Set OutFile=FileObj.CreateTextFile(UserDirectory & cGameName & ".txt", True)

	OutFile.WriteLine TestKickAngleW
	OutFile.WriteLine TestKickAngleE
	OutFile.WriteLine TestKickAngleR
	OutFile.WriteLine TestKickAngleY
	OutFile.WriteLine TestKickAngleU
	OutFile.WriteLine TestKickAngleI
	OutFile.WriteLine TestKickAngleP
	OutFile.WriteLine TestKickAngleA
	OutFile.WriteLine TestKickAngleS
	OutFile.WriteLine TestKickAngleF
	OutFile.WriteLine TestKickAngleG
	OutFile.WriteLine TestKickAngleH
	OutFile.WriteLine TestKickAngleJ
	OutFile.WriteLine TestKickAngleK
	OutFile.Close

	Set OutFile = Nothing
	Set FileObj = Nothing
End Sub

Sub LoadTestKickAngles
	Dim FileObj, OutFile, TextStr
 
    Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then MsgBox "User directory missing": Exit Sub

	If FileObj.FileExists(UserDirectory & cGameName & ".txt") then 

		Set OutFile = FileObj.GetFile(UserDirectory & cGameName & ".txt")
		Set TextStr = OutFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream = True) then Exit Sub

		TestKickAngleW = TextStr.ReadLine
		TestKickAngleE = TextStr.ReadLine
		TestKickAngleR = TextStr.ReadLine
		TestKickAngleY = TextStr.ReadLine
		TestKickAngleU = TextStr.ReadLine
		TestKickAngleI = TextStr.ReadLine
		TestKickAngleP = TextStr.ReadLine
		TestKickAngleA = TextStr.ReadLine
		TestKickAngleS = TextStr.ReadLine
		TestKickAngleF = TextStr.ReadLine
		TestKickAngleG = TextStr.ReadLine
		If (TextStr.AtEndOfStream = True) then TestKickAngleH = TestKickAngleHDefault: Else: TestKickAngleH = TextStr.ReadLine: End If
		If (TextStr.AtEndOfStream = True) then TestKickAngleJ = TestKickAngleJDefault: Else: TestKickAngleJ = TextStr.ReadLine: End If
		If (TextStr.AtEndOfStream = True) then TestKickAngleK = TestKickAngleKDefault: Else: TestKickAngleK = TextStr.ReadLine: End If
'		TestKickAngleJ = TextStr.ReadLine
'		TestKickAngleK = TextStr.ReadLine
		TextStr.Close


	Else
		'create file
		TestKickAngleW = TestKickAngleWDefault: 
		TestKickAngleE = TestKickAngleEDefault 
		TestKickAngleR = TestKickAngleRDefault
		TestKickAngleY = TestKickAngleYDefault
		TestKickAngleU = TestKickAngleUDefault
		TestKickAngleI = TestKickAngleIDefault    
		TestKickAngleP = TestKickAnglePDefault
		TestKickAngleA = TestKickAngleADefault
		TestKickAngleS = TestKickAngleSDefault
		TestKickAngleF = TestKickAngleFDefault
		TestKickAngleG = TestKickAngleGDefault
		TestKickAngleH = TestKickAnglehDefault
		TestKickAngleJ = TestKickAngleJDefault
		TestKickAngleK = TestKickAngleKDefault
		SaveTestKickAngles

	End If

	Set OutFile = Nothing
	Set FileObj = Nothing

End Sub


'******************************************************
'*******	Set Up VR Backglass Flashers	*******
'******************************************************
Dim BGObj

Sub SetBackglass()

	For Each BGObj In Backglass_flashers
		BGObj.x = BGobj.x
		BGObj.height = - BGObj.y + 140
		BGObj.y =90 'adjusts the distance from the backglass towards the user
	Next

End Sub

'**********************************************

'*********************************






' ***************************************************************************
'          VR Backglass Code
' ****************************************************************************


' ***************************************************************************
'          (EM) 1-4 player 5x drums, 1 credit drum CORE CODE
' ****************************************************************************


' ********************* POSITION EM REEL DRUMS ON BACKGLASS *************************

Dim xoff,yoff,zoff,xrot,zscale, xcen,ycen
Dim inx
xoff =472
yoff = 0
zoff =835
xrot = -90

Const USEEMS = 4 ' 1-4 set between 1 to 4 based on number of players

const idx_emp1r1 =0 'player 1
const idx_emp2r1 =5 'player 2
const idx_emp3r1 =10 'player 3
const idx_emp4r1 =15 'player 4
const idx_emp4r6 =20 'credits


Dim BGObjEM(1)
if USEEMS = 1 then
	BGObjEM(0) = Array(emp1r1, emp1r2, emp1r3, emp1r4, emp1r5, _
	Empty,Empty,Empty,Empty,Empty,_
	Empty,Empty,Empty,Empty,Empty,_
	Empty,Empty,Empty,Empty,Empty,_
	emp4r6) ' credits
elseif USEEMS = 2 then
	BGObjEM(0) = Array(emp1r1, emp1r2, emp1r3, emp1r4, emp1r5, _
	emp2r1, emp2r2, emp2r3, emp2r4, emp2r5, _
	Empty,Empty,Empty,Empty,Empty,_
	Empty,Empty,Empty,Empty,Empty,_
	emp4r6) ' credits
elseif USEEMS = 3 then
	BGObjEM(0) = Array(emp1r1, emp1r2, emp1r3, emp1r4, emp1r5, _
	emp2r1, emp2r2, emp2r3, emp2r4, emp2r5, _
	emp3r1, emp3r2, emp3r3, emp3r4, emp3r5, _
	Empty,Empty,Empty,Empty,Empty,_
	emp4r6) ' credits
elseif USEEMS = 4 then
	BGObjEM(0) = Array(emp1r1, emp1r2, emp1r3, emp1r4, emp1r5, _
	emp2r1, emp2r2, emp2r3, emp2r4, emp2r5, _
	emp3r1, emp3r2, emp3r3, emp3r4, emp3r5, _
	emp4r1, emp4r2, emp4r3, emp4r4, emp4r5, _
	emp4r6) ' credits
end If

Sub center_objects_em()
Dim cnt,ii, xx, yy, yfact, xfact, objs
'exit sub
yoff = -150
zscale = 0.0000001
xcen =(960 /2) - (17 / 2)
ycen = (1065 /2 ) + (313 /2)

yfact = -25
xfact = 0

cnt =0
	For Each objs In BGObjEM(0)
	If Not IsEmpty(objs) then
		if objs.name = emp4r6.name then
		yoff = 45 ' credit drum is 60% smaller
		Else
		yoff = 2
		end if

	xx =objs.x 
		
	objs.x = (xoff - xcen) + xx + xfact
	yy = objs.y 
	objs.y =yoff 

		If yy < 0 then
		yy = yy * -1
		end if

	objs.z = (zoff - ycen) + yy - (yy * zscale) + yfact
	
	'objs.rotx = xrot
	end if
	cnt = cnt + 1
	Next

end sub



' ********************* UPDATE EM REEL DRUMS CORE LIB *************************

Dim cred,ix, np,npp, reels(5, 7), scores(6,2)

'reset scores to defaults
for np =0 to 5 
scores(np,0 ) = 0
scores(np,1 ) = 0
Next

'reset EM drums to defaults
For np =0 to 3 
	For  npp =0 to 6
	reels(np, npp) =0 ' default to zero 
	Next
Next


Sub SetScore(player, ndx , val) 

Dim ncnt 

	if player = 5 or player = 6 then
		if val > 0 then
			If(ndx = 0)Then ncnt = val * 10
			If(ndx = 1)Then ncnt = val 

			scores(player, 0) = scores(player, 0) + ncnt  
		end if
	else
		if val > 0 then

		If(ndx = 0)then ncnt = val * 10000
		If(ndx = 1)then ncnt = val * 1000
		If(ndx = 2)Then ncnt = val * 100
		If(ndx = 3)Then ncnt = val * 10
		If(ndx = 4)Then ncnt = val 

		scores(player, 0) = scores(player, 0) + ncnt  
		'scores(player, 0) + ncnt  

		end if
	end if
End Sub


Sub SetDrum(player, drum , val) 
Dim cnt
Dim objs : objs =BGObjEM(0)

	If val = 0 then
		Select case player
		case -1: ' the credit drum
		If Not IsEmpty(objs(idx_emp4r6)) then
		objs(idx_emp4r6).ObjrotX = 0 ' 285
		'cnt =objs(idx_emp4r6).ObjrotX
		end if
		Case 0: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp1r1)) then: objs(idx_emp1r1).ObjrotX = 0: end if' 283
				Case 2: If Not IsEmpty(objs(idx_emp1r1+1)) then: objs(idx_emp1r1+1).ObjrotX=0: end if
				Case 3: If Not IsEmpty(objs(idx_emp1r1+2)) then: objs(idx_emp1r1+2).ObjrotX=0: end if
				Case 4: If Not IsEmpty(objs(idx_emp1r1+3)) then: objs(idx_emp1r1+3).ObjrotX=0: end if
				Case 5: If Not IsEmpty(objs(idx_emp1r1+4)) then: objs(idx_emp1r1+4).ObjrotX=0: end if
		End Select
		Case 1: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp2r1)) then: objs(idx_emp2r1).ObjrotX = 0: end if' 283
				Case 2: If Not IsEmpty(objs(idx_emp2r1+1)) then: objs(idx_emp2r1+1).ObjrotX=0: end if
				Case 3: If Not IsEmpty(objs(idx_emp2r1+2)) then: objs(idx_emp2r1+2).ObjrotX=0: end if
				Case 4: If Not IsEmpty(objs(idx_emp2r1+3)) then: objs(idx_emp2r1+3).ObjrotX=0: end if
				Case 5: If Not IsEmpty(objs(idx_emp2r1+4)) then: objs(idx_emp2r1+4).ObjrotX=0: end if
		End Select
		Case 2: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp3r1)) then: objs(idx_emp3r1).ObjrotX = 0: end if' 283
				Case 2: If Not IsEmpty(objs(idx_emp3r1+1)) then: objs(idx_emp3r1+1).ObjrotX=0: end if
				Case 3: If Not IsEmpty(objs(idx_emp3r1+2)) then: objs(idx_emp3r1+2).ObjrotX=0: end if
				Case 4: If Not IsEmpty(objs(idx_emp3r1+3)) then: objs(idx_emp3r1+3).ObjrotX=0: end if
				Case 5: If Not IsEmpty(objs(idx_emp3r1+4)) then: objs(idx_emp3r1+4).ObjrotX=0: end if
		End Select
		Case 3: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp4r1)) then: objs(idx_emp4r1).ObjrotX = 0: end if' 283
				Case 2: If Not IsEmpty(objs(idx_emp4r1+1)) then: objs(idx_emp4r1+1).ObjrotX=0: end if
				Case 3: If Not IsEmpty(objs(idx_emp4r1+2)) then: objs(idx_emp4r1+2).ObjrotX=0: end if
				Case 4: If Not IsEmpty(objs(idx_emp4r1+3)) then: objs(idx_emp4r1+3).ObjrotX=0: end if
				Case 5: If Not IsEmpty(objs(idx_emp4r1+4)) then: objs(idx_emp4r1+4).ObjrotX=0: end if
		End Select
	End Select

	else
	Select case player

		Case -1: ' the credit drum
		'emp4r6.ObjrotX = emp4r6.ObjrotX + val
		If Not IsEmpty(objs(idx_emp4r6)) then
		objs(idx_emp4r6).ObjrotX = objs(idx_emp4r6).ObjrotX + val 
		end if

		Case 0: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp1r1)) then: objs(idx_emp1r1).ObjrotX= objs(idx_emp1r1).ObjrotX + val: end if
				Case 2: If Not IsEmpty(objs(idx_emp1r1+1)) then: objs(idx_emp1r1+1).ObjrotX= objs(idx_emp1r1+1).ObjrotX + val: end if
				Case 3: If Not IsEmpty(objs(idx_emp1r1+2)) then: objs(idx_emp1r1+2).ObjrotX= objs(idx_emp1r1+2).ObjrotX + val: end if
				Case 4: If Not IsEmpty(objs(idx_emp1r1+3)) then: objs(idx_emp1r1+3).ObjrotX= objs(idx_emp1r1+3).ObjrotX + val: end if
				Case 5: If Not IsEmpty(objs(idx_emp1r1+4)) then: objs(idx_emp1r1+4).ObjrotX= objs(idx_emp1r1+4).ObjrotX + val: end if
		End Select
		Case 1: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp2r1)) then: objs(idx_emp2r1).ObjrotX= objs(idx_emp2r1).ObjrotX + val: end if
				Case 2: If Not IsEmpty(objs(idx_emp2r1+1)) then: objs(idx_emp2r1+1).ObjrotX= objs(idx_emp2r1+1).ObjrotX + val: end if
				Case 3: If Not IsEmpty(objs(idx_emp2r1+2)) then: objs(idx_emp2r1+2).ObjrotX= objs(idx_emp2r1+2).ObjrotX + val: end if
				Case 4: If Not IsEmpty(objs(idx_emp2r1+3)) then: objs(idx_emp2r1+3).ObjrotX= objs(idx_emp2r1+3).ObjrotX + val: end if
				Case 5: If Not IsEmpty(objs(idx_emp2r1+4)) then: objs(idx_emp2r1+4).ObjrotX= objs(idx_emp2r1+4).ObjrotX + val: end if
		End Select
		Case 2: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp3r1)) then: objs(idx_emp3r1).ObjrotX= objs(idx_emp3r1).ObjrotX + val: end if
				Case 2: If Not IsEmpty(objs(idx_emp3r1+1)) then: objs(idx_emp3r1+1).ObjrotX= objs(idx_emp3r1+1).ObjrotX + val: end if
				Case 3: If Not IsEmpty(objs(idx_emp3r1+2)) then: objs(idx_emp3r1+2).ObjrotX= objs(idx_emp3r1+2).ObjrotX + val: end if
				Case 4: If Not IsEmpty(objs(idx_emp3r1+3)) then: objs(idx_emp3r1+3).ObjrotX= objs(idx_emp3r1+3).ObjrotX + val: end if
				Case 5: If Not IsEmpty(objs(idx_emp3r1+4)) then: objs(idx_emp3r1+4).ObjrotX= objs(idx_emp3r1+4).ObjrotX + val: end if
		End Select
		Case 3: 
		Select Case drum 
				Case 1: If Not IsEmpty(objs(idx_emp4r1)) then: objs(idx_emp4r1).ObjrotX= objs(idx_emp4r1).ObjrotX + val: end if
				Case 2: If Not IsEmpty(objs(idx_emp4r1+1)) then: objs(idx_emp4r1+1).ObjrotX= objs(idx_emp4r1+1).ObjrotX + val: end if
				Case 3: If Not IsEmpty(objs(idx_emp4r1+2)) then: objs(idx_emp4r1+2).ObjrotX= objs(idx_emp4r1+2).ObjrotX + val: end if
				Case 4: If Not IsEmpty(objs(idx_emp4r1+3)) then: objs(idx_emp4r1+3).ObjrotX= objs(idx_emp4r1+3).ObjrotX + val: end if
				Case 5: If Not IsEmpty(objs(idx_emp4r1+4)) then: objs(idx_emp4r1+4).ObjrotX= objs(idx_emp4r1+4).ObjrotX + val: end if
		End Select

	End Select
	end if
End Sub


Sub SetReel(player, drum, val)

'TextBox1.text = "playr:" & player +1 & " drum:" & drum & "val:" & val  

Dim  inc , cur, dif, fix, fval

inc = 33.5
fval = -5 ' graphic seam between 5 & 6 fix value, easier to fix here than photoshop 

If  (player <= 3) or (drum = -1) then

	If drum = -1 then drum = 0

	cur =reels(player, drum)	

	If val <> cur then ' something has changed
	Select Case drum

		Case 0: ' credits drum

			if val > cur then
				dif =val - cur
				fix =0
					If cur < 5 and cur+dif > 5 then
					fix = fix- fval
					end if
				dif = dif * inc
					
				dif = dif-fix
					
				SetDrum -1,0,  -dif
			Else
				if val = 0 Then	
				SetDrum -1,0,  0' reset the drum to abs. zero
				Else
				dif = 11 - cur
				dif = dif + val

				dif = dif * inc
				dif = dif-fval

				SetDrum -1,0,   -dif
				end if
			end if
		Case 1: 
		'TB1.text = val
		if val > cur then
			dif =val - cur
			fix =0
				If cur < 5 and cur+dif > 5 then
				fix = fix- fval
				end if
			dif = dif * inc
				
			dif = dif-fix
				
			SetDrum player,drum,  -dif
		Else
			if val = 0 Then	
			SetDrum player,drum,  0' reset the drum to abs. zero
			Else
			dif = 11 - cur
			dif = dif + val

			dif = dif * inc
			dif = dif-fval

			SetDrum player,drum,   -dif
			end if

		end if
		reels(player, drum) = val

		Case 2: 
		'TB2.text = val

		if val > cur then
			dif =val - cur
			fix =0
				If cur < 5 and cur+dif > 5 then
				fix = fix- fval
				end if
			dif = dif * inc
			dif = dif-fix
			SetDrum player,drum,  -dif
		Else
			if val = 0 Then	
			SetDrum player,drum,  0 ' reset the drum to abs. zero
			Else
			dif = 11 - cur
			dif = dif + val
			dif = dif * inc
			dif = dif-fval
			SetDrum player,drum,  -dif 
			end if
		end if
		reels(player, drum) = val

		Case 3: 
		'TB3.text = val

		if val > cur then
			dif =val - cur
			fix =0
				If cur < 5 and cur+dif > 5 then
				fix = fix- fval
				end if
			dif = dif * inc
			dif = dif-fix
			
			SetDrum player,drum,  -dif
		Else
			if val = 0 Then	
			SetDrum player,drum,  0 ' reset the drum to abs. zero
			Else
			dif = 11 - cur
			dif = dif + val
			dif = dif * inc
			dif = dif-fval
			SetDrum player,drum,  -dif 
			end if

		end if
		reels(player, drum) = val

		Case 4: 
		'TB4.text = val

		if val > cur then
			dif =val - cur
			fix =0
				If cur < 5 and cur+dif > 5 then
				fix = fix- fval
				end if
			dif = dif * inc
			dif = dif-fix
			SetDrum player,drum,  -dif 
		Else
			if val = 0 Then	
			SetDrum player,drum,  0 ' reset the drum to abs. zero
			Else
			dif = 11 - cur
			dif = dif + val
			dif = dif * inc
			dif = dif-fval
			SetDrum player,drum,  -dif 
			end if

		end if
		reels(player, drum) = val

		Case 5: 
		'TB5.text = val

		if val > cur then
			dif =val - cur
			fix =0
				If cur < 5 and cur+dif > 5 then
				fix = fix- fval
				end if
			dif = dif * inc
			dif = dif-fix
			SetDrum player,drum,  -dif 
		Else
			if val = 0 Then	
			SetDrum player,drum,  0 ' reset the drum to abs. zero
			Else
			dif = 11 - cur
			dif = dif + val
			dif = dif * inc
			dif = dif-fval
			SetDrum player,drum,  -dif 
			end if

		end if
		reels(player, drum) = val
	 End Select

	end if
end if
End Sub

Dim EMMODE: EMMODE = 0
'Dim Score10,Score100,Score1000,Score10000,Score100000
Dim Score1000,Score10000,Score100000, ActivePLayer
Dim nplayer,playr,value,curscr,curplayr

' EMMODE = 1
'ex: UpdateVRReels 0-3, 0-3, 0-199999, n/a, n/a, n/a, n/a, n/a, n/a
' EMMODE = 0
'ex: UpdateVRReels 0-3, 0-3, n/a, 0-1,0-99999 ,0-9999, 0-999, 0-99, 0-9  

Sub UpdateVRReels (Player,nReels ,nScore, n100K, Score10000 ,Score1000,Score100,Score10,Score1)

' to-do find out if player is one or zero based, if 1 based subtract 1.
value =nScore'Score(Player)
	nplayer = Player -1

	curscr = value
	curplayr = nplayer


scores(0,1) = scores(0,0)
	scores(0,0) = 0
	scores(1,1) = scores(1,0)
	scores(1,0) = 0
	scores(2,1) = scores(2,0)
	scores(2,0) = 0
	scores(3,1) = scores(3,0)
	scores(3,0) = 0

	For  ix =0 to 6
		reels(0, ix) =0 
		reels(1, ix) =0 
		reels(2, ix) =0 
		reels(3, ix) =0 
	Next
	
	For  ix =0 to 4
		
	SetDrum ix, 1 , 0 
	SetDrum ix, 2 , 0  
	SetDrum ix, 3 , 0  
	SetDrum ix, 4 , 0  
	SetDrum ix, 5 , 0  
		
	Next 
	
	For playr =0 to nReels
	
		if EMMODE = 0 then
		If (ActivePLayer) = playr Then
		nplayer = playr

		SetReel nplayer, 1 , Score10000 : SetScore nplayer,0,Score10000
		SetReel nplayer, 2 , Score1000 : SetScore nplayer,1,Score1000
		SetReel nplayer, 3 , Score100 : SetScore nplayer,2,Score100
		SetReel nplayer, 4 , Score10 : SetScore nplayer,3,Score10 
		SetReel nplayer, 5 , 0 : SetScore nplayer,4,0 ' assumes ones position is always zero 

		else
		nplayer = playr
		value =scores(nplayer, 1) 
	
	
	' do ten thousands
		if(value >= 90000)  then:  SetReel nplayer, 1 , 9 : SetScore nplayer,0,9 : value = value - 90000: end if
		if(value >= 80000)  then:  SetReel nplayer, 1 , 8 : SetScore nplayer,0,8 : value = value - 80000: end if
		if(value >= 70000)  then:  SetReel nplayer, 1 , 7 : SetScore nplayer,0,7 : value = value - 70000: end if
		if(value >= 60000)  then:  SetReel nplayer, 1 , 6 : SetScore nplayer,0,6 : value = value - 60000: end if
		if(value >= 50000)  then:  SetReel nplayer, 1 , 5 : SetScore nplayer,0,5 : value = value - 50000: end if
		if(value >= 40000)  then:  SetReel nplayer, 1 , 4 : SetScore nplayer,0,4 : value = value - 40000: end if
		if(value >= 30000)  then:  SetReel nplayer, 1 , 3 : SetScore nplayer,0,3 : value = value - 30000: end if
		if(value >= 20000)  then:  SetReel nplayer, 1 , 2 : SetScore nplayer,0,2 : value = value - 20000: end if
		if(value >= 10000)  then:  SetReel nplayer, 1 , 1 : SetScore nplayer,0,1 : value = value - 10000: end if


	' do thousands
		if(value >= 9000)  then:  SetReel nplayer, 2 , 9 : SetScore nplayer,1,9 : value = value - 9000: end if
		if(value >= 8000)  then:  SetReel nplayer, 2 , 8 : SetScore nplayer,1,8 : value = value - 8000: end if
		if(value >= 7000)  then:  SetReel nplayer, 2 , 7 : SetScore nplayer,1,7 : value = value - 7000: end if
		if(value >= 6000)  then:  SetReel nplayer, 2 , 6 : SetScore nplayer,1,6 : value = value - 6000: end if
		if(value >= 5000)  then:  SetReel nplayer, 2 , 5 : SetScore nplayer,1,5 : value = value - 5000: end if
		if(value >= 4000)  then:  SetReel nplayer, 2 , 4 : SetScore nplayer,1,4 : value = value - 4000: end if
		if(value >= 3000)  then:  SetReel nplayer, 2 , 3 : SetScore nplayer,1,3 : value = value - 3000: end if
		if(value >= 2000)  then:  SetReel nplayer, 2 , 2 : SetScore nplayer,1,2 : value = value - 2000: end if
		if(value >= 1000)  then:  SetReel nplayer, 2 , 1 : SetScore nplayer,1,1 : value = value - 1000: end if

		'do hundreds
		
		if(value >= 900) then: SetReel nplayer, 3 , 9 : SetScore nplayer,2,9 : value = value - 900: end if
		if(value >= 800) then: SetReel nplayer, 3 , 8 : SetScore nplayer,2,8 : value = value - 800: end if
		if(value >= 700) then: SetReel nplayer, 3 , 7 : SetScore nplayer,2,7 : value = value - 700: end if
		if(value >= 600) then: SetReel nplayer, 3 , 6 : SetScore nplayer,2,6 : value = value - 600: end if
		if(value >= 500) then: SetReel nplayer, 3 , 5 : SetScore nplayer,2,5 : value = value - 500: end if
		if(value >= 400) then: SetReel nplayer, 3 , 4 : SetScore nplayer,2,4 : value = value - 400: end if
		if(value >= 300) then: SetReel nplayer, 3 , 3 : SetScore nplayer,2,3 : value = value - 300: end if
		if(value >= 200) then: SetReel nplayer, 3 , 2 : SetScore nplayer,2,2 : value = value - 200: end if
		if(value >= 100) then: SetReel nplayer, 3 , 1 : SetScore nplayer,2,1 : value = value - 100: end if

		'do tens
		if(value >= 90) then: SetReel nplayer, 4 , 9 : SetScore nplayer,3,9 : value = value - 90: end if
		if(value >= 80) then: SetReel nplayer, 4 , 8 : SetScore nplayer,3,8 : value = value - 80: end if
		if(value >= 70) then: SetReel nplayer, 4 , 7 : SetScore nplayer,3,7 : value = value - 70: end if
		if(value >= 60) then: SetReel nplayer, 4 , 6 : SetScore nplayer,3,6 : value = value - 60: end if
		if(value >= 50) then: SetReel nplayer, 4 , 5 : SetScore nplayer,3,5 : value = value - 50: end if
		if(value >= 40) then: SetReel nplayer, 4 , 4 : SetScore nplayer,3,4 : value = value - 40: end if
		if(value >= 30) then: SetReel nplayer, 4 , 3 : SetScore nplayer,3,3 : value = value - 30: end if
		if(value >= 20) then: SetReel nplayer, 4 , 2 : SetScore nplayer,3,2 : value = value - 20: end if
		if(value >= 10) then: SetReel nplayer, 4 , 1 : SetScore nplayer,3,1 : value = value - 10: end if

		'do ones
		if(value >= 9) then: SetReel nplayer, 5 , 9 : SetScore nplayer,4,9 : value = value - 9: end if
		if(value >= 8) then: SetReel nplayer, 5 , 8 : SetScore nplayer,4,8 : value = value - 8: end if
		if(value >= 7) then: SetReel nplayer, 5 , 7 : SetScore nplayer,4,7 : value = value - 7: end if
		if(value >= 6) then: SetReel nplayer, 5 , 6 : SetScore nplayer,4,6 : value = value - 6: end if
		if(value >= 5) then: SetReel nplayer, 5 , 5 : SetScore nplayer,4,5 : value = value - 5: end if
		if(value >= 4) then: SetReel nplayer, 5 , 4 : SetScore nplayer,4,4 : value = value - 4: end if
		if(value >= 3) then: SetReel nplayer, 5 , 3 : SetScore nplayer,4,3 : value = value - 3: end if
		if(value >= 2) then: SetReel nplayer, 5 , 2 : SetScore nplayer,4,2 : value = value - 2: end if
		if(value >= 1) then: SetReel nplayer, 5 , 1 : SetScore nplayer,4,1 : value = value - 1: end if

		end if
		Else
			If curplayr = playr Then
			nplayer = curplayr
			value = curscr
			else
			value =scores(playr, 1) ' store score
			nplayer = playr
			end if

		scores(playr, 0)  = 0 ' reset score

		if(value >= 100000) then

			'if nplayer = 0 then: FL100K1.visible = 1
			'if nplayer = 1 then: FL100K2.visible = 1
			'if nplayer = 2 then: FL100K3.visible = 1
			'if nplayer = 3 then: FL100K4.visible = 1

		value = value - 100000
		
		end if


	' do ten thousands
		if(value >= 90000)  then:  SetReel nplayer, 1 , 9 : SetScore nplayer,0,9 : value = value - 90000: end if
		if(value >= 80000)  then:  SetReel nplayer, 1 , 8 : SetScore nplayer,0,8 : value = value - 80000: end if
		if(value >= 70000)  then:  SetReel nplayer, 1 , 7 : SetScore nplayer,0,7 : value = value - 70000: end if
		if(value >= 60000)  then:  SetReel nplayer, 1 , 6 : SetScore nplayer,0,6 : value = value - 60000: end if
		if(value >= 50000)  then:  SetReel nplayer, 1 , 5 : SetScore nplayer,0,5 : value = value - 50000: end if
		if(value >= 40000)  then:  SetReel nplayer, 1 , 4 : SetScore nplayer,0,4 : value = value - 40000: end if
		if(value >= 30000)  then:  SetReel nplayer, 1 , 3 : SetScore nplayer,0,3 : value = value - 30000: end if
		if(value >= 20000)  then:  SetReel nplayer, 1 , 2 : SetScore nplayer,0,2 : value = value - 20000: end if
		if(value >= 10000)  then:  SetReel nplayer, 1 , 1 : SetScore nplayer,0,1 : value = value - 10000: end if


	' do thousands
		if(value >= 9000)  then:  SetReel nplayer, 2 , 9 : SetScore nplayer,1,9 : value = value - 9000: end if
		if(value >= 8000)  then:  SetReel nplayer, 2 , 8 : SetScore nplayer,1,8 : value = value - 8000: end if
		if(value >= 7000)  then:  SetReel nplayer, 2 , 7 : SetScore nplayer,1,7 : value = value - 7000: end if
		if(value >= 6000)  then:  SetReel nplayer, 2 , 6 : SetScore nplayer,1,6 : value = value - 6000: end if
		if(value >= 5000)  then:  SetReel nplayer, 2 , 5 : SetScore nplayer,1,5 : value = value - 5000: end if
		if(value >= 4000)  then:  SetReel nplayer, 2 , 4 : SetScore nplayer,1,4 : value = value - 4000: end if
		if(value >= 3000)  then:  SetReel nplayer, 2 , 3 : SetScore nplayer,1,3 : value = value - 3000: end if
		if(value >= 2000)  then:  SetReel nplayer, 2 , 2 : SetScore nplayer,1,2 : value = value - 2000: end if
		if(value >= 1000)  then:  SetReel nplayer, 2 , 1 : SetScore nplayer,1,1 : value = value - 1000: end if

		'do hundreds
		
		if(value >= 900) then: SetReel nplayer, 3 , 9 : SetScore nplayer,2,9 : value = value - 900: end if
		if(value >= 800) then: SetReel nplayer, 3 , 8 : SetScore nplayer,2,8 : value = value - 800: end if
		if(value >= 700) then: SetReel nplayer, 3 , 7 : SetScore nplayer,2,7 : value = value - 700: end if
		if(value >= 600) then: SetReel nplayer, 3 , 6 : SetScore nplayer,2,6 : value = value - 600: end if
		if(value >= 500) then: SetReel nplayer, 3 , 5 : SetScore nplayer,2,5 : value = value - 500: end if
		if(value >= 400) then: SetReel nplayer, 3 , 4 : SetScore nplayer,2,4 : value = value - 400: end if
		if(value >= 300) then: SetReel nplayer, 3 , 3 : SetScore nplayer,2,3 : value = value - 300: end if
		if(value >= 200) then: SetReel nplayer, 3 , 2 : SetScore nplayer,2,2 : value = value - 200: end if
		if(value >= 100) then: SetReel nplayer, 3 , 1 : SetScore nplayer,2,1 : value = value - 100: end if

		'do tens
		if(value >= 90) then: SetReel nplayer, 4 , 9 : SetScore nplayer,3,9 : value = value - 90: end if
		if(value >= 80) then: SetReel nplayer, 4 , 8 : SetScore nplayer,3,8 : value = value - 80: end if
		if(value >= 70) then: SetReel nplayer, 4 , 7 : SetScore nplayer,3,7 : value = value - 70: end if
		if(value >= 60) then: SetReel nplayer, 4 , 6 : SetScore nplayer,3,6 : value = value - 60: end if
		if(value >= 50) then: SetReel nplayer, 4 , 5 : SetScore nplayer,3,5 : value = value - 50: end if
		if(value >= 40) then: SetReel nplayer, 4 , 4 : SetScore nplayer,3,4 : value = value - 40: end if
		if(value >= 30) then: SetReel nplayer, 4 , 3 : SetScore nplayer,3,3 : value = value - 30: end if
		if(value >= 20) then: SetReel nplayer, 4 , 2 : SetScore nplayer,3,2 : value = value - 20: end if
		if(value >= 10) then: SetReel nplayer, 4 , 1 : SetScore nplayer,3,1 : value = value - 10: end if

		'do ones
		if(value >= 9) then: SetReel nplayer, 5 , 9 : SetScore nplayer,4,9 : value = value - 9: end if
		if(value >= 8) then: SetReel nplayer, 5 , 8 : SetScore nplayer,4,8 : value = value - 8: end if
		if(value >= 7) then: SetReel nplayer, 5 , 7 : SetScore nplayer,4,7 : value = value - 7: end if
		if(value >= 6) then: SetReel nplayer, 5 , 6 : SetScore nplayer,4,6 : value = value - 6: end if
		if(value >= 5) then: SetReel nplayer, 5 , 5 : SetScore nplayer,4,5 : value = value - 5: end if
		if(value >= 4) then: SetReel nplayer, 5 , 4 : SetScore nplayer,4,4 : value = value - 4: end if
		if(value >= 3) then: SetReel nplayer, 5 , 3 : SetScore nplayer,4,3 : value = value - 3: end if
		if(value >= 2) then: SetReel nplayer, 5 , 2 : SetScore nplayer,4,2 : value = value - 2: end if
		if(value >= 1) then: SetReel nplayer, 5 , 1 : SetScore nplayer,4,1 : value = value - 1: end if


		end if

	Next
End Sub 

'******************************************************
'*******	       VR Plunger	        *******
'******************************************************

Sub TimerVRPlunger_Timer
  If VR_Primary_plunger.Y < 2185.833 then
       VR_Primary_plunger.Y = VR_Primary_plunger.Y + 4
  End If
End Sub

Sub TimerVRPlunger2_Timer
	VR_Primary_plunger.Y = 2070.833 + (5* Plunger.Position) -20
End Sub


'******************************************************
'*******     VR Backglass Lighting    *******
'******************************************************

Sub FlasherMatch
	If Match = 100 Then FlM00.visible = 1 : FlM00A.visible = 1 : FlM00B.visible = 1 Else FlM00.visible = 0 : FlM00A.visible = 0 : FlM00B.visible = 0  End If
	If Match = 10 Then FlM10.visible = 1 : FlM10A.visible = 1 : FlM10B.visible = 1 Else FlM10.visible = 0 : FlM10A.visible = 0 : FlM10B.visible = 0 End If
	If Match = 20 Then FlM20.visible = 1 : FlM20A.visible = 1 : FlM20B.visible = 1 Else FlM20.visible = 0 : FlM20A.visible = 0 : FlM20B.visible = 0 End If
	If Match = 30 Then FlM30.visible = 1 : FlM30A.visible = 1 : FlM30B.visible = 1 Else FlM30.visible = 0 : FlM30A.visible = 0 : FlM30B.visible = 0 End If
	If Match = 40 Then FlM40.visible = 1 : FlM40A.visible = 1 : FlM40B.visible = 1 Else FlM40.visible = 0 : FlM40A.visible = 0 : FlM40B.visible = 0 End If
	If Match = 50 Then FlM50.visible = 1 : FlM50A.visible = 1 : FlM50B.visible = 1 Else FlM50.visible = 0 : FlM50A.visible = 0 : FlM50B.visible = 0 End If
	If Match = 60 Then FlM60.visible = 1 : FlM60A.visible = 1 : FlM60B.visible = 1 Else FlM60.visible = 0 : FlM60A.visible = 0 : FlM60B.visible = 0 End If
	If Match = 70 Then FlM70.visible = 1 : FlM70A.visible = 1 : FlM70B.visible = 1 Else FlM70.visible = 0 : FlM70A.visible = 0 : FlM70B.visible = 0 End If
	If Match = 80 Then FlM80.visible = 1 : FlM80A.visible = 1 : FlM80B.visible = 1 Else FlM80.visible = 0 : FlM80A.visible = 0 : FlM80B.visible = 0 End If
	If Match = 90 Then FlM90.visible = 1 : FlM90A.visible = 1 : FlM90B.visible = 1 Else FlM90.visible = 0 : FlM90A.visible = 0 : FlM90B.visible = 0 End If
End Sub

Sub FlasherBalls
	If BallInPlay = 1 Then FlBIP1.visible = 1 : FlBIP1A.visible = 1 Else FlBIP1.visible = 0 : FlBIP1A.visible = 0 End If
	If BallInPlay = 2 Then FlBIP2.visible = 1 : FlBIP2A.visible = 1 Else FlBIP2.visible = 0 : FlBIP2A.visible = 0 End If
	If BallInPlay = 3 Then FlBIP3.visible = 1 : FlBIP3A.visible = 1 Else FlBIP3.visible = 0 : FlBIP3A.visible = 0 End If
	If BallInPlay = 4 Then FlBIP4.visible = 1 : FlBIP4A.visible = 1 Else FlBIP4.visible = 0 : FlBIP4A.visible = 0 End If
	If BallInPlay = 5 Then FlBIP5.visible = 1 : FlBIP5A.visible = 1 Else FlBIP5.visible = 0 : FlBIP5A.visible = 0 End If
End Sub

Sub FlasherPlayers
	If Players = 1 Then FlPl1.visible = 1 Else FlPl1.visible = 0 End If
	If Players = 2 Then FlPl2.visible = 1 Else FlPl2.visible = 0 End If
	If Players = 3 Then FlPl3.visible = 1 Else FlPl3.visible = 0 End If
	If Players = 4 Then FlPl4.visible = 1 Else FlPl4.visible = 0 End If
End Sub

Sub FlasherCurrentPlayer
	If Player = 1 Then : for each Object in VRBGPlayer1 : object.visible = 1 : Next: Else : for each Object in VRBGPlayer1 : object.visible = 0 :Next
	If Player = 2 Then : for each Object in VRBGPlayer2 : object.visible = 1 : Next: Else : for each Object in VRBGPlayer2 : object.visible = 0 :Next
	If Player = 3 Then : for each Object in VRBGPlayer3 : object.visible = 1 : Next: Else : for each Object in VRBGPlayer3 : object.visible = 0 :Next
	If Player = 4 Then : for each Object in VRBGPlayer4 : object.visible = 1 : Next: Else : for each Object in VRBGPlayer4 : object.visible = 0 :Next
End Sub

Sub Flasher100k
	If Player = 1 Then for each Object in VRBG100k1 : object.visible = 1 : next
	If Player = 2 Then for each Object in VRBG100k2 : object.visible = 1 : next
	If Player = 3 Then for each Object in VRBG100k3 : object.visible = 1 : next
	If Player = 4 Then for each Object in VRBG100k4 : object.visible = 1 : next
	If Player = 4 Then for each Object in VRBG100k4 : object.visible = 1 : next
End Sub

Dim OTT1, OTT2, OTT3, OTT4

Sub FlasherOverTheTop
	If Player = 1 Then
		If OTT1 = 0 Then
			If VRRoom > 0 Then
				for each Object in VRBGOTT : object.visible = 1 : Next
				TimerOTT.enabled = 1
			End If
			Playsound "BuzzerOTT"
			OTT1 = 1
		End If
	End If
	If Player = 2 Then
		If OTT2 = 0 Then
			If VRRoom > 0 Then
				for each Object in VRBGOTT : object.visible = 1 : Next
				TimerOTT.enabled = 1
			End If
			Playsound "BuzzerOTT"
			OTT2 = 1
		End If
	End IF
	If Player = 3 Then
		If OTT3 = 0 Then
			If VRRoom > 0 Then
				for each Object in VRBGOTT : object.visible = 1 : Next
				TimerOTT.enabled = 1
			End If
			Playsound "BuzzerOTT"
			OTT3 = 1
		End If
	End IF
	If Player = 4 Then
		If OTT4 = 0 Then
			If VRRoom > 0 Then
				for each Object in VRBGOTT : object.visible = 1 : Next
				TimerOTT.enabled = 1
			End If
			Playsound "BuzzerOTT"
			OTT4 = 1
		End If
	End IF
End Sub
		
Sub TimerOTT_Timer
	for each Object in VRBGOTT : object.visible = 0 : Next
	TimerOTT.enabled = False
End Sub


If VRRoom > 0 Then
	SetBackglass
	center_objects_em
	for each Object in VRCabinet : object.visible = 1 : next
	If VRRoom = 1 Then
		for each Object in VRRoomMinimal : object.visible = 1 : next
		BeerTimer.enabled = False
		ClockTimer1.enabled = False
	End If
	If VRRoom = 2 Then
		for each Object in VRRoomFancy : object.visible = 1 : next
		BeerTimer.enabled = True
		ClockTimer1.enabled = True
		VRFrontRightLeg.z = 45
		VRLeftFrontLeg.z = 45
		VRBackRightLeg.z = 60
		VRBackLeftLeg.z = 60		
	End If
	
	vn_CabinetGroupVR.visible = 1
	vn_CabinetGroup.visible = 0

	For each obj in DesktopCrap
		obj.visible=False
	next

Else

	for each Object in VRCabinet : object.visible = 0 : next
	for each Object in VRBGAll : object.visible = 0 : next
	TimerVRPlunger.Enabled = False
	TimerVRPlunger2.Enabled = False
	BeerTimer.enabled = False
	ClockTimer1.enabled = False
End If

' ***** Beer Bubble Code - Rawd *****
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



' ***************** VR Clock code below - THANKS RASCAL ******************
Dim CurrentMinute ' for VR clock 
' VR Clock code below....
Sub ClockTimer1_Timer()
	Pminutes.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6
	Phours.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2)
    Pseconds.RotAndTra2 = (Second(Now()))*6
	CurrentMinute=Minute(Now())
End Sub
 ' ********************** END CLOCK CODE   *********************************

